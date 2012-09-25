###############################################################################
# Merge DHS datafiles for use in R.
library(foreign)
library(rgdal)
library(maptools)
library(reshape)

source("0_data_path.R")

processed_data_dir <- paste(base_dir, "Processed_data", sep="/")
merged_data_dir <- paste(base_dir, "Merged_data", sep="/")
debug_data_dir <- paste(base_dir, "Debug_data", sep="/")

###############################################################################
# Read in and merge the statcompiler datasets.
###############################################################################
data_filenames <- dir(processed_data_dir)[grepl("_processed.Rdata",
        dir(processed_data_dir))]
load(paste(processed_data_dir, data_filenames[1], sep="/"))
merged_data_long <- DHS_data
for (file in data_filenames[2:length(data_filenames)]) {
    load(paste(processed_data_dir, file, sep="/"))
    merged_data_long <- merge(merged_data_long, DHS_data, all=TRUE, sort=TRUE)
}

###############################################################################
# Handle country key file
###############################################################################
# The country key assigns a continent and ISO code to each country, for use in 
# analyzing results by continent.
country_key <- read.csv("Country_Key/DHS_country_key.csv", encoding="latin1", 
                        stringsAsFactors=FALSE)
merged_data_long <- merge(country_key, merged_data_long, all=TRUE)
# Make up a country code for Tibet since DHS lists it with a missing code.
merged_data_long$CC[merged_data_long$Country == "Tibet"] <- "TB"
merged_data_long$CC_3[merged_data_long$Country == "Tibet"] <- "TIB"

###############################################################################
# Pre-cleaning and conversion to wide format
###############################################################################
topic_rename_key <- read.csv("Variable_Rename_Key/topic_rename_key.csv", stringsAsFactors=FALSE)
merged_data_long$Topic_Short <- merged_data_long$Topic
for (row_num in 1:nrow(topic_rename_key)) {
    long_topic <- topic_rename_key[row_num, ]$Topic_Long
    short_topic <- topic_rename_key[row_num, ]$Topic_Short
    merged_data_long$Topic_Short[merged_data_long$Topic_Short == long_topic] <- short_topic
}

merged_data_long <- merged_data_long[!is.na(merged_data_long$Value), ]
merged_data_long <- merged_data_long[!is.na(merged_data_long$Characteristic), ]
merged_data_long <- merged_data_long[!(merged_data_long$Characteristic == ""), ]

merged_data_long$By.Variable[is.na(merged_data_long$By.Variable)] <- ""
merged_data_long$Var_name <- with(merged_data_long,
                                  paste(Topic_Short,
                                        abbreviate(gsub('[-().]', ' ', Table)), 
                                        abbreviate(gsub('[-().]', ' ', 
                                                        Indicator)), 
                                        abbreviate(gsub('[-().]', ' ', 
                                                        By.Variable)), 
                                        sep="_"))
merged_data_long$Var_name <- gsub(' ', '_', merged_data_long$Var_name)
# Strip the following underscores on Var_names for which the By.Variable is 
# undefined
merged_data_long$Var_name <- gsub('_$', '', merged_data_long$Var_name)
var_name_key <- with(merged_data_long, data.frame(Var_name, Topic, Table, 
                                                  Indicator))
var_name_key <- var_name_key[!duplicated(var_name_key$Var_name), ]
var_name_key <- with(var_name_key, var_name_key[order(Var_name, Topic, Table, 
                                                      Indicator), ])
write.csv(var_name_key, file=paste(merged_data_dir, "variable_name_key.csv", 
                                   sep="/"), row.names=FALSE)
dupe_rows <- duplicated(merged_data_long)

if (sum(dupe_rows) > 0) {
    warning(paste("Removing", sum(dupe_rows), "duplicated rows from 
                  merged_data_long"))
}
merged_data_long <- merged_data_long[!duplicated(merged_data_long), ]

# Add an ID variable uniquely identifying each characteristic row. Note that 
# 'Characteristic' must be converted to ASCII, with blanks substituted for 
# characters that can't be converted. This is because the 'abbreviate' function 
# cannot handle strings that are not in ASCII.
merged_data_long$RowID <- with(merged_data_long,
                               paste(CC_3, abbreviate(Survey), 
                                     abbreviate(Category), 
                                     abbreviate(iconv(Characteristic, 
                                                      to="ASCII", sub="")), 
                                     sep="_"))

merged_data  <- recast(merged_data_long, RowID + Country + Continent + 
                              CC + CC_3 + Year + Survey + Category + 
                              Characteristic + Characteristic.parent ~ 
                              Var_name, measure.var="Value")
###############################################################################
# Post-merge cleaning
###############################################################################

# Eliminate a data entry error (WFRt_Wnfr_Wtfr of 151.8)
merged_data$WFRt_Wnfr_Wtfr[merged_data$WFRt_Wnfr_Wtfr>20] <- NA

###############################################################################
# Handle region shapefile file
###############################################################################
# The region shapefile assigns the conservation zone percent in, area in, etc. 
# and region areas to the regional data.
DHS_regions <- readOGR(paste(base_dir, "Shapefiles", sep="/"), "DHS_Regions")
# Fix the zero values for AreaIn variables - when read from the DHS_regions 
# there are NAs in all the places that should be zero for these variables
DHS_regions$WWFInPct[is.na(DHS_regions$WWFInPct)] <- 0
DHS_regions$WWFInkm[is.na(DHS_regions$WWFInkm)] <- 0
DHS_regions$CIInPct[is.na(DHS_regions$CIInPct)] <- 0
DHS_regions$CIInkm[is.na(DHS_regions$CIInkm)] <- 0

# Drop the Country field from the DHS Regions dataset so it doesn't get 
# duplicated during the merge (the CC_3 field will be used for the matching).
DHS_regions <- DHS_regions[, names(DHS_regions) != "Country"]

# Get a subset of just the regional data to deal with the merging. Remove these 
# rows from the main dataset, as they will be added back after the merge.
regional_data <- merged_data[merged_data$Category %in% c("Region", "Sub-region"), ]
non_regional_data <- merged_data[!(merged_data$Category %in% c("Region", "Sub-region")), ]

# Need to remove from the regional data any rows where overlapping sub-regional 
# and regional data are listed (where data is listed twice for the same 
# geographic area, at two different spatial scales). Do this by first making a 
# list of all upper level region names for which lower level data is listed.  
# Then remove all rows where the group type is "Region" (the highest level) and 
# the group name is in the list of upper-level regions for which lower-level 
# data is also listed.
# To deal with the case where there are two upper-level regions within the 
# dataset that have the same name, make a vector combining country, survey, and 
# region name all into a single field "combined_names".
combined_names <- paste(regional_data$Country, regional_data$Survey,
        regional_data$Characteristic)
parent_regions_combined_names <- paste(regional_data$Country, regional_data$Survey,
        regional_data$Characteristic.parent)
# Now use the combined names to remove regions where there are overlapping 
# higher and lower-level regions.
regional_data <- regional_data[!((regional_data$Category == "Region") &
    (combined_names %in% parent_regions_combined_names)), ]

regional_data_pre_merge <- regional_data
write.csv(regional_data_pre_merge, file=paste(debug_data_dir, 
                                              "regional_data_pre_merge.csv", 
                                              sep="/"))

# Do two merges, then rbind them together. The first will cover the rows where 
# each survey year has specific GIS boundary data, the second will cover rows 
# where all survey years are covered by the same boundary data.
regional_data_year_specific_rows <- DHS_regions$GISDataYr != "All" & DHS_regions$GISDataYr != ""
regional_data_other_rows <- DHS_regions$GISDataYr == "All" | DHS_regions$GISDataYr == ""

merge_one <- merge(regional_data, DHS_regions[regional_data_year_specific_rows,],
        by.x=c("CC_3", "Characteristic", "Year"),
        by.y=c("CountryISO","Region", "GISDataYr"))
# Add in a GISDataYr column to merge_one since it was eliminated in the merge 
# (since for these rows the Year and GISDataYr are the same).
merge_one <- cbind(merge_one, GISDataYr=merge_one$Year)
merge_two <- merge(regional_data, DHS_regions[regional_data_other_rows,],
        by.x=c("CC_3", "Characteristic"),
        by.y=c("CountryISO","Region"))
regional_data <- rbind(merge_one, merge_two)

# Correct NAs due to error in Nambia - Otjozondjupa region. Otjozondjupa falls 
# in the Nambia WWF conservation zone, and is roughly the same distance from 
# the CI zones as the Namibia - Omaheke region.  TODO: calculate the correct 
# distance in the GIS shapefile.
#otjo_row <- which((regional_data$CC_3 == "NAM") &
#        (regional_data$Group.lowest == "Otjozondjupa"))
#regional_data[otjo_row,]$WWFNrName <- "Namibia"
#regional_data[otjo_row,]$WWFNrkm <- 0
#regional_data[otjo_row,]$CINrName <- "Succulent Karoo"
#regional_data[otjo_row,]$CINrkm <- 905824.171767

pre_merge_ISO_characteristic <- paste(regional_data_pre_merge$CC_3, regional_data_pre_merge$Characteristic)
post_merge_ISO_characteristic <- paste(regional_data$CC_3, regional_data$Characteristic)
unmatched_regions <- regional_data_pre_merge[!(pre_merge_ISO_characteristic %in% post_merge_ISO_characteristic),]
write.csv(unmatched_regions, file=paste(debug_data_dir, 
                                        "unmatched_regions.csv", sep="/"))

# Now add the regional rows back to the main dataset.
merged_data <- merge(regional_data, non_regional_data, all=TRUE)

merged_data <- merged_data[names(merged_data) != "DHSSubrgn"]

# Ensure the Country fields are all filled out properly.  
merged_data$Country <- country_key$Country[match(merged_data$CC_3,
        country_key$CC_3)]

# Sort by Continent, then Country, etc., 
merged_data <- merged_data[order(merged_data$Continent, merged_data$Country,
        merged_data$Year, merged_data$Survey, merged_data$Category, 
        merged_data$Characteristic), ]

if (sum(duplicated(merged_data$RowsID)) > 0) {
    stop("RowID values in merged_data are not unique")
}

###############################################################################
# Add in the SDT and wealth indicators (these apply to regional data only).
###############################################################################
regional_data <- merged_data[merged_data$Category %in% c("Region", "Sub-region"),]
other_data <- merged_data[!(merged_data$Category %in% c("Region", "Sub-region")),]
# For the wealth and SDT indicators, need a function to compute quartile 
# rankings:
qrank <- function(values, rev=FALSE) {
    quarts <- quantile(values, c(.25, .5, .75, 1), na.rm=TRUE)
    ranks <- rep(NA, length(values))
    if (rev==TRUE) rankorder <- 4:1
    else rankorder <- 1:4
    for (n in 1:4) {
        ranks[(values <= quarts[n]) & is.na(ranks)] <- rankorder[n]
    } 
    return(ranks)
}

###############################################################################
# Add SDT columns
# SDT_Fert is the average of the actual TFR and the wanted TFR
SDT_Fert <- (regional_data$WFRt_Wnfr_Wtfr + regional_data$FRt_Frtr_Ttfr) / 2

# SDT_Fert_Q is the adjusted quartile ranking of SDT_Fert. Rows in the first 
# quartile are assigned an SDT_Fert_Q of 4, rows in the second quartile an 
# SDT_Fert_Q of 3, etc, so use the rev=TRUE option to qrank.
SDT_Fert_Q <- qrank(SDT_Fert, rev=TRUE)

# Infant mortality rate is "Mrtr_Im10". Again, lower percentages indicate higher 
# level of demog. transition, so use the rev=TRUE ranking.
SDT_IMR_Q <- qrank(regional_data$ICM_Mrtr_Im10, rev=TRUE)

# SDT_Pop15 is an indicator of population momentum, the percentage of the 
# population under age 15. Again, lower percentages indicate higher level of 
# demog. transition, so use the rev=TRUE ranking.
SDT_Pop15 <- with(regional_data, HHPop_Hsma_0_4_Totl + HHPop_Hsma_5_9_Totl +
        HHPop_Hsma_1014_Totl)
SDT_Pop15_Q <- qrank(SDT_Pop15, rev=TRUE)

# To correct for missing data, average the available quartile rankings for each 
# row and multiply by 3 to retain the 3-12 scaling.
SDT <- apply(cbind(SDT_Fert_Q, SDT_IMR_Q, SDT_Pop15_Q), 1, mean,
        na.rm=TRUE)
SDT[is.nan(SDT)] <- NA #NaNs result from missing quartile ranks

# SDT_Missing indicates the number of rankings missing for each row.
SDT_Missing <- is.na(SDT_Fert_Q) + is.na(SDT_IMR_Q) + is.na(SDT_Pop15_Q)

# Add predicted rates of natural increase (RNI) and doubling times (Td). This 
# means of projecting RNI and Td was devised by David, and uses the observed 
# correlation between the log of the TFR and the RNI to predict RNI and Td when 
# the TFR is known.
obs_pop_data <- read.csv("RNI_prediction/Observed_RNI.csv")
RNI_model <- lm(RNI ~ LN_TFR, data=obs_pop_data)
DHS_log_obs_TFR <- data.frame(LN_TFR=log(regional_data$FRt_Frtr_Ttfr))

RNI_pred <- predict(RNI_model, DHS_log_obs_TFR)
RNI_pred[is.infinite(RNI_pred)] <- NA
# Calculate the predicted doubling times as log(2) / log(1+r/100)
Td_pred <- log(2) / log(1 + (RNI_pred / 100))

regional_data <- cbind(regional_data, SDT, RNI_pred, Td_pred, HHPop_Hsma_0_15_Totl=SDT_Pop15)

###############################################################################
# Now make a wealth index based on percentage ownership of:
#   Bicycle
#   Motorcycle
#   Privatecar
#   Radio
#   Telephone
#   Television
#   Refrigerator
#   None of the above
Possess_Rankings <- qrank(regional_data$DrbGds_Hshp_Notp, rev=TRUE)
Possess_Rankings <- cbind(Possess_Rankings, qrank(regional_data$DrbGds_Hshp_Bcyc))
Possess_Rankings <- cbind(Possess_Rankings, qrank(regional_data$DrbGds_Hshp_Mtrc))
Possess_Rankings <- cbind(Possess_Rankings, qrank(regional_data$DrbGds_Hshp_Prvc))
Possess_Rankings <- cbind(Possess_Rankings, qrank(regional_data$DrbGds_Hshp_Radi))
Possess_Rankings <- cbind(Possess_Rankings, qrank(regional_data$DrbGds_Hshp_Tlph))
Possess_Rankings <- cbind(Possess_Rankings, qrank(regional_data$DrbGds_Hshp_Tlvs))
Possess_Rankings <- cbind(Possess_Rankings, qrank(regional_data$DrbGds_Hshp_Rfrg))
Possess_Missings <- apply(is.na(Possess_Rankings), 1, sum)
# To correct for missing data, average the available quartile rankings for each 
# row and subtract 1 to have scale range from 1-4
Wealth <- apply(Possess_Rankings, 1, mean, na.rm=TRUE)

regional_data <- cbind(regional_data, Wealth)

###############################################################################
# Now add the regional data back in to the main dataset.
merged_data <- merge(other_data, regional_data, all=TRUE)

# Make a version of the regional DHS data that can be saved as an R 
# SpatialPolygonsDataFrame and as a shapefile.
DHS_regions <- DHS_regions[match(regional_data$GISPolyID, DHS_regions$GISPolyID),]
# Renumber the FIDs of the DHS_regions dataset to prevent non-unique row-names
DHS_regions <- spChFIDs(DHS_regions, as.character(1:nrow(DHS_regions)))
row.names(regional_data) <- row.names(DHS_regions)
DHS_regions <- DHS_regions[-c(1:ncol(DHS_regions))]
regional_data <- spCbind(DHS_regions, regional_data)

###############################################################################
# Write out data
###############################################################################
# Reorder the columns so the order makes more sense
first_columns <- c("RowID", "CC", "CC_3", "Country", "Continent", "Survey", 
                   "Year", "GISDataYr", "GISPolyID", "Category", 
                   "Characteristic", "Characteristic.parent", "EnteredBy", 
                   "Perimkm", "Areakm", "WWFInName", "WWFInkm", 
                   "WWFInPct","WWFNrName", "WWFNrkm", "CIInName", "CIInkm", 
                   "CIInPct", "CINrName", "CINrkm", "WDPAIInNm", "WDPAIInkm", 
                   "WDPAIInPct", "WDPANInNm", "WDPANInkm", "WDPANInPct", "SDT", 
                   "RNI_pred", "Td_pred", "Wealth")

merged_data_beg_cols <- merged_data[first_columns]
merged_data_end_cols <- merged_data[!(names(merged_data) %in% first_columns)]
merged_data_end_cols <- merged_data_end_cols[sort(names(merged_data_end_cols))]
merged_data <- cbind(merged_data_beg_cols, merged_data_end_cols)

regional_data_beg_cols <- regional_data[first_columns]
regional_data_end_cols <- as.data.frame(regional_data[!(names(regional_data) %in% first_columns)])
regional_data_end_cols <- regional_data_end_cols[sort(names(regional_data_end_cols))]
regional_data <- spCbind(regional_data_beg_cols, regional_data_end_cols)

# Reorder the Year and GISDataYr factors to ensure the levels are in 
# chronological order.
merged_data$Year <- as.ordered(as.character(merged_data$Year))
merged_data$GISDataYr <- as.ordered(as.character(merged_data$GISDataYr))

save(merged_data, file=paste(merged_data_dir, "DHS_merged_data.Rdata", 
                             sep="/"))
write.csv(merged_data, file=paste(merged_data_dir, "DHS_merged_data.csv", 
                                  sep="/"), row.names=FALSE, na=".")
#write.dta(merged_data, file=paste(merged_data_dir, 
#"DHS_merged_data.dta", sep="/"))

# Save the spatial dataframe and shapefile
save(regional_data, file=paste(merged_data_dir, 
                               "DHS_merged_data_regional_sp.Rdata", sep="/"))
#writeOGR(regional_data, paste(merged_data_dir, "DHS_merged_data_regional_shapefile", sep="/")
#"ESRI Shapefile")
