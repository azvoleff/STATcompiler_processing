###############################################################################
# Merge DHS datafiles for use in R.
require(foreign)
require(rgdal)
require(maptools)

base_dir <- "M:/Data/Global/DHS/Statcompiler_Processing"

processed_data_dir <- paste(base_dir, "Processed_data", sep="/")
merged_data_dir <- paste(base_dir, "Merged_data", sep="/")
debug_data_dir <- paste(base_dir, "Debug_data", sep="/")

data_filenames <- dir(processed_data_dir)[grepl("_processed.Rdata",
        dir(processed_data_dir))]

load(paste(processed_data_dir, data_filenames[1], sep="/"))
merged_data <- DHS_data
# Delete the footnote column before merging the datasets because it can lead to 
# additional rows being mistakenly added for matching groups if a country has 
# two differently numbered footnotes for different surveys.
merged_data <- DHS_data[-grep("Footnote", names(DHS_data))]

###############################################################################
# Read in and merge the statcompiler datasets.
###############################################################################

for (file in data_filenames[2:length(data_filenames)]) {
    load(paste(processed_data_dir, file, sep="/"))
    DHS_data <- DHS_data[-grep("Footnote", names(DHS_data))]
    merged_data <- merge(merged_data, DHS_data, all=TRUE, sort=TRUE)
}

###############################################################################
# Handle country key file
###############################################################################
# The country key assigns a continent and ISO code to each country, for use in 
# analyzing results by continent.
country_key <- read.csv("Country_Key/DHS_country_key.csv", encoding="latin1", 
                        stringsAsFactors=FALSE)
merged_data <- merge(country_key, merged_data)

###############################################################################
# Do final data cleaning.
###############################################################################

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

#GISPolyID <- paste(DHS_regions$CountryISO, DHS_regions$Region, 
#                   DHS_regions$GISDataYr, sep="-")
#DHS_regions <- spCbind(DHS_regions, GISPolyID)

# Drop the Country field from the DHS Regions dataset so it doesn't get 
# duplicated during the merge (the CountryISO field will be used for the 
# matching).
DHS_regions <- DHS_regions[,names(DHS_regions) != "Country"]

# Get a subset of just the regional data to deal with the merging. Remove these 
# rows from the main dataset, as they will be added back after the merge.
regional_data <- merged_data[merged_data$Group.Type %in% c("Region", "Sub-region"),]
merged_data <- merged_data[!(merged_data$Group.Type %in% c("Region", "Sub-region")),]

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

# Before doing this, take care of Egypt - don't use the sub-regions in Egypt, 
# only upper level regions (see Will Anderson's notes on this).
regional_data <- regional_data[!((regional_data$Group.Type == "Sub-region") &
    (regional_data$CountryISO == "EGY")),]

combined_names <- paste(regional_data$Country, regional_data$Survey_Year,
        regional_data$Region)
# Now use the combined names to remove regions where there are overlapping 
# higher and lower-level regions.
higher.regions.combined_names <- unique(combined_names[!is.na(regional_data$Group.sub)])
regional_data <- regional_data[!((regional_data$Group.Type == "Region") &
    (combined_names %in% higher.regions.combined_names)),]

regional_data_pre_merge <- regional_data
write.csv(regional_data_pre_merge, file=paste(debug_data_dir, 
                                              "regional_data_pre_merge.csv", 
                                              sep="/"))

# Do two merges, then rbind them together. The first will cover the rows where 
# each survey year has specific GIS boundary data, the second will cover rows 
# where all survey years are covered by the same boundary data.
regional_data_year_specific_rows <- DHS_regions$GISDataYr!="All" & DHS_regions$GISDataYr!=""
regional_data_other_rows <- DHS_regions$GISDataYr=="All" | DHS_regions$GISDataYr==""

merge_one <- merge(regional_data, DHS_regions[regional_data_year_specific_rows,],
        by.x=c("CountryISO", "Group.lowest", "Survey_Year"),
        by.y=c("CountryISO","Region", "GISDataYr"))
# Add in a GISDataYr column to merge_one since it was eliminated in the 
# merge (since for these rows the Survey_Year and GISDataYr are the same).
merge_one <- cbind(merge_one, GISDataYr=merge_one$Survey_Year)
merge_two <- merge(regional_data, DHS_regions[regional_data_other_rows,],
        by.x=c("CountryISO", "Group.lowest"),
        by.y=c("CountryISO","Region"))
merged_regional_data <- rbind(merge_one, merge_two)
regional_data <- merged_regional_data

# Correct NAs due to error in Nambia - Otjozondjupa region. Otjozondjupa falls 
# in the Nambia WWF conservation zone, and is roughly the same distance from 
# the CI zones as the Namibia - Omaheke region.  TODO: calculate the correct 
# distance in the GIS shapefile.
#otjo_row <- which((regional_data$CountryISO == "NAM") &
#        (regional_data$Group.lowest == "Otjozondjupa"))
#regional_data[otjo_row,]$WWFNrName <- "Namibia"
#regional_data[otjo_row,]$WWFNrkm <- 0
#regional_data[otjo_row,]$CINrName <- "Succulent Karoo"
#regional_data[otjo_row,]$CINrkm <- 905824.171767

pre_merge_ISO_region <- paste(regional_data_pre_merge$CountryISO, regional_data_pre_merge$Group.lowest)
post_merge_ISO_region <- paste(merged_regional_data$CountryISO, merged_regional_data$Group.lowest)
unmatched_regions <- regional_data_pre_merge[!(pre_merge_ISO_region %in% post_merge_ISO_region),]
write.csv(unmatched_regions, file=paste(debug_data_dir, 
                                        "unmatched_regions.csv", sep="/"))

# Now add the regional rows back to the main dataset.
merged_data <- merge(regional_data, merged_data, all=TRUE)

merged_data <- merged_data[names(merged_data) != "DHSSubrgn"]

# Ensure the Country fields are all filled out properly.  
merged_data$Country <- country_key$Country[match(merged_data$CountryISO,
        country_key$CountryISO)]

# Sort by Continent, then Country, etc., 
merged_data <- merged_data[order(merged_data$Continent, merged_data$Country,
        merged_data$Survey_Year, merged_data$Group.Type, merged_data$Group,
        merged_data$Group.sub),]

###############################################################################
# Add in the SDT and wealth indicators (these apply to regional data only).
###############################################################################
regional_data <- merged_data[merged_data$Group.Type %in% c("Region", "Sub-region"),]
merged_data <- merged_data[!(merged_data$Group.Type %in% c("Region", "Sub-region")),]
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
SDT_Fert <- (regional_data$Wan_tot_fert_r + regional_data$Tot_fert_r) / 2
# Eliminate a data entry error (SDT_Fert of 154.15)
SDT_Fert[SDT_Fert>20] <- NA

# SDT_Fert_Q is the adjusted quartile ranking of SDT_Fert.
#

# Rows in the first quartile are assigned an SDT_Fert_Q of 4, rows in the 
# second quartile an SDT_Fert_Q of 3, etc, so use the rev=TRUE option to qrank.
SDT_Fert_Q <- qrank(SDT_Fert, rev=TRUE)

# Infant mortality rate is "mort_1q0". Again, lower percentages indicate higher 
# level of demog. transition, so use the rev=TRUE ranking.
SDT_IMR_Q <- qrank(regional_data$mort_1q0, rev=TRUE)

# SDT_Pop15 is an indicator of population momentum, the percentage of the 
# population under age 15. Again, lower percentages indicate higher level of 
# demog. transition, so use the rev=TRUE ranking.
SDT_Pop15 <- with(regional_data, HH_Pop_Total_0_4 + HH_Pop_Total_5_9 +
        HH_Pop_Total_10_14)
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
# First: Ignore an erroneous TFR of 156.5 in India, by setting it to NA
regional_data$Tot_fert_r[regional_data$Tot_fert_r > 20] <- NA
obs_pop_data <- read.csv("RNI_prediction/Observed_RNI.csv")
RNI_model <- lm(RNI ~ LN_TFR, data=obs_pop_data)
DHS_obs_TFR <- data.frame(LN_TFR=log(regional_data$Tot_fert_r))

RNI_pred <- predict(RNI_model, DHS_obs_TFR)
RNI_pred[is.infinite(RNI_pred)] <- NA
# Calculate the predicted doubling times as log(2) / log(1+r/100)
Td_pred <- log(2) / log(1 + (RNI_pred / 100))

regional_data <- cbind(regional_data, SDT, RNI_pred, Td_pred, HH_Pop_Total_0_15=SDT_Pop15)

# Now make a wealth index based on percentage ownership of:
#   Bicycle
#   Motorcycle
#   Privatecar
#   Radio
#   Telephone
#   Television
#   Refrigerator
#   None of the above
Possess_Rankings <- qrank(regional_data$Possess_None, rev=TRUE)
Possess_Rankings <- cbind(Possess_Rankings, qrank(regional_data$Possess_Bicycle))
Possess_Rankings <- cbind(Possess_Rankings, qrank(regional_data$Possess_Motorcycle))
Possess_Rankings <- cbind(Possess_Rankings, qrank(regional_data$Possess_Privatecar))
Possess_Rankings <- cbind(Possess_Rankings, qrank(regional_data$Possess_Radio))
Possess_Rankings <- cbind(Possess_Rankings, qrank(regional_data$Possess_Telephone))
Possess_Rankings <- cbind(Possess_Rankings, qrank(regional_data$Possess_Television))
Possess_Rankings <- cbind(Possess_Rankings, qrank(regional_data$Possess_Refrigerator))

Possess_Missings <- apply(is.na(Possess_Rankings), 1, sum)

# To correct for missing data, average the available quartile rankings for each 
# row and subtract 1 to have scale range from 1-4
Wealth <- apply(Possess_Rankings, 1, mean, na.rm=TRUE)

regional_data <- cbind(regional_data, Wealth)

# Now add the regional data back in to the main dataset.
merged_data <- merge(merged_data, regional_data, all=TRUE)

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
# Add a variable to allow later unique identification of each observation 
# (useful to figure out later on which row a particular case came from).
row_id <- 1:nrow(merged_data)
merged_data <- cbind(row_id, merged_data)

# Reorder the columns so the order makes more sense
first_columns <- c("row_id", "CountryISO", "Country", "Continent", "Survey", "Survey_Year", "GISDataYr", "GISPolyID", "Group.Type", "Group",  "Group.sub", "Group.lowest", "EnteredBy", "Perimkm", "Areakm", "WWFInName", "WWFInkm", "WWFInPct","WWFNrName", "WWFNrkm", "CIInName", "CIInkm", "CIInPct", "CINrName", "CINrkm", "SDT", "RNI_pred", "Td_pred", "Wealth")
merged_data <- cbind(merged_data[first_columns],
        merged_data[!(names(merged_data) %in% first_columns)])
regional_data <- cbind(regional_data[first_columns[-1]],
        regional_data[!(names(regional_data) %in% first_columns[-1])])

# Reorder the Survey_Year and GISDataYr factors to ensure the levels are in 
# chronological order.
merged_data$Survey_Year <- as.ordered(as.character(merged_data$Survey_Year))
merged_data$GISDataYr <- as.ordered(as.character(merged_data$GISDataYr))

save(merged_data, file=paste(merged_data_dir, "DHS_merged_data.Rdata", 
                             sep="/"))
write.csv(merged_data, file=paste(merged_data_dir, "DHS_merged_data.csv", 
                                  sep="/"), row.names=FALSE, na=".")
#write.dta(merged_data, file="Merged_data/DHS_merged_data.dta")

# Save the spatial dataframe and shapefile
save(regional_data, file=paste(merged_data_dir, 
                               "DHS_merged_data_regional_sp.Rdata", sep="/"))
#writeOGR(regional_data, "Merged_data", "DHS_merged_data_regional_shapefile", 
#"ESRI Shapefile")
