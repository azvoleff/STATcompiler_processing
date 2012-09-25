###############################################################################
# Export merged DHS datasets for use in Stata or R.

require("foreign")

source("0_data_path.R")

merged_data_dir <- paste(base_dir, "Merged_data", sep="/")

load(paste(merged_data_dir, "DHS_merged_data.Rdata", sep="/"))

# Allow selecting ONLY data from the most recent survey from each country. This 
# Rdatafile gives the "countries" dataframe that will be used later to select 
# out only the most recent survey from each country.
load(paste(merged_data_dir, "DHS_merged_data_surveys_by_country.Rdata", 
           sep="/"))

###############################################################################
# Export regional_data
###############################################################################
regional_data <- merged_data[merged_data$Category %in% c("Region", "Sub-region"),]
# Remove columns that are empty in the regional only data
empty_cols <- apply(regional_data, 2, function(col) length(which(!is.na(col)))==0)
regional_data <- regional_data[!(empty_cols)]
save(regional_data, file=paste(merged_data_dir, 
                               "DHS_merged_data_regional.Rdata", sep="/"))
write.csv(regional_data, file=paste(merged_data_dir, 
                                    "DHS_merged_data_regional.csv", sep="/"), 
          row.names=FALSE, na=".")

# Make a subset of the regional data that includes only the most recent survey 
# from each country.
#regional_data_lastsurvey <- subset(regional_data, Survey %in% countries$Survey.1)
#write.csv(regional_data_lastsurvey,
#        file=paste(merged_data_dir, "DHS_merged_data_regional_lastsurvey.csv", 
#        sep="/"), row.names=FALSE, na=".")
#save(regional_data_lastsurvey,
#        file=paste(merged_data_dir, 
#        "DHS_merged_data_regional_lastsurvey.Rdata", sep="/"))

###############################################################################
# Export urbal/rural data 
###############################################################################
urban_rural_data <- merged_data[merged_data$Category == "Residence",]
# Remove columns that are empty in the Urban/Rural only data
empty_cols <- apply(urban_rural_data, 2,
        function(col) length(which(!is.na(col)))==0)
urban_rural_data <- urban_rural_data[!(empty_cols)]

# Write out the urban_rural data for ALL surveys (multiple surveys per country 
# when available).
write.csv(urban_rural_data,
        file=paste(merged_data_dir, "DHS_merged_data_urban_rural.csv", 
                   sep="/"),
        row.names=FALSE, na=".")
save(urban_rural_data, file=paste(merged_data_dir, 
                                  "DHS_merged_data_urban_rural.Rdata", 
                                  sep="/"))

# Make a subset of the urban_rural data that includes only the most recent 
# survey from each country.
#urban_rural_data_lastsurvey <- subset(urban_rural_data, Survey %in% countries$Survey.1)
#write.csv(urban_rural_data_lastsurvey,
#        file=paste(merged_data_dir, 
#        "DHS_merged_data_urban_rural_lastsurvey.csv", sep="/"),
#        row.names=FALSE, na=".")
#save(urban_rural_data_lastsurvey,
#        file=paste(merged_data_dir, 
#        "DHS_merged_data_urban_rural_lastsurvey.Rdata", sep="/"))
