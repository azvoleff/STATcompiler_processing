###############################################################################
# Export merged DHS datasets for use in Stata or R.

require("foreign")

base_dir <- "M:/Data/Global/DHS/Statcompiler_Processing"

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
regional_data <- merged_data[merged_data$Group.Type %in% c("Region", "Sub-region"),]
# Remove columns that are empty in the regional only data
empty_cols <- apply(regional_data, 2, function(col) length(which(!is.na(col)))==0)
regional_data <- regional_data[!(empty_cols)]
save(regional_data, file=paste(merged_data_dir, 
                               "DHS_merged_data_regional.Rdata", sep="/"))
write.csv(regional_data, file=paste(merged_data_dir, 
                                    "DHS_merged_data_regional.csv", sep="/"), 
          row.names=FALSE, na=".")

# Rename the regional data variables so that the names match those used by 
# Aoife/Laura.
var_rename_key <- read.csv("Variable_Rename_Key/combined_variable_names.csv",
        encoding="latin1", stringsAsFactors=FALSE)
# Only use rows where there is a mapping both ways:
var_rename_key <- var_rename_key[(!is.na(var_rename_key$Var.Me) &
        !is.na(var_rename_key$Var.Laura)),]
for (n in 1:nrow(var_rename_key)) {
     col_num <- which(names(regional_data) == var_rename_key[n,]$Var.Me)
     if (length(col_num) > 1) stop("Multiple matches during variable renaming")
     names(regional_data)[col_num] <- var_rename_key[n,]$Var.Laura
 }
# Write out the version of the data for Aoife and Laura
write.csv(regional_data, file=paste(merged_data_dir, 
                                    "DHS_merged_data_regional_Aoife+Laura.csv", 
                                    sep="/"),
          row.names=FALSE, na=".")
#write.dta(regional_data, file="Merged_data/DHS_merged_data_regional.dta")

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
urban_rural_data <- merged_data[merged_data$Group.Type=="Residence",]
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
