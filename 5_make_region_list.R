###############################################################################
# Export list of regions in merged DHS datasets.

require("foreign")

source("0_data_path.R")

merged_data_dir <- paste(base_dir, "Merged_data", sep="/")

load(paste(merged_data_dir, "DHS_merged_data_regional.Rdata", sep="/"))

regions_list <- regional_data[c("CountryISO", "Survey", "Survey_Year", "GISDataYr", "Group.Type", "Group", "Group.sub", "Group.lowest")]

new_order <- with(regions_list, order(CountryISO, Survey, Survey_Year, Group.Type, Group, Group.sub))
regions_list <- regions_list[new_order,]

write.csv(regions_list,
        file=paste(merged_data_dir, "DHS_merged_data_region_list.csv", sep="/"),
        row.names=FALSE, na=".")
