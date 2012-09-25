###############################################################################
# Export list of regions in merged DHS datasets.

require("foreign")

source("0_data_path.R")

merged_data_dir <- paste(base_dir, "Merged_data", sep="/")

load(paste(merged_data_dir, "DHS_merged_data_regional.Rdata", sep="/"))

regions_list <- regional_data[c("CC_3", "Survey", "Year", "GISDataYr", "Category", "Characteristic", "Characteristic.parent")]

new_order <- with(regions_list, order(CC_3, Survey, Year, Category, Characteristic, Characteristic.parent))
regions_list <- regions_list[new_order,]

write.csv(regions_list,
        file=paste(merged_data_dir, "DHS_merged_data_region_list.csv", sep="/"),
        row.names=FALSE, na=".")
