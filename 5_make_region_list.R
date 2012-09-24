###############################################################################
# Export list of regions in merged DHS datasets.

require("foreign")

load("Merged_data/DHS_merged_data_regional.Rdata")

regions_list <- regional_data[c("CountryISO", "Survey", "Survey_Year", "GISDataYr", "Group.Type", "Group", "Group.sub", "Group.lowest")]

new_order <- with(regions_list, order(CountryISO, Survey, Survey_Year, Group.Type, Group, Group.sub))
regions_list <- regions_list[new_order,]

write.csv(regions_list,
        file="Merged_data/DHS_merged_data_region_list.csv",
        row.names=FALSE, na=".")
