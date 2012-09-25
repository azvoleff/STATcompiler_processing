###############################################################################
# Cleans DHS data for use in R.
source("0_data_path.R")

processed_data_dir <- paste(base_dir, "Processed_data", sep="/")

# For "excel_filename", give the name of the cleaned csv file (with footnotes 
# and extra columns deleted, and variables renamed) without the ".csv" 
# extension. "skipped_rows" indicates how many header rows to skip. Skip all 
# the header rows but the first column header row before the data begins (the 
# row that contains the variable names to be used in R).
raw_data_dir <- paste(base_dir, "Raw_data", sep="/")
data_filenames <- dir(raw_data_dir)[grepl(".csv", dir(raw_data_dir))]

# This function is used to carry down values in a column. It finds the first 
# non-empty value in a column and carries that down the column to fill all the 
# empty cells until the next non-empty cell. It then repeats the process.
carry.down <- function(v)
{
    non.empty <- which((!is.na(v)) & (v != ""))
    rep_num <- non.empty[2:(length(non.empty))] -
            non.empty[1:(length(non.empty)-1)]
    # Need to specially handle the rep count for the last non empty item.
    rep_num <- c(rep_num, (length(v) - non.empty[length(non.empty)] + 1))
    # Also need to specially handle cases where the first non empty item is not 
    # the first item in the vector. For this case, fill the vector before the 
    # first non-empty item with NAs
    rep_items <- v[non.empty]
    if (non.empty[1]!=1) {
        rep_items <- c(NA, rep_items)
        # Here non.empty[1] represents the index of the first non empty item.  
        # So if the first non-empty is the second item in the vector, an NA 
        # only needs to be repeated once, to fill the first spot.
        rep_num <- c(non.empty[1] - 1, rep_num)
    }
    v <- rep(rep_items, rep_num)
}

# Loop over each statcompiler file and reformat the data adding new columns as 
# needed.
for (file_num in 1:length(data_filenames)) {
    this_file <- data_filenames[file_num]
    file_prefix <- sub(".csv", "", this_file)
    data_filename <- paste(raw_data_dir, this_file, sep="/")

    DHS_data <- read.csv(data_filename, stringsAsFactors=FALSE)

    ###########################################################################
    # Add the Group.Type and Group.sub columns
    ###########################################################################
    region_rows <- DHS_data$Category == "Region"

    # Add a group.lowest column that lists the lowest level of aggregation for 
    # regional data. For data with no Sub-category, the value in this column will 
    # be the same as the value in the Group column. For data with a Sub-category, 
    # the value in this column will be taken from the Group.sub column.
    
    if ("Region" %in% DHS_data$Category) {
        # Need to check for Sub-categories, which are not indicated directly in 
        # the heading column. A row is for a Sub-category if the group is 
        # preceded by a "..".
        DHS_data[grepl("^\\.\\.", DHS_data$Characteristic), ]$Category <- "Sub-region"
        sub_region_rows <- DHS_data$Category == "Sub-region"

        # Delete the group names from the rows that are actually sub regions, 
        # and then carry down the group names. This will result in the Category 
        # for a sub region being the region it is a part of, while the 
        # Characteristic.sub for a sub region lists the region name.
        DHS_data$Characteristic <- sub("^\\.\\.", "", DHS_data$Characteristic)

        DHS_data$Characteristic.parent <- DHS_data$Characteristic
        DHS_data$Characteristic.parent[sub_region_rows] <- ""
        DHS_data$Characteristic.parent <- carry.down(DHS_data$Characteristic.parent)
        DHS_data$Characteristic.parent[!sub_region_rows] <- NA
    }

    ###########################################################################
    # Write out the processed data
    ###########################################################################
    save(DHS_data, file=paste(processed_data_dir, paste(file_prefix, 
                                                        "_processed.Rdata", 
                                                        sep=""),
                              sep="/"))
}
