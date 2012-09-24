###############################################################################
# Cleans DHS data for use in R.

# For "excel_filename", give the name of the cleaned csv file (with footnotes 
# and extra columns deleted, and variables renamed) without the ".csv" 
# extension. "skipped_rows" indicates how many header rows to skip. Skip all 
# the header rows but the first column header row before the data begins (the 
# row that contains the variable names to be used in R).
raw_data_dir <-"Raw_data"
data_filenames <- dir(raw_data_dir)[grepl("_table_only.csv", dir(raw_data_dir))]

# The regular expressions used to select out the country, year, and any 
# relevant footnotes from the otherwise useless headings included in the excel 
# files from Statcompiler.
country.regex <- "^([(]?[A-Za-z']+[-)[:space:]]?)*"
year.regex <- "([0-9]{1,4}-?)+"
note.regex <- "[(][0-9]+[)]"
# NOTE: In Linux, might also need to include [:punct:] in the below regex. But 
# this will mess up the country names, as it will catch the parentheses in 
# country names like "Congo (Brazzaville)".
whitespace.regex <- "^([[:space:]])*|([[:space:]])*$"

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
        rep_num <- c(non.empty[1]-1, rep_num)
    }
    v <- rep(rep_items, rep_num)
}

# Loop over each statcompiler file and reformat the data adding new columns as 
# needed.
for (file_num in 1:length(data_filenames)) {
    this_file <- data_filenames[file_num]
    file_prefix <- sub(".csv", "", this_file)
    data_filename <- paste(raw_data_dir, this_file, sep="/")

    # Figure out how many lines of the header to skip before the file lists the 
    # row names.  Some files need two lines skipped, some lines need 3 skipped.  
    # Figure out how many to skip by finding the first row with an entry in the 
    # first column. The column names row is immediately before this row.
    first_rows <- scan(data_filename, what="list", sep="\n", nlines=10,
            quiet=TRUE)
    # Find the first row beginning with a field with a character other than a 
    # comma or whitespace. This is the first row with a heading, meaning the 
    # column names are given in the row immediately prior to that. Therefore 
    # need to skip one less row than the number of the row in which the column 
    # names appear (this number is two less than the number of the first row 
    # with a non-blank first field).
    first_rows <- substr(first_rows, 1, 1)
    num_skipped_rows <- match(TRUE, grepl("[a-zA-Z0-9]",first_rows)) - 2
    
    DHS_data <- read.csv(data_filename, skip=num_skipped_rows,
            encoding="latin1", stringsAsFactors=FALSE, na.strings=c("NA", "-"))

    names(DHS_data)[1] <- "Heading.Original"
    names(DHS_data)[2] <- "Group"

    ###########################################################################
    # First clean the data.
    ###########################################################################
    # Excel reformats columns with entries that it thinks are dates. This 
    # messes up age groups when they are listed as, for example "2-3", which 
    # gets changed to "3/2/2010". This happens in the "Group" column, so fix 
    # these entries.
    DHS_data$Group[DHS_data$Group == "2-Mar"] <- "2-3"
    DHS_data$Group[DHS_data$Group == "4-May"] <- "4-5"
    DHS_data$Group[DHS_data$Group == "4-Jun"] <- "4-6"
    DHS_data$Group[DHS_data$Group == "6-Jul"] <- "6-7"
    DHS_data$Group[DHS_data$Group == "5-Sep"] <- "5-9"
    DHS_data$Group[DHS_data$Group == "8-Sep"] <- "8-9"
    DHS_data$Group[DHS_data$Group == "Oct-14"] <- "10-14"
    DHS_data$Group[DHS_data$Group == "10-Nov"] <- "10-11"
    DHS_data$Group[DHS_data$Group == "Dec-13"] <- "12-13"

    # Some datasets use "Age 5-year groups" as a heading rather than "Age in 5 
    # year categories", though they are identical. Correct this so they are 
    # coded the same.
    DHS_data$Heading.Original <- sub("Age 5-year groups",
            "Age in 5 year categories", DHS_data$Heading.Original)

    # Remove the RHS signifier
    DHS_data$Heading.Original <- sub("RHS", "", DHS_data$Heading.Original)
    # Recode states and districts to be "Regions". They are all synonymous for 
    # the analysis we are doing.
    DHS_data$Heading.Original <- sub("Regions \\(states\\)", "Region",
            DHS_data$Heading.Original)
    DHS_data$Heading.Original <- sub("District", "Region",
            DHS_data$Heading.Original)
    # Remove leading and trailing whitespace from group names, but NOT from 
    # heading column, as a leading space on a heading means the heading is a 
    # group.type.
    DHS_data$Group <- gsub(whitespace.regex, "", DHS_data$Group)

    DHS_data <- cbind(Country=rep(NA, nrow(DHS_data)),
            Survey=rep(NA, nrow(DHS_data)), Survey_Year=rep(NA, nrow(DHS_data)),
            Footnote=rep(NA, nrow(DHS_data)), Group.Type=rep(NA, nrow(DHS_data)),
            Group.sub=rep(NA, nrow(DHS_data)),
            Group.lowest=rep(NA, nrow(DHS_data)), DHS_data)

    ###########################################################################
    # Find indices for rows that have headings, and those that actually have 
    # data in them. The DHS output stores survey names and aggregation types 
    # (group.type) in the same column. If the Heading.Original value is 
    # preceded by an empty space, it is a group.type heading row. If 
    # Heading.Original is not empty and the first character of Heading.Original 
    # is not a space, then it is a survey name heading. If the Heading.Original 
    # is blank, it is a data row.
    group_type_rows <- which(grepl("^[[:space:]]",
            substr(DHS_data$Heading.Original,1,1)))
    survey_name_rows <- which((!grepl("^[[:space:]]",
            substr(DHS_data$Heading.Original,1,1))) &
            nchar(DHS_data$Heading.Original)>1)
    data_rows <- which(DHS_data$Heading.Original == "")

    if (length(c(group_type_rows, survey_name_rows, data_rows)) !=
            nrow(DHS_data))
    {
        stop("Number of rows in dataframe does not match the total number of group
                type, survey name, and data rows")
    }
    ###########################################################################
    # Add the Country, Survey_Year, and Footnote columns
    ###########################################################################
    DHS_data$Country[survey_name_rows] <- DHS_data$Heading.Original[survey_name_rows]
    DHS_data$Survey_Year[survey_name_rows] <- DHS_data$Heading.Original[survey_name_rows]
    DHS_data$Footnote[survey_name_rows] <- DHS_data$Heading.Original[survey_name_rows]

    DHS_data$Country <- sub(year.regex, "", sub(note.regex, "", DHS_data$Country))
    DHS_data$Survey_Year <- sub(note.regex, "", sub(country.regex, "", DHS_data$Survey_Year))
    # Also trim parentheses from note
    DHS_data$Footnote <- gsub("[()]*", "", sub(year.regex, "",
            sub(country.regex, "", DHS_data$Footnote)))

    # Trim leading and trailing whitespace from all strings
    DHS_data$Country <- gsub(whitespace.regex, "", DHS_data$Country)
    DHS_data$Survey_Year <- gsub(whitespace.regex, "", DHS_data$Survey_Year)
    DHS_data$Footnote <- gsub(whitespace.regex, "", DHS_data$Footnote)

    # Not all surveys have footnotes. For those without footnotes, code 
    # this field as "None"
    DHS_data$Footnote[DHS_data$Footnote==""] <- "None"

    # Carry down the country, year, and note
    DHS_data$Country <- carry.down(DHS_data$Country)
    DHS_data$Survey_Year <- carry.down(DHS_data$Survey_Year)
    DHS_data$Footnote <- carry.down(DHS_data$Footnote)

    ###########################################################################
    # Add the Survey column
    ###########################################################################
    # Add a "Survey" column to denote which survey each row is from. Use the 
    # Heading.Original column to name each survey.
    DHS_data$Survey[survey_name_rows] <- DHS_data$Heading.Original[survey_name_rows]
    DHS_data$Survey <- sub(note.regex, "", DHS_data$Survey)
    DHS_data$Survey <- gsub(whitespace.regex, "", DHS_data$Survey)
    DHS_data$Survey <- carry.down(DHS_data$Survey)

    ###########################################################################
    # Add the Group.Type and Group.sub columns
    ###########################################################################
    DHS_data$Group.Type[group_type_rows] <- DHS_data$Heading.Original[group_type_rows]
    DHS_data$Group.Type <- gsub(whitespace.regex, "", DHS_data$Group.Type)
    DHS_data$Group.Type <- gsub(whitespace.regex, "", DHS_data$Group.Type)
    DHS_data$Group.Type <- carry.down(DHS_data$Group.Type)

    # Some countries begin their region names  (in the group column) with a 
    # "-".  Remove this.
    region_rows <- which(DHS_data$Group.Type=="Region")
    DHS_data$Group[region_rows] <- sub("^-", "",
            DHS_data$Group[region_rows])

    # Add a group.lowest column that lists the lowest level of aggregation for 
    # regional data. For data with no Sub-region, the value in this column will 
    # be the same as the value in the Group column. For data with a Sub-region, 
    # the value in this column will be taken from the Group.sub column.
    DHS_data$Group.lowest <- DHS_data$Group
    
    # Delete the blank rows that contain only headings or group types (no data)
    DHS_data <- DHS_data[data_rows,]

    if ("Region" %in% DHS_data$Group.Type)
    {
        # Need to check for Sub-regions, which are not indicated directly in 
        # the heading column. A row is for a Sub-region if the group is 
        # preceded by a "..".
        DHS_data[grepl("^\\.\\.", DHS_data$Group),]$Group.Type <- "Sub-region"
        sub_region_rows <- which(DHS_data$Group.Type == "Sub-region")
        DHS_data$Group.sub[sub_region_rows] <- DHS_data[sub_region_rows,]$Group
        # Remove the ".." indicators that show if a groupname is for a 
        # Sub-region, ans store the sub-region names in the Group.sub column
        DHS_data$Group.sub <- sub("^\\.\\.", "", DHS_data$Group.sub)

        # Delete the group names from the rows that are actually sub regions, 
        # and then carry down the group names. This will result in the Group 
        # for a sub region being the region it is a part of, while the 
        # Group.sub for a sub region lists the region name.
        DHS_data$Group[sub_region_rows] <- ""
        DHS_data$Group <- carry.down(DHS_data$Group)

        # Update the Group.lowest column so it lists the sub-region as the 
        # lowest level where applicable.
        DHS_data$Group.lowest[sub_region_rows] <- 
                DHS_data$Group.sub[sub_region_rows]

        # Several of the Indonesian sub-regions (in the 2002-2003 and 2007 
        # data) are listed as members of different higher-level regions in 
        # different datasets.  For example: in some  datasets the Lampung 
        # sub-region is listed as being part of "Java-Bali" region, while in 
        # others it is listed as a part of the "Outer Java-Bali I" region. This 
        # leads to duplication of the regions in the merged data if this is not 
        # handled specially.  HOWEVER - the Indonesian sub-region names ARE 
        # unique.  So after eliminating the upper level regional data for 
        # Indonesia, it can all be combined together successfully without 
        # replication.
        # First delete the unneeded higher level region rows (they are: 
        # "Java-Bali", "Outer Java-Bali I", and "Outer Java-Bali II").
        DHS_data <- DHS_data[!((DHS_data$Country == "Indonesia") &
                (DHS_data$Group.Type=="Region")),]
        IDN_rows <- (DHS_data$Country == "Indonesia") & (DHS_data$Group.Type=="Sub-region")
        if (length(which(IDN_rows)) > 0) {
            # Now re-classify the sub-regions as regions
            DHS_data$Group.Type[IDN_rows] <- "Region"
            DHS_data$Group[IDN_rows] <- DHS_data$Group.sub[IDN_rows]
            DHS_data$Group.lowest[IDN_rows] <- DHS_data$Group.sub[IDN_rows]
            DHS_data$Group.sub[IDN_rows] <- NA
        }
    }

    ###########################################################################
    # Write out the processed data
    ###########################################################################
    # Delete the now unused Heading.Original column
    DHS_data <- DHS_data[-which(names(DHS_data)=="Heading.Original")]

    save(DHS_data, file=paste("Processed_data/", file_prefix, "_processed.Rdata", sep=""))
}
