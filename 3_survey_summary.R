###############################################################################
# Saves a summary of Countries included in the DHS data, including number of 
# surveys per country, and the date of each survey for each country.

require("foreign")

source("0_data_path.R")

merged_data_dir <- paste(base_dir, "Merged_data", sep="/")

load(paste(merged_data_dir, "DHS_merged_data.Rdata", sep="/"))

countries <- data.frame(Country=unique(merged_data$Country))
continents <- merged_data$Continent[match(countries$Country, merged_data$Country)]
CC_3 <- merged_data$CC_3[match(countries$Country, merged_data$Country)]

countries <- cbind(countries, Continent=continents, CC_3=CC_3,
        NumSurveys=rep(0, nrow(countries)), Survey1=rep(NA, nrow(countries)),
        Survey2=rep(NA, nrow(countries)), Survey3=rep(NA, nrow(countries)),
        Survey4=rep(NA, nrow(countries)), Survey5=rep(NA, nrow(countries)),
        Survey6=rep(NA, nrow(countries)))

survey_1_col <- grep("Survey1", names(countries))
for (n in 1:nrow(countries)) {
    country <- countries$Country[n]
    surveys <- unique(merged_data$Survey[merged_data$Country==country])
    surveys <- sort(surveys, decreasing=TRUE)
    countries$NumSurveys[n] <- length(surveys)
    countries[n, survey_1_col:(survey_1_col + length(surveys) - 1)] <- surveys
}

# And reorder the data frame before saving to CSV
countries <- countries[order(countries$Continent, countries$Country),]

# Reorder the factor levels before plotting
countries$CC_3 <- reorder(countries$CC_3, 1:nrow(countries))
countries$Country <- reorder(countries$Country, 1:nrow(countries))
countries$Continent <- reorder(countries$Continent, 1:nrow(countries))

save(countries, file=paste(merged_data_dir, 
                           "DHS_merged_data_surveys_by_country.Rdata", 
                           sep="/"))
write.csv(countries, file=paste(merged_data_dir, 
                                "DHS_merged_data_surveys_by_country.csv", 
                                sep="/"),
        row.names=FALSE)
#write.dta(countries, file=paste(merged_data_dir, "DHS_merged_data_surveys_by_country.dta", sep="/"))

require(ggplot2)
q <- qplot(CC_3, NumSurveys, geom="bar", stat='identity', ylab="Number of surveys",
        xlab="Country ISO Code", colour=Continent, data=countries)
q + theme(axis.text.x=element_text(angle=90, vjust=.2, size=6))
ggsave(paste(merged_data_dir, "DHS_merged_data_surveys_by_country.png", 
             sep="/"), width=9,
        height=6.5, dpi=300)
