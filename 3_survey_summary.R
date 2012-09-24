###############################################################################
# Saves a summary of Countries included in the DHS data, including number of 
# surveys per country, and the date of each survey for each country.

require("foreign")

load("Merged_data/DHS_merged_data.Rdata")

countries <- data.frame(Country=unique(merged_data$Country))
continents <- merged_data$Continent[match(countries$Country, merged_data$Country)]
ISO <- merged_data$CountryISO[match(countries$Country, merged_data$Country)]

countries <- cbind(countries, Continent=continents, ISO=ISO,
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
countries$ISO <- reorder(countries$ISO, 1:nrow(countries))
countries$Country <- reorder(countries$Country, 1:nrow(countries))
countries$Continent <- reorder(countries$Continent, 1:nrow(countries))

save(countries, file="Merged_data/DHS_merged_data_surveys_by_country.Rdata")
write.csv(countries, file="Merged_data/DHS_merged_data_surveys_by_country.csv",
        row.names=FALSE)
#write.dta(countries, file="Merged_data/DHS_merged_data_surveys_by_country.dta")

require(ggplot2)
q <- qplot(ISO, NumSurveys, geom="bar", ylab="Number of surveys",
        xlab="Country ISO Code", colour=Continent, data=countries)
q + opts(axis.text.x=theme_text(angle=90, hjust=1, size=6))
ggsave("Merged_data/DHS_merged_data_surveys_by_country.png", width=9,
        height=6.5, dpi=300)
