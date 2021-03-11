################################################################################
## DESCRIPTION ## Create a cleaned dataset for March 2021 STEAM Workshop
## INPUTS ##
## 1. Downloaded COVID death data from https://covidtracking.com/data/state/washington/race-ethnicity/historical
## 2. Demographic data from https://censusreporter.org/data/table/?table=B03002&geo_ids=04000US53&primary_geo_id=04000US53#valueType|estimate 
## OUTPUTS ## Cleaned dataset for students to work with
## AUTHOR ## Justin Lo
## DATE ## 08 March 2021
################################################################################
library(data.table)
library(dplyr)
library(readxl)
library(openxlsx)
# Read in downloaded data
wa_covid_data <- read.csv("washington-race-ethnicity-historical.csv")
wa_pop_data <- read_xlsx("acs2019_1yr_B03002_04000US53/acs2019_1yr_B03002_04000US53/acs2019_1yr_B03002_04000US53.xlsx", skip = 2)

##### Prep covid data first ######
# only keep the death columns because WA has 100% reporting
# Also removing the NA rows from 20200422 and 20200419
covid_deaths <- as.data.table(wa_covid_data[!is.na(wa_covid_data$Deaths_Ethnicity_Hispanic), 
                                            c("State", "Date", grep("Deaths_", names(wa_covid_data), value = TRUE))])

# Make real dates
covid_deaths <- transform(covid_deaths, Date = as.Date(as.character(Date), "%Y%m%d"))
covid_deaths$Date <- format(covid_deaths$Date, "%Y-%b-%d")
# Reshape long
covid_deaths <- melt(covid_deaths, id.vars = c("State", "Date"))
# Calculate the difference between each row
covid_deaths[, diff := shift(value) - value, by = "variable"]
# Account for deaths that happened before the first date
covid_deaths[Date == min(covid_deaths$Date), diff := value + diff, by = "variable"]
# Make a new column for month based on the value in Date column
covid_deaths[, Month := substr(Date, start = 1, stop = 8)]
# Sum by month
covid_deaths[, Monthly_Deaths := sum(diff, na.rm = TRUE), by = c("variable", "Month")]
# Pull out unique death data by month and group
covid_m_deaths <- unique(covid_deaths[,.(State, Month, variable, Monthly_Deaths)])
covid_m_deaths[, Race_Ethnicity := gsub("Deaths_", "", covid_m_deaths$variable)]

##### Prep WA demographic data #####
wa_pop_data <- as.data.table(wa_pop_data)
# Rename the first column
names(wa_pop_data)[1] <- "Group"

# Rename the population column
setnames(wa_pop_data, "Value", "Population")
# Only keep rows 1 through 12 because 13 through 21 include subdivisions of Hispanic or Latino
wa_pop_data <- wa_pop_data[1:12, ]
# Create the same Race_Ethnicity column to match the variables in the COVID dataset
wa_pop_data[Group == "Total:", Race_Ethnicity := "Total"]
wa_pop_data[Group == "Not Hispanic or Latino:", Race_Ethnicity := "Ethnicity_NonHispanic"]
wa_pop_data[Group == "White alone", Race_Ethnicity := "White"]
wa_pop_data[Group == "Black or African American alone", Race_Ethnicity := "Black"]
wa_pop_data[Group == "American Indian and Alaska Native alone", Race_Ethnicity := "AIAN"]
wa_pop_data[Group == "Asian alone", Race_Ethnicity := "Asian"]
wa_pop_data[Group == "Native Hawaiian and Other Pacific Islander alone", Race_Ethnicity := "NHPI"]
wa_pop_data[Group == "Some other race alone", Race_Ethnicity := "Other"]
wa_pop_data[Group == "Two or more races:", Race_Ethnicity := "Multiracial"]
wa_pop_data[Group == "Hispanic or Latino:", Race_Ethnicity := "Ethnicity_Hispanic"]

# Append a group for LatinX
temp_row <- wa_pop_data[Group == "Hispanic or Latino:"]
temp_row[, Race_Ethnicity := "LatinX"]
wa_pop_data <- rbind(wa_pop_data, temp_row)

##### Final monthly datasets
monthly_data <- merge(covid_m_deaths, wa_pop_data[,.(Race_Ethnicity, Population)], by = "Race_Ethnicity", all.x = TRUE)
monthly_data$variable <- NULL

# First make a dataset so students can calculate the sum
clean_months <- dcast(monthly_data[, -("Population")], formula = ... ~ Race_Ethnicity, value.var = c("Monthly_Deaths"), sep = "_")
setcolorder(clean_months, neworder = c(1, 2, 13, 7, 6, 3, 4, 5, 9, 10, 11, 12, 14, 8))
clean_months[, temp_date := as.Date(paste0(Month, "-01"), format = "%y-%b-%d")]
clean_months <- arrange(clean_months, temp_date) %>%
  mutate(temp_date = NULL)

# Append on extra rows for sum, mean, and variance
temp_months <- clean_months[1:3, 1:2]

temp_months[1, Month := "Sum of all Months"]
temp_months[2, Month := "Mean of all Months"]
temp_months[3, Month := "Variance of all Months"]

temp_months <- cbind(temp_months, 
                     round(rbind(sapply(clean_months[,-c(1, 2)], sum),
                           sapply(clean_months[,-c(1, 2)], mean),
                           sapply(clean_months[,-c(1, 2)], var))), 0)
temp_months$V3 <- NULL
# Second make a dataset with total deaths and population attached
monthly_data[, All_Deaths := sum(Monthly_Deaths), by = "Race_Ethnicity"]
clean_total <- unique(monthly_data[,.(State, Race_Ethnicity, All_Deaths, Population)])
start_date <- min(covid_deaths$Date)
end_date <- max(covid_deaths$Date)
clean_total[, Date_Range := paste0(start_date, " to ", end_date)]
setcolorder(clean_total, c(1, 5, 2:4))
clean_total[, Death_Rate := All_Deaths/Population]
clean_total[, Death_Rate_per100k := round(Death_Rate * 100000, 0)]
clean_total <- clean_total[c(11, 5, 4, 1:3, 7:9, 10, 12, 6), ]

# Make a list with both datasets
key <- list("Monthly_Data" = rbind(clean_months, temp_months, use.names = TRUE),
            "Total_Rates" = clean_total)

write.xlsx(key, file = "STEAM_Mar_2021_workshop_data_key.xlsx")

# Make a student version
clean_total$Death_Rate <- NA
clean_total$Death_Rate_per100k <- NA
out <- list("Monthly_Data" = rbind(clean_months, temp_months[, 1:2], use.names = TRUE, fill = TRUE),
            "Total_Rates" = clean_total)

write.xlsx(out, file = "STEAM_Mar_2021_workshop_data_student.xlsx")

