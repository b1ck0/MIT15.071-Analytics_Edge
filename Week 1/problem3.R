setwd("~/GitHub/AnalyticsEdge_week1_problem3")

## 1.1 reading the data + finding the number of observations
CPS <- read.csv("CPSData.csv")
nrow(CPS)

## 1.2 Among the interviewees with a value reported for the Industry variable, what is the most common industry of employment? 
sort(table(CPS$Industry))

## 1.3 Which state has the fewest interviewees?
##     Which state has the largest number of interviewees?
sort(table(CPS$State)) 

## 1.4 What proportion of interviewees are citizens of the United States?
mean(CPS$Citizenship != "Non-Citizen")

## 1.5 For which races are there at least 250 interviewees in the CPS dataset of Hispanic ethnicity?
table(CPS$Race, CPS$Hispanic == TRUE)

## 2.1 Which variables have at least one interviewee with a missing (NA) value?
summary(CPS)

## 2.2 Correlation between marige status and other variables
table(CPS$Region, is.na(CPS$Married))
table(CPS$Sex, is.na(CPS$Married))
table(CPS$Age, is.na(CPS$Married))
table(CPS$Citizenship, is.na(CPS$Married))

## 2.3 How many states had all interviewees living in a non-metropolitan area ?
##     How many states had all interviewees living in a metropolitan area?
table(CPS$State, is.na(CPS$MetroAreaCode))

## 2.4 Which region of the United States has the largest proportion of interviewees living in a non-metropolitan area?
table(CPS$Region, is.na(CPS$MetroAreaCode))

## 2.5 Which state has a proportion of interviewees living in a non-metropolitan area closest to 30%?
##     Which state has the largest proportion of non-metropolitan interviewees, ignoring states where all interviewees were non-metropolitan?
sort(tapply(is.na(CPS$MetroAreaCode), CPS$State, mean))

## 3.1 How many observations (codes for metropolitan areas) are there in MetroAreaMap?
##     How many observations (codes for countries) are there in CountryMap?
MetroAreaMap <- read.csv("MetroAreaCodes.csv")
CountryMap <- read.csv("CountryCodes.csv")

nrow(MetroAreaMap)
nrow(CountryMap)

## 3.2 How many interviewees have a missing value for the new metropolitan area variable?
CPS = merge(CPS, MetroAreaMap, by.x="MetroAreaCode", by.y="Code", all.x=TRUE)
str(CPS)
summary(CPS)

## 3.3 Which of the following metropolitan areas has the largest number of interviewees?
sort(summary(CPS$MetroArea))
sum(CPS$MetroArea == "Atlanta-Sandy Springs-Marietta, GA", na.rm = TRUE)
sum(CPS$MetroArea == "Baltimore-Towson, MD", na.rm = TRUE)
sum(CPS$MetroArea == "Boston-Cambridge-Quincy, MA-NH", na.rm = TRUE)
sum(CPS$MetroArea == "San Francisco-Oakland-Fremont, CA", na.rm = TRUE)

## 3.4 Which metropolitan area has the highest proportion of interviewees of Hispanic ethnicity?
sort(tapply(CPS$Hispanic == TRUE, CPS$MetroArea, mean))

## 3.5 Determine the number of metropolitan areas in the United States from which at least 20% of interviewees are Asian
sort(tapply(CPS$Race == "Asian", CPS$MetroArea, mean))

## 3.6 Determine which metropolitan area has the smallest proportion of interviewees who have received no high school diploma
sort(tapply(CPS$Education == "No high school diploma", CPS$MetroArea, mean, na.rm=TRUE))

## 4.1 How many interviewees have a missing value for the new country of birth variable?
CPS = merge(CPS, CountryMap, by.x="CountryOfBirthCode", by.y="Code", all.x=TRUE)
summary(CPS$Country)

## 4.2 Among all interviewees born outside of North America, which country was the most common place of birth?
sort(table(CPS$Country))

## 4.3 What proportion of the interviewees from the "New York-Northern New Jersey-Long Island, NY-NJ-PA" metropolitan area have a country of birth that is not the United States?
tapply(CPS$Country != "United States", CPS$MetroArea == "New York-Northern New Jersey-Long Island, NY-NJ-PA", mean, na.rm = TRUE)

## 4.4 Which metropolitan area has the largest number of interviewees with a country of birth in India?
table(CPS$MetroArea,CPS$Country == "India")

## In Brazil?
table(CPS$MetroArea,CPS$Country == "Brazil")

## In Somalia?
table(CPS$MetroArea,CPS$Country == "Somalia")
