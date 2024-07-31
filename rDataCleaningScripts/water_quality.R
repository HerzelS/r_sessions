## Water quality data

### CLEANING THE DATA
library(tidyverse)
library(lubridate)

## Read data
#water_csv <- read_csv('raw data/austinwater.csv')

#saveRDS(water_csv, file = "rData/water.RDS") 
water <- readRDS("rData/water.RDS")


## Wrangle data
## Select only key columns for analysis
water <- water %>%
  select(SITE_NAME, SITE_TYPE, SAMPLE_DATE, PARAM_TYPE, PARAMETER, RESULT, UNIT)

glimpse(water)

## Rename columns
water <- water %>%
  rename(siteName = SITE_NAME, siteType = SITE_TYPE, sampleDate = SAMPLE_DATE,
         parameter = PARAMETER, parameterType = PARAM_TYPE, result = RESULT,
         unit = UNIT)
glimpse(water)

## Select specific filters
unique(water$parameter)

## Search for PH one way
water %>%
  filter(str_detect(parameter, 'PH')) %>% # where PH is contained
  select(parameter) %>% # only retain the parameter column
  unique() # select unique values in the parameter column

## Search for PH another way
filtered_water <- water %>%
  filter(parameter == 'PH' | parameter == 'WATER TEMPERATURE')

glimpse(filtered_water)
summary(filtered_water)

## Convert data types
# create factors
filtered_water <- filtered_water %>%
  mutate(siteType = as.factor(siteType),
         parameterType = as.factor(parameterType),
         parameter = as.factor(parameter),
         unit = as.factor(unit))

summary(filtered_water)

## Clean the sample date
filtered_water$sampleDate

filtered_water <- filtered_water %>%
  mutate(sampleDate = mdy_hms(sampleDate))

summary(filtered_water)

## Look at the units of measurement
## Correct Fee values
filtered_water <- filtered_water %>%
  mutate(unit = recode(unit, 'Feet' = 'Deg. Fahrenheit'))
summary(filtered_water)

## Remove the MG/L
filtered_water <- filtered_water %>%
  filter(!unit =='MG/L')

summary(filtered_water)

## Drop levels that do not have anything i.e. the MG/L
filtered_water <- filtered_water %>%
  mutate(unit = droplevels(unit))

summary(filtered_water)

## Check for outliers using a scatter plot

ggplot(filtered_water, mapping = aes(x = sampleDate, y = result)) +
  geom_point()

filter(filtered_water, result > 1000000) # check for the outlier

filtered_water <- filtered_water %>%
  filter(result <= 1000)
summary(filtered_water)


## Box plot
ggplot2::ggplot(data = filtered_water, mapping = aes(x = unit, y = result)) +
  geom_boxplot()

## Degree Celsius over 60 degrees celsius should probably be Fahrenheit
## Convert the unit back to text, work on it and convert in back to factor
filtered_water <- filtered_water %>%
  mutate(unit = as.character(unit)) %>%
  mutate(unit = ifelse((unit == 'Deg. Celsius' & result > 60),
                       'Deg. Fahrenheit', unit)) %>%
  mutate(unit = as.factor(unit))

## Box plot
ggplot2::ggplot(data = filtered_water, mapping = aes(x = unit, y = result)) +
  geom_boxplot()

## Clean tempratures recorded in both F and C
fahrenheit <- which(filtered_water$unit == 'Deg. Fahrenheit') #check for rows numbers were
# unit is in F

# Store these numbers in a vector for later use in an object called Fahrenheit
filtered_water$result[fahrenheit] <-
  (filtered_water$result[fahrenheit] - 32) * (5/9)

## Box plot
ggplot2::ggplot(data = filtered_water, mapping = aes(x = unit, y = result)) +
  geom_boxplot()

# Change the values to C not F
filtered_water$unit[fahrenheit] <- 'Deg. Celsius'

## Box plot
ggplot2::ggplot(data = filtered_water, mapping = aes(x = unit, y = result)) +
  geom_boxplot()

summary(filtered_water)

# Drop levels without data
filtered_water$unit <- droplevels(filtered_water$unit)

summary(filtered_water)

## Put measures form water temperature from same time and location into a single rows
### Remove the parameter Type and Unit variables
filtered_water <- filtered_water %>%
  select(-parameterType, -unit)
summary(filtered_water)

## Pivot the data wider
filtered_water_wide <- pivot_wider(filtered_water,
                                   names_from = parameter,
                                   values_from = result)

## NB There is an error of duplicate values
## Check if there are duplicates
dupe_check <- filtered_water[,-5] ## checking everything else except the last column
duplicated(dupe_check)

dupes <- which(duplicated(dupe_check))


filtered_water <- filtered_water[-dupes,] ## Drop duplicates


filtered_water_wide <- pivot_wider(filtered_water,
                                   names_from = parameter,
                                   values_from = result)

## Check the data set now
filtered_water_wide

## Clean column names
filtered_water_wide <- filtered_water_wide %>%
  rename(pH = PH, temperature = "WATER TEMPERATURE")

## Clean data set
summary(filtered_water_wide)
