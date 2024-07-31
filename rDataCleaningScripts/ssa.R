# Data Wrangling in R
# Social Security Disability Case Study

# Load the tidyverse
library(tidyverse)
library(lubridate)
library(stringr)

# Read in the dataset
#s <- read_csv("raw data/ssadisability.csv")

#saveRDS(s, file = "rData/ssa.RDS")
ssa <- readRDS("rData/ssa.RDS")

# Take a look at how this was imported
glimpse(ssa)

# Make the dataset long
ssa_long <- pivot_longer(ssa, !Fiscal_Year, names_to='month', values_to='applications')

# And what do we get?
print(ssa_long, n=20)

# Split the month and application type
ssa_long <- ssa_long %>%
  separate(month, c("month", "application_method"), sep="_")

# What does that look like?
print(ssa_long, n=20)

# What values do we have for months?
unique(ssa_long$month)

# Convert month to standard abbreviations
ssa_long <- ssa_long %>%
  mutate(month=substr(month,1,3))

# What values do we now have for months and years?
unique(ssa_long$month)
unique(ssa_long$Fiscal_Year)

# Convert Fiscal_Year from alphanumeric strings to actual years
ssa_long <- ssa_long %>%
  mutate(Fiscal_Year=str_replace(Fiscal_Year, "FY", "20"))

# What values do we now have for years?
unique(ssa_long$Fiscal_Year)

# Build a date string using the first day of the month
paste('01', ssa_long$month, ssa_long$Fiscal_Year)

ssa_long <- ssa_long %>%
  mutate(date=dmy(paste("01", ssa_long$month, ssa$Fiscal_Year)))

# What do those look like?
unique(ssa_long$date)

# Government fiscal years differ from calendar years in that they are named for the
# calendar year where they end.  The government fiscal year begins in October.
# So October 2016 is actually in FY17.
#
# We need to convert these to calendar dates before we try to plot them, so we
# need to find months >=10 and subtract one year from them

# Let's find the affected rows and decerement the years by one
ssa_long <- ssa_long %>%
  mutate(Fiscal_Year=as.numeric(Fiscal_Year)) %>%
  mutate(Fiscal_Year=ifelse(month(date)>=10, Fiscal_Year-1, Fiscal_Year)) %>%
  mutate(date=dmy(paste("01", month, Fiscal_Year)))

# Let's look at where we are
summary(ssa_long)

# Remove Fiscal_Year and month columns
ssa_long <- ssa_long %>%
  select(-Fiscal_Year, -month)

# Change application method to fator
ssa_long <- ssa_long %>%
  mutate(application_method = as.factor(application_method))

summary(ssa_long)

ssa <- pivot_wider(ssa_long, names_from = application_method, values_from = applications)

print(ssa, n = 20)

view(ssa, n = 20)
view(ssa_long, n = 20)

ssa <- ssa %>%
  mutate(online_percentage = Internet/Total * 100)

ggplot2::ggplot(data = ssa, mapping = aes(x = date, y = online_percentage)) +
  geom_line()
