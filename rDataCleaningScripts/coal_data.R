# Data Wrangling
# Coal Consumption Case Study

library(tidyverse) # load tidyverse
library(ggplot2)

#c <- read_csv('raw data/coal.csv', skip = 2) # skip first two rows

#saveRDS(c, "rData/coal.RDS")

coal <- readRDS("rData/coal.RDS")

glimpse(coal) # take a look at the coal data

colnames(coal)[1] <- 'region' # name the first column region

summary(coal) # get a summary of the variables. Now region is included.

# Make it tidy (One obesrvation for each row)
# Use pivot(longer)

coal_long <- pivot_longer(coal, !region, names_to = 'year', values_to = 'coal_consumption')

glimpse(coal_long)

# Convert year from character to integer

coal_long <- coal_long %>%
  mutate(year = as.integer(year))

summary(coal_long)

# Convert coal_consumption to numeric because it has commas
coal_long <- coal_long %>%
  mutate(coal_consumption = as.numeric(coal_consumption))

summary(coal_long)

# Separate countries from continents
# get unique values
unique(coal_long$region)

# build a vector with continents
noncountries <- c('North America', 'Central & South America', 'Antactica', 'Europe', 'Eurasia', 'Middle East', 'Africa', 'Asia & Oceania', 'World')

# build two separate data sets
coal_region <- coal_long %>%
  filter(region %in% noncountries)

unique(coal_region)

coal_countries <- coal_long %>%
  filter(!region %in% noncountries)

unique(coal_countries)

### Visualization
## Line graph
ggplot(data = coal_region, mapping = aes(x = year, y = coal_consumption)) +
  geom_line(mapping = aes(colour = region))




