# File: histograms.R
# Project: r_sessions

# LOAD PACKAGES ######

# Load packages
library(tidyverse)
library(readxl)

# LOAD DATA #######
df <- read_csv("raw data/state_trends.csv")

# HISTOGRAM ######

?hist

# Histogram with defaults 
hist(df$data_science)

# Histogram with options
hist(df$data_science,
     breaks = 7,
     main = "Histogram of Searches for \"Data Science\"",
     sub = "(Source: state_trends.csv)",
     ylab = "Frequency",
     xlab = "Searches for \"Data Science\"",
     border = NA,
     col = "#CD0000")

# DENSITY PLOT ######

?density

# Density plot with defaults
plot(density(df$data_science))


# Density plot with options
df |>
  pull(data_science) |> # Use pull() instead of select() if you want one variable
  as.numeric() |>
  density() |>
  plot(
    main = "Density Plot of Searches for \"Data Science\"",
    sub = "(Source: state_trends.csv)",
    ylab = "Frequency",
    xlab = "Searches for \"Data Science\""
  )

# Use polygon to ADD a filled density plot
df |>
  pull(data_science) |>
  as.numeric() |>
  density() |>
  polygon(col = "#CD0000")

  