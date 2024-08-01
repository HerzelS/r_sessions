# LOAD PACKAGES ################
library(tidyverse)
library(readxl)

# LOAD DATA ################

# Also convert adjacent variables to factors
# acros means will use several variables i.e. region through psych region
# turning these into factors
# Bar charts want factor variables

df <- read_csv("raw data/state_trends.csv") |>
  mutate(across(c(region:psy_reg), factor)) |> 
  print()

# BARPLOT OF FREQUENCIES ###########

## Generic plotting ######
plot(df$psy_reg)

## Generic plot using pipes ####
df |>
  select(psy_reg)|>
  plot()

## Using barplot() #####
df |>
  select(psy_reg) |>
  table() |> # Put data in appropriate format
  barplot()

## Sort bars by descresing values (NOT for ordinal X) ###
df |>
  select(psy_reg) |>
  table() |>
  sort(decreasing = TRUE) |> # Sort table
  barplot()

## Add options to plot
df |>
  select(psy_reg) |>
  table() |>
  sort(decreasing = FALSE) |>
  barplot(
    main = "Personalities of 48 Contiguos US States",
    sub = "(Sources: state_trends.csv",
    horiz = TRUE,
    ylab = "Personality Profile",
    xlab = "Number of States",
    xlim = c(0, 25), # Limits for X axis
    border = NA, # No borders on bars,
    col = "#CD0000" # red3
  )

# STACKED BARPLOT OF FREQUENICES #####
## 100% stacked bar ####
df |>
  select(region, psy_reg) |>
  plot()

## Stacked bars ###
### Stacked bars: step 1: Create table ###
df_t <- df |>
  select(psy_reg, region) |>
  table() |>
  print()

## Stacked bars: step 2a: create graph w/legend
df_t |> barplot(legend = rownames(df_t))

## Stacked bars: step 2b: create graph w/o lenged
df_t |> barplot()

# SIDE-BY-SIDE BARPLOTS OF FREQUENCIES #######

## Side-by-side bar w/legend
df_t |>
  barplot(
    legend = rownames(df_t),
    beside = T # Put bars next to each other
  )
## Side-by-side bar w/o legend
df_t |> barplot(beside = TRUE)
