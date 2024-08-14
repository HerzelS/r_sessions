# File: boxPlots
# Project: r_sessions

# LOAD DATA ######
# Also convert all character variables to factors
df <- read_csv("raw data/state_trends.csv") |>
  mutate(across(where(is_character), as_factor)) |>
  print()

# BOXPLOT OF FREQUENCIES ######
?plot
?boxplot

## Box plot with defaults ###
boxplot(df$dance)


# Who is the outlier?
df |>
  filter(dance > 90) |>
  select(state, dance)

# Boxplot with options
df |>
  select(dance) |>
  boxplot(
    horizontal = TRUE,
    notch = TRUE, # Confidence interval for median
    main = "Boxplot of Searches for \"Dance\"",
    sub = "(Source: state_trends.csv)",
    xlab = "Searches for \"Dance\"",
    col = "#CD0000"
  )

# BOXPLOTS FOR MULTIPLE VARIABLES
df |>
  select(basketball:hockey) |>
  boxplot()

# Who are the outliers on "hockey?"
df |>
  filter(hockey > 45) |>
  select(state, hockey) |>
  arrange(desc(hockey))


# BOXPLOTS BY GROUP

# Boxplots by group using plot()
df |>
  select(has_nhl, hockey) |>
  plot()

# Who has the outlier on "No"
df |>
  filter(has_nhl == "No") |>
  filter(hockey > 80) |>
  select(state, hockey)

# Boxplots by group using plot()
df |>
  select(has_nhl, hockey) |>
  plot(
    horizontal = TRUE,
    notch = TRUE,
    main = "Boxplt of Searches for \"Hockey\"",
    sub = "(Source: state_trends.csv)",
    xlab = "Searches for \"Hockey\"",
    ylab = "State has NHL Hockey Team",
    col = "#CD0000"
    
  )




