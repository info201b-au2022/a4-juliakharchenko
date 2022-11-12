library(tidyverse)

# The functions might be useful for A4
source("../source/a4-helpers.R")

## Test queries ----
#----------------------------------------------------------------------------#
# Simple queries for basic testing
#----------------------------------------------------------------------------#
# Return a simple string
test_query1 <- function() {
  return ("Hello world")
}

# Return a vector of numbers
test_query2 <- function(num=6) {
  v <- seq(1:num)
  return(v)
}

## Section 2  ---- 
#----------------------------------------------------------------------------#
incarceration <- get_data()

# Year with highest number of Black Americans in jail
highest_black_incarceration_year <- incarceration %>%
  arrange(black_jail_pop, desc(black_jail_pop), na.rm=TRUE) %>%
  filter(year == max(year, na.rm=TRUE)) %>%
  head(n=1) %>%
  pull(year)

# Average population rate of Black Americans in jail in the highest incarceration year across counties
highest_black_incarceration_pop <- incarceration %>%
  filter(year == max(year)) %>%
  summarize(avg_black_jail_pop_rate = mean(black_jail_pop_rate, na.rm=TRUE)) 

# Average percentage of Black Americans in jail compared to total Black population per county across counties
highest_black_incarceration_average <- incarceration %>%
  mutate(black_pop_proportion_in_jail = black_jail_pop / black_pop_15to64) %>%
  filter(year == max(year)) %>%
  summarize(avg_black_incarceration_prop = mean(black_pop_proportion_in_jail, na.rm=TRUE)) 
  
  

#----------------------------------------------------------------------------#

## Section 3  ---- 
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population
# Your functions might go here ... <todo:  update comment>
#----------------------------------------------------------------------------#
# Returns a dataframe that only has the values for the year and total jail population
get_year_jail_pop <- function() {
  incarceration_subset <- incarceration %>%
    group_by(year) %>%
    summarize(total_jail_population = sum(total_jail_pop, na.rm=TRUE))
return(incarceration_subset)   
}

# Returns a chart that plots the growth of the US prison population from 1970 to 2018
plot_jail_pop_for_us <- function()  {
  options(scipen=100)
  data <- get_year_jail_pop()
  plot <- ggplot(data, mapping=aes(x=year, y=total_jail_population)) +
    geom_bar(stat='identity') + 
    labs(
      x = "Year",
      y = "Total Jail Population", 
      title = "Increase of Jail Population in U.S. (1970-2018)",
      caption = "Measuring how the population of incarcerated individuals across America has grown since 1970."
    )
  return(plot)   
} 

total_us_prison_pop_chart <- plot_jail_pop_for_us()

## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Prison Population by State 
# Returns a dataframe that only has the values for the year and total jail population for each given state
get_jail_pop_by_states <- function(states) {
  incarceration <- incarceration %>%
    filter(state %in% states) %>%
    group_by(state, year) %>%
    summarize(total_jail_population = sum(total_jail_pop, na.rm=TRUE))
return(incarceration)
}

# Returns a chart that compares increases of jail population across multiple gven states per year in the US
plot_jail_pop_by_states <- function(states) {
  data <- get_jail_pop_by_states(states)
  plot <- ggplot(data,
                 aes(x=year,
                     y=total_jail_population,
                     group=state,
                     color=state)) + 
    geom_line() +
    labs(
      x = "Year",
      y = "Total Jail Population", 
      title = "State Comparisons of Increases of Jail Population in U.S. (1970-2018)",
      caption = "Measuring how the population of incarcerated individuals across certain U.S. states has grown since 1970."
    )
return(plot)
}

state_plot <- plot_jail_pop_by_states(c("FL", "WA", "CO", "OR", "NY"))
#----------------------------------------------------------------------------#

## Section 5  ---- 
#----------------------------------------------------------------------------#
# Returns a dataframe that only has the values for the year and total jail population for each given race
get_jail_pop_by_race <- function() {
  incarceration <- incarceration %>%
    group_by(black_jail_pop, white_jail_pop) %>%
    summarize(total_jail_population = sum(total_jail_pop, na.rm=TRUE))
return(incarceration)
}

plot_jail_pop_by_race <- function() {
  data <- get_jail_pop_by_race()
  plot <- ggplot(data, aes(x=black_jail_pop, y=white_jail_pop)) +
    geom_point() + 
    labs(
      x = "Population of Black Americans in Jail",
      y = "Population of White Americans in Jail", 
      title = "Comparisons of Increases of Jail Population in U.S. Across Racial Backgrounds (1970-2018)",
      caption = "Comparing how the population of incarcerated individuals across different racial backgrounds has grown since 1970."
    )
return(plot)
}

jail_by_race_chart <- plot_jail_pop_by_race()
#----------------------------------------------------------------------------#

## Section 6  ---- 
#----------------------------------------------------------------------------#

# Function that converts abbreviation of a state to its full name
st_to_state <- function(x){
  c(state.name, 'District of Columbia')[match(x, c(state.abb, 'DC'))]
}

# Creates a dataframe consisting of two elements: the state, and the average percentage of the Black population that is in 
#   jail across the states
get_incarceration_map_data <- function() {
  incarceration_map_data <- incarceration %>%
    # Drops na for easier calculations
    drop_na(black_jail_pop) %>%
    drop_na(black_pop_15to64) %>%
    # Only includes most recent year
    filter(year == 2018) %>%
    # Finds the proportion of the population of Black residents in jail per county
    mutate(black_pop_prop_in_jail = black_jail_pop / black_pop_15to64) %>%
    # Converts the abbreviations of the states into their full state names for joining
    mutate(state = tolower(st_to_state(state))) %>%
    group_by(state) %>%
    # Finds the average percentage of Black residents in jail in each county per state
    summarize(percent_of_black_residents_in_jail = mean(black_pop_prop_in_jail, na.rm=TRUE) * 100)
return(incarceration_map_data)
}

# Gets latitude and longitude information about each state and joins the incarceration data map with this map
create_map <- function() {
  incarceration_data <- get_incarceration_map_data()
  state_shape <- map_data("state") %>%
    # Rename for joining
    rename(state = region) %>%
    # Joins both dataframes by the state
    left_join(incarceration_data, by="state")
return(state_shape)
}

# Creates an initial dataframe to put into the map
state_shape <- create_map()

# Defines a minimalist theme for maps
blank_theme <- theme_bw() +
  theme(
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank()
  )

# Create a map of the continental U.S. 
america <- ggplot(state_shape) +
  geom_polygon(
    mapping = aes(x = long, y= lat, group = group, fill=percent_of_black_residents_in_jail),
    color = "white",
    size  = .1, 
  ) +
  coord_map() +
  scale_fill_continuous(low = "#132B43", high="Red") +
  labs(
    x = "",
    y = "",
    title = "Average Percentage of Incarcerated Black Citizens Across the States",
    caption = "\nMap detailing what percentage of Black residents per county \nare incarcerated, showing the discrepancies and \nshocking percentages between states.",
    fill = "Average % of Incarcerated Black Citizens"
    ) +
  blank_theme


