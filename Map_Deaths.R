install.packages("maps")
install.packages("cowplot")
install.packages("socviz")
install.packages("coord_map")
library(ggplot2)
library(socviz)
library(dplyr)
library(maps)
library(cowplot)
install.packages("readr")
library(readr)
install.packages("coord_map")
library(coord_map)
library(tidyverse)
library(viridis)
install.packages("tidyverse")
install.packages("usmap")

library(usmap)
library(ggplot2)




cases = read.csv("C://Users//sid12//Downloads//COVID-19-master//COVID-19-master//csse_covid_19_data//csse_covid_19_time_series//time_series_covid19_confirmed_US.csv")

cases <- na.omit(cases)
us_states <- map_data("state")
View(us_states)

state_cases <- cases %>% filter(tolower(Province_State) %in% us_states$region)

View(state_cases)

state_cases$region <- tolower(state_cases$Province_State)

us_states_cases <- left_join(us_states, state_cases)

state_cases <- tolower(cases$Province_State)

state_cases$state=state_cases$Province_State




plot_usmap(
  data = state_cases, values = "Cases", lines = "red", include = c(), labels = TRUE
) +
  scale_fill_continuous(
    low = "white", high = "red", name = "Deaths Percentage", label = scales::comma
  ) +
  labs(title = "US States", subtitle = "Total cases in United States") +
  theme(legend.position = "right")





plot_usmap(
  data = cases0, values = "Deaths_count", lines = "red", include = c(), labels = TRUE
) +
  scale_fill_continuous(
    low = "white", high = "red", name = "Deaths Percentage", label = scales::percent
  ) +
  labs(title = "US States", subtitle = "Total deaths in United States") +
  theme(legend.position = "right")