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
install.packages("ggsn")
library(usmap)
library(ggplot2)
library(ggsn)
install.packages("grid")
library(grid)


cases = read.csv("C://Users//sid12//Downloads//COVID-19-master//COVID-19-master//csse_covid_19_data//csse_covid_19_time_series//time_series_covid19_confirmed_US.csv")

cases <- na.omit(cases)

us_states <- map_data("state")


state_cases <- cases %>% filter(tolower(Province_State) %in% us_states$region)
View(state_cases)
state_cases$region <- tolower(state_cases$Province_State)

state_cases$per_cases=state_cases$Cases

cases0 <- state_cases %>% group_by(Province_State) %>%
  summarise(lat = mean(Lat), long = mean(Long_), cases_count = mean((Cases)))

cases0$state=cases0$Province_State





plot_usmap(
  data = cases0, values = "cases_count", lines = "red", include = c(), exclude = c('AK', 'HI'), labels = TRUE
) +
  labs(title = "Total Covid-19 cases across United States") +
  scale_fill_continuous(
    low = "yellow", high = "red", name = "Average Cases", label = scales::comma
  ) +
  theme(legend.position = "right",legend.title=element_text(size=12), legend.text=element_text(size=10))





