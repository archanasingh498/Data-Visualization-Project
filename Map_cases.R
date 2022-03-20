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


#cases = readr::read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv", col_types = readr::cols())


#cases_chk <- cases %>% filter(Province_State =="Alabama")

View(cases)

cases = read.csv("C://Users//sid12//Downloads//COVID-19-master//COVID-19-master//csse_covid_19_data//csse_covid_19_time_series//time_series_covid19_deaths_US2.csv")

cases <- na.omit(cases)

View(cases)


cases0$state=cases0$Province_State







plot_usmap(
  data = cases0, values = "Deaths_count", lines = "red", include = c(), labels = TRUE
) +
  scale_fill_continuous(
    low = "white", high = "red", name = "Deaths Percentage", label = scales::percent
  ) +
  labs(title = "US States", subtitle = "Total deaths in United States") +
  theme(legend.position = "right")





us_states <- map_data("state")
View(us_states)

state_cases <- cases %>% filter(tolower(Province_State) %in% us_states$region)

View(state_cases)

state_cases$region <- tolower(state_cases$Province_State)

us_states_cases <- left_join(us_states, state_cases)

state_cases <- tolower(cases$Province_State)


cases0 <- state_cases %>% group_by(Province_State) %>%
  summarise(Deaths_count = mean(Deaths_Percentage))

cases = read.csv("time_series_covid19_deaths_US1.csv")
cases
us_states <- map_data("state")


state_cases <- cases %>% filter(tolower(Province_State) %in% us_states$region)
View(state_cases)
state_cases$region <- tolower(state_cases$Province_State)

state_cases$per_death=(state_cases$Deaths/state_cases$Population)*100

cases0 <- state_cases %>% group_by(Province_State) %>%
  summarise(lat = mean(Lat), long = mean(Long_), Deaths_count = mean((per_death)))



View(cases0)

#temp <- us_states_cases %>% filter(1/30/20)

#View(temp)

p0 <- ggplot(data = cases0,
             mapping = aes(x = long, y = lat, group = Province_State, fill = Deaths_count))
p1 <- p0 +geom_polygon(color = "gray90", size = 0.1) 

p1




p0 <- ggplot(data = state_cases,
             mapping = aes(x = Long_, y = Lat, fill = per_death))
p1 <- p0 +geom_polygon(color = "black", size = 0.3) +
  coord_map(projection = "albers", lat0 = 20, lat1 = 30) +
  theme_map() + labs(fill = "Total vaccinations percentage") 
p1
