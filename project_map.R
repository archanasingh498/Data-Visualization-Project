library(ggplot2)
library(socviz)
library(dplyr)
library(maps)
install.packages("mapproj")
library(cowplot)


###########################total vaccinations#################################
vaccines = read.csv("C:\\Users\\sid12\\Downloads\\covid-19-data-master\\covid-19-data-master\\public\\data\\vaccinations\\country_data\\United States.csv")
View(vaccines)

p <- ggplot(data = vaccines,
            mapping = aes(x = as.Date(date) ,y = (total_vaccinations/10000000), group = 1))
p + geom_line(size = 2,color="blue")
  scale_x_date(labels = scales::date_format()) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(x = "Timeline", y = "% vaccinated", title="Total Vaccine Count Over Time") +
  theme(plot.title = element_text(size=18,hjust=0.5),axis.title = element_text(size=16,face="bold"),axis.text=element_text(size=12,face="bold"))
  
  
  p <- ggplot(data = vaccines,
              mapping = aes(x = as.Date(date) ,y = total_vaccinations, color = state))
  p + geom_bar() 

  
############################################################################################################

vaccinations = read.csv("C:\\Users\\sid12\\Downloads\\covid-19-data-master\\covid-19-data-master\\public\\data\\vaccinations\\us_state_vaccinations.csv")

vaccinations

vacc = read.csv("C:\\Users\\sid12\\Downloads\\data\\data\\Covid19USA.csv")

vacc


vacc %>% select(state, confirmed, deaths) %>% sample()


us_states <- map_data("state")
View(us_states)
dim(us_states)

vacc$region <- tolower(vacc$state)
us_states_cases <- left_join(us_states, vacc)


p0 <- ggplot(data = us_states_cases,
             aes(x = long, y = lat, group = group, fill = confirmed))
p1 <- p0 +geom_polygon(color = "gray90", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45)

p1


party_colors <- c('#2E74C0', '#CB454A')

vaccinations %>% select(state, total_vaccinations, people_vaccinated, people_fully_vaccinated_per_hundred,  total_vaccinations_per_hundred, people_fully_vaccinated) %>% sample(5)

us_states <- map_data("state")
View(us_states)
dim(us_states)

vaccinations$region <- tolower(vaccinations$state)
us_states_vacc <- left_join(us_states, vaccinations)

head(us_states_vacc)


p <- ggplot(data = us_states,
            mapping = aes(x = long, y = lat),
            group = group, fill = region)
p + geom_polygon(color = "gray90", size = 0.1) + guides(fill = "none")


p + geom_polygon(color = "gray90", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  guides(fill = "none")


p0 <- ggplot(data = us_states_vacc,
             aes(x = long, y = lat, group = group, fill = total_vaccinations_per_hundred ))
p1 <- p0 + geom_polygon(color = "gray90", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45)

p1


p0 <- ggplot(data = us_states_vacc,
             aes(x = long, y = lat, group = state, fill = total_vaccinations_per_hundred))
p1 <- p0 +geom_polygon(color = "gray90", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45)

p1

p2 <- p1 +scale_fill_gradient2()
p2 + labs(title = "Total vaccinations")+ theme_map() + labs(fill = "Percent")

p3 <- p1 + scale_fill_gradient2(low = "red", mid = scales::muted("purple"), high = "blue", breaks = c(-50, -25, 0, 25, 50)) +
  labs(title = "Total vaccinations")+ theme_map() + labs(fill = "Percent")

p3 + theme_map()

county_map %>% sample_n(5)
dim(county_map)

county_full <- left_join(county_map, county_data, by = "id")

p <- ggplot(data = county_full,
            mapping = aes(x = long, y = lat, fill = pop_dens, group = group))
p1 <- p + geom_polygon(color = "gray90", size = 0.05) + coord_equal()

p1






p0 <- ggplot(data = subset(us_states_vacc, region %nin% "district of columbia"),
             aes(x = long, y = lat, group = group, fill = total_vaccinations))
p1 <- p0 +geom_polygon(color = "gray90", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45)

p1


p2

county_map %>% sample_n(5)
dim(county_map)



county_data %>% select(state, total_vaccinations, people_vaccinated, people_fully_vaccinated_per_hundred,  total_vaccinations_per_hundred, people_fully_vaccinated) %>% sample_n(5)
county_full <- left_join(county_map, county_data, by = "id")




p2 <- p1 +scale_fill_gradient2(low = "red", mid = scales::muted("purple"), high = "blue", breaks = c(-25, 0, 25, 50, 75)) +
  labs(title = "Winning margins")+ theme_map() + labs(fill = "Percent")

p <- ggplot(data = county_full,
            mapping = aes(x = long, y = lat, fill = people_vaccinated, group = group))
p1 <- p + geom_polygon(color = "gray90", size = 0.05) + coord_equal()

p1
