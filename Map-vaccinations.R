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
install.packages("ggrepel")
library(curl)
library(viridis)
library(usmap)
library(ggrepel)


vaccinations = readr::read_csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/us_state_vaccinations.csv", col_types = readr::cols())


vaccinations <- na.omit(vaccinations)

View(vaccinations)

us_states <- map_data("state")



View(map_data)
View(us_states)
state_vaccines <- vaccinations %>% filter(tolower(location) %in% us_states$region)

View(state_vaccines)

state_vaccines$region <- tolower(state_vaccines$location)

us_states_cases <- left_join(us_states, state_vaccines)

View(us_states_cases)
#us_states_cases$per_vaccines=(us_states_cases$people_vaccinated/us_states_cases$total_distributed)*100
us_states_cases$per_vaccines=us_states_cases$people_vaccinated_per_hundred

cases0 <- us_states_cases %>% group_by(location) %>%
  summarise(Long = mean(long), Lat = mean(lat), vaccines_count = mean((per_vaccines)))



#temp <- us_states_cases %>% filter(date=="2021-01-27")

View(cases0)

cases0$state=cases0$location


plot_usmap(
  data = cases0, values = "vaccines_count", lines = "red", include = c(), exclude = c('AK', 'HI'), labels = TRUE
) +
  labs(title = "      Percentage of people fully vaccinated in United States") +
  scale_fill_continuous(
    low = "white", high = "darkgreen", name = "% vaccinated", labels = scales::comma
  ) +
  theme(legend.position = "right",legend.title=element_text(size=12), legend.text=element_text(size=10))




p0 <- ggplot(data = cases0,
             aes(x = Long, y = Lat, group = location, fill = vaccines_count))
p1 <- p0 +geom_polygon(color = "black", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45)

p1

p2 <- p1 +scale_fill_gradient2()
p2 + labs(title = "Total vaccinations")+ theme_map() + labs(fill = "Percent")

p3 <- p1 + scale_fill_gradient2(low = "white", high = "darkgreen") +
  labs(title = "         Percentage of the total population fully vaccinationed by state in US")+ theme_map() + labs(fill = "% vaccinated")

p3 + theme_map()




cases0 <- us_states_cases %>% group_by(region) %>%
  summarise(vaccinations_count = mean((total_vaccinations_per_hundred)))


cases0$region=cases0$region


View(cases0)


temp <- us_states_cases %>% filter(date=="2021-01-27")

temp <- cases0 %>% filter(date=="2021-01-27")


View(temp)




map_with_data(
  data = temp, values = "vaccinations_count", lines = "red", include = c(), labels = TRUE
) +
  scale_fill_continuous(
    low = "white", high = "green", name = "Cases", label = scales::comma
  ) +
  theme(legend.position = "right",legend.title=element_text(size=12), legend.text=element_text(size=10)) +
ggsn::scalebar(data = us_map, dist = 500, dist_unit = "km",
               border.size = 0.4, st.size = 4,
               box.fill = c('black','white'),
               transform = FALSE, model = "WGS84") + 
  # put legend at the bottom, adjust legend title and text font sizes
  theme(legend.position = "bottom",
        legend.title=element_text(size=12), 
        legend.text=element_text(size=10))




p0 <- ggplot(data = temp,
             aes(x = long, y = lat, group = location, fill = total_vaccinations_per_hundred))
p1 <- p0 +geom_polygon(color = "black", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45)

p1

p2 <- p1 +scale_fill_gradient2()
p2 + labs(title = "Total vaccinations")+ theme_map() + labs(fill = "Percent")

p3 <- p1 + scale_fill_gradient2(low = "white", high = "darkgreen") +
  labs(title = "              Total vaccinations in United States")+ theme_map() + labs(fill = "% vaccinated")

p3 + theme_map()



x_boundary = -77
x_limits <- c(-50, NA) # optional, for label repelling

ggplot(data = temp, aes(x=long, y=lat, group=group))+
  geom_polygon(aes(fill=vaccinations_count))+
  geom_path()+ 
  scale_fill_gradientn(colours = rev(heat.colors(10)),na.value = "grey90",
                       guide = guide_colourbar(barwidth = 25, barheight = 0.4,
                                               #put legend title on top of legend
                                               title.position = "top")) +
  # if need to repel labels... could further finetune
  geom_label_repel(data = temp[x>=x_boundary,],
                   aes(x = x,y = y, label = abb, fill = vaccinations_count),
                   arrow = arrow(length = unit(0.02, "npc"), ends = "first"),
                   force = 5, hjust = 1, size = 3,
                   xlim  = x_limits, inherit.aes = F
  ) +
  # the normal labels: 
  geom_text(data=temp[x<x_boundary,], aes(x=x,y=y, label=abb), 
            size=3, inherit.aes=F) +
  coord_map() + 
  theme_classic() + 
  labs(fill = "vaccinations_count", x = "Longitude", y = "Latitude") + 
  
  # map scale
  ggsn::scalebar(data = dt2, dist = 500, dist_unit = "km",
                 border.size = 0.4, st.size = 4,
                 box.fill = c('black','white'),
                 transform = TRUE, model = "WGS84") + 
  # put legend at the bottom, adjust legend title and text font sizes
  theme(legend.position = "bottom",
        legend.title=element_text(size=12),  # font size of the legend 
        legend.text=element_text(size=10),
        axis.title.x=element_blank(),  # remove axis, title, ticks
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.line=element_blank())