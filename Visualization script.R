
##Header####
## Who:2233800
##What: Data Visualization Assessment Script
## Date: 09/03/2023

##Content####

## Setup
## Read in the data/Remove NA values
## Load necessary library
## Create Barplot showing  plastics produced by countries
## visualize the plastics produced on a map
## Which parent company produced more pet waste  in philippines?
## How much pet was produced in the years in view (2019, 2020)
## Numbers of plastic waste by type and Year
##Plastic waste clearing around the world

# Setup

#setwd

#Assign the  data to a dataframe "plastics"
plastics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-26/plastics.csv')

#Remove NA values

plastics <- na.omit(plastics)

# Load necessary library
library(maps)
library(tidyverse)
library(dplyr)
library(rnaturalearth)
library(rnaturalearthdata)
library(rgeos)
library(sf)
library(rworldmap)
library(patchwork)
library(Cairo)
library(extrafont)
library(plotly)
library(viridis)
webshot::install_phantomjs(force = TRUE)


# Barplot showing  plastics produced by countries

## Change the country expression

plastics$country = gsub("United States of America", "USA", plastics$country)
plastics$country = gsub("United Kingdom of Great Britain & Northern Ireland",
                        "UK$ N Ireland", plastics$country)
plastics$country = gsub("United Arab Emirates", "UAE", plastics$country)

#filter to remove double country name

plastics <- plastics%>%
  filter(country != "NIGERIA")
# Replot to see changes
plastics %>%
  group_by(country) %>%
  summarize(total_waste = sum(grand_total)) %>%
  
  plot_ly(x = ~country, y = ~total_waste, type = "bar",
          marker = list(color = "#1F77B4")) %>%
  layout(title = "Plastic Waste Generation by Country",
         xaxis = list(title = "Country"),
         yaxis = list(title = "Total Waste Generated"),
         hovermode = "closest",
         plot_bgcolor = "#F5F5F5",
         paper_bgcolor = "#F5F5F5",
         font = list(color = "#333333"),
         bargap = 0.1)



# visualize the plastics produced on a map

#Data prep----
country_plastics <- plastics %>%
  group_by(country) %>%
  summarize(total_waste = sum(grand_total)) 

# Get worldmap
world_map <- map_data("world")

world_map <- getMap(resolution="high")

# Get centroids per country polygon
centroids <- gCentroid(world_map, byid=TRUE)

# Define df with centroids including x and y coordinate
centroid_df <- data.frame(centroids)

p <- cbind(rownames(centroid_df), data.frame(centroid_df, row.names =NULL ))
colnames(p) = c("country", "x", "y")


# Join df on countries
plastics_geo <- left_join(plastics, p,
                          by = 
                            "country")
plastics_geo <- na.omit(plastics_geo)

# Convert df to sf object
plastics_geo_sf <- sf::st_as_sf(plastics_geo, coords = c("x","y"), crs = 4326) 


# Change crs
st_crs(plastics_geo_sf) = 4326


# data visualisation----

# Download country polygons
world <- ne_countries(scale = "medium", returnclass = "sf")

map <- ggplot() +
  # Add country polygons
  geom_sf(data = world, fill = "#FFFFFF", color = "#2A363B", size = .2) +
  
  # Add point centroid locations
  geom_sf(data = plastics_geo_sf, color = "black", size = 3, alpha = .5) +
  geom_sf(data = plastics_geo_sf, color = "red", size = 1, alpha = .75)+
  
  
  # Add title and subtitle
  labs(subtitle = "Waste Plastics",
       title = "\nPLASTIC WASTE AROUND THE WORLD?") +
  
  # Get globe projection, set to northpole and europe
  coord_sf(crs = "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs") +
  
  # Theming
  theme_minimal() +
  theme(plot.background = element_rect(fill = "#000000",
                                       color ="#000000"),
        plot.title = element_text(colour = "#FFFFFF",
                                  hjust = .5,
                                  family = "Bell MT",
                                  size = 20),
        plot.subtitle = element_text(colour ="#FFFFFF",
                                     hjust = .5,
                                     family = "Verdana"),
        panel.grid.major = element_line(size = .1))
map


#save plot
ggsave(map, filename = "globe.png", width = 14.8, height = 19, units = "cm", type = "cairo-png", dpi = 300)


# Which parent company produced more pet waste  in philippines?

#create a new dataset named plastics_my

plastics_my <-  plastics%>% select(country, parent_company,year, pet)%>% 
  filter(country=="Philippines", year == 2020, pet > 0)%>% as.data.frame()
#plot the bar graph using ggplot

ggplot(plastics_my)+ aes(x= parent_company, y = pet)+
  geom_bar(stat ="identity",
           fill = "goldenrod")+coord_flip()



# How much pet was produced in the years in view (2019, 2020)

#This will be determined using vioplot

library(vioplot)

#Create Vioplot
vioplot(pet ~ year, data = plastics, xlab= "YEAR", ylab = "PET",
        col = "lightblue", main = "Pet plastic produced between 
                    2019 and 2020")

# 5 Numbers of plastic waste by type and Year

# Reshape data from wide to long format

plastics_long <- plastics %>%
  pivot_longer(cols = c("hdpe", "ldpe", "o", "pet", "pp", "ps", "pvc"),
               names_to = "plastic_type", values_to = "value")

# Create grouped bar chart of plastic waste by type and year using ggplot

ggplot(plastics_long, aes(x = year, y = value, fill = plastic_type)) +
  geom_col(position = "dodge") +
  scale_fill_viridis_d(option = "magma")+
  ggtitle("Plastic Waste by Type and Year") +
  xlab("Year") +
  ylab("Value") +
  theme_minimal() +
  scale_y_continuous(limits = c(0, max(plastics_long$value) * 1.1))
theme(legend.position = "bottom")

#6 Plastic waste clearing around the world

#Divide the plot into two colunmn
par(mfrow = c(1, 2), mar = c(4, 4, 2, 1), oma = c(0, 0, 2, 0))

#plot the two graphs side by side
with(plastics, {
  plot(volunteers, num_events,col = "lightblue",
       pch = 16, main = "volunteers and number of events")
  
  plot(year, num_events, pch = 16, col= "green", main = "year and number of events")
  
  mtext("Plastic wastes Clearing event around the world", outer = TRUE)
})
