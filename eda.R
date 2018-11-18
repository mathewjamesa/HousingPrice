#install.packages('ggcorrplot') #uncomment to install
library('ggcorrplot')
library('tidyverse')

#load data
load('data_final.Rdata')

#let's check summary statistics first
summary(data_final)

colnames(data_final)

#let's identify numeric variables
numericVars <- c('PRICE', 'BEDS', 'BATHS', 'SQUARE_FEET', 'YEAR_BUILT', 
                 'LATITUDE', 'LONGITUDE', 'min_dist_cta', 'num_cta_1mile', 
                 'crime_per_1000', 'life_exp_2010', 'unemployment', 'perc_housing_crowded', 
                 'perc_household_below_poverty', 'perc_16+_unempl', 'perc_25+_no_school_diploma', 
                 'perc_under18_over64', 'income_per_capite', 'hardship_index',
                 'percent_level1_school', 'percent_level2_school')

#let's identify categorical variables
catVars <- c('PROPERTY_TYPE', 'LOCATION')

data_final[,numericVars]
data_final[,catVars]

#======================================
#let's first compute correlation matrix
#and create correlation plot
#======================================
corr <- round(cor(data_final[,numericVars]), 2)
ggcorrplot(corr, hc.order = TRUE, outline.col = "white", method = "circle")


#======================================
#Histogram of the house prices
#======================================
ggplot(data_final, aes(x=PRICE)) + geom_histogram(fill="steelblue", alpha = .8) + 
  ggtitle('House prices distribution') + 
  scale_x_continuous(labels = scales::comma)

#======================================
#Sales price vs all numerical variables (scatterplots)
#======================================
#library('GGally')
#ggpairs(data_final[,numericVars])


#======================================
#Bar plots of all categorical variables
#======================================
ggplot(data_final[,catVars], aes(PROPERTY_TYPE)) + geom_bar(fill="steelblue", alpha = .8) + ggtitle('Number of offers by property type')

## set the levels in order
data_final <- within(data_final, LOCATION <- factor(LOCATION, levels=names(sort(table(LOCATION), decreasing=TRUE))))
ggplot(data_final, aes(LOCATION)) + geom_bar(fill="steelblue", alpha = .8) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ggtitle('Number of offers by community')

#======================================
#Boxplots of variables per community level
#======================================
#ggplot(data_final, aes(LOCATION)) + geom_bar(fill="steelblue", alpha = .8) + ggtitle('Number of offers by property type') + facet_wrap(~ PROPERTY_TYPE)


#======================================
#Spatial analysis (median house price)
#======================================

#let's visualize chicago communities
library(leaflet)

#plot map of Chicago
#chi <- leaflet() %>%
#  addTiles() %>%  # Add default OpenStreetMap map tiles
#  setView(lng=-87.624565, lat=41.875919, zoom = 10) 
#chi  # Print the map

#load shape file of Chicago communities
library(rgdal)
chicom <- readOGR("data/chicago.shp")

#testing
#get coordinates
library('ggmap')
geocode("Chicago", output = "more", source = "dsk")

"""
data_final %>% leaflet(options = 
          leafletOptions(dragging = TRUE,
                         minZoom = 10, 
                         maxZoom = 16)) %>% 
  addProviderTiles('CartoDB.PositronNoLabels') %>% 
  setView(lng = -87.65005, lat = 41.875919, zoom = 10) %>%
  setMaxBounds(lng1 = -87.47623 + .05, lat1 = 42.07454 + .05, 
               lng2 = -87.9828 + .05, lat2 = 41.60723 + .05) %>%
  addCircleMarkers(lng = ~LONGITUDE, lat = ~LATITUDE, radius=0.3, label = data_final$LOCATION)
"""

"""
#plot all items on map
data_final %>% leaflet(options = 
                              leafletOptions(dragging = TRUE,
                                             minZoom = 10, 
                                             maxZoom = 16)) %>% 
  addProviderTiles('CartoDB.PositronNoLabels') %>% 
  setView(lng = -87.65005, lat = 41.875919, zoom = 10) %>%
  setMaxBounds(lng1 = -87.47623 + .05, lat1 = 42.07454 + .05, 
               lng2 = -87.9828 + .05, lat2 = 41.60723 + .05) %>%
  addCircleMarkers(lng = ~LONGITUDE, lat = ~LATITUDE, radius=0.3, label = data_final$LOCATION)
"""


#joining shape data with data_final
#data_final$community_no <- factor(data_final$community_no)

#chicom@data <- chicom@data %>% 
#  left_join(data_final, by = c("area_num_1" = "community_no"))

# create color palette with colorNumeric()
#comm_median_price <- chicom@data %>% group_by(area_num_1) %>% summarise(median(PRICE))

#comm_median_price$area_num_1 <- as.numeric(comm_median_price$area_num_1)
#comm_median_price$`median(PRICE)` <- as.numeric(comm_median_price$`median(PRICE)`)

#nc_pal <- colorNumeric("red", domain = chicom@data$crime_per_1000)

#plot community shapes
#chicom %>% leaflet() %>% addPolygons(weight = 1, color = ~nc_pal(crime_per_1000), 
#                                     label = ~paste0("Crime per 1000: ", crime_per_1000)) %>% 
#  addProviderTiles('CartoDB.PositronNoLabels') %>% 
#  setView(lng = -87.65005, lat = 41.875919, zoom = 10) %>%
#  setMaxBounds(lng1 = -87.47623 + .05, lat1 = 42.07454 + .05, 
#               lng2 = -87.9828 + .05, lat2 = 41.60723 + .05)


#let's aggregate our data per community and join it with data in shape file
dataViz <- data_final %>% group_by(community_no) %>% 
  summarise(meanPrice = mean(PRICE), sdPrice=sd(PRICE), coefVar=(sdPrice/meanPrice), medianPrice=median(PRICE),
            minPrice=min(PRICE), maxPrice=max(PRICE), medianYearB=median(YEAR_BUILT), meanYearB=mean(YEAR_BUILT),
            crimePer1000=mean(crime_per_1000), percLevel1Schools=mean(percent_level1_school), percLevel2Schools=mean(percent_level2_school))

#convert area_number in chicom@data to numeric
chicom@data$area_numbe <- as.numeric(as.character(chicom@data$area_numbe))
chicom@data$area_num_1 <- as.numeric(as.character(chicom@data$area_num_1))

#now joining data with data in shape file
chicom@data <- chicom@data %>% 
  inner_join(dataViz, by = c("area_numbe" = "community_no"))

#check data for LOOP
chicom@data %>% filter(community=='LOOP')

#now let's plot it
#setting pallette on a log scale
npal <- colorNumeric("Blues", domain = chicom@data$crimePer1000)
#quantile palette
qpal <- colorQuantile("Blues", chicom@data$crimePer1000, n = 7)

chicom %>% leaflet() %>% 
  addPolygons(weight = 1, stroke = FALSE, smoothFactor = 0.2, fillOpacity = 0.8, color = ~npal(crimePer1000), 
              label = ~paste(community, ' - ', "Crime per 1000: ", crimePer1000),
              highlight = highlightOptions(weight = 5, color = "red", bringToFront = TRUE)) %>% 
  addLegend(pal = npal, values = ~crimePer1000, opacity = 0.75, title = "Crime rate per 1000", position = "bottomleft") %>%
  addProviderTiles('CartoDB.PositronNoLabels') %>% 
  setView(lng = -87.65005, lat = 41.875919, zoom = 10) %>%
  setMaxBounds(lng1 = -87.47623 + .05, lat1 = 42.07454 + .05, 
               lng2 = -87.9828 + .05, lat2 = 41.60723 + .05)



#======================================
#Proximity to CTA / CTA stations with house prices
#======================================


#======================================
#Crime per 1000 per community level
#======================================
#setting pallette on a log scale
npal <- colorNumeric("Blues", domain = chicom@data$crimePer1000)
#quantile palette
qpal <- colorQuantile("Blues", chicom@data$crimePer1000, n = 7)

chicom %>% leaflet() %>% 
  addPolygons(weight = 1, stroke = FALSE, smoothFactor = 0.2, fillOpacity = 0.8, color = ~npal(crimePer1000), 
              label = ~paste(community, ' - ', "Crime per 1000: ", crimePer1000),
              highlight = highlightOptions(weight = 5, color = "red", bringToFront = TRUE)) %>% 
  addLegend(pal = npal, values = ~crimePer1000, opacity = 0.75, title = "Crime rate per 1000", position = "bottomleft") %>%
  addProviderTiles('CartoDB.PositronNoLabels') %>% 
  setView(lng = -87.65005, lat = 41.875919, zoom = 10) %>%
  setMaxBounds(lng1 = -87.47623 + .05, lat1 = 42.07454 + .05, 
               lng2 = -87.9828 + .05, lat2 = 41.60723 + .05)

#======================================
#Distribution of house year built on map 
#======================================


#======================================
#Distribuion of prices based on bath/beds
#======================================


#======================================
#Income per capita per community level
#======================================


#======================================
#Education related 
#======================================


#======================================
#Coefficient of variation per community level
#======================================

