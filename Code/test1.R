

library(sp)
library(sf)
library(geojsonio)
library(leaflet)
library(leaflet.extras)
library(RColorBrewer)
library(readxl)
library(dplyr)
library(mapview)
library(hablar)
library(tidyr)
library(htmlwidgets)
library(tidyverse)

################

# Import data 
LUI_main<- read_excel("Data/Cluster analysis v1.8 JH for WPIE.xlsx", sheet = 1)
LUI_sub<- read_excel("Data/Cluster analysis v1.8 JH for WPIE.xlsx", sheet = 2)

# Clean up 
LUI_main<-LUI_main[-1,-1]
LUI_sub<- LUI_sub[c(-1,-575:-579),-1]  

# Match by PC code
combi <- merge(LUI_sub,LUI_main,by = "PC code", all.y = T)
combi <- combi[,-9]

#remame cols so it's easier for later
colnames(combi) <- c("pcon19cd", "pcon19nm", "Spending power", "Dependency", "Crime", "Deprivation", "Health", "Blight", "Levelling-Up index")

############

# JSON MAP - https://geoportal.statistics.gov.uk/datasets/westminster-parliamentary-constituencies-december-2019-boundaries-uk-bgc
# Import json
pc2019 <- geojson_sf("https://opendata.arcgis.com/datasets/937997590f724a398ccc0100dbd9feee_0.geojson")

# set colour pallette
pallette <- c("#660a0b", "#8d443a","#b0776e", "#d1aba5", "#efe2e0")

##########################################
##########################################

## MAP 1 - overall index

#select for map 1
map1<- combi[,c(1,2,9)]

#merge with master 
pc_data1 <- merge(pc2019,map1, by.y= "pcon19cd", by.x="pcon19cd", all.x= T)

# a new column for ntile
pc_data1$ntile <- ntile(pc_data1$`Levelling-Up index`, 5)  

factpal <- colorFactor(pallette, domain = pc_data1$ntile) #"Set3 is a colorbrewer preset https://www.r-graph-gallery.com/38-rcolorbrewers-palettes.html

#this makes the hover over popup label
labels2 <- sprintf(pc_data1$pcon19nm.x) %>% lapply(htmltools::HTML)


# mapping

#The map oblect
my.map <- leaflet() %>% setView(-1.161877,52.350304, 6) %>% #setView gives centre coordinates and zoom level
  setMapWidgetStyle(list(background = "white")) %>%  
  
  addPolygons(data = pc_data1, stroke = T, color = ~factpal(pc_data1$ntile),opacity = 1, fillOpacity = 1, label = labels2)  %>% 
  
  addLegend(pal = factpal, values = pc_data1$ntile, labels=pc_data1$ntile, opacity=1, position = "bottomright") %>% 
  removeDrawToolbar(clearFeatures = T) %>% 
  addResetMapButton()

my.map


################
################

# MAP 2 - spending power

#select for map 2
map2<- combi[,c(1,2,3)]

#merge with master 
pc_data2 <- merge(pc2019,map2, by.y= "pcon19cd", by.x="pcon19cd", all.x= T)

# a new column for ntile
pc_data2$ntile <- ntile(pc_data2$`Spending power`, 5)  

factpal <- colorFactor(pallette, domain = pc_data2$ntile) #"Set3 is a colorbrewer preset https://www.r-graph-gallery.com/38-rcolorbrewers-palettes.html


# mapping

#The map oblect
my.map <- leaflet() %>% setView(-1.161877,52.350304, 6) %>% #setView gives centre coordinates and zoom level
  setMapWidgetStyle(list(background = "white")) %>%  
  
  addPolygons(data = pc_data2, stroke = T, color = ~factpal(pc_data2$ntile),opacity = 1, fillOpacity = 1, label = labels2)  %>% 
  
  addLegend(pal = factpal, values = pc_data2$ntile, labels=pc_data2$ntile, opacity=1, position = "bottomright") %>% 
  removeDrawToolbar(clearFeatures = T) %>% 
  addResetMapButton()

my.map

################
################

## MAP 3 - dependency 

#select for map 3 
map3<- combi[,c(1,2,4)]

#merge with master 
pc_data3 <- merge(pc2019,map3, by.y= "pcon19cd", by.x="pcon19cd", all.x= T)

# a new column for ntile
pc_data3$ntile <- ntile(pc_data3$Dependency, 5)  

factpal <- colorFactor(pallette, domain = pc_data3$ntile) #"Set3 is a colorbrewer preset https://www.r-graph-gallery.com/38-rcolorbrewers-palettes.html


# mapping

#The map oblect
my.map <- leaflet() %>% setView(-1.161877,52.350304, 6) %>% #setView gives centre coordinates and zoom level
  setMapWidgetStyle(list(background = "white")) %>%  
  
  addPolygons(data = pc_data3, stroke = T, color = ~factpal(pc_data3$ntile),opacity = 1, fillOpacity = 1, label = labels2)  %>% 
  
  addLegend(pal = factpal, values = pc_data3$ntile, labels=pc_data3$ntile, opacity=1, position = "bottomright") %>% 
  removeDrawToolbar(clearFeatures = T) %>% 
  addResetMapButton()

my.map

################
################

## MAP 4 - Crime

#select for map 3 
map4<- combi[,c(1,2,5)]

#merge with master 
pc_data4 <- merge(pc2019,map4, by.y= "pcon19cd", by.x="pcon19cd", all.x= T)

# a new column for ntile
pc_data4$ntile <- ntile(pc_data4$Crime, 5)  

factpal <- colorFactor(pallette, domain = pc_data4$ntile) #"Set3 is a colorbrewer preset https://www.r-graph-gallery.com/38-rcolorbrewers-palettes.html


# mapping

#The map oblect
my.map <- leaflet() %>% setView(-1.161877,52.350304, 6) %>% #setView gives centre coordinates and zoom level
  setMapWidgetStyle(list(background = "white")) %>%  
  
  addPolygons(data = pc_data4, stroke = T, color = ~factpal(pc_data4$ntile),opacity = 1, fillOpacity = 1, label = labels2)  %>% 
  
  addLegend(pal = factpal, values = pc_data4$ntile, labels=pc_data4$ntile, opacity=1, position = "bottomright") %>% 
  removeDrawToolbar(clearFeatures = T) %>% 
  addResetMapButton()

my.map

################
################

## MAP 5 - Deprivation

#select for map 3 
map5<- combi[,c(1,2,6)]

#merge with master 
pc_data5 <- merge(pc2019,map5, by.y= "pcon19cd", by.x="pcon19cd", all.x= T)

# a new column for ntile
pc_data5$ntile <- ntile(pc_data5$Deprivation, 5)  

factpal <- colorFactor(pallette, domain = pc_data5$ntile) #"Set3 is a colorbrewer preset https://www.r-graph-gallery.com/38-rcolorbrewers-palettes.html


# mapping

#The map oblect
my.map <- leaflet() %>% setView(-1.161877,52.350304, 6) %>% #setView gives centre coordinates and zoom level
  setMapWidgetStyle(list(background = "white")) %>%  
  
  addPolygons(data = pc_data5, stroke = T, color = ~factpal(pc_data5$ntile),opacity = 1, fillOpacity = 1, label = labels2)  %>% 
  
  addLegend(pal = factpal, values = pc_data5$ntile, labels=pc_data5$ntile, opacity=1, position = "bottomright") %>% 
  removeDrawToolbar(clearFeatures = T) %>% 
  addResetMapButton()

my.map

################
################

## MAP 5 - Deprivation
# Wales is missing

#select for map 3 
map5<- combi[,c(1,2,6)]

#merge with master 
pc_data5 <- merge(pc2019,map5, by.y= "pcon19cd", by.x="pcon19cd", all.x= T)

# a new column for ntile
pc_data5$ntile <- ntile(pc_data5$Deprivation, 5)  

factpal <- colorFactor(pallette, domain = pc_data5$ntile) #"Set3 is a colorbrewer preset https://www.r-graph-gallery.com/38-rcolorbrewers-palettes.html


# mapping

#The map oblect
my.map <- leaflet() %>% setView(-1.161877,52.350304, 6) %>% #setView gives centre coordinates and zoom level
  setMapWidgetStyle(list(background = "white")) %>%  
  
  addPolygons(data = pc_data5, stroke = T, color = ~factpal(pc_data5$ntile),opacity = 1, fillOpacity = 1, label = labels2)  %>% 
  
  addLegend(pal = factpal, values = pc_data5$ntile, labels=pc_data5$ntile, opacity=1, position = "bottomright") %>% 
  removeDrawToolbar(clearFeatures = T) %>% 
  addResetMapButton()

my.map


################
################

## MAP 6 - Deprivation
# Wales is missing

#select for map 6
map6<- combi[,c(1,2,7)]

#merge with master 
pc_data6 <- merge(pc2019,map6, by.y= "pcon19cd", by.x="pcon19cd", all.x= T)

# a new column for ntile
pc_data6$ntile <- ntile(pc_data6$Health, 5)  

factpal <- colorFactor(pallette, domain = pc_data6$ntile) #"Set3 is a colorbrewer preset https://www.r-graph-gallery.com/38-rcolorbrewers-palettes.html


# mapping

#The map oblect
my.map <- leaflet() %>% setView(-1.161877,52.350304, 6) %>% #setView gives centre coordinates and zoom level
  setMapWidgetStyle(list(background = "white")) %>%  
  
  addPolygons(data = pc_data6, stroke = T, color = ~factpal(pc_data6$ntile),opacity = 1, fillOpacity = 1, label = labels2)  %>% 
  
  addLegend(pal = factpal, values = pc_data6$ntile, labels=pc_data6$ntile, opacity=1, position = "bottomright") %>% 
  removeDrawToolbar(clearFeatures = T) %>% 
  addResetMapButton()

my.map


################
################

## MAP 7 - Blight

#select for map 6
map7<- combi[,c(1,2,8)]

#merge with master 
pc_data7 <- merge(pc2019,map7, by.y= "pcon19cd", by.x="pcon19cd", all.x= T)

# a new column for ntile
pc_data7$ntile <- ntile(pc_data7$Blight, 5)  

factpal <- colorFactor(pallette, domain = pc_data7$ntile) #"Set3 is a colorbrewer preset https://www.r-graph-gallery.com/38-rcolorbrewers-palettes.html


# mapping

#The map oblect
my.map <- leaflet() %>% setView(-1.161877,52.350304, 6) %>% #setView gives centre coordinates and zoom level
  setMapWidgetStyle(list(background = "white")) %>%  
  
  addPolygons(data = pc_data7, stroke = T, color = ~factpal(pc_data7$ntile),opacity = 1, fillOpacity = 1, label = labels2)  %>% 
  
  addLegend(pal = factpal, values = pc_data7$ntile, labels=pc_data7$ntile, opacity=1, position = "bottomright") %>% 
  removeDrawToolbar(clearFeatures = T) %>% 
  addResetMapButton()

my.map

