
###########################################################################
## OPTION 2: TRYING LAYERS ##

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

library(flexdashboard)

library(rmarkdown)

#install.packages("flexdashboard")
#install.packages("rmarkdown")

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
pc2019 <- geojson_sf("https://opendata.arcgis.com/datasets/4c191bee309d4b2d8b2c5b94d2512af9_0.geojson")

# set colour pallette
pallette <- c("#660a0b", "#8d443a","#b0776e", "#d1aba5", "#efe2e0")

##########################################
##########################################


## MAP 1 - overall index

#select for map 0
map0<- combi[,c(1,2,9)]

#merge with master 
pc_data0 <- merge(pc2019,map0, by.y= "pcon19cd", by.x="pcon19cd", all.x= T)

#new column for levelling up
pc_data0$new[pc_data0$`Levelling-Up index` <= -10] <- "Priorities"
pc_data0$new[pc_data0$`Levelling-Up index` > -10] <- "Borderliners"
pc_data0$new[pc_data0$`Levelling-Up index` > 10] <- "Achievers"


#remove NA
pc_data0<- na.omit(pc_data0)

# set colour pallette
pallette0 <- c("#efe2e0", "#b0776e","#660a0b" )

#generate as factor
pc_data0$new<- as.factor(as.character(pc_data0$new))


factpal0 <- colorFactor(pallette0, pc_data0$new) #"Set3 is a colorbrewer preset https://www.r-graph-gallery.com/38-rcolorbrewers-palettes.html

#this makes the hover over popup label
labels0 <- sprintf("<strong>%s</strong> <br/>%s<sup></sup> <br/>%s overall score<sup></sup>", pc_data0$pcon19nm.x, pc_data0$new, round(pc_data0$`Levelling-Up index`,0))  %>% lapply(htmltools::HTML)



################
################

# test map 1



leaflet(options= leafletOptions(minZoom= 6)) %>%
  setView(lng = -2.302120,
          lat = 52.350304,
          zoom= 6) %>% #setView gives centre coordinates and zoom level
  
  setMapWidgetStyle(list(background = "white")) %>%  addProviderTiles(providers$CartoDB.PositronNoLabels, providerTileOptions(opacity = 1) ) %>% 
  
  addPolygons(data = pc_data0, stroke = T, color = ~factpal0(new),opacity = 1, fillOpacity = 0.9, weight=1, label = labels0,  highlight= highlightOptions(color="white", weight=2, bringToFront= T))  %>% 
  
  addLegend(pal = factpal0, values = pc_data1$new, labels=pc_data0$new, opacity=1, position = "topright", title="Levelling Up Index") %>% 
  removeDrawToolbar(clearFeatures = T) 



################
################



# MAP 2 - spending power

#select for map 2
map2<- combi[,c(1,2,3)]

#merge with master 
pc_data2 <- merge(pc2019,map2, by.y= "pcon19cd", by.x="pcon19cd", all.x= T)

# a new column for ntile
pc_data2$ntile <- ntile(pc_data2$`Spending power`, 5)  

factpal2 <- colorFactor(pallette, domain = pc_data2$ntile) #"Set3 is a colorbrewer preset https://www.r-graph-gallery.com/38-rcolorbrewers-palettes.html


#this makes the hover over popup label
labels2 <- sprintf("<strong>%s</strong><br/>%s ranking<sup></sup>", pc_data2$pcon19nm.x, round(pc_data2$`Spending power`,0)) %>% lapply(htmltools::HTML)


################
################

## MAP 3 - dependency 

#select for map 3 
map3<- combi[,c(1,2,4)]

#merge with master 
pc_data3 <- merge(pc2019,map3, by.y= "pcon19cd", by.x="pcon19cd", all.x= T)

# a new column for ntile
pc_data3$ntile <- ntile(pc_data3$Dependency, 5)  

factpal3 <- colorFactor(pallette, domain = pc_data3$ntile) #"Set3 is a colorbrewer preset https://www.r-graph-gallery.com/38-rcolorbrewers-palettes.html

#this makes the hover over popup label
labels3 <- sprintf("<strong>%s</strong><br/>%s ranking<sup></sup>", pc_data3$pcon19nm.x, round(pc_data3$Dependency,0)) %>% lapply(htmltools::HTML)


################
################

## MAP 4 - Crime

#select for map 3 
map4<- combi[,c(1,2,5)]

#merge with master 
pc_data4 <- merge(pc2019,map4, by.y= "pcon19cd", by.x="pcon19cd", all.x= T)

# a new column for ntile
pc_data4$ntile <- ntile(pc_data4$Crime, 5)  

factpal4 <- colorFactor(pallette, domain = pc_data4$ntile) #"Set3 is a colorbrewer preset https://www.r-graph-gallery.com/38-rcolorbrewers-palettes.html

#this makes the hover over popup label
labels4 <- sprintf("<strong>%s</strong><br/>%s ranking<sup></sup>", pc_data4$pcon19nm.x, round(pc_data4$Crime,0)) %>% lapply(htmltools::HTML)


################
################

## MAP 5 - Deprivation

#select for map 3 
map5<- combi[,c(1,2,6)]

#merge with master 
pc_data5 <- merge(pc2019,map5, by.y= "pcon19cd", by.x="pcon19cd", all.x= T)

# a new column for ntile
pc_data5$ntile <- ntile(pc_data5$Deprivation, 5)  

factpal5 <- colorFactor(pallette, domain = pc_data5$ntile) #"Set3 is a colorbrewer preset https://www.r-graph-gallery.com/38-rcolorbrewers-palettes.html

#this makes the hover over popup label
labels5 <- sprintf("<strong>%s</strong><br/>%s ranking<sup></sup>", pc_data5$pcon19nm.x, round(pc_data5$Deprivation,0)) %>% lapply(htmltools::HTML)

################
################

## MAP 6 - health
# Wales is missing

#select for map 6
map6<- combi[,c(1,2,7)]

#merge with master 
pc_data6 <- merge(pc2019,map6, by.y= "pcon19cd", by.x="pcon19cd", all.x= T)

# a new column for ntile
pc_data6$ntile <- ntile(pc_data6$Health, 5)  

factpal6 <- colorFactor(pallette, domain = pc_data6$ntile) #"Set3 is a colorbrewer preset https://www.r-graph-gallery.com/38-rcolorbrewers-palettes.html

#this makes the hover over popup label
labels6 <- sprintf("<strong>%s</strong><br/>%s ranking<sup></sup>", pc_data6$pcon19nm.x, round(pc_data6$Health,0)) %>% lapply(htmltools::HTML)


################
################

## MAP 7 - Blight

#select for map 6
map7<- combi[,c(1,2,8)]

#merge with master 
pc_data7 <- merge(pc2019,map7, by.y= "pcon19cd", by.x="pcon19cd", all.x= T)

# a new column for ntile
pc_data7$ntile <- ntile(pc_data7$Blight, 5)  

factpal7 <- colorFactor(pallette, domain = pc_data7$ntile) #"Set3 is a colorbrewer preset https://www.r-graph-gallery.com/38-rcolorbrewers-palettes.html

#this makes the hover over popup label
labels7 <- sprintf("<strong>%s</strong><br/>%s ranking<sup></sup>", pc_data7$pcon19nm.x, round(pc_data7$Blight,0)) %>% lapply(htmltools::HTML)




#####################################


#Map layers
my.maptest <- leaflet() %>% setView(-1.161877,52.350304, 6) %>% #setView gives centre coordinates and zoom level
  setMapWidgetStyle(list(background = "white")) %>%  
  addProviderTiles(providers$CartoDB.Positron, providerTileOptions(opacity = 1)) %>% 
  
  addPolygons(data = pc_data1, stroke = T,
              color = ~factpal1(pc_data1$ntile),
              opacity = 1, fillOpacity = 0.9, 
              weight=1, label = labels1,  
              highlight= highlightOptions(color="white", weight=2, bringToFront= T),
              group="Levelling Up Index")  %>%
  
  addPolygons(data = pc_data2, stroke = T,
              color = ~factpal2(pc_data2$ntile),
              opacity = 1, fillOpacity = 0.9, 
              weight=1, label = labels2,  
              highlight= highlightOptions(color="white", weight=2, bringToFront= T),
              group="Spending power") %>% 
  
  addPolygons(data = pc_data3, stroke = T,
              color = ~factpal3(pc_data3$ntile),
              opacity = 1, fillOpacity = 0.9, 
              weight=1, label = labels3,  
              highlight= highlightOptions(color="white", weight=2, bringToFront= T),
              group="Dependency") %>% 
  
  addPolygons(data = pc_data4, stroke = T,
              color = ~factpal4(pc_data4$ntile),
              opacity = 1, fillOpacity = 0.9, 
              weight=1, label = labels4,  
              highlight= highlightOptions(color="white", weight=2, bringToFront= T),
              group="Crime") %>% 
  
  addPolygons(data = pc_data5, stroke = T,
              color = ~factpal5(pc_data5$ntile),
              opacity = 1, fillOpacity = 0.9, 
              weight=1, label = labels5,  
              highlight= highlightOptions(color="white", weight=2, bringToFront= T),
              group="Deprivation") %>% 
  
  addPolygons(data = pc_data6, stroke = T,
              color = ~factpal6(pc_data6$ntile),
              opacity = 1, fillOpacity = 0.9, 
              weight=1, label = labels6,  
              highlight= highlightOptions(color="white", weight=2, bringToFront= T),
              group="Health") %>% 
  
  addPolygons(data = pc_data7, stroke = T,
              color = ~factpal7(pc_data7$ntile),
              opacity = 1, fillOpacity = 0.9, 
              weight=1, label = labels7,  
              highlight= highlightOptions(color="white", weight=2, bringToFront= T),
              group="Blight") %>% 
  
  addLegend(pal = factpal1,
            values = pc_data1$ntile, 
            labels=pc_data1$ntile,
            opacity=1, position = "bottomright", title="Ranking quintiles <br>(1 = low)") %>% 
  
  
  addLayersControl(baseGroups=c("Levelling Up Index","Spending power", "Dependency",
                                   "Crime","Deprivation", "Health", "Blight"),
                   options=layersControlOptions(collapsed=FALSE)) %>% 
  
  removeDrawToolbar(clearFeatures = T) %>% 
  addResetMapButton()

my.maptest



### save 
saveWidget(my.maptest, file="layers_LUI.html")

