
library(dismo)
library(data.table)
# TOKYO 

Tokyo <- as.data.frame(rbind(
# 
c("Marché Poisson", 
"Tsukiji Market, 5 Chome-2-1 Tsukiji, Chuo, Tokyo 104-0045, Japon")
,
c("palais imperial", 
"Imperial Palace, 1-1 Chiyoda, Tokyo 100-8111, Japon")
, 
c("musee d'art mori",
"Mori Art Museum, Roppongi Hills, 6 Chome-10-1 Roppongi, Tokyo 106-0032, Japon")
,
c("Aoyama Rei en cimetiere",
"2 Chome-32-2 Minamiaoyama, Minato-ku, Tōkyō-to 107-0062, Japon")
,
c("Carrefour Shibuya",
"Shibuya intersection, Japon, 井ノ頭通り")
,
c("Sanctuaire shintoiste Meiji jingu", 
"Meiji-jingū, 1-1 Yoyogikamizonocho, Shibuya, Tokyo 151-8557, Japon")
,
c("Sanctuaire Senso-ji",
"Akihabara Station, 1 Chome Sotokanda, Chiyoda, Tokyo, Japon")
,
c("musee Edo Tokyo maquettes", 
"Edo-Tokyo Museum, 1 Chome-4-1 Yokoami, Sumida, Tokyo 130-0015, Japon")
,
c("musee national des nouvelles sciences et de l'innovation",
"Miraikan Museum of Emerging Science and Innovation Tokyo, 2 Chome-3-6 Aomi, 江東区 Tokyo 135-0064, Japon")
,
c("Parc de loisir sur le theme du bain",
"Oedo-Onsenmonogatari, Japon")
,
c("UENO Tokyo National Museum",
"Tokyo National Museum, Japon")
,
c("Ueno Zoo and park",
"Zoo d'Ueno, 9-83 Uenokoen, Taito, Tokyo 110-0007, Japon")
, 
c("cimetiere", 
"7 Chome-16 Yanaka, Taitō-ku, Tōkyō-to 110-0001, Japon")
, 
c("maison", 
  "1 chome 3-9 Kojima, Taitō-ku, Tōkyō-to 110-0001, Japon")

))

names(Tokyo) <- c("lieu", "adresse")

## Geocoder

geocodeT <- function(x){dismo::geocode(x, oneRecord = TRUE)}
Tokyo.gps2 <- sapply(as.data.frame(Tokyo$adresse), geocodeT)

Tokyo.gps <- data.frame(Tokyo$lieu , Tokyo.gps2[[2]], do.call(cbind, Tokyo.gps2[3:length(Tokyo.gps2)]))
names(Tokyo.gps) <- dimnames(Tokyo.gps2)[[1]]


Tokyo.gps$num <- rownames(Tokyo.gps)


## placer sur une carte


library(leaflet)
library(htmltools)


#villes d interet en rouge par exemple
Tokyo.gps$color <- "red"
Tokyo.gps[Tokyo.gps$num %in% c(13, 12, 11),]$color <- "yellow"
Tokyo.gps[Tokyo.gps$num %in% c(3, 4, 5, 6),]$color <- "blue"
Tokyo.gps[Tokyo.gps$num %in% c(1, 10, 9),]$color <- "#00DDCC"
Tokyo.gps[Tokyo.gps$num %in% c(14),]$color <- "black"

m = leaflet(Tokyo.gps)  %>% addProviderTiles("Esri.WorldImagery") #%>% addTiles() #
m %>% fitBounds(min(Tokyo.gps$xmin),  min(Tokyo.gps$ymin), max(Tokyo.gps$xmax),  max(Tokyo.gps$ymax))
m %>% addCircles(Tokyo.gps$longitude, Tokyo.gps$latitude, radius = 100, opacity=.8, 
                 col=Tokyo.gps$color, popup = ~htmlEscape(paste(Tokyo.gps$num, Tokyo.gps$originalPlace))) 


# CartoDB.Positron
# Esri.WorldImagery

# All plugins : http://leaflet-extras.github.io/leaflet-providers/preview/index.html

