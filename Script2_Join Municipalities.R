library(sf)
library(geobr)
library(ggplot2)
library(dplyr)
library(plyr)
PR<-PR[-493369,]
biomas <- read_biomes(2019)
biomas<-biomas[-7,]
#Read the NASA data
SC <- read.table("SC.csv", header=T, sep=" ")

#Read municipality and biomes spatial data
mun<-read_municipality("SC", 2019)

DF$wday <- lubridate::wday(DF$Date)
#Join the municipality and biomes spatial data
mun_biomes <- st_join(mun, biomas, join = st_intersects)

#Join the municipality and biomes with the NASA data
states_sf <- st_transform(mun_biomes, 4326)
points_sf = st_as_sf(SC, coords = c("mun_lon","mun_lat"), crs = 4326, agr = "constant")
result <- as.data.frame( st_join(points_sf, states_sf, join = st_intersects) )
SC<-result[-c(1,3,4,5,24,30,31)]
write.table(SC, "SC.csv", row.names = F)

#plot
mun2 <- st_transform(mun, 4326)
res2 <- st_join(points_sf, states_sf, join = st_intersects)

ggplot() +
  geom_sf(data = mun2, fill = "#2D3E50", color = "#FEBF57", size=.15)+
  geom_sf(data = res2, aes(color = name_region))