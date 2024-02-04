library(ggmap)
library(ggrepel)

rm(list=ls())

setwd("C:/Users/syedm/Desktop/Distance paper")

data <- read.csv("LBK.csv")


#API
ggmap::register_google(key = "AIzaSyA-UiMPuDvL_CFU34lyIDtow_B020R_sh8")

#getting Atlanta map
get_local_spot <-  get_map("Lubbock Texas", maptype = "roadmap", zoom = 11) 
map1 <- ggmap(get_local_spot)

data$mean_dist <- rowMeans(data[ , c("Distance", "Distance2")], na.rm=TRUE)


lats<-c(33.51,33.605)
lons<-c(-101.95,-101.85)
bb<-make_bbox(lon=lons,lat=lats,f=0.05)
lbk<-get_map(bb,zoom=12,maptype="roadmap")
lbk_map <- ggmap(lbk)

lbk_map +
  geom_point(data = data, aes(x = Longitude, y = Latitude, colour = mean_dist), size = 3.5, alpha=0.75) + #shape=5
  #scale_color_brewer(palette = "Paired") +
  #labs(x = "Longitude", y="Latitude", colour="Middle school district") + 
  guides(colour = guide_legend(override.aes = list(size=5))) + 
  geom_point(aes(x=-101.8949,y=33.5943),colour="red", size=5, shape=17) + 
  geom_point(aes(x=-101.9254,y=33.5393),colour="red", size=5, shape=17) + 
  geom_point(aes(x=-101.92578,y=33.52777),colour="#E7B800", size=5, shape=15) + 
  #geom_label_repel(aes(x = -101.8949, y = 33.5943, label = "BAL"), size = 3) +
  annotate("label", x = -101.8949, y = 33.5943, label = "Texas Tech", size=4.5, hjust=-0.15, vjust=0, alpha=0.50) +
  annotate("label", x = -101.9254, y = 33.5393, label = "South Plains Mall", size=4.5, hjust=-0.10, vjust=-0.15, alpha=0.50) + 
  annotate("label", x = -101.92578, y = 33.52777, label = "Mean Location", size=4.5, hjust=-0.10, vjust=0.15, alpha=0.50) +
  theme(legend.position="none") + xlab("Longitude") + ylab("Latitude")

colMeans(data[ , c("Longitude", "Latitude")], na.rm=TRUE)




data <- read.csv("COL.csv")

lats<-c(39.60,40.40)
lons<-c(-83.55,-82.00)
bb<-make_bbox(lon=lons,lat=lats,f=0.05)
col<-get_map(bb,zoom=10,maptype="roadmap")
col_map <- ggmap(col)

col_map +
  geom_point(data = data, aes(x = hlong, y = hlat, colour = dist_cbus), size = 1, alpha=0.50) + #shape=5
  #scale_color_brewer(palette = "Paired") +
  #labs(x = "Longitude", y="Latitude", colour="Middle school district") + 
  guides(colour = guide_legend(override.aes = list(size=5))) + 
  geom_point(aes(x=-83.0305,y=40.0067),colour="red", size=5, shape=17) + 
  geom_point(aes(x=-83.0060,y=39.9692),colour="red", size=5, shape=17) + 
  geom_point(aes(x=-82.96275,y=40.00867),colour="#E7B800", size=5, shape=15) + 
  #geom_label_repel(aes(x = -101.8949, y = 33.5943, label = "BAL"), size = 3) +
  annotate("label", x = -83.0305, y = 40.0067, label = "Ohio State University", size=4.5, hjust=0.35, vjust=-0.50, alpha=0.50) +
  annotate("label", x = -83.0060, y = 39.9692, label = "Nationwide Center", size=4.5, hjust=-0.10, vjust=0.75, alpha=0.50) + 
  annotate("label", x = -82.96275, y = 40.00867, label = "Mean Location", size=4.5, hjust=-0.10, vjust=0.40, alpha=0.50) +
  theme(legend.position="none") + xlab("Longitude") + ylab("Latitude")

colMeans(data[ , c("hlong", "hlat")], na.rm=TRUE)


#summary statistics 


data <- read.csv("LBK.csv") 

data <- read.csv("COL.csv")



data <- read.csv("LBK.csv") 
data_old <- read.csv("LBK_old.csv") 

data_old$unique <- paste(data_old$Price, data_old$Latitude, data_old$Longitude)
data$unique <- paste(data$Price, data$Latitude, data$Longitude)

data$Inc <- NULL

data_old = subset(data_old, select = c(Inc, unique))

data <- merge(data, data_old, by="unique", all.x=TRUE)

summary(lm(Price ~ SquareFoot+Lot+Garage+HouseAge+Env+Inc, data=data))

length(unique(data_old$Price))

mean(data$Price)

data %>% mutate(dist_TTU = distHaversine(cbind(-101.8949, 33.5943), cbind(Longitude, Latitude))) -> data

data %>% mutate(dist_Mall = distHaversine(cbind(-101.9254, 33.5393), cbind(Longitude, Latitude))) -> data





library(dplyr)
library(geosphere)

data <- read.csv("COL.csv")

data %>% mutate(dist_OSU = distHaversine(cbind(-83.0305, 40.0067), cbind(hlong, hlat))) -> data

data %>% mutate(dist_NWD = distHaversine(cbind(-83.0060, 39.9692), cbind(hlong, hlat))) -> data

data$dist_OSU <- data$dist_OSU/1000
data$dist_NWD <- data$dist_NWD/1000

data$dist_OSU = sqrt((-83.0305 - data$hlong)^2+(40.0067 - data$hlat)^2)

data$dist_NWD = sqrt((-83.0060 - data$hlong)^2+(39.9692 - data$hlat)^2)



data <- read.csv("LBK.csv")

data %>% mutate(dist_TTU = distHaversine(cbind(-101.8949, 33.5943), cbind(Longitude, Latitude))) -> data

data %>% mutate(dist_Mall = distHaversine(cbind(-101.9254, 33.5393), cbind(Longitude, Latitude))) -> data

data$dist_TTU = sqrt((-101.8949 - data$Longitude)^2+(33.5943 - data$Latitude)^2)

data$dist_Mall = sqrt((-101.9254 - data$Longitude)^2+(33.5393 - data$Latitude)^2)




ggmap::register_google(key = "AIzaSyA-UiMPuDvL_CFU34lyIDtow_B020R_sh8")

#getting Atlanta map
get_local_spot <-  get_map("Lubbock Texas", maptype = "roadmap", zoom = 11) 
map1 <- ggmap(get_local_spot)

data$dist_TTU <- data$dist_TTU/1000
data$dist_Mall <- data$dist_Mall/1000

data$mean_dist <- rowMeans(data[ , c("dist_TTU", "dist_Mall")], na.rm=TRUE)


lats<-c(33.51,33.605)
lons<-c(-101.95,-101.85)
bb<-make_bbox(lon=lons,lat=lats,f=0.05)
lbk<-get_map(bb,zoom=12,maptype="roadmap")
lbk_map <- ggmap(lbk) 

data <- subset(data, dist_TTU < 190)

lbk_map +
  geom_point(data = data, aes(x = Longitude, y = Latitude, colour = mean_dist), size = 3.5, alpha=0.75) + #shape=5
  #scale_color_brewer(palette = "Paired") +
  #labs(x = "Longitude", y="Latitude", colour="Middle school district") + 
  guides(colour = guide_legend(override.aes = list(size=5))) + 
  geom_point(aes(x=-101.8949,y=33.5943),colour="red", size=5, shape=17) + 
  geom_point(aes(x=-101.9254,y=33.5393),colour="red", size=5, shape=17) + 
  geom_point(aes(x=-101.92578,y=33.52777),colour="#E7B800", size=5, shape=15) + 
  #geom_label_repel(aes(x = -101.8949, y = 33.5943, label = "BAL"), size = 3) +
  annotate("label", x = -101.8949, y = 33.5943, label = "Texas Tech", size=4.5, hjust=-0.15, vjust=0, alpha=0.50) +
  annotate("label", x = -101.9254, y = 33.5393, label = "South Plains Mall", size=4.5, hjust=-0.10, vjust=-0.15, alpha=0.50) + 
  annotate("label", x = -101.92578, y = 33.52777, label = "Mean Location", size=4.5, hjust=-0.10, vjust=0.15, alpha=0.50) +
  theme(legend.position="none") + xlab("Longitude") + ylab("Latitude")

colMeans(data[ , c("Longitude", "Latitude")], na.rm=TRUE)

colMeans(data[ , c("Longitude", "Latitude")], na.rm=TRUE)


mean(data$Distance)
mean(data$dist_TTU)

mean(data$Distance2)
mean(data$dist_Mall)
