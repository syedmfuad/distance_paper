
rm(list=ls())

library(tidyverse)
library(spdep)
library(spatialreg)
library(rgdal)
library(rgeos)
library(spatialreg)

setwd("C:/Users/syedm/Desktop/Distance paper")

data <- read.csv("HHData.csv")

data$TTULong <- -101.8746
data$TTULat <- 33.5846

data$MallLong <- -101.92548
data$MallLat <- 33.53938 

data$RandomLong <- max(data$Longitude)
data$RandomLat <- max(data$Latitude)

data$YLong1 <- data$Longitude-data$TTULong 
data$XLat1 <- data$Latitude-data$TTULat
data$YLong1Sqrd <- (data$Longitude-data$TTULong)^2
data$XLat1Sqrd <- (data$Latitude-data$TTULat)^2 

data$YLong2 <- data$Longitude-data$MallLong 
data$XLat2 <- data$Latitude-data$MallLat
data$YLong2Sqrd <- (data$Longitude-data$MallLong)^2
data$XLat2Sqrd <- (data$Latitude-data$MallLat)^2

data$YLong3 <- data$Longitude-data$RandomLong 
data$XLat3 <- data$Latitude-data$RandomLat
data$YLong3Sqrd <- (data$Longitude-data$RandomLong)^2
data$XLat3Sqrd <- (data$Latitude-data$RandomLat)^2 





library(geosphere)
library(dplyr)

data %>% rowwise() %>% 
  mutate(distTTU = distHaversine(c(Longitude, Latitude), c(TTULong, TTULat))) -> data

data %>% rowwise() %>% 
  mutate(distMall = distHaversine(c(Longitude, Latitude), c(MallLong, MallLat))) -> data 

data %>% rowwise() %>% 
  mutate(distRandom = distHaversine(c(Longitude, Latitude), c(RandomLong, RandomLat))) -> data


library(sp)
library(spdep)
library(sf)

# Assuming 'data' is your data frame with 'Longitude' and 'Latitude'
coordinates(data) <- ~Longitude + Latitude  # Convert to spatial points

# Set the projection if known (e.g., WGS84 is EPSG:4326)
proj4string(data) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

# Convert to metric system if necessary (using sf package)
data_sf <- st_as_sf(data, coords = c("Longitude", "Latitude"), crs = 4326)
data_sf <- st_transform(data_sf, crs = 3395)  # Example metric system CRS

# Create the weights matrix with a 500-meter threshold
dist_threshold <- 500  # 500 meters
neighbors <- dnearneigh(st_coordinates(data_sf), 0, dist_threshold)

# Convert to a spatial weights matrix
weights_matrix <- nb2listw(neighbors, style = "W", zero.policy = TRUE)



# Load necessary library
library(spdep)

# Assuming you have already created 'data' and 'weights_matrix'

# Create spatially lagged variables
data$lag_SquareFoot <- lag.listw(weights_matrix, data$SquareFoot)
data$lag_Lot <- lag.listw(weights_matrix, data$Lot)
data$lag_HouseAge <- lag.listw(weights_matrix, data$HouseAge)
data$lag_Garage <- lag.listw(weights_matrix, data$Garage)
data$lag_ExpBird <- lag.listw(weights_matrix, data$ExpBird)
data$lag_Inc <- lag.listw(weights_matrix, data$Inc)

data$lag_YLong1 <- lag.listw(weights_matrix, data$YLong1)
data$lag_XLat1 <- lag.listw(weights_matrix, data$XLat1)
data$lag_YLong1Sqrd <- lag.listw(weights_matrix, data$YLong1Sqrd)
data$lag_XLat1Sqrd <- lag.listw(weights_matrix, data$XLat1Sqrd)

data$lag_YLong2 <- lag.listw(weights_matrix, data$YLong2)
data$lag_XLat2 <- lag.listw(weights_matrix, data$XLat2)
data$lag_YLong2Sqrd <- lag.listw(weights_matrix, data$YLong2Sqrd)
data$lag_XLat2Sqrd <- lag.listw(weights_matrix, data$XLat2Sqrd)

data$lag_YLong3 <- lag.listw(weights_matrix, data$YLong3)
data$lag_XLat3 <- lag.listw(weights_matrix, data$XLat3)
data$lag_YLong3Sqrd <- lag.listw(weights_matrix, data$YLong3Sqrd)
data$lag_XLat3Sqrd <- lag.listw(weights_matrix, data$XLat3Sqrd)


coefficients_list <- list() 
results_list <- list() 

# Run the linear model with spatially lagged variables
model1 <- lm(log(Price) ~ SquareFoot + Lot + HouseAge + Garage + ExpBird + I(Inc/1000), 
             data = data)

summary(model1) 

coefficients_list[[1]] <- coef(model1)

results_df <- data.frame(AIC = AIC(model1), LogLik = logLik(model1), AdjR2 = summary(model1)$adj.r.squared)
results_list[[1]] <- results_df 

# Run the linear model with spatially lagged variables
model2 <- lm(log(Price) ~ SquareFoot + Lot + HouseAge + Garage + ExpBird + I(Inc/1000) + 
               I(distTTU/1000), 
             data = data)

summary(model2) 

coefficients_list[[2]] <- coef(model2)

results_df <- data.frame(AIC = AIC(model2), LogLik = logLik(model2), AdjR2 = summary(model2)$adj.r.squared)
results_list[[2]] <- results_df 

# Run the linear model with spatially lagged variables
model3 <- lm(log(Price) ~ SquareFoot + Lot + HouseAge + Garage + ExpBird + I(Inc/1000) + 
               I(distMall/1000), 
             data = data)

summary(model3) 

coefficients_list[[3]] <- coef(model3)

results_df <- data.frame(AIC = AIC(model3), LogLik = logLik(model3), AdjR2 = summary(model3)$adj.r.squared)
results_list[[3]] <- results_df


# Run the linear model with spatially lagged variables
model4 <- lm(log(Price) ~ SquareFoot + Lot + HouseAge + Garage + ExpBird + I(Inc/1000) + 
               I(distRandom/1000), 
             data = data)

summary(model4) 

coefficients_list[[4]] <- coef(model4) 

results_df <- data.frame(AIC = AIC(model4), LogLik = logLik(model4), AdjR2 = summary(model4)$adj.r.squared)
results_list[[4]] <- results_df

# Run the linear model with spatially lagged variables
model5 <- lm(log(Price) ~ SquareFoot + Lot + HouseAge + Garage + ExpBird + I(Inc/1000) + 
               I(distTTU/1000) + I(distMall/1000) + I(distRandom/1000), 
             data = data)

summary(model5) 

coefficients_list[[5]] <- coef(model5) 

results_df <- data.frame(AIC = AIC(model5), LogLik = logLik(model5), AdjR2 = summary(model5)$adj.r.squared)
results_list[[5]] <- results_df


# Run the linear model with spatially lagged variables
model6 <- lm(log(Price) ~ SquareFoot + Lot + HouseAge + Garage + ExpBird + I(Inc/1000) + 
             YLong1 + XLat1 + YLong1Sqrd + XLat1Sqrd, 
             data = data)

summary(model6)

coefficients_list[[6]] <- coef(model6) 

results_df <- data.frame(AIC = AIC(model6), LogLik = logLik(model6), AdjR2 = summary(model6)$adj.r.squared)
results_list[[6]] <- results_df


# Run the linear model with spatially lagged variables
model7 <- lm(log(Price) ~ SquareFoot + Lot + HouseAge + Garage + ExpBird + I(Inc/1000) + 
             YLong2 + XLat2 + YLong2Sqrd + XLat2Sqrd, 
             data = data)

summary(model7) 

coefficients_list[[7]] <- coef(model7) 

results_df <- data.frame(AIC = AIC(model7), LogLik = logLik(model7), AdjR2 = summary(model7)$adj.r.squared)
results_list[[7]] <- results_df


# Run the linear model with spatially lagged variables
model8 <- lm(log(Price) ~ SquareFoot + Lot + HouseAge + Garage + ExpBird + I(Inc/1000) + 
             YLong3 + XLat3 + YLong3Sqrd + XLat3Sqrd, 
             data = data)

summary(model8)

coefficients_list[[8]] <- coef(model8) 

results_df <- data.frame(AIC = AIC(model8), LogLik = logLik(model8), AdjR2 = summary(model8)$adj.r.squared)
results_list[[8]] <- results_df


# Run the linear model with spatially lagged variables
model9 <- lm(log(Price) ~ SquareFoot + Lot + HouseAge + Garage + ExpBird + I(Inc/1000) + 
             YLong1 + XLat1 + YLong1Sqrd + XLat1Sqrd + 
             YLong2 + XLat2 + YLong2Sqrd + XLat2Sqrd + 
             YLong3 + XLat3 + YLong3Sqrd + XLat3Sqrd, 
             data = data)

summary(model9)

coefficients_list[[9]] <- coef(model9) 

results_df <- data.frame(AIC = AIC(model9), LogLik = logLik(model9), AdjR2 = summary(model9)$adj.r.squared)
results_list[[9]] <- results_df


# Run the linear model with spatially lagged variables
model10 <- lm(log(Price) ~ SquareFoot + Lot + HouseAge + Garage + ExpBird + I(Inc/1000) + 
             lag_SquareFoot + lag_Lot + lag_HouseAge + lag_Garage + lag_ExpBird + I(lag_Inc/1000), 
             data = data)

summary(model10)

coefficients_list[[10]] <- coef(model10) 

results_df <- data.frame(AIC = AIC(model10), LogLik = logLik(model10), AdjR2 = summary(model10)$adj.r.squared)
results_list[[10]] <- results_df



coefficients_df <- bind_rows(coefficients_list)

coefficients_df <- data.frame(t(coefficients_df))


results_df <- do.call(rbind, results_list)

results_df <- data.frame(t(results_df))







# Run the linear model with spatially lagged variables
model1 <- lm(Price ~ SquareFoot + Lot + HouseAge + Garage + ExpBird + Inc + 
              #YLong1 + XLat1 + YLong1Sqrd + XLat1Sqrd + 
              lag_SquareFoot + lag_Lot + lag_HouseAge + lag_Garage + lag_ExpBird + lag_Inc, 
              #lag_YLong1 + lag_XLat1 + lag_YLong1 + lag_XLat1, 
            data = data)

# Summary of the model
summary(model1) 

# Run the linear model with spatially lagged variables
model2 <- lm(Price ~ SquareFoot + Lot + HouseAge + Garage + ExpBird + Inc +
              YLong1 + XLat1 + YLong1Sqrd + XLat1Sqrd, 
            data = data)

# Summary of the model
summary(model2)

logLik(model1)
sqrt(2/3)* summary(model1)$sigma

logLik(model2)
sqrt(2/3)* summary(model2)$sigma








