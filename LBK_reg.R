

library(ggmap)
library(ggrepel)

rm(list=ls())

setwd("C:/Users/syedm/Desktop/Distance paper")

data <- read.csv("HHData.csv")

data$TTULong <- -101.8746
data$TTULat <- 33.5846

data$MallLong <- -101.92548
data$MallLat <- 33.53938

data$YLong1 <- data$Longitude-data$TTULong 
data$XLat1 <- data$Latitude-data$TTULat
data$YLong1Sqrd <- (data$Longitude-data$TTULong)^2
data$XLat1Sqrd <- (data$Latitude-data$TTULat)^2 

data$YLong2 <- data$Longitude-data$MallLong 
data$XLat2 <- data$Latitude-data$MallLat
data$YLong2Sqrd <- (data$Longitude-data$MallLong)^2
data$XLat2Sqrd <- (data$Latitude-data$MallLat)^2 

library(geosphere)
library(dplyr)

data %>% rowwise() %>% 
  mutate(distTTU = distHaversine(c(Longitude, Latitude), c(TTULong, TTULat))) -> data

data %>% rowwise() %>% 
  mutate(distMall = distHaversine(c(Longitude, Latitude), c(MallLong, MallLat))) -> data 


summary(mod <- lm(log(Price) ~ SquareFoot+Lot+HouseAge+Garage+ExpBird+I(Inc/1000), data=data)) 

AIC(mod)
logLik(mod)

summary(mod <- lm(log(Price) ~ SquareFoot+Lot+HouseAge+Garage+ExpBird+I(Inc/1000)+I(distTTU/1000), data=data)) 

AIC(mod)
logLik(mod)

summary(mod <- lm(log(Price) ~ SquareFoot+Lot+HouseAge+Garage+ExpBird+I(Inc/1000)+I(distMall/1000), data=data)) 

AIC(mod)
logLik(mod) 

summary(mod <- lm(log(Price) ~ SquareFoot+Lot+HouseAge+Garage+ExpBird+I(Inc/1000)+I(distTTU/1000)+I(distMall/1000), data=data)) 

AIC(mod)
logLik(mod)

summary(mod <- lm(log(Price) ~ SquareFoot+Lot+HouseAge+Garage+ExpBird+I(Inc/1000)+YLong1+XLat1+YLong1Sqrd+XLat1Sqrd, data=data)) 

AIC(mod)
logLik(mod)

summary(mod <- lm(log(Price) ~ SquareFoot+Lot+HouseAge+Garage+ExpBird+I(Inc/1000)+YLong2+XLat2+YLong2Sqrd+XLat2Sqrd, data=data)) 

AIC(mod)
logLik(mod)

# Step 1: Determine the borders
min_long <- min(data$Longitude)-25
max_long <- max(data$Longitude)+25
min_lat <- min(data$Latitude)-25
max_lat <- max(data$Latitude)+25

# Step 2, 3, 4: Loop to simulate, analyze, and store results
coefficients_list1 <- list()  # Initialize a list to store results
coefficients_list2 <- list()
coefficients_list3 <- list()

for(i in 1:100) {
  # Randomly simulate a set of coordinates

    data$SimLong = runif(n = 1, min = min_long, max = max_long)
    data$SimLat = runif(n = 1, min = min_lat, max = max_lat) 
    
    data$YLong3 <- data$Longitude-data$SimLong 
    data$XLat3 <- data$Latitude-data$SimLat
    data$YLong3Sqrd <- (data$Longitude-data$SimLong)^2
    data$XLat3Sqrd <- (data$Latitude-data$SimLat)^2 
    
    data %>% rowwise() %>% 
      mutate(distSim = distHaversine(c(Longitude, Latitude), c(SimLong, SimLat))) -> data 
    
    model1 <- lm(Price ~ SquareFoot+Lot+HouseAge+Garage+ExpBird+Inc+
                 YLong3+XLat3+YLong3Sqrd+XLat3Sqrd, data=data)
    
    model2 <- lm(Price ~ SquareFoot+Lot+HouseAge+Garage+ExpBird+Inc+
                   distSim, data=data) 
    
    model3 <- lm(Price ~ SquareFoot+Lot+HouseAge+Garage+ExpBird+Inc, data=data)

    coefficients_list1[[i]] <- coef(model1) 
    coefficients_list2[[i]] <- coef(model2) 
    coefficients_list3[[i]] <- coef(model3) 
  
}

# Convert the list of coefficients into a dataframe
coefficients_df1 <- as.data.frame(do.call(rbind, coefficients_list1))
coefficients_df2 <- as.data.frame(do.call(rbind, coefficients_list2))
coefficients_df3 <- as.data.frame(do.call(rbind, coefficients_list3))



# Specify the name of the coefficient
coefficient_name <- "ExpBird"

# Plot the histogram for the first dataframe
hist(coefficients_df2[[coefficient_name]], col = rgb(0, 0, 1, 0.5), 
     main = paste("Histograms of", coefficient_name),
     xlab = coefficient_name, xlim = range(c(coefficients_df1[[coefficient_name]], coefficients_df2[[coefficient_name]])), 
     ylim = c(0, max(hist(coefficients_df1[[coefficient_name]], plot = FALSE)$counts, 
                     hist(coefficients_df2[[coefficient_name]], plot = FALSE)$counts)))

# Overlay the histogram for the second dataframe
#hist(coefficients_df2[[coefficient_name]], col = rgb(1, 0, 0, 0.5), add = TRUE)

vline_position <- 0
abline(v = mean(coefficients_df1[[coefficient_name]]), col = "red", lwd = 4) 
abline(v = mean(coefficients_df3[[coefficient_name]]), col = "black", lwd = 4) 

# Add a legend
#legend("topright", legend = c("df1", "df2"), fill = c(rgb(0, 0, 1, 0.5), rgb(1, 0, 0, 0.5)))

# Summarize the coefficient in both dataframes
cat("Summary of", coefficient_name, "in df1:\n")
print(summary(coefficients_df1[[coefficient_name]]))
sd(coefficients_df1[[coefficient_name]])
cat("\nSummary of", coefficient_name, "in df2:\n")
print(summary(coefficients_df2[[coefficient_name]]))
sd(coefficients_df2[[coefficient_name]])



















#rownames(coefficients_df) <- paste("Iteration", 1:nrow(coefficients_df))

if("SquareFoot" %in% names(coefficients_df1)) {
  hist(coefficients_df1$SquareFoot, main = "Histogram of SquareFoot Coefficients", xlab = "Coefficient Value")
} else {
  cat("The 'SquareFoot' column does not exist in the dataframe.\n")
}

if("SquareFoot" %in% names(coefficients_df2)) {
  hist(coefficients_df2$SquareFoot, main = "Histogram of SquareFoot Coefficients", xlab = "Coefficient Value")
} else {
  cat("The 'SquareFoot' column does not exist in the dataframe.\n")
}

if("SquareFoot" %in% names(coefficients_df3)) {
  hist(coefficients_df3$SquareFoot, main = "Histogram of SquareFoot Coefficients", xlab = "Coefficient Value")
} else {
  cat("The 'SquareFoot' column does not exist in the dataframe.\n")
}


