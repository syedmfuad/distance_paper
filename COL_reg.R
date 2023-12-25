

library(ggmap)
library(ggrepel)

rm(list=ls())

setwd("C:/Users/syedm/Desktop/Distance paper")

data <- read.csv("COL.csv")

data$OSULong <- -83.0305
data$OSULat <- 40.0067

data$NWDLong <- -83.0060
data$NWDLat <- 39.9692

data$YLong1 <- data$hlong-data$OSULong 
data$XLat1 <- data$hlat-data$OSULat
data$YLong1Sqrd <- (data$hlong-data$OSULong)^2
data$XLat1Sqrd <- (data$hlat-data$OSULat)^2 

data$YLong2 <- data$hlong-data$NWDLong 
data$XLat2 <- data$hlat-data$NWDLat
data$YLong2Sqrd <- (data$hlong-data$NWDLong)^2
data$XLat2Sqrd <- (data$hlat-data$NWDLat)^2 

library(geosphere)
library(dplyr)

data %>% rowwise() %>% 
  mutate(distOSU = distHaversine(c(hlong, hlat), c(OSULong, OSULat))) -> data

data %>% rowwise() %>% 
  mutate(distNWD = distHaversine(c(hlong, hlat), c(NWDLong, NWDLat))) -> data 

data$twostory <- ifelse(data$onestory == 1, 0, 1)

summary(mod <- lm(lnhprice ~ buildingsqft+I(lotsize/1000)+agehouse+twostory+incomethous+offensecap+pctwhite_cbg, data=data)) 

AIC(mod)
logLik(mod)

summary(mod <- lm(lnhprice ~ buildingsqft+I(lotsize/1000)+agehouse+twostory+incomethous+offensecap+pctwhite_cbg+I(distOSU/1000), data=data)) 

AIC(mod)
logLik(mod)

summary(mod <- lm(lnhprice ~ buildingsqft+I(lotsize/1000)+agehouse+twostory+incomethous+offensecap+pctwhite_cbg+I(distNWD/1000), data=data)) 

AIC(mod)
logLik(mod) 

summary(mod <- lm(lnhprice ~ buildingsqft+I(lotsize/1000)+agehouse+twostory+incomethous+offensecap+pctwhite_cbg+I(distOSU/1000)+I(distNWD/1000), data=data)) 

AIC(mod)
logLik(mod)

summary(mod <- lm(lnhprice ~ buildingsqft+I(lotsize/1000)+agehouse+twostory+incomethous+offensecap+pctwhite_cbg+YLong1+XLat1+YLong1Sqrd+XLat1Sqrd, data=data)) 

AIC(mod)
logLik(mod)

summary(mod <- lm(lnhprice ~ buildingsqft+I(lotsize/1000)+agehouse+twostory+incomethous+offensecap+pctwhite_cbg+YLong2+XLat2+YLong2Sqrd+XLat2Sqrd, data=data)) 

AIC(mod)
logLik(mod)

# Step 1: Determine the borders
min_long <- min(data$hlong)-25
max_long <- max(data$hlong)+25
min_lat <- min(data$hlat)-25
max_lat <- max(data$hlat)+25

# Step 2, 3, 4: Loop to simulate, analyze, and store results
coefficients_list1 <- list()  # Initialize a list to store results
coefficients_list2 <- list()
coefficients_list3 <- list()

for(i in 1:100) {
  # Randomly simulate a set of coordinates
  
  data$SimLong = runif(n = 1, min = min_long, max = max_long)
  data$SimLat = runif(n = 1, min = min_lat, max = max_lat) 
  
  data$YLong3 <- data$hlong-data$SimLong 
  data$XLat3 <- data$hlat-data$SimLat
  data$YLong3Sqrd <- (data$hlong-data$SimLong)^2
  data$XLat3Sqrd <- (data$hlat-data$SimLat)^2 
  
  data %>% rowwise() %>% 
    mutate(distSim = distHaversine(c(hlong, hlat), c(SimLong, SimLat))) -> data 
  
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

# Now you can use the dataframe for plotting
if("SquareFoot" %in% names(coefficients_df1)) {
  hist(coefficients_df1$SquareFoot, main = "Histogram of SquareFoot Coefficients", xlab = "Coefficient Value")
} else {
  cat("The 'SquareFoot' column does not exist in the dataframe.\n")
}

# Now you can use the dataframe for plotting
if("SquareFoot" %in% names(coefficients_df2)) {
  hist(coefficients_df2$SquareFoot, main = "Histogram of SquareFoot Coefficients", xlab = "Coefficient Value")
} else {
  cat("The 'SquareFoot' column does not exist in the dataframe.\n")
}

# Now you can use the dataframe for plotting
if("SquareFoot" %in% names(coefficients_df3)) {
  hist(coefficients_df3$SquareFoot, main = "Histogram of SquareFoot Coefficients", xlab = "Coefficient Value")
} else {
  cat("The 'SquareFoot' column does not exist in the dataframe.\n")
}


