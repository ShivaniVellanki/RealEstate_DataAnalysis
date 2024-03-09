#Importing Libraries
library(corrplot)
library(ggplot2)
library(knitr)
library(plyr)
library(dplyr)
library(Hmisc)
library(finalfit)
library(funModeling) 
library(data.table)
library(car)
library(correlation)
library(skimr)
library(tidyverse)
library(table1)
library(MASS)
library(leaps)

# Importing Dataset from CSV file to R
houseDF <- read_csv('/Users/shivanivellanki/Downloads/archive (2)/data.csv')
houseDF

#Decsriptive Statistics using Summary function
summary(houseDF)
str(houseDF)

# Creating a Correlation matrix for houseData
houseDFCor <- cor(houseDF[unlist(lapply(houseDF,is.numeric),use.names= FALSE)])
view(houseDFCor)

# Correlation plot
corrplot(houseDFCor)

# ScatterPlot of variable that has Highest correlation with price ( Closest to 0.5)
scatterplot(price ~ sqft_living, data=houseDF,
            main="Highest relation between price ~ sqft_living ",
            xlab="Living Room Area", ylab="Price",
            col ='blue')

# ScatterPlot of variable that has Lowest correlation with price
scatterplot(price ~ yr_built, data=houseDF,
            main="Highest relation between price ~ yr_built ",
            xlab="Year Built", ylab="Price",
            col ='blue')

# Regression model using 3 continous variables
houseDFRegMod <- lm(formula = price ~ sqft_living + bathrooms + bedrooms, data=houseDF)
houseDFRegMod

summary(houseDFRegMod)

# Plotting the Regression Model
plot(houseDFRegMod)

# Checking for Multicollinearity 
vif(houseDFRegMod)

# Checking for outliers in the model
outlierTest(model = houseDFRegMod)

# deleting the outlier rows
houseDF2 <- houseDF[-c(4351,4347,2287,2762,1638),]
houseDFRegMod2 <- lm(formula = price ~ sqft_living + bathrooms + sqft_above, data=houseDF2)
houseDFRegMod2
summary(houseDFRegMod2)
outlierTest(model = houseDFRegMod2)
plot(houseDFRegMod2)

# Best Model
houseDFBestMod <- regsubsets(price ~ sqft_living + bathrooms + sqft_above, data=houseDF )
summary(houseDFBestMod)
