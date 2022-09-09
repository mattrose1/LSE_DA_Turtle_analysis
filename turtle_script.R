##########################################################
## WEEK 4 - VISUALISE AND GATHER INSIGHTS
##########################################################
## install the packages
install.packages('tidyverse')
library(tidyverse)
install.packages('ggplot2')
library(ggplot2)
install.packages('dplyr')
library(dplyr)

## check the working directory
getwd()

## import the turtle sales data
turtle.sales <- read.csv('turtle_sales.csv', header=TRUE)
## view the imported CSV file
View(turtle.sales)

## create the turtle sales subset without Ranking, Year, Genre and Publisher columns
turtle.subset <- select(turtle.sales, -Ranking, -Year, -Genre, - Publisher)

## view the subset of data
View(turtle.subset)

## view the subset data summary
summary(turtle.subset)

## register the ggplot2 library
library(ggplot2)

## produce a scatterplot looking at Global Sales and Platform using the subset data
qplot(Global_Sales, Product, data=turtle.subset)

## produce a histogram with adjusted bins, looking at the distribution of EU Sales
qplot(EU_Sales, Product, data=turtle.subset)

## produce a histogram with adjusted bins, looking at the distribution of EU Sales
qplot(NA_Sales, Product, data=turtle.subset)

## create a boxplot to identify outliers in North America sales
qplot(Platform, Global_Sales, data=turtle.subset, geom='boxplot')

######################################################################
## WEEK 5 - CLEAN, MANIPULATE AND VISUALISE
#######################################################################
## create a new turtle dataframe grouping by the Product ID and summarising 
## the sales for each region and globally

turtle.groupby <- turtle.subset %>% group_by(Product) %>%
  summarise(total_NA_Sales = sum(NA_Sales),
            total_EU_Sales = sum(EU_Sales),
            total_Global_Sales = sum(Global_Sales),
            .groups = 'drop')

## view and summarise the new dataframe
View(turtle.groupby)
summary(turtle.groupby)


##calculating the mean, min and max for all sales
## mean
mean(turtle.subset$NA_Sales)
mean(turtle.subset$EU_Sales)
mean(turtle.subset$Global_Sales)

## min
min(turtle.subset$NA_Sales)
min(turtle.subset$EU_Sales)
min(turtle.subset$Global_Sales)


## max
max(turtle.subset$NA_Sales)
max(turtle.subset$EU_Sales)
max(turtle.subset$Global_Sales)

## subset summary
summary(turtle.subset)


## qqnorm analysis of all sales data
qqnorm(turtle.subset$NA_Sales)
qqline(turtle.subset$NA_Sales)
qqnorm(turtle.subset$EU_Sales)
qqline(turtle.subset$EU_Sales)
qqnorm(turtle.subset$Global_Sales)
qqline(turtle.subset$Global_Sales)

## shapiro test of all sales data
shapiro.test(turtle.subset$NA_Sales)
shapiro.test(turtle.subset$EU_Sales)
shapiro.test(turtle.subset$Global_Sales)

## install moments to be able to use skewness and kurtosis
install.packages('moments')
library(moments)

## run skewness and kurtosis over all sales data
skewness(turtle.subset$NA_Sales)
kurtosis(turtle.subset$NA_Sales)
skewness(turtle.subset$EU_Sales)
kurtosis(turtle.subset$EU_Sales)
skewness(turtle.subset$Global_Sales)
kurtosis(turtle.subset$Global_Sales)

#####################################################
## WEEK 6 - MODELLING
#####################################################
## SIMPLE LINEAR REGRESSION
#####################################################

## FIND CORRELATION BETWEEN SALES VARIABLES
## EU SALES AND GLOBAL SALES

cor(turtle.subset$EU_Sales, turtle.subset$Global_Sales)
plot(turtle.subset$Global_Sales, turtle.subset$EU_Sales, data=turtle.subset)

## Create an EU Sales model against Global Sales
EUSales.model <- lm(Global_Sales ~ EU_Sales, data=turtle.subset)
EUSales.model

## View the full model summary
summary(EUSales.model)

## T-VALUE OF 34.241 & R-SQUARED VALUE OF 77.01%
######################################################

## NA SALES AND GLOBAL SALES
cor(turtle.subset$NA_Sales, turtle.subset$Global_Sales)
plot(turtle.subset$Global_Sales, turtle.subset$NA_Sales, data=turtle.subset)


## Create a NA Sales model against Global Sales
NASales.model <- lm(NA_Sales ~ Global_Sales, data=turtle.subset)

## view the model
NASales.model

## View the model summary
summary(NASales.model)

## T-VALUE OF 49.30 & R-SQUARED VALUE OF 87.41% REPRESENTS
## A HIGHLY SIGNIFICANT VALUE


##########################################################
## MULTIPLE LINEAR REGRESSION
##########################################################

turtle.subset2 <- select(turtle.subset, -Platform)
  
## CORRELATION BETWEEN THE NUMERIC VALUES USING THE NEW TURTLE SUBSET
cor(turtle.subset2)

## CREATE A MULTIPLE LINEAR REGRESSION
turtle.mlr = lm(Global_Sales ~ NA_Sales + EU_Sales, 
                data=turtle.subset2)

summary(turtle.mlr)

## R-SQUARED VALUE OF 96.87 % REPRESENTS A VERY HIGHLY SIGNIFICANT VALUE 
## OF NA AND EU SALES AGAINST GLOBAL SALES

############################################################
## PREDICT VALUES
############################################################
## create dataframe for holding predicted sales values

region.sales <- data.frame(NA_Sales = c(34.02, 3.93, 2.73, 2.26, 22.08), 
                           EU_Sales = c(23.80, 1.56, 0.65, 0.97, 0.52))

# Predict Global Sales using new values
predict(turtle.mlr, region.sales)

## PREDICTED VALUES FROM THE MODEL ARE: 
## 71.468572, 6.856083, 4.248367, 4.134744, 26.431567

## OBSERVED VALUES FROM SALES DATA ARE:
## 67.85, 6.04, 4.32, 3.53, 23.21


