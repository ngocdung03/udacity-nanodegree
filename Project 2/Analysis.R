rm(list = ls())
library(tidyverse)
library(ggplot2)
library(stringr)
#library(lubridate)
setwd("C:/Users/ngocdung/Dropbox/Front-end developer/src/data-science-path/udacity-nanodegree/Project 2/Support materials")
list.files()
month_sales <- read.csv("p2-2010-pawdacity-monthly-sales-p2-2010-pawdacity-monthly-sales.csv", stringsAsFactors = F)
population <- read.csv("p2-partially-parsed-wy-web-scrape.csv", stringsAsFactors = F)
naics <- read.csv("p2-wy-453910-naics-data.csv", stringsAsFactors = F)
demographic <- read.csv("p2-wy-demographic-data.csv", stringsAsFactors = F)

#### Data wrangling ####
# Separate two columns by "|", rename some columns, remove N/A rows
population2 <- population %>% 
  separate(col=City.County,into=c("City","County"), sep='\\|') %>%   #"|" is a special character, escape twice   
  #str_replace_all(.$City," \\?","") %>% 
  mutate(
    Estimate = parse_number(X2014.Estimate),
    Census2010 = parse_number(X2010.Census),
    Census2000 = parse_number(X2000.Census)) %>% 
  filter(City!='')

# Remove ?" characters
population2["City"] <- str_replace_all(population2$City,"\\?","")  

# Remove trailing whitespace
population2["City"] <- trimws(population2$City)   #unique()

# Merge datasets by City 
month_sales2 <- month_sales %>% 
  mutate(City = CITY) %>%
  merge(., population2[which(colnames(population2) %in% c("City","Census2010"))], by="City") %>%
  merge(., demographic, by="City") %>% 
  mutate(Total.sales = January+February+March+April+May+June+July+August+September+October+November+December)

# Calculate sum
apply(month_sales2[c("Census2010",
                     "Total.sales",
                     "Households.with.Under.18",
                     "Land.Area", 
                     "Population.Density", 
                     "Total.Families")],
      2,sum)
  
# Calculate average  
apply(month_sales2[c("Census2010",
                     "Total.sales",
                     "Households.with.Under.18",
                     "Land.Area", 
                     "Population.Density", 
                     "Total.Families")],
      2,mean)

is_outlier <- function(vector) {
  q25 <- unname(quantile(vector, 0.25))
  q75 <- unname(quantile(vector, 0.75))
  iqr <- q75 - q25
  if(min(vector) < q25 - 1.5*iqr | max(vector) > q75 + 1.5*iqr) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

apply(month_sales2[c("Census2010",
                     "Total.sales",
                     "Households.with.Under.18",
                     "Land.Area", 
                     "Population.Density", 
                     "Total.Families")],
      2,is_outlier)

summary(month_sales2[c("Census2010",
                     "Total.sales",
                     "Households.with.Under.18",
                     "Land.Area", 
                     "Population.Density", 
                     "Total.Families")])

# Display the boolean matrix for is_outlier of cities x vars