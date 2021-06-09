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

train <- month_sales2[c("City", 
                        "Census2010",
                        "Total.sales",
                        "Households.with.Under.18",
                        "Land.Area", 
                        "Population.Density", 
                        "Total.Families")]
              
row.names(train) <- row.names(month_sales2)
# Calculate sum
apply(train, 2,sum)
  
# Calculate average  
apply(train, 2,mean)

# Display the boolean matrix for is_outlier of cities x vars
is_outlier2 <- function(vector) {
  q25 <- unname(quantile(vector, 0.25))
  q75 <- unname(quantile(vector, 0.75))
  iqr <- q75 - q25
  a <- vector(mode="logical", length=length(vector))
  names(a) <- train$City
  lower <- q25 - 1.5*iqr
  upper <- q75 + 1.5*iqr
  for (i in 1:length(vector)){              #mistake without '1:'
    if((vector[i] < lower) | (vector[i] > upper)) {
      a[i] <- TRUE
    }
    else{   
      a[i] <- FALSE
    }
  }
  return(a)
}
apply(train[-1],2,is_outlier2)

#write.csv(train, "training_dataset.csv",row.names = F)
