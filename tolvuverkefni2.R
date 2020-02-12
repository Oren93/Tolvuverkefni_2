library(tidyverse)
library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)

oo <- read.csv(file = 'husnaedi.csv', fileEncoding = "UTF-8", sep =';')
oo = oo %>% rename("curent_value" = "nuvirdi",
                   "type" = "teg_eign",
                   "area" = "matssvaedi",
                   "size" = "ibm2",)
## a)
set.seed(0601)
hverfi<-sample(c(20,90,100),1)
oo <- oo%>%filter(area==hverfi)
oo <- subset(oo, select = -c(area))
oo <- filter(oo, type =="Íbúðareign")
remove(hverfi)

## b)
# mean(oo$current_value)
# min(oo$current_value)
# max(oo$current_value)

ggplot(oo, aes( x=curent_value)) + 
  geom_histogram()+xlab("price")
price_mean = mean(oo$curent_value)

## c)
set.seed(0601)
someVector <- sapply((1:5000),
  function(x) {mean(sample(oo$curent_value, x,replace = TRUE))})      

qplot(x = 1:5000,
      y = someVector,
      geom="line")+ xlab("sample size")+ ylab("mean price") +
      geom_hline(yintercept = price_mean, col="red")


qplot(x = 1:5000,
      y = sapply((1:5000), function(x)
      {mean(sample(oo$curent_value, x,replace = TRUE))}),
      geom="line")+ xlab("sample size")+ ylab("mean price") +
      geom_hline(yintercept = price_mean, col="red")


