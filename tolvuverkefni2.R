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
oo <- subset(oo, select = -c(type))
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

## d)
staerd <- c(2,5,20,400)
staerd1 <- replicate(n = 10000, mean(sample(oo$curent_value,
        staerd[1],replace = TRUE)),simplify = TRUE )
staerd2 <- replicate(n = 10000, mean(sample(oo$curent_value,
        staerd[2],replace = TRUE)),simplify = TRUE )
staerd3 <- replicate(n = 10000, mean(sample(oo$curent_value,
        staerd[3],replace = TRUE)),simplify = TRUE )
staerd4 <- replicate(n = 10000, mean(sample(oo$curent_value,
        staerd[4],replace = TRUE)),simplify = TRUE )
remove(staerd)

## e)
staerd <- tibble(staerd1=staerd1, staerd2 = staerd2,
                            staerd3 = staerd3,staerd4=staerd4)
remove(staerd1,staerd2,staerd3,staerd4)
