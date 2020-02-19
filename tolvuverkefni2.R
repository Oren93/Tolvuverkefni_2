library(tidyverse)
library(readr)
library(dplyr)
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
# mean(oo$curent_value)
# min(oo$curent_value)
# max(oo$curent_value)

ggplot(oo, aes( x=curent_value)) + 
  geom_histogram()+xlab("Price (thousands - ISK)")+ylab("Frequency - Number of properties")
price_mean = mean(oo$curent_value)

## c)
set.seed(0601)
someVector <- sapply((1:5000),
  function(x) {mean(sample(oo$curent_value, x,replace = TRUE))})      

qplot(x = 1:5000,
      y = someVector,
      geom="line")+ xlab("Sample size")+ ylab("Mean price") +
      geom_hline(yintercept = price_mean, col="red")
rm(someVector)

qplot(x = 1:5000,
      y = sapply((1:5000), function(x)
      {mean(sample(oo$curent_value, x,replace = TRUE))}),
      geom="line")+ xlab("sample size")+ ylab("mean price") +
      geom_hline(yintercept = price_mean, col="red")

## d)
staerd <- c(2,5,20,400)
for (x in 1:4) {
assign(paste(c("staerd", x), collapse = ""),
replicate(n = 10000, mean(sample(oo$curent_value,
staerd[x],replace = TRUE)),simplify = TRUE ))
  
  
}
remove(staerd,x)

## e)
num <- c(1:10000)
staerd <- tibble(num,staerd1=staerd1, staerd2 = staerd2,
                 staerd3 = staerd3,staerd4=staerd4)
remove(staerd1,staerd2,staerd3,staerd4)

st <- gather(staerd,key="sample", value=staerd,
            c(staerd1,staerd2,staerd3,staerd4))
st

#works ggplot
ggplot(data = st, aes(x = staerd)) +
    geom_histogram(bins=50)+geom_vline(xintercept = price_mean, col="red")+ ##facet_wrap(~sample)
  facet_wrap(~sample, nrow = 2, ncol = 2, scales = "fixed",
             shrink = TRUE, labeller = "label_value", as.table = TRUE,
             switch = NULL, drop = TRUE, dir = "h", strip.position = "top")
  
#qplot
qplot(st,aes(x =num,y = staerd),     geom="line")+ xlab("x")+ ylab("y") +
  geom_hline(yintercept = price_mean, col="red")+ ##facet_wrap(~sample)
  facet_wrap(~sample, nrow = 2, ncol = 2, scales = "fixed",
             shrink = TRUE, labeller = "label_value", as.table = TRUE,
             switch = NULL, drop = TRUE, dir = "h", strip.position = "top")

qplot(staerd,data=st) + facet_wrap(~sample)


## f)
ggplot(data = st, aes(x = staerd)) +
  geom_histogram(bins=50)+geom_vline(xintercept = price_mean, col="red")+ ##facet_wrap(~sample)
  facet_wrap(~sample, nrow = 2, ncol = 2, scales = "free",
             shrink = TRUE, labeller = "label_value", as.table = TRUE,
             switch = NULL, drop = TRUE, dir = "h", strip.position = "top")


## g)

tolfr <- tibble(item=c("n = 2", "n = 5", "n = 20", "n = 400", "dataframe"),
 mean = c(mean(staerd$staerd1), mean(staerd$staerd2),
 mean(staerd$staerd3), mean(staerd$staerd4), mean(oo$curent_value)),
 variance = c(var(staerd$staerd1), var(staerd$staerd2),
 var(staerd$staerd3), var(staerd$staerd4), var(oo$curent_value)))


## h)




## i)



