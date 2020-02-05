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


oo <- oo%>%filter(area==90)
oo <- subset(oo, select = -c(area))
