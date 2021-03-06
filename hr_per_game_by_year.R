rm(list = ls())
library(tidyverse)
library(retrosheet)

years <- c(1901:2020)

f <- grep("HR", gamelogFields, value = TRUE)

for (i in years){
  games <- getPartialGamelog(i, glFields = f)
  cur_hrs <- sum(games$HmHR) + sum(games$VisHR)
  print(paste(i, ": ", cur_hrs))
}
