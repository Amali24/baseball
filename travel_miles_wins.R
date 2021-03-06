rm(list = ls())
library(tidyverse)
library(retrosheet)
retro_2019_sched <- getRetrosheet("schedule", 2019)
