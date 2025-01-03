### 01 #################################################################################################################
setwd("C:/Data/04_Programs/R/AoC/2024/")
source("./aoc_functions.r")
tf = data.table::fread("./Data/input01.txt")

## A ##

with(tf, sum(abs(sort(V1)-sort(V2))))

## B ##

merge(tf %>% select(V1), tf %>% select(V2), by.x = "V1", by.y = "V2") %>% "[["(1) %>% sum()
