### 02 #################################################################################################################
setwd("C:/Data/04_Programs/R/AoC/2024/")
source("./aoc_functions.r")
tf = readr::read_lines("./Data/input02.txt")

## A ##

out1 = sapply(tf, function(x){
  arow = splitText(x) %>% as.integer() %>% diff()
  (all(arow > 0) | all(arow < 0)) & all(abs(arow) <= 3)
})
sum(out1)

## B ##

out2 = sapply(tf, function(x){
  arow = splitText(x) %>% as.integer()
  sapply(seq_along(arow), function(k){
    brow = arow[-k] %>% diff()
    (all(brow > 0) | all(brow < 0)) & all(abs(brow) <= 3)  
  }) %>% any()
})
sum(out2)