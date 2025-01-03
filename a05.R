### 05 #################################################################################################################
setwd("C:/Data/04_Programs/R/AoC/2024/")
source("./aoc_functions.r")

md = readr::read_lines("./Data/input05_orders.txt")
od = readr::read_lines("./Data/input05_orderings.txt")

## A ##

sapply(od, function(pages){
  z = strsplit(pages, split = ",")[[1]]
  all_comb = outer(z, z, function(x,y) paste0(x, "|", y))
  inv_comb = all_comb[lower.tri(all_comb)]
  if (!any(inv_comb %in% md)) as.integer(z[length(z) %/% 2 + 1]) else 0
}) %>% sum()

## B ##