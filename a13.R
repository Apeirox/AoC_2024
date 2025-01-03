### 13 #################################################################################################################
setwd("C:/Data/04_Programs/R/AoC/2024/")
source("./aoc_functions.r")
md = readr::read_lines("./Data/input13.txt")

library(gmp)

solveEquations = function(a1, b1, c1, a2, b2, c2) {
  a1 = as.bigq(a1)
  b1 = as.bigq(b1)
  c1 = as.bigq(c1)
  a2 = as.bigq(a2)
  b2 = as.bigq(b2)
  c2 = as.bigq(c2)
  
  det = a1 * b2 - a2 * b1
  x = (c1 * b2 - c2 * b1) / det
  y = (a1 * c2 - a2 * c1) / det
  c(x, y)
}

## A ##

md_list = regmatches(md, gregexpr("\\d+", md))
md_nums = split(md_list, ceiling(seq_along(md_list) / 4)) %>% lapply(function(x) {unlist(x) %>% as.integer()})

out = sapply(md_nums, function(x){
  j = (x[6]*x[1]/x[2]-x[5])/(x[4]*x[1]/x[2]-x[3])
  k = (x[5]-j*x[3])/x[1]
  if (k >= -1e-12 & abs(round(k) - k) < 1e-12 & j >= -1e-12 & abs(round(j) - j) < 1e-12)
    c(3*k,j) else c(0,0)
}) %>% t()

sum(out)

## B ##

out = sapply(md_nums, function(x){
  a1 = as.character(x[1])
  b1 = as.character(x[3])
  c1 = as.character(x[5]+1e13)
  a2 = as.character(x[2])
  b2 = as.character(x[4])
  c2 = as.character(x[6]+1e13)
  out = solveEquations(a1, b1, c1, a2, b2, c2)
  if (out[1] %>% is.whole() & out[2] %>% is.whole()) asNumeric((3*out[1]+out[2])) else 0
}) %>% t()

sum(out)
