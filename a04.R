### 04 #################################################################################################################
setwd("C:/Data/04_Programs/R/AoC/2024/")
source("./aoc_functions.r")
tf = readr::read_lines("./Data/input04.txt")

chechAllDirs = function(mtr){
  rbind(head(diag(mtr),4) == c("S", "A", "M", "X"),
        tail(diag(mtr),4) == c("X", "M", "A", "S"),
        mtr[4,1:4] == c("S", "A", "M", "X"),
        mtr[4,4:7] == c("X", "M", "A", "S"),
        mtr[1:4,4] == c("S", "A", "M", "X"),
        mtr[4:7,4] == c("X", "M", "A", "S"),
        (diag(mtr[nrow(mtr):1,]) %>% head(4)) == c("S", "A", "M", "X"),
        (diag(mtr[nrow(mtr):1,]) %>% tail(4)) == c("X", "M", "A", "S")) %>% apply(1, all) %>% sum()
}

## A ##

mat = strsplit(tf, split = "") %>% do.call(what = "rbind")
mat = rbind("-", "-", "-", mat, "-", "-", "-")
mat = cbind("-", "-", "-", mat, "-", "-", "-")

sapply(0:(nrow(mat)-7), function(k){
  sapply(0:(ncol(mat)-7), function(j){
    chechAllDirs(mat[1:7+k, 1:7+j])
  })
}) %>% sum()

## B ##

px = c("M.M",
       ".A.",
       "S.S")

pat1 = strsplit(px, split = "") %>% do.call(what = "rbind")
pat2 = t(pat1)
pat3 = pat1[3:1,]
pat4 = t(pat3)

sapply(0:(nrow(mat)-3), function(k){
  sapply(0:(ncol(mat)-3), function(j){
    (sum(mat[1:3+k, 1:3+j] == pat1) == 5) |
      (sum(mat[1:3+k, 1:3+j] == pat2) == 5) |
      (sum(mat[1:3+k, 1:3+j] == pat3) == 5) |
      (sum(mat[1:3+k, 1:3+j] == pat4) == 5)
  })
}) %>% sum()
