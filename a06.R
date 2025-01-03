### 06 #################################################################################################################
setwd("C:/Data/04_Programs/R/AoC/2024/")
source("./aoc_functions.r")
# md = readr::read_lines("input6_test.txt")
md = readr::read_lines("./Data/input06.txt")

## A ##

mat = strsplit(md, split = "") %>% do.call(what="rbind")

current_index = which(mat == "^", arr.ind = T)
blocked_index = which(mat == "#", arr.ind = T)
visited_index = current_index

direction = list()
direction[[1]] = structure(c(-1L, 0L), dim = 1:2)
direction[[2]] = structure(c(0L, 1L), dim = 1:2)
direction[[3]] = structure(c(1L, 0L), dim = 1:2)
direction[[4]] = structure(c(0L, -1L), dim = 1:2)
direction[[5]] = structure(c(-1L, 0L), dim = 1:2)

k = 1
while (current_index[1] > 0 & current_index[1] <= nrow(mat) & current_index[2] > 0 & current_index[2] <= ncol(mat)){
  if (any(apply(blocked_index, 1, function(row) all(row == (current_index + direction[[k]]))))) {k=k+1} else {
    print(k)
    current_index = current_index + direction[[k]]
    visited_index = rbind(visited_index, current_index)
    if (k == 5) k = 1
  }
}

nrow(visited_index[!duplicated(visited_index),]) - 1

## B ##