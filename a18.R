### 18 #################################################################################################################
setwd("C:/Data/04_Programs/R/AoC/2024/")
source("./aoc_functions.r")
md = readr::read_lines("./Data/input18.txt")

## A ##
mat = matrix(1e6, 71, 71)
mat[1,1] = 0
mat = cbind(Inf, rbind(Inf, mat, Inf), Inf)

coords = strsplit(md, split = ",")
coords = lapply(coords, as.numeric)

for (k in 1:1024){
  mat[coords[[k]][2]+2,coords[[k]][1]+2] = Inf
}

for (n in 1:5000){
  mat[,-1] = ifelse(mat[,-1] == Inf, Inf, pmin(mat[,-nrow(mat)]+1, mat[,-1], na.rm = TRUE))
  mat[-1,] = ifelse(mat[-1,] == Inf, Inf, pmin(mat[-ncol(mat),]+1, mat[-1,], na.rm = TRUE))
  mat[,-ncol(mat)] = ifelse(mat[,-ncol(mat)] == Inf, Inf, pmin(mat[,-1]+1, mat[,-ncol(mat)], na.rm = TRUE))
  mat[-nrow(mat),] = ifelse(mat[-nrow(mat),] == Inf, Inf, pmin(mat[-1,]+1, mat[-nrow(mat),], na.rm = TRUE))
  if (n %% 25 == 0) print(n)
}

mat[nrow(mat)-1, ncol(mat)-1]

## B ##

mat = matrix(1e6, 71, 71)
mat[1,1] = 0
mat = cbind(Inf, rbind(Inf, mat, Inf), Inf)

coords = strsplit(md, split = ",")
coords = lapply(coords, as.numeric)

for (k in 1:2973){
  mat[coords[[k]][2]+2,coords[[k]][1]+2] = Inf
}

for (n in 1:3000){
  mat[,-1] = ifelse(mat[,-1] == Inf, Inf, pmin(mat[,-nrow(mat)]+1, mat[,-1], na.rm = TRUE))
  mat[-1,] = ifelse(mat[-1,] == Inf, Inf, pmin(mat[-ncol(mat),]+1, mat[-1,], na.rm = TRUE))
  mat[,-ncol(mat)] = ifelse(mat[,-ncol(mat)] == Inf, Inf, pmin(mat[,-1]+1, mat[,-ncol(mat)], na.rm = TRUE))
  mat[-nrow(mat),] = ifelse(mat[-nrow(mat),] == Inf, Inf, pmin(mat[-1,]+1, mat[-nrow(mat),], na.rm = TRUE))
  if (n %% 25 == 0) print(n)
  if (mat[nrow(mat)-1, ncol(mat)-1] < 1e6) break
}
coords[[2974]]

## print maze

mat2 = mat
mat2[mat2 < 1000] = "."
mat2[mat2 == "Inf"] = "#"
mat2[mat2 == "1000000"] = "#"
apply(mat2, 1, paste0, collapse = "") %>% as.matrix(ncol = 1)

