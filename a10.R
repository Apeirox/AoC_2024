### 10 #################################################################################################################
setwd("C:/Data/04_Programs/R/AoC/2024/")
source("./aoc_functions.r")
md = readr::read_lines("./Data/input10.txt")

## A ##

mat = strsplit(md, split = "") %>% lapply(function(x) as.integer(x)) %>% do.call(what="rbind")
mat = rbind(-1, mat, -1)
mat = cbind(-1, mat, -1)

st_index = which(mat == 0, arr.ind = T)

nine_visits = sapply(seq.int(nrow(st_index)), function(j){
  
  visited = matrix(0, nrow = nrow(mat), ncol = ncol(mat))
  visited[st_index[j,1], st_index[j,2]] = 1
  
  for (k in 1:9){
    visited[,-1] = visited[,-1] | visited[,-ncol(visited)] & mat[,-ncol(mat)] == (k-1) & mat[,-1] == k
    visited[,-ncol(visited)] = visited[,-ncol(visited)] | visited[,-1] & mat[,-1] == (k-1) & mat[,-ncol(visited)] == k
    visited[-1,] = visited[-1,] | visited[-nrow(visited),] & mat[-nrow(mat),] == (k-1) & mat[-1,] == k
    visited[-nrow(visited),] = visited[-nrow(visited),] | visited[-1,] & mat[-1,] == (k-1) & mat[-nrow(visited),] == k
  }
  if (j %% 10 == 0) print(j)
  sum(ifelse(visited == 1, mat, -1) == 9)
})

sum(nine_visits)

## B ##

mat_score = mat == 9

for (k in 8:0){
  mat_score[3:nrow(mat)-1,3:ncol(mat)-1] = ifelse(mat[3:nrow(mat)-1,3:ncol(mat)-1] == k,
                                                  (mat[3:nrow(mat)-2,3:ncol(mat)-1] == k+1)*mat_score[3:nrow(mat)-2,3:ncol(mat)-1]+
                                                    (mat[3:nrow(mat)-1,3:ncol(mat)-2] == k+1)*mat_score[3:nrow(mat)-1,3:ncol(mat)-2]+
                                                    (mat[3:nrow(mat)-0,3:ncol(mat)-1] == k+1)*mat_score[3:nrow(mat)-0,3:ncol(mat)-1]+
                                                    (mat[3:nrow(mat)-1,3:ncol(mat)-0] == k+1)*mat_score[3:nrow(mat)-1,3:ncol(mat)-0],
                                                  mat_score[3:nrow(mat)-1,3:ncol(mat)-1])
  print(paste0("------- ",k))
  #print(mat_score[3:nrow(mat)-1,3:ncol(mat)-1])
}

(mat_score*(mat==0)) %>% sum()
