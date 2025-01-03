### 08 #################################################################################################################
setwd("C:/Data/04_Programs/R/AoC/2024/")
source("./aoc_functions.r")
md = readr::read_lines("./Data/input08.txt")

## A ##

mat = strsplit(md, split = "") %>% do.call(what="rbind")

indices = which(mat == "0", arr.ind = T)
diffs_1 = outer(indices[,1], indices[,1], function(x,y) {x+x-y})
diffs_2 = outer(indices[,2], indices[,2], function(x,y) {x+x-y})

all_indices = cbind(diffs_1[row(diffs_1) != col(diffs_1)], diffs_2[row(diffs_2) != col(diffs_2)])

for (k in c(as.character(1:9), letters, LETTERS)){
  indices = which(mat == k, arr.ind = T)
  diffs_1 = outer(indices[,1], indices[,1], function(x,y) {x+x-y})
  diffs_2 = outer(indices[,2], indices[,2], function(x,y) {x+x-y})
  
  all_indices = rbind(all_indices, cbind(diffs_1[row(diffs_1) != col(diffs_1)], diffs_2[row(diffs_2) != col(diffs_2)]))
}

all_indices = all_indices[all_indices[,1] > 0 & all_indices[,1] <= nrow(mat) &
                            all_indices[,2] > 0 & all_indices[,2] <= ncol(mat),]

all_indices = all_indices[!duplicated(all_indices),]
nrow(all_indices)

## B ##

all_indices = structure(numeric(0), dim = c(0L, 2L))

for (j in c(0, (-50):50)){
  for (k in c(as.character(0:9), letters, LETTERS)){
    indices = which(mat == k, arr.ind = T)
    diffs_1 = outer(indices[,1], indices[,1], function(x,y) {x+j*(x-y)})
    diffs_2 = outer(indices[,2], indices[,2], function(x,y) {x+j*(x-y)})
    
    all_indices = rbind(all_indices, cbind(diffs_1 %>% as.vector(), diffs_2 %>% as.vector()))
  }
}

all_indices = all_indices[all_indices[,1] > 0 & all_indices[,1] <= nrow(mat) &
                            all_indices[,2] > 0 & all_indices[,2] <= ncol(mat),]

all_indices = all_indices[!duplicated(all_indices),]
nrow(all_indices)

