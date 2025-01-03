### 16 #################################################################################################################
setwd("C:/Data/04_Programs/R/AoC/2024/")
source("./aoc_functions.r")
md = readr::read_lines("./Data/input16.txt")

## A ##

mat = strsplit(md, split = "") %>% do.call(what="rbind")
exit_index = which(mat == "E", arr.ind = T)

# score matrices that take into account the direction faced

fe = ifelse(mat == "S", 0, Inf)
fs = matrix(Inf, ncol = ncol(mat), nrow = nrow(mat))
fw = matrix(Inf, ncol = ncol(mat), nrow = nrow(mat))
fn = matrix(Inf, ncol = ncol(mat), nrow = nrow(mat))
visited = ifelse(mat == "E", 1, 0)

mat[mat == "E"] = "."
mat[mat == "S"] = "."

for (k in 1:10000){
  
  fea = fe; fna = fn; fwa = fw; fsa = fs
  
  # step
  fe[,-1] = ifelse(mat[,-1] == ".", pmin(fe[,-ncol(fe)]+1, fe[,-1]), Inf)
  fn[-nrow(fn),] = ifelse(mat[-nrow(fn),] == ".", pmin(fn[-1,]+1, fn[-nrow(fn),]), Inf)
  fw[,-ncol(fw)] = ifelse(mat[,-ncol(fw)] == ".", pmin(fw[,-1]+1, fw[,-ncol(fw)]), Inf)
  fs[-1,] = ifelse(mat[-1,] == ".", pmin(fs[-nrow(fs),]+1, fs[-1,]), Inf)
  
  # rotate
  fe = pmin(fe, fn+1000, fs+1000)
  fn = pmin(fn, fe+1000, fw+1000)
  fw = pmin(fw, fn+1000, fs+1000)
  fs = pmin(fs, fe+1000, fw+1000)
  
  if (k %% 10 == 0) print(k)
  if (all(fea == fe & fna == fn & fwa == fw & fsa == fs)) break
}

res = min(c(fe[exit_index], fn[exit_index], fw[exit_index], fs[exit_index]))
res

min_dist_1 = pmin(fe,fn,fw,fs) # input for B

## B ## walking backwards

mat = strsplit(md, split = "") %>% do.call(what="rbind")

# score matrices that take into account the direction faced
fs = ifelse(mat == "E", res, -Inf)
fe = matrix(-Inf, ncol = ncol(mat), nrow = nrow(mat))
fw = matrix(-Inf, ncol = ncol(mat), nrow = nrow(mat))
fn = matrix(-Inf, ncol = ncol(mat), nrow = nrow(mat))
visited = ifelse(mat == "E", 1, 0)

mat[mat == "E"] = "."
mat[mat == "S"] = "."

for (k in 1:10000){
  
  fea = fe; fna = fn; fwa = fw; fsa = fs
  
  # step
  fe[,-1] = ifelse(mat[,-1] == ".", pmax(fe[,-ncol(fe)]-1, fe[,-1]), -Inf)
  fn[-nrow(fn),] = ifelse(mat[-nrow(fn),] == ".", pmax(fn[-1,]-1, fn[-nrow(fn),]), -Inf)
  fw[,-ncol(fw)] = ifelse(mat[,-ncol(fw)] == ".", pmax(fw[,-1]-1, fw[,-ncol(fw)]), -Inf)
  fs[-1,] = ifelse(mat[-1,] == ".", pmax(fs[-nrow(fs),]-1, fs[-1,]), -Inf)
  
  # rotate
  fe = pmax(fe, fn-1000, fs-1000)
  fn = pmax(fn, fe-1000, fw-1000)
  fw = pmax(fw, fn-1000, fs-1000)
  fs = pmax(fs, fe-1000, fw-1000)
  
  if (k %% 10 == 0) print(k)
  if (all(fea == fe & fna == fn & fwa == fw & fsa == fs)) break
}

min_dist_2 = pmax(fe,fn,fw,fs)

visited = min_dist_2 == min_dist_1 | min_dist_2 == (min_dist_1+1000)
visited[visited] = "#"
visited[visited == "FALSE"] = "."

apply(visited, 1, paste0, collapse = "") %>% as.matrix(ncol = 1) 
sum(visited == "#")

