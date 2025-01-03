### 07 #################################################################################################################
setwd("C:/Data/04_Programs/R/AoC/2024/")
source("./aoc_functions.r")
md = readr::read_lines("./Data/input07.txt")

allResults = function(x){
  mat = expand.grid(rep(list(c(0, 1)), length(x)-1)) 
  sapply(1:nrow(mat), function(k){
    out = x[1]
    for (j in 2:length(x)){
      if (mat[k,j-1] == 0) out = out+x[j] else out = out*x[j]
    }
    out
  })
}

allResults2 = function(x, target = Inf){
  mat = expand.grid(rep(list(c(0, 1, 2)), length(x)-1)) 
  sapply(1:nrow(mat), function(k){
    out = x[1]
    for (j in 2:length(x)){
      if (mat[k,j-1] == 0) out = out+x[j]
      if (mat[k,j-1] == 1) out = out*x[j]
      if (mat[k,j-1] == 2) out = as.numeric(paste0(out,x[j]))
      if (out > target) {out = 0; break}
    }
    out
  })
}

## A ##

myv = strsplit(md, split = ": | ")

out = sapply(myv, function(x){
  if (as.numeric(x[1]) %in% allResults(as.numeric(x[-1]))) as.numeric(x[1]) else 0
})

unlist(out) %>% sum()

## B ## sloooooow

n = 1
out = sapply(myv, function(x){
  print(n)
  n <<- n+1
  if (as.numeric(x[1]) %in% allResults2(as.numeric(x[-1]), as.numeric(x[1]))) as.numeric(x[1]) else 0
})

unlist(out) %>% sum()