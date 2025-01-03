### 14 #################################################################################################################
setwd("C:/Data/04_Programs/R/AoC/2024/")
source("./aoc_functions.r")
md = readr::read_lines("./Data/input14.txt")

nr = 103
nc = 101

## A ##

robs = regmatches(md, gregexpr("-*\\d+", md))
robs = robs %>% lapply(function(x) as.numeric(x)) %>% do.call(what = rbind)

out_r = robs[,2] + 100*robs[,4]
out_c = robs[,1] + 100*robs[,3]
out_join_mod = cbind(out_r %% nr, out_c %% nc)

out_join_q = case_when(out_join_mod[,1] < (nr-1)/2 & out_join_mod[,2] < (nc-1)/2 ~ "A", 
                       out_join_mod[,1] < (nr-1)/2 & out_join_mod[,2] > (nc-1)/2 ~ "B",
                       out_join_mod[,1] > (nr-1)/2 & out_join_mod[,2] < (nc-1)/2 ~ "C",
                       out_join_mod[,1] > (nr-1)/2 & out_join_mod[,2] > (nc-1)/2 ~ "D",
                       .default = NA)

prod(table(out_join_q))

## B ## writes txt files to disc

smdist = sapply(1:10000, function(j){
  out_r = robs[,2] + j*robs[,4]
  out_c = robs[,1] + j*robs[,3]
  out_join_mod = cbind(out_r %% nr, out_c %% nc)
  
  if (j %% 100 == 0) print(paste0("--------", j))
  median_dist = tapply(out_join_mod[order(out_join_mod[,2], out_join_mod[,1]), 1],
                       out_join_mod[order(out_join_mod[,2], out_join_mod[,1]), 2],
                       function(x){median(diff(x))})
  median_dist_freq = table(median_dist)
  names(median_dist_freq)[median_dist_freq == max(median_dist_freq)][1] %>% as.numeric()
})

for (j in which(smdist == 1 | smdist >= 22)){ # anomalous values
  out_r = robs[,2] + j*robs[,4]
  out_c = robs[,1] + j*robs[,3]
  out_join_mod = cbind(out_r %% nr, out_c %% nc)
  
  mtr = matrix(" ", nrow = nr, ncol = nc)
  for (k in 1:nrow(out_join_mod)){
    mtr[out_join_mod[k,1]+1, out_join_mod[k,2]+1] = "#"
  }
  print(paste0("--------", j))
  out = apply(mtr, 1, paste0, collapse = "") %>% as.matrix(ncol = 1)
  out = c(out, paste0("--------------------THAT WAS: ", j, "------------------"))
  readr::write_delim(out %>% as.data.frame(), "tree.txt", delim = "", append = TRUE)
}

