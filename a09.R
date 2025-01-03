### 09 #################################################################################################################
setwd("C:/Data/04_Programs/R/AoC/2024/")
source("./aoc_functions.r")
tf = readr::read_lines("./Data/input09.txt")
options(scipen = 999)

## A ##

lns = strsplit(tf, split = "")[[1]] %>% as.integer()

blocks = rep(as.vector(rbind(0:(length(lns) %/% 2), rep(NA, length(lns) %/% 2 + 1))) %>% head(-1), times = lns)
rev_index = which(cumsum(is.na(blocks)) == rev(cumsum(!is.na(rev(blocks)))))

blocks_out = blocks[2:rev_index-1]
blocks_out[is.na(blocks_out[2:rev_index-1])] = blocks[rev_index:length(blocks)] %>% na.omit() %>% rev()

((seq_along(blocks_out)-1)*blocks_out) %>% sum()

## B ##