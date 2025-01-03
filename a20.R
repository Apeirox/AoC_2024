### 20 #################################################################################################################
setwd("C:/Data/04_Programs/R/AoC/2024/")
source("./aoc_functions.r")
md = readr::read_lines("./Data/input20.txt")

getAllCheats = function(submaze, cheat_treshold = 50){
  hrad = nrow(submaze) %/% 2 + 1
  cheat_lng = ifelse(abs(row(submaze)-hrad)+abs(col(submaze)-hrad) <= hrad-1,
                     submaze - submaze[hrad,hrad] - (abs(row(submaze)-hrad)+abs(col(submaze)-hrad)),
                     Inf)
  cheat_lng = cheat_lng[cheat_lng > 0 & cheat_lng < Inf & !is.na(cheat_lng)]
  cheat_lng[cheat_lng >= cheat_treshold]
}

## A ##

maze = strsplit(md, split = "") %>% do.call(what="rbind")
maze_int = convertMazeToInt(maze)
imaze_final = stepThruMaze(imaze = maze_int[[1]], exit = maze_int[[2]])
imaze_final[maze_int[[2]]+1] # steps to reach exit; maze_int[[2]]+1 if add_walls == T; maze_int[[2]] if add_walls == F

cheats_lns = c(abs(imaze_final[,-(1:2)]-imaze_final[,-(ncol(imaze_final)-1:0)]),
               abs(imaze_final[-(1:2),]-imaze_final[-(nrow(imaze_final)-1:0),]))

cheat_lns_res = cheats_lns[cheats_lns > 2 & cheats_lns < Inf & !is.na(cheats_lns)]-2

table(cheat_lns_res)
sum(cheat_lns_res >= 100)

## B ##

n = 18
inf_matrix = matrix(Inf, nrow = n, ncol = ncol(imaze_final))
imaze_final_superwalls = rbind(inf_matrix, imaze_final, inf_matrix)
inf_matrix = matrix(Inf, ncol = n, nrow = nrow(imaze_final_superwalls))
imaze_final_superwalls = cbind(inf_matrix, imaze_final_superwalls, inf_matrix)

route_indices = which(imaze_final_superwalls < Inf, arr.ind = T)

valid_cheats = sapply(sequence(nrow(route_indices)), function(k){
  if (k %% 100 == 0) print(k)
  getAllCheats(imaze_final_superwalls[route_indices[k,1]+(-20):20, route_indices[k,2]+(-20):20], 100)
}) %>% unlist()

table(valid_cheats)
length(valid_cheats)