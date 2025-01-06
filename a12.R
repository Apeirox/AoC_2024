### 12 #################################################################################################################
setwd("C:/Data/04_Programs/R/AoC/2024/")
source("./aoc_functions.r")
md = readr::read_lines("./Data/input12.txt")

## A ##

maze = strsplit(md, split = "") %>% do.call(what="rbind")
maze = cbind("#", rbind("#", maze, "#"), "#")

maze_score = 0

while (any(maze != "#")) {
  
  index_start = which(maze != "#", arr.ind = T)[1,]
  
  maze_n = ifelse(maze == maze[index_start[1], index_start[2]], 1e6, Inf)
  maze_n[index_start[1], index_start[2]] = 0
  maze_step = stepThruMaze(maze_n, add_walls = F)
  
  getRegionScore = function(maze_i){
    maze_f = ifelse(maze_i != "~", FALSE, TRUE)
    
    maze_f[,-1] = maze_f[,-1] + maze_i[,-nrow(maze_f)]
    maze_f[-1,] = maze_f[-1,] + maze_i[-ncol(maze_f),]
    maze_f[,-ncol(maze_f)] = maze_f[,-ncol(maze_f)] + maze_i[,-1]
    maze_f[-nrow(maze_f),] = maze_f[-nrow(maze_f),] + maze_i[-1,]
    maze_f = ifelse(maze_i, 4 - maze_f, 0)
    
    sum(maze_f)*sum(maze_i)
  }
  
  region_score = getRegionScore(maze_step < 1e5)
  maze_score = maze_score + region_score
  print(region_score)
  
  maze = ifelse(maze_step < 1e5, "#", maze)
}

maze_score