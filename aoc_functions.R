########################################################################################################################
### AoC global functions

pacman::p_load(magrittr, tidyverse, tibble, dplyr)

splitText = function(x, sep = " "){stringi::stri_split(x, fixed = sep) %>% unlist()}

convertMazeToInt = function(maze, start = "S", exit = "E", wall = "#", path_symbol = 1e6){
  # convert a maze represented by characters to one represented by integers
  strt_index = which(maze == "S", arr.ind = T)
  exit_index = which(maze == "E", arr.ind = T)
  imaze = ifelse(maze == wall, Inf, path_symbol)
  imaze[strt_index] = 0
  list(imaze, exit_index)
}

stepThruMaze = function(imaze, exit = structure(c(2, ncol(imaze)-1)), add_walls = T, path_symbol = 1e6){
  # uses matrix addition to flood-walk the maze - in one step, each walkable point receives the minimum of its own score 
  # and its (neighbour scores + 1)
  # Inf = wall
  # 1e6 = path
  # maze should contain a starting point (usually a "0")
  # add walls 'contains' the maze in another layer of walls if there wasn't one 
  
  if (add_walls) imaze = cbind(Inf, rbind(Inf, imaze, Inf), Inf)
  
  maze_changed = T; n=0
  while (maze_changed) {
    n = n+1
    imaze_a = imaze
    imaze[,-1] = ifelse(imaze[,-1] == Inf, Inf, pmin(imaze[,-nrow(imaze)]+1, imaze[,-1], na.rm = TRUE)) # right
    imaze[-1,] = ifelse(imaze[-1,] == Inf, Inf, pmin(imaze[-ncol(imaze),]+1, imaze[-1,], na.rm = TRUE)) # down
    imaze[,-ncol(imaze)] = ifelse(imaze[,-ncol(imaze)] == Inf, Inf, pmin(imaze[,-1]+1, imaze[,-ncol(imaze)], na.rm = TRUE)) # left
    imaze[-nrow(imaze),] = ifelse(imaze[-nrow(imaze),] == Inf, Inf, pmin(imaze[-1,]+1, imaze[-nrow(imaze),], na.rm = TRUE)) # up
    if (n %% 50 == 0) print(n)
    maze_changed = !(identical(imaze_a, imaze))
  }
  imaze[imaze == path_symbol] = Inf # unreachable
  imaze
}
