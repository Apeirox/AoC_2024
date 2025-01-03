########################################################################################################################
### AoC global functions

pacman::p_load(magrittr, tidyverse, tibble, dplyr)
splitText = function(x, sep = " "){stringi::stri_split(x, fixed = sep) %>% unlist()}

