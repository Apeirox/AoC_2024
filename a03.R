### 03 #################################################################################################################
setwd("C:/Data/04_Programs/R/AoC/2024/")
source("./aoc_functions.r")
tf = readr::read_lines("./Data/input03.txt")

extractAndMultiply = function(sng){
  mp = stringi::stri_extract_all(sng, regex = "mul\\(\\d+,\\d+\\)", simplify = T)
  a = stringi::stri_extract_all(mp, regex = "\\d+") %>% do.call(what = "rbind")
  sum(as.integer(a[,1])*as.integer(a[,2]))
}

## A ##

tf1 = tf %>% paste0(collapse = "")
extractAndMultiply(tf1)

## B ##

tf2 = stringi::stri_replace_all(tf1, regex = "don't\\(\\).*?(do\\(\\)|$)", replacement = "")
extractAndMultiply(tf2)