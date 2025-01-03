### 11 #################################################################################################################
setwd("C:/Data/04_Programs/R/AoC/2024/")
source("./aoc_functions.r")

add_to_cache = function(index1, index2, value) {
  if (is.null(cache[[as.character(index1)]])) {
    cache[[as.character(index1)]] <<- list()
  }
  cache[[as.character(index1)]][[as.character(index2)]] <<- value
}

get_from_cache = function(index1, index2) {
  if (!is.null(cache[[as.character(index1)]]) && !is.null(cache[[as.character(index1)]][[as.character(index2)]])) {
    return(cache[[as.character(index1)]][[as.character(index2)]])
  } else {
    return(NULL)
  }
}

countStones = function(x, n){
  cvalue = get_from_cache(x, n)
  if (!is.null(cvalue)) return(cvalue)
  if (is.null(cvalue) & n == 1) {
    ndig = trunc(log10(x))+1
    add_to_cache(x, 1, 2-ndig %% 2)
    return(2-ndig %% 2)
  }
  if (is.null(cvalue) & n > 1) {
    if (x == 0) {
      cs1 = countStones(1, n-1)
      add_to_cache(1, n-1, cs1)
      return(cs1)
    }
    ndig = trunc(log10(x))+1
    if (ndig %% 2 == 1) {
      cs1 = countStones(x*2024, n-1)
      add_to_cache(x*2024, n-1, cs1)
      return(cs1)
    } else {
      cs1 = countStones(x %/% 10^(ndig/2), n-1)
      cs2 = countStones(x %% 10^(ndig/2), n-1)
      add_to_cache(x %/% 10^(ndig/2), n-1, cs1)
      add_to_cache(x %% 10^(ndig/2), n-1, cs2)
      return(cs1+cs2)
    }
  }
}

## A ##

cache = list()
add_to_cache(0,1,1)

out_25 = sapply(c(0, 5601550, 3914, 852, 50706, 68, 6, 645371), countStones, 25)
sum(out_25)

## B ##

out_75 = sapply(c(0, 5601550, 3914, 852, 50706, 68, 6, 645371), countStones, 75)
sum(out_75)

