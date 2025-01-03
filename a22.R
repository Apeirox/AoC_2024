### 22 #################################################################################################################
setwd("C:/Data/04_Programs/R/AoC/2024/")
source("./aoc_functions.r")
md = readr::read_lines("./Data/input22.txt")

dec2bin = function(fnum) {
  bin_vect = rep(0, 1 + floor(log(fnum, 2)))
  while (fnum >= 2) {
    pow = floor(log(fnum, 2))
    bin_vect[1 + pow] <- 1
    fnum = fnum - 2^pow
  } # while
  bin_vect[1] = fnum %% 2
  paste(rev(bin_vect), collapse = "")
}

binaryMulti = function(binstr, ord = log2(64)){
  if (ord >= 1) {
    paste0(binstr, paste0(rep(0, ord), collapse = ""))
  } else {
    binstr %>% stringr::str_sub(end = ord-1)
  }
}

binaryModulo = function(binstr, ord = 24){
  binstr %>% stringr::str_sub(start = -ord)
}

binaryXor <- function(bin1, bin2) {
  
  if (nchar(bin1) < nchar(bin2)){ bin1 = paste0(paste0(rep(0, nchar(bin2)-nchar(bin1)), collapse = ""), bin1)
  } else {
    if (nchar(bin1) > nchar(bin2)) bin2 = paste0(paste0(rep(0, nchar(bin1)-nchar(bin2)), collapse = ""), bin2)
  }
  
  num1 = as.numeric(strsplit(bin1, NULL)[[1]]) 
  num2 = as.numeric(strsplit(bin2, NULL)[[1]])
  result = xor(num1, num2) 
  result_str = paste(result, collapse = "")
  return(result_str)
}

xor = function(a, b) {return(((a | b) & !(a & b)) %>% as.integer())}

nextSecret = function(x){
  x = dec2bin(x)
  x = binaryMulti(binstr = x, ord = log2(64)) %>% binaryXor(x) %>% binaryModulo(log2(16777216))
  x = binaryMulti(x, log2(1/32)) %>% binaryXor(x) %>% binaryModulo(log2(16777216))
  x = binaryMulti(x, log2(2048)) %>% binaryXor(x) %>% binaryModulo(log2(16777216))
  return(strtoi(x, base = 2))
}

gen2000th = function(x){
  for (k in 1:2000){
    x = nextSecret(x)
  }
  return(x)
}

## A ## slowish

tn = as.integer(md)

out = sapply(1:length(tn), function(k){
  if (k %% 10 == 0) print(k)
  gen2000th(tn[k])
})

sum(out)