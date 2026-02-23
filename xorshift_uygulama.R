# Temel Xorshift (32-bit) Uygulaması
xorshift32 <- function(seed) {
  x <- as.integer(seed)
  x <- bitwXor(x, bitwShiftL(x, 13))
  x <- bitwXor(x, bitwShiftR(x, 17))
  x <- bitwXor(x, bitwShiftL(x, 5))
  
  return(x)
}

# Test 
current_seed <- 123456789
for(i in 1:5) {
  current_seed <- xorshift32(current_seed)
  print(current_seed)
}
