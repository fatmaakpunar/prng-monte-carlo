n <- 400            # üretilecek sayı adedi 319 da 111 in tekrarı dönüyor
a <- 15815208         # 8 basamaklı seed
u <- numeric(n)

for(i in 1:n){
  a <- a^2
  a <- as.numeric(substr(sprintf("%016.0f", a), 5, 12))
  u[i] <- a / 10^8
}

u

Box.test(u, lag=20, type="Ljung-Box")
