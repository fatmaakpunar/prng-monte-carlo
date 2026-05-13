nokta_sayisi <- c(10, 100, 1000, 10000)

for (i in 1:length(nokta_sayisi)) {
  n <- nokta_sayisi[i]
  daire_icindeki_noktalar = 0
  
  for (j in 1:n){
    x <- runif(1)
    y <- runif(1)
    kosul <- x**2 + y**2
   
    if (kosul <= 1){
      daire_icindeki_noktalar <- daire_icindeki_noktalar +1
    }
  }
  pi_tahmini <- 4 * daire_icindeki_noktalar / n
  
  cat(sprintf("\n--- n = %d  Sonuçları ---\n", n))
  cat("Pi tahmini:", pi_tahmini, "\n")
}
 
#####################################################
nokta_sayisi <- c(10, 100, 1000, 10000)
par(mfrow = c(2, 2))
for (i in 1:length(nokta_sayisi)) {
  n <- nokta_sayisi[i]
  x <- runif(n)
  y <- runif(n)
  kosul <- x**2 + y**2
  daire_icindeki_noktalar <- sum(kosul <= 1)
  pi_tahmini <- 4 * daire_icindeki_noktalar / n
  cat(sprintf("\n--- n = %d  Sonuçları ---\n", n))
  cat("Pi tahmini:", pi_tahmini, "\n")
  renkler <- ifelse(kosul <= 1, "blue", "red")
  plot(x, y, 
       col = renkler, 
       pch = 20, 
       cex = ifelse(n >= 1000, 0.4, 1), 
       main = sprintf("n = %d | Pi = %.4f", n, pi_tahmini), 
       xlab = "X", 
       ylab = "Y", 
       asp = 1) 
  
  curve(sqrt(1 - x^2), from = 0, to = 1, add = TRUE, col = "black", lwd = 2)
}

par(mfrow = c(1, 1))

