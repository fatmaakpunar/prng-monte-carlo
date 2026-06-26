N <- c(10, 100, 1000, 10000)
for (i in 1:length(N))                                                            
           
                                                  
N <- c(10, 100, 1000, 10000)
for (i in 1:length(N)) {
  n <- N[i]            
  for (j in 1:n){
    x <- rnorm(n)
    f_x <- exp(-x^2/2)
    p_x <- dnorm(x)
  }
  integral_tahmini <- mean(f_x/p_x)
  cat(sprintf("\n--- n = %d  Sonuçları ---\n", n))
  cat("Gerçek Değer (Karekök 2*Pi):" , sqrt(2 * pi), "\n")
  cat("Monte Carlo İntegral Tahmini:", integral_tahmini, "\n")
}