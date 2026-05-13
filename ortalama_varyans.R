theta <- 5
n_degerleri <- c(10, 100, 1000, 10000) 
tkr <- 1000 

varyans_T1 <- numeric(length(n_degerleri))
varyans_T2 <- numeric(length(n_degerleri))
varyans_T3 <- numeric(length(n_degerleri))

for (i in 1:length(n_degerleri)) {
  
  n <- n_degerleri[i]
  
  orneklem_T1 <- numeric(tkr)
  orneklem_T2 <- numeric(tkr)
  orneklem_T3 <- numeric(tkr)
  
  
  for (j in 1:tkr) {
    u <- runif(n, min =0, max=theta)

    orneklem_T1[j] <- mean(u)
    orneklem_T2[j] <- u[1]
    orneklem_T3[j] <- u[n]
  }
  
  ort_T1 <- mean(orneklem_T1)
  var_T1 <- var(orneklem_T1)
  
  ort_T2 <- mean(orneklem_T2)
  var_T2 <- var(orneklem_T2)
  
  ort_T3 <- mean(orneklem_T3)
  var_T3 <- var(orneklem_T3)
  

  
  # consol çıktıları (cat)
  cat(sprintf("\n--- n = %d İçin Örneklem Sonuçları ---\n", n))
  cat("T1 Ortalaması     : ", ort_T1, "\n")
  cat("T2 Ortalaması        : ", ort_T2, "\n")
  cat("T3 Ortalaması     : ", ort_T3, "\n")
  cat("T1 Varyansı       : ", var_T1, "\n")
  cat("T2 Varyansı          : ", var_T2, "\n")
  cat("T3 Varyansı          : ", var_T3, "\n")
}
