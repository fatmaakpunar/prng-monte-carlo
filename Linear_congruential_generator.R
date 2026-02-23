# Parametreler
m <- 53     # Modül (Asal sayı)
a <- 2      # Çarpan (İlkel kök)
x0 <- 1     # Başlangıç değeri (Tohum)
N <- 50     # Üretilecek örneklem hacmi

# Boş vektörler
X <- numeric(N)
U <- numeric(N)

x_guncel <- x0
for (i in 1:N) {
  x_guncel <- (a * x_guncel) %% m
  X[i] <- x_guncel
  U[i] <- x_guncel / m
}

sonuclar <- data.frame(Adim = 1:N, X_degeri = X, U_degeri = round(U, 4))
print(head(sonuclar, 50))

# Latis 
U_i <- U[1:(N-1)]
U_i_plus_1 <- U[2:N]

plot(U_i, U_i_plus_1, 
     main = "Çarpımsal Üreteç Latis Özelliği (m=53, a=2)",
     xlab = expression(U[i]), 
     ylab = expression(U[i+1]), 
     pch = 19, col = "darkblue", cex = 1.2)

grid(col = "gray", lty = "dotted")
