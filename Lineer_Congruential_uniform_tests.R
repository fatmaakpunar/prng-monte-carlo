# Parametreler
m <- 10000
a <- 21
b <- 3
x0 <- 1
n <- 100

# Lineer kongruans üretici
x <- numeric(n)
x[1] <- (a * x0 + b) %% m

for (i in 2:n) {
  x[i] <- (a * x[i-1] + b) %% m
}

# (0,1) aralığına dönüştürme
u <- x / m

# İlk 10 değer
x[1:10]
u[1:10]

#Ljung-Box test
Box.test(u, lag=20, type="Ljung-Box")

#2D Serial Chi square test
u1 <- u[1:(length(u)-1)]
u2 <- u[2:length(u)]

k <- 3
breaks <- seq(0, 1, length.out = k + 1)

row_cat <- cut(u1, breaks = breaks, include.lowest = TRUE, labels = FALSE)
col_cat <- cut(u2, breaks = breaks, include.lowest = TRUE, labels = FALSE)

obs <- table(row_cat, col_cat)

N <- length(u1)
E <- N / (k^2)

chi_sq <- sum((obs - E)^2 / E)
df <- k^2 - 1
p_value <- 1 - pchisq(chi_sq, df)

obs
chi_sq
df
p_value

#H0:veriler Uniform (Düzgün) dağılıma uygundur.
#H1:veriler Uniform (Düzgün) dağılıma uygun değildir.

#Anderson Darling Test uniform için
library(nortest)
library(goftest)
ad.test(u, null="punif") 

#kolmogorov smirnov test uniform için
install.packages("dgof")
library("dgof")
ks.test(u, "punif")
