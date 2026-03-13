set.seed(0)

f <- function(x) dnorm(x, mean = 0, sd = 1, log = FALSE)
g <- function(x) ifelse(x >= 1 & x <= 10, 1/9, 0)

x_vals <- seq(1, 10, length.out = 10000)
c_const <- max(f(x_vals) / g(x_vals))

samples <- c()

while(length(samples) < 100){
  y <- rexp(1)   # aday
  u <- rexp(1)                      
  
  if(u <= f(y) / (c_const * g(y))){
    samples <- c(samples, y)
  }
}

samples

#Histogram 
hist(samples,
     probability = TRUE,
     breaks = 10,
     main = "Kabul-Ret ile Üretilen Örnekler",
     xlab = "x")

curve(dnorm(x, mean = 0, sd = 1),
      add = TRUE,
      lwd = 2)
#değerlerz
mean(samples)  #ortalama
sd(samples) #standart sapma

#anderson darling test
ad_result <- ad.test(samples)
print(ad_result)
