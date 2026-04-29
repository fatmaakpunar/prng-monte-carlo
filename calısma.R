library(moments)
#n <- 10000
tkr <-5
teta <- 5
xx <- 3
alha <- 0.05
orneklem_ortalamasi<-numeric(tkr)
orneklem_varyansi<-numeric(tkr)
orneklem_carpiklik<-numeric(tkr)
orneklem_kurtosis<-numeric(tkr)
for (i in 1:tkr){
  U <-runif(n)
  x <- -teta*log(1-U)
  
  orneklem_ortalamasi[i]<-mean(x)
  orneklem_varyansi[i]<-var(x)
  orneklem_carpiklik[i]<-skewness(x)
  orneklem_kurtosis[i]<-kurtosis(x)
}

cat("orneklem ortalaması: ", mean(orneklem_ortalamasi), "/n")
cat("orneklem varyansı: ", mean(orneklem_varyansi), "/n")
cat("orneklem çarpıklığı: ", mean(orneklem_carpiklik), "/n")
cat("orneklem basıklığı: ", mean(orneklem_kurtosis), "/n/n")
##################################################################################
U <- runif(n)
x <- -teta*log(1-U)

#teorik
tort <- teta
tvar <- teta^2
tols <- 1-exp(-xx/teta)
ttersleri <- -teta*log(1-alha)

#ampirirk
aort <- mean(x)
avar <- var(x)
say <- sum(x/xx)
aols <- say/n
xsira <- sort(x)
atersleri <- xsira[floor((alha*n))]

#sonuçlar
cat("--- TEORİK VE AMPİRİK KARŞILAŞTIRMA ---\n")
cat("Teorik ortalama:",tort, "Ampirik Ortalama:",aort)
cat("Teorik varyans:",tvar, "Ampirik Varyans:",avar)
cat("Teorik P(x<3):",tols, "Ampirik P(x<3):", aols)
cat("Teorik alfaya karşılık gelen değer",ttersleri, "Ampirik alfaya karşılık gelen değer",atersleri)
##################################################################################################
theta <- 5
n_degerleri <- c(10, 50, 100, 500, 1000, 5000)
tkr <- 1000
for (i in 1:length(n_degerleri)){
  n <- n_degerleri[i]
  orneklem_UMVUE <- numeric(tkr)
  orneklem_T2 <- numeric(tkr)
  for (j in 1:tkr){
    U <- runif(n)
    x <- -theta*log(1-U)
    orneklem_UMVUE[j] <- mean(x)
    orneklem_T2[j] <- x[1] - x[2] + mean(x)
  }
  ort_UMVUE <- mean(orneklem_UMVUE)
  ort_T2 <- mean(orneklem_T2)
  var_UMVUE <- var(orneklem_UMVUE)
  var_T2 <- var(orneklem_T2)
  
  cat(sprintf("/n----n=%d İçin Örnekleme Sonuçları------/n", n, "/n"))
  cat("Ortalama UMVUE: ", ort_UMVUE, "Ortalama T2", ort_T2, "/n")
  cat("varyans UMVUE: ", var_UMVUE, "varyans T2", var_T2, "/n")
}
################################################################
f <- function(x){
   (1/sqrt(2*pi))*exp(-0.5*x^2)
}

g <- function(x){
  exp(-x)/(1+exp(-x))^2
}

xd <- seq(-10, 10, length.out=1000)
od <- f(xd)/g(xd)
c <- max(od)

n <- 100000
samples <- numeric(n)
i <- 0
k <- 0
while (i<n){
  k <- k+1
  u_aday <- runif(1)
  y <- log(u_aday/(1-u_aday))
  u_test <- runif(1)
  oran <- f(y)/(c*g(y))
  if (u_test <= oran){
    i <- i+1
    samples[i] <- y
  }
}

hist(samples, breaks = 60, prob = TRUE, 
     main = "Manuel Lojistik Önerili Kabul-Ret",
     col = "pink", border = "white")
curve(f(x), add = TRUE, col = "purple", lwd = 2)
#####################################################################
f <- function(x){
  (1/sqrt(2*pi))*exp(-0.5*x^2)
}

g <- function(x){
  1/(pi*(1+x^2))
}

xd <- seq(-10, 10, length.out=1000)
od <- f(xd)/g(xd)
c <- max(od)

n <- 100000
samples <- numeric(n)
i <- 0
k <- 0
while (i<n){
  k <- k+1
  u_aday <- runif(1)
  y <- tan(pi*(u_aday-0.5))
  u_test <- runif(1)
  oran <- f(y)/(c*g(y))
  if (u_test <= oran){
    i <- i+1
    samples[i] <- y
  }
}

hist(samples, breaks=60, prob=TRUE,
     main="cauchy dağılımı",
     col="blue", border="white")
curve(f(x), add=TRUE, col="red", lwd=2)
#####################################################################
f <- function(x){
  (1/sqrt(2*pi))*exp(-0.5*x^2)
}

g <- function(x){
  0.5*exp(-abs(x))
}

xd <- seq(-10, 10, length.out=1000)
od <- f(xd)/g(xd)
c <- max(od)

n <- 100000
samples <- numeric(n)
i <- 0
k <- 0
while (i<n){
  k <- k+1
  u_aday <- runif(1)
  y <- ifelse(u_aday<0.5,
               log(2*u_aday),
               -log(2*(1-u_aday)))
        
  u_test <- runif(1)
  oran <- f(y)/(c*g(y))
  if (u_test <= oran){
    i <- i+1
    samples[i] <- y
  }
}

hist(samples, breaks=60, prob=TRUE,
     main="lAPLACE DAĞILIMI",
     col="lightblue", border="white")
curve(f(x), add=TRUE, col="lightpink", lwd=2)
