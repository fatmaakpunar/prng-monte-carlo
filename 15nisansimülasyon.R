library(moments)
#set.seed(123)
teta=5 
n=10000
tkr<-5 #örneklemi tekrar
xx<- 3
alfa=0.05

orneklem_ortalamalari <- numeric(tkr)
orneklem_varyanslari <- numeric(tkr)
orneklem_skewleri <- numeric(tkr)
orneklem_kurtolari <- numeric(tkr)

for (i in 1:tkr) {
  
  
  u <- runif(n)
  x= -teta*log(1-u) #ters dönüşüm
  
  orneklem_ortalamalari[i] <- mean(x)
  orneklem_varyanslari[i] <- var(x)
  orneklem_skewleri[i] <- skewness(x)
  orneklem_kurtolari[i] <- kurtosis(x)
  
}

cat("Örneklem Ortalaması Y: ", mean(orneklem_ortalamalari))
cat("Örneklem Varyansı Y: ", var(orneklem_varyanslar))
cat("Örneklem Skewness Y: ", skewness(orneklem_skewleri))
cat("Örneklem Kurtosis Y: ", kurtosis(orneklem_kurtolari))

u <- runif(n)
x= -teta*log(1-u)


Aort=teta
Avar=teta^2
Aols =1-exp(-xx/teta)
Atersleri = teta*log(1-alfa)

Oort=mean(x)
Ovar=var(x)
Oskew=skewness(x)
Okurto=kurtosis(x)

Ools=say/n

xsira=sort(x)
Otersleri=xsira[floor(alfa*n)]




say = 0

for (j in 1:n ) {
  if(x[i] < xx) {
    say=say+1
  }
  
}

orneklem_olasılıkları <- say/n

#**********************************************************************#
theta <- 5

n_degerleri <- c(10, 100, 1000, 10000)

#boş vektörler
UMVUE <- numeric(length(n_degerleri))
T2 <- numeric(length(n_degerleri))

#veri üretimi+hesaplama
for (i in 1:length(n_degerleri)) {
  n <- n_degerleri[i]
  u <- runif(n)
  veri <- theta * log(u) #ters dönüşüm
  
  UMVUE[i] <- mean(veri)
  T2[i] <- veri[1] - veri[2] + mean(veri)
}

orneklem_ortalamalari[i] <- mean(veri)
orneklem_varyanslari <- var(veri)
#sonunda grafik olcak
