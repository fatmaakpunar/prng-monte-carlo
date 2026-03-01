#Üstel dağılım
teta <- 3 # parametre
n <- 1000 # örneklem büyüklüğü
h <- 1/n # aralığın genişliği
U <- n+1
sındeg <- seq(from = 0, to =1, length.out =U)
orn <- c()
for(i in 1:n)
{k <- sındeg[i]
m <- sındeg[i]+h
orn <- c(orn, runif(1, k, m))}
ustel_sayı <-( -(teta) * ( log(1-orn)) )
ustel_sayı

#Uyum iyiliği testi Vasicek Song
#install.packages('vsgoftest') #Zorunlu paket [19].
library('vsgoftest')
#install.packages('devtools')
#devtools::install_github('pregnault/vsgoftest')
vs.test(ustel_sayı, densfun = 'dexp')

#Uyum iyiliği testi anderson darling
ad_sonuc <- ad.test(ustel_sayı, "pexp", rate = 1/mean(ustel_sayı))
ad_sonuc

#----------------------------------------------------------------------------------------------------------------------
#Pareto dağılımı
alpha <- 3 # skaler parametre
v <- 5 # şekil parametresi
h <- 1/n # aralığın boyu
U <- n+1
sındeg <- seq(from = 0, to =1, length.out =U)
orn <- c()
for(i in 1:n)
{k <- sındeg[i]
m <- sındeg[i]+h
orn <- c(orn, runif(1, k, m))}
pareto_sayı <- ((alpha)/((1-orn)^(1/v)))
pareto_sayı

#Uyum iyiliği testi VS
library('vsgoftest')
vs.test(pareto_sayı, densfun = 'dpareto')

#Uyum iyiliği testi AD
#if(!require(EnvStats)) install.packages("EnvStats") pareto fonksiyonları var
library(EnvStats)
ad_sonuc2 <- ad.test(pareto_sayı, "ppareto", location = alpha, shape = v)
ad_sonuc2
#----------------------------------------------------------------------------------------------------------------------

#düzgün dağılım
a <- 0
b <- 1
h <- 1/n # aralığın boyu
U <- n+1
sındeg <- seq(from = 0, to =1, length.out =U)
orn <- c()
for(i in 1:n)
{k <- sındeg[i]
m <- sındeg[i]+h
orn <- c(orn, runif(1, k, m))}
düzgünsayı <- (a + (b - a) * orn)
düzgünsayı

#uyum iyiliği testi
library('vsgoftest')
vs.test(düzgünsayı, densfun = 'dunif')
#Uyum iyiliği testi anderson darling
library(nortest)
library(goftest)
ad.test(düzgünsayı, null="punif") 
