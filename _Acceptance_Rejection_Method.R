#install.packages("AcceptReject")
library(AcceptReject)
#POİSSON DAĞILIMI
set.seed(0)
data <- AcceptReject::accept_reject( #arka planda AcceptReject U(0,1) üretir
  n = 1000L, #1000 tane tam sayı "L"
  f = dpois, #poisson
  continuous = FALSE,
  args_f = list(lambda = 0.7), 
  xlim = c(0, 20),
  parallel = FALSE
)
print(data)

# Gözlemlenen her değer için gerçek olasılık fonksiyonunu hesaplamak
values <- unique(data)
true_prob <- dpois(values, lambda = 0.7)

#Gözlem vektöründeki her değer için gözlemlenen olasılığı hesaplama
obs_prob <- table(data) / length(data)

# Olasılıkların ve gözlemlerin grafiğini çizmek
dev.off()
par(mfrow = c(1,1))
par(mar = c(4,4,2,1))
plot(values, true_prob, type = "p", pch = 16, col = "blue",
     xlab = "x", ylab = "Olasılık", main = "Olasılık Fonksiyonu")

#Gözlemlenen olasılıkların toplanması
points(as.numeric(names(obs_prob)), obs_prob, pch = 16L, col = "red")
legend("topright", legend = c("Gerçek olasılık", "Gözlemlenen olasılık"),
       col = c("blue", "red"), pch = 16L, cex = 0.8)
grid()

#Hedef dağılım: Poisson(0.7)
#Kabul-ret yöntemiyle bu dağılımdan 1000 örnek üretildi
#Bu örneklerin gözlenen olasılıkları hesaplandı
#Gerçek teorik olasılıklarla karşılaştırıldı
#Yakınsa, yöntem başarılı denir

#-------------------------------------------------------------------------------------------------------------------
#BİNOM DAĞILIMI
set.seed(0)

# Generating observations
data <- AcceptReject::accept_reject(
  n = 2000L,
  f = dbinom,
  continuous = FALSE,
  args_f = list(size = 5, prob = 0.5),
  xlim = c(0, 20),
  parallel = FALSE
)

print(data)

# gerçek olasılık fonksiyonu
values <- unique(data)
true_prob <- dbinom(values, size = 5, prob = 0.5)

# Gözlem vektöründeki her değer için gözlemlenen olasılığı hesaplama
obs_prob <- table(data) / length(data)

# Olasılıkların ve gözlemlerin grafiğini çizme
plot(values, true_prob, type = "p", pch = 16, col = "blue",
     xlab = "x", ylab = "olasılıklar", main = "olasılık fonksiyonu")

# Gözlemlenen olasılıkların toplanması
points(as.numeric(names(obs_prob)), obs_prob, pch = 16L, col = "red")
legend("topright", legend = c("gerçek olasılık", "gözlemlenen olasılık"),
       col = c("blue", "red"), pch = 16L, cex = 0.8)
grid()
