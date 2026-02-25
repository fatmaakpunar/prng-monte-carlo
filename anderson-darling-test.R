#H0:veriler Uniform (Düzgün) dağılıma uygundur.
#H1:veriler Uniform (Düzgün) dağılıma uygun değildir.
library(nortest) 
veriler <- c(0.849, 0.755, 0.550, 0.515, 0.185, 0.442, 0.880, 0.958, 0.278, 0.965, 
             0.419, 0.885, 0.241, 0.215, 0.269, 0.574, 0.200, 0.765, 0.622, 0.926)
# ad.test fonksiyonu genelde normallik içindir, uniform için goftest kullanılır:
library(goftest)
ad.test(veriler, null="punif") #(0,3384 > 0,05)
#p-değeri alpha değerinden büyük olduğu için H0 (Yokluk Hipotezi) reddedilemez.
#%95 güven düzeyinde, ürettiğin bu verilerin Uniform (Düzgün) dağılıma uygun olduğu sonucuna varılmıştır.
