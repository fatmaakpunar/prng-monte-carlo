library(moments)
teta=5 
n=10000
tkr<-5 #örneklemi tekrar
xx<- 3
alfa=0.05

# ---------------------------------------------------------
# BÖLÜM 1: ÇOKLU ÖRNEKLEM SİMÜLASYONU (tkr = 5)
# ---------------------------------------------------------
orneklem_ortalamalari <- numeric(tkr) #Bana 5 birimlik (tkr kadar) boş bir yer ayır
orneklem_varyanslari <- numeric(tkr)
orneklem_skewleri <- numeric(tkr)
orneklem_kurtolari <- numeric(tkr)

for (i in 1:tkr) {
  u <- runif(n) #uniform dağılımla n tane veri üret
  x <- -teta * log(1 - u) # Ters dönüşüm
  
  orneklem_ortalamalari[i] <- mean(x) #ortalamaların ortalaması
  orneklem_varyanslari[i] <- var(x) #varyansların varyansı
  orneklem_skewleri[i] <- skewness(x) #çarpıklık
  orneklem_kurtolari[i] <- kurtosis(x) #basıklık
}
#5 değerin ortalamaları
cat("--- ÇOKLU ÖRNEKLEM SONUÇLARI ---\n")
cat("Örneklem Ortalamalarının Ortalaması: ", mean(orneklem_ortalamalari), "\n")
cat("Örneklem Varyanslarının ort   : ", mean(orneklem_varyanslari), "\n")
cat("Örneklem Skewness ort       : ", mean(orneklem_skewleri), "\n")
cat("Örneklem Kurtosis ort        : ", mean(orneklem_kurtolari), "\n\n")


# ---------------------------------------------------------
# BÖLÜM 2: TEK BİR BÜYÜK ÖRNEKLEMDE TEORİK vs AMPİRİK KARŞILAŞTIRMA
# ---------------------------------------------------------
u <- runif(n)
x <- -teta * log(1 - u)

# Teorik (Analitik) Değerler
Aort <- teta
Avar <- teta^2
Aols <- 1 - exp(-xx / teta)
Atersleri <- -teta * log(1 - alfa)

# Ampirik (Örneklemden Elde Edilen) Değerler
Oort <- mean(x)
Ovar <- var(x)
Oskew <- skewness(x)
Okurto <- kurtosis(x)

# P(X < xx) Olasılığı (For döngüsü yerine R'ın vektörel yapısı kullanıldı)
say <- sum(x < xx) 
Ools <- say / n

# Alfa (Quantile) Hesaplaması
xsira <- sort(x)
Otersleri <- xsira[floor(alfa * n)] 
# Alternatif ve daha güvenli R fonksiyonu: quantile(x, alfa)

# Karşılaştırma Çıktıları
cat("--- TEORİK VE AMPİRİK KARŞILAŞTIRMA ---\n")
cat("Teorik ortalama:",Aort, "Ampirik Ortalama:",Oort)
cat("Teorik varyans:",Avar, "Ampirik Varyans:",Ovar)
cat("Teorik P(x<3):",Aols, "Ampirik P(x<3):", Ools)
cat("Teorik alfaya karşılık gelen değer",Atersleri, "Ampirik alfaya karşılık gelen değer",Otersleri)
