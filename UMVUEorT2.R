# Gerekli parametreler
theta <- 5
n_degerleri <- c(10, 50, 100, 500, 1000, 5000) 
tkr <- 1000 # Değişkenliği ölçmek için her n değerini 1000 kere simüle edeceğiz

#varyansları saklayacağımız boş vektörler
varyans_UMVUE <- numeric(length(n_degerleri))
varyans_T2 <- numeric(length(n_degerleri))

cat("===================================================\n")
cat("      TAHMİN EDİCİLERİN SİMÜLASYON SONUÇLARI       \n")
cat("===================================================\n")

for (i in 1:length(n_degerleri)) {
  
  n <- n_degerleri[i]
  
  # Her 'n' için 1000 tekrarlık sonuçları tutacak geçici vektörler
  orneklem_UMVUE <- numeric(tkr)
  orneklem_T2 <- numeric(tkr)
  
  # İç döngü: n boyutunda örneklemi 'tkr' kere çekiyoruz
  for (j in 1:tkr) {
    u <- runif(n)
    veri <- -theta * log(1-u) # Üstel dağılım ters dönüşüm
    
    orneklem_UMVUE[j] <- mean(veri)
    orneklem_T2[j] <- veri[1] - veri[2] + mean(veri)
  }

  # 1000 simülasyonun istatistiklerini hesaplıyoruz
  ort_UMVUE <- mean(orneklem_UMVUE)
  var_UMVUE <- var(orneklem_UMVUE)
  
  ort_T2 <- mean(orneklem_T2)
  var_T2 <- var(orneklem_T2)
  
  #varyansları kaydediyoruz
  varyans_UMVUE[i] <- var_UMVUE
  varyans_T2[i] <- var_T2
  
  # consol çıktıları (cat)
  cat(sprintf("\n--- n = %d İçin Örneklem Sonuçları ---\n", n))
  cat("UMVUE Ortalaması     : ", ort_UMVUE, "\n")
  cat("T2 Ortalaması        : ", ort_T2, "\n")
  cat("UMVUE Varyansı       : ", var_UMVUE, "\n")
  cat("T2 Varyansı          : ", var_T2, "\n")

}
# ---------------------------------------------------------
# ÇİZGİ GRAFİĞİ (LINE PLOT) OLUŞTURMA
# ---------------------------------------------------------

# n değerlerine karşı Tahmin Edicilerin Varyanslarının grafiği
plot(n_degerleri, varyans_T2, type="b", col="red", lwd=2, pch=16,
     ylim=c(0, max(varyans_T2)), # Y eksenini T2'nin en yüksek varyansına göre ayarla
     xlab="Örneklem Boyutu (n)", 
     ylab="Tahmin Edicinin Varyansı (Değişkenliği)",
     main="UMVUE ve T2 Varyanslarının n ile Değişimi")

# UMVUE varyans çizgisini aynı grafiğe ekle
lines(n_degerleri, varyans_UMVUE, type="b", col="blue", lwd=2, pch=16)

# Hangi çizginin ne olduğunu gösteren bilgi kutusu (Legend)
legend("topright", legend=c("T2 Varyansı", "UMVUE Varyansı"), 
       col=c("red", "blue"), lwd=2, pch=16)
