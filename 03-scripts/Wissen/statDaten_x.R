rawdata <- read.csv(file = "testdaten.csv", header = TRUE)
wData <- data.frame(x = rawdata$Age)

# Entfernung ungueltiger Werte
N <- nrow(wData)
del <- 0
for(i in 1:N){
  if(is.na(wData$x[i])){
    wData <- wData[-c(i),]
    del <- del+1
    N <- nrow(wData)
  }
}
if(del > 0){
  del <- del/2
  print(paste(del, "ungültige Einträge"))
}
# Berechnung des Mittelwerts
mean <- mean(wData$x)
# Berechnung der Varianzen
var <- var(wData$x)
# Berechnung der Standardabweichung
std <- sd(wData$x)
min <- min(wData$x)
max <- max(wData$x)

# Konsolenausgabe der Werte
print(paste("Stichprobengroesse:", N))
print(paste("Minimum:", min, ", Maximum:", max))
print(paste("Mittelwert X:", mean))
print(paste("Varianz X:", var))
print(paste("Standardabweichung X:", std))


# Grafische Darstellung
wData_sorted <- sort(wData$x, decreasing = FALSE)
# Balkendiagramm
#barplot(wData_sorted, main = "Alter", xlab = "Lfd. Nr.", ylab="Alter")

# Histogramm
hist(wData$x, main="Altersverteilung", xlab = "Alter", ylab ="Häufigkeit", breaks=seq(min, max, length=1), col ="red")
box()
