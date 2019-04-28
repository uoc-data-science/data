rawdata <- read.csv(file = "testdaten.csv", header = TRUE)


wData <- data.frame(x = rawdata$Height.inches., y = rawdata$Weight.lbs.)

# Entfernung ungueltiger Werte
N <- nrow(wData)
del <- 0
for(i in 1:N){
  if(is.na(wData$x[i]) || is.na(wData$y[i])){
    wData <- wData[-c(i),]
    del <- del+1
    N <- nrow(wData)
  }
}
if(del > 0){
  del <- del/2
  print(paste(del, "ungültige Einträge"))
}
# Berechnung der Mittelwerte
mean_x <- mean(wData$x)
mean_y <- mean(wData$y)
# Berechnung der Varianzen
var_x <- var(wData$x)
var_y <- var(wData$y)
# Berechnung der Standardabweichungen
std_x <- sd(wData$x)
std_y <- sd(wData$y)
# Berechnung der Kovarianz
cov_xy <- cov(wData$x, wData$y)
# Berechnung des Korrelationskoeffizienten
cor_xy <- cor(wData$x, wData$y)
# Konsolenausgabe der Werte
print(paste("Stichprobengroesse:", N))
print(paste("Mittelwert X:", mean_x, " Y:", mean_y))
print(paste("Varianz X:", var_x, " Y:", var_y))
print(paste("Standardabweichung X:", std_x, " Y", std_y))
print(paste("Kovarianz XY:", cov_xy))
print(paste("Korrelationskoeffizient XY:", cor_xy))
plot(wData$x, wData$y)
