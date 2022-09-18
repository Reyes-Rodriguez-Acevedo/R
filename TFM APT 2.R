###### PAGINAS Y PAQUETES PARA DESCARGAR DATOS FINANCIEROS EN R

# Quantmod --- Yahoo finance /Oanda/ GoogleFinance 
# FRED
# Quandl
# fIMport
#wbstats  --- Datos del Banco Mundial

install.packages("quantmod")
library(quantmod)

install.packages("xts")
library(xts)

install.packages("zoo")
library(zoo)

install.packages("tseries")
library(tseries)

install.packages("timeDate")
library(timeDate)

install.packages("timeSeries")
library(timeSeries)

install.packages("fImport")
library(fImport)

install.packages("Quandl")
library(Quandl)

install.packages("rjson")
library(rjson)

install.packages("dplyr")
library(dplyr)

install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)

install.packages("xlsx")
library(xlsx)

install.packages("readxl")
library(readxl)

install.packages("fImport")
library(fImport)

install.packages("TSstudio")
library(TSstudio)

install.packages("fPortfolio")
library(fPortfolio)

install.packages("lubridate")
library(lubridate)

install.packages("stringr")
library(stringr)

# https://dstanley4.github.io/apaTables/articles/apaTables.html
install.packages("apaTables")
library(apaTables)

install.packages("texreg")
library(texreg)

# Instalar paquete in factoextra para calcular el analisis de correspondencia 
install.packages("FactoMineR")
library(FactoMineR)

# Instalar paquete factoextra para ayudar en la interpretacion y 
# visualizacion del analisis de correspondencia
install.packages("factoextra")
library(factoextra)

# confirmatory factor analysis (CFA)
# https://tutorials.methodsconsultants.com/posts/confirmatory-factor-analysis-using-the-lavaan-package-in-r/#:~:text=CFA%20models%20are%20specified%20in,the%20variables%20y1%20to%20y4%20.
install.packages("lavaan")
library(lavaan)

# Test de Wald
# https://stat.ethz.ch/pipermail/r-help-es/2014-May/007609.html
install.packages("aod")
library(aod)
### Wald estadistico prueba
### https://www.institutomora.edu.mx/testu/sitepages/martinpaladino/modelos_logit_con_r.html
### chi2 estadistico contraste (valor critico)
### https://rpubs.com/osoramirez/111403
### operaciones con matrices
### https://r-coder.com/operaciones-matrices-r/#Inversa_de_una_matriz_en_R

install.packages("pls")
library(pls)


# Directorio de trabajo
getwd()

# Cambiar Directorio de trabajo
setwd("C:/Users/Reyes/Documents/ADE MBA/TRABAJO FIN DE MASTER 48217/R")

### Datos
# PrincComp <- read_excel("PrincComp.xlsx", sheet = "Hoja1")
# class(PrincComp)

load("ACP2.RData")
# apt <- cbind.data.frame(Rcarteras2[ , c(3:12)], autovectores)

# Rcarteras2 <- read_excel("Rcarteras2.xlsx", sheet = "Rcarteras2")
# class(Rcarteras2)
# 
# apt <- cbind.data.frame(PrincComp)

apt <- cbind.data.frame(acp34)
colnames(apt) <- c(
        "X1", 
        "X2", 
        "X3", 
        "X4", 
        "X5", 
        "X6", 
        "X7", 
        "X8", 
        "X9", 
        "X10", 
        "X11", 
        "X12", 
        "X13", 
        "X14", 
        "X15", 
        "X16", 
        "X17", 
        "X18", 
        "X19", 
        "X20", 
        "X21", 
        "X22", 
        "X23", 
        "X24", 
        "X25", 
        "X26", 
        "X27", 
        "X28", 
        "X29", 
        "X30", 
        "X31", 
        "X32", 
        "X33", 
        "X34"
)

apt$Cartera1 <- Rcarteras2$Cartera1 - Rcarteras2$LT.r
apt$Cartera2 <- Rcarteras2$Cartera2 - Rcarteras2$LT.r
apt$Cartera3 <- Rcarteras2$Cartera3 - Rcarteras2$LT.r
apt$Cartera4 <- Rcarteras2$Cartera4 - Rcarteras2$LT.r
apt$Cartera5 <- Rcarteras2$Cartera5 - Rcarteras2$LT.r
apt$Cartera6 <- Rcarteras2$Cartera6 - Rcarteras2$LT.r
apt$Cartera7 <- Rcarteras2$Cartera7 - Rcarteras2$LT.r
apt$Cartera8 <- Rcarteras2$Cartera8 - Rcarteras2$LT.r
apt$Cartera9 <- Rcarteras2$Cartera9 - Rcarteras2$LT.r
apt$Cartera10 <- Rcarteras2$Cartera10 - Rcarteras2$LT.r

# apt <- cbind.data.frame(Rcarteras2[ , c(3:12)], PrincComp)
# class(apt)

save(apt, file = "apt.RData")
write.xlsx(apt, file = "apt.xlsx", 
           sheetName = "apt", row.names = FALSE, col.names = TRUE)

load("reg.cartera.apt.RData")

### APT

### Cartera1
reg.cartera1 <- lm(Cartera1 ~ X1 + X2 + X3 + X4 + X5, data = apt)
summary(reg.cartera1)
apa.reg.table(reg.cartera1, filename= "reg.cartera1.APT.doc", table.number= 1)
write.xlsx(summary(reg.cartera1)$coefficients, file = "reg.cartera1.APT.xlsx", 
           sheetName = "apt1", row.names = TRUE)

### Cartera2
reg.cartera2 <- lm(Cartera2 ~ X1 + X2 + X3 + X4 + X5, data = apt)
summary(reg.cartera2)
apa.reg.table(reg.cartera2, filename= "reg.cartera2.APT.doc", table.number= 2)
write.xlsx(summary(reg.cartera2)$coefficients, file = "reg.cartera2.APT.xlsx", 
           sheetName = "apt2", row.names = TRUE)

### Cartera3
reg.cartera3 <- lm(Cartera3 ~ X1 + X2 + X3 + X4 + X5, data = apt)
summary(reg.cartera3)
apa.reg.table(reg.cartera3, filename= "reg.cartera3.APT.doc", table.number= 3)
write.xlsx(summary(reg.cartera3)$coefficients, file = "reg.cartera3.APT.xlsx", 
           sheetName = "apt3", row.names = TRUE)

### Cartera4
reg.cartera4 <- lm(Cartera4 ~ X1 + X2 + X3 + X4 + X5, data = apt)
summary(reg.cartera4)
apa.reg.table(reg.cartera4, filename= "reg.cartera4.APT.doc", table.number= 4)
write.xlsx(summary(reg.cartera4)$coefficients, file = "reg.cartera4.APT.xlsx", 
           sheetName = "apt4", row.names = TRUE)

### Cartera5
reg.cartera5 <- lm(Cartera5 ~ X1 + X2 + X3 + X4 + X5, data = apt)
summary(reg.cartera5)
apa.reg.table(reg.cartera5, filename= "reg.cartera5.APT.doc", table.number= 5)
write.xlsx(summary(reg.cartera5)$coefficients, file = "reg.cartera5.APT.xlsx", 
           sheetName = "apt5", row.names = TRUE)

### Cartera6
reg.cartera6 <- lm(Cartera6 ~ X1 + X2 + X3 + X4 + X5, data = apt)
summary(reg.cartera6)
apa.reg.table(reg.cartera6, filename= "reg.cartera6.APT.doc", table.number= 6)
write.xlsx(summary(reg.cartera6)$coefficients, file = "reg.cartera6.APT.xlsx", 
           sheetName = "apt6", row.names = TRUE)

### Cartera7
reg.cartera7 <- lm(Cartera7 ~ X1 + X2 + X3 + X4 + X5, data = apt)
summary(reg.cartera7)
apa.reg.table(reg.cartera7, filename= "reg.cartera7.APT.doc", table.number= 7)
write.xlsx(summary(reg.cartera7)$coefficients, file = "reg.cartera7.APT.xlsx", 
           sheetName = "apt7", row.names = TRUE)

### Cartera8
reg.cartera8 <- lm(Cartera8 ~ X1 + X2 + X3 + X4 + X5, data = apt)
summary(reg.cartera8)
apa.reg.table(reg.cartera8, filename= "reg.cartera8.APT.doc", table.number= 8)
write.xlsx(summary(reg.cartera8)$coefficients, file = "reg.cartera8.APT.xlsx", 
           sheetName = "apt8", row.names = TRUE)

### Cartera9
reg.cartera9 <- lm(Cartera9 ~ X1 + X2 + X3 + X4 + X5, data = apt)
summary(reg.cartera9)
apa.reg.table(reg.cartera9, filename= "reg.cartera9.APT.doc", table.number= 9)
write.xlsx(summary(reg.cartera9)$coefficients, file = "reg.cartera9.APT.xlsx", 
           sheetName = "apt9", row.names = TRUE)

### Cartera10
reg.cartera10 <- lm(Cartera10 ~ X1 + X2 + X3 + X4 + X5, data = apt)
summary(reg.cartera10)
apa.reg.table(reg.cartera10, filename= "reg.cartera10.APT.doc", table.number= 10)
write.xlsx(summary(reg.cartera10)$coefficients, file = "reg.cartera10.APT.xlsx", 
           sheetName = "apt10", row.names = TRUE)

save(reg.cartera1, 
     reg.cartera2, 
     reg.cartera3, 
     reg.cartera4, 
     reg.cartera5, 
     reg.cartera6, 
     reg.cartera7, 
     reg.cartera8, 
     reg.cartera9, 
     reg.cartera10, 
     file = "reg.cartera.apt.RData")

print(reg.cartera1$coefficients[1])
print(reg.cartera1$residuals)
load("reg.cartera.apt.RData")


### Test conjunto Alfas de Jensen

T <- c(228)
# T <- c(220)
T.inverso <- 1 / T
N <- c(10)
N.inverso <- 1 / N
K <- c(5)
K.inverso <- 1 / K

alfa <- matrix(c(reg.cartera1$coefficients[1], 
                 reg.cartera2$coefficients[1], 
                 reg.cartera3$coefficients[1], 
                 reg.cartera4$coefficients[1], 
                 reg.cartera5$coefficients[1], 
                 reg.cartera6$coefficients[1], 
                 reg.cartera7$coefficients[1], 
                 reg.cartera8$coefficients[1], 
                 reg.cartera9$coefficients[1], 
                 reg.cartera10$coefficients[1]), 
               ncol = 1, byrow = TRUE)

alfa.t <- matrix(c(reg.cartera1$coefficients[1], 
                   reg.cartera2$coefficients[1], 
                   reg.cartera3$coefficients[1], 
                   reg.cartera4$coefficients[1], 
                   reg.cartera5$coefficients[1], 
                   reg.cartera6$coefficients[1], 
                   reg.cartera7$coefficients[1], 
                   reg.cartera8$coefficients[1], 
                   reg.cartera9$coefficients[1], 
                   reg.cartera10$coefficients[1]), 
                 ncol = 10, byrow = FALSE)

u <- matrix(c(reg.cartera1$residuals, 
              reg.cartera2$residuals, 
              reg.cartera3$residuals, 
              reg.cartera4$residuals, 
              reg.cartera5$residuals, 
              reg.cartera6$residuals, 
              reg.cartera7$residuals, 
              reg.cartera8$residuals, 
              reg.cartera9$residuals, 
              reg.cartera10$residuals), 
            ncol = 10, byrow = FALSE)

u.t <- matrix(c(reg.cartera1$residuals, 
                reg.cartera2$residuals, 
                reg.cartera3$residuals, 
                reg.cartera4$residuals, 
                reg.cartera5$residuals, 
                reg.cartera6$residuals, 
                reg.cartera7$residuals, 
                reg.cartera8$residuals, 
                reg.cartera9$residuals, 
                reg.cartera10$residuals), 
              ncol = 228, byrow = TRUE)

class(u)
class(u.t)
class(alfa)
class(alfa.t)

Sigma.u <- T.inverso * u.t %*% u
Sigma.inverso <- solve(Sigma.u)

J <- T * alfa.t %*% Sigma.inverso %*% alfa
print(J)
qchisq(0.95,N)
qchisq(0.99,N)

factores <- as.matrix(cbind.data.frame(apt$X1, apt$X2, apt$X3, apt$X4, apt$X5))
factores.media <- as.matrix(colMeans(data.frame(factores)))
varcovfactores <- as.matrix(cov(factores))
fsigmaf <- t(factores.media) %*% solve(varcovfactores) %*% factores.media

J1 <- T * solve(as.matrix(1 + fsigmaf)) %*% alfa.t %*% Sigma.inverso %*% alfa
print(J1)
qchisq(0.95,N)
qchisq(0.99,N)

JF <- ((T - N - 1) / N) * solve(as.matrix(1 + fsigmaf)) %*% alfa.t %*% Sigma.inverso %*% alfa
print(JF)
qf(0.95,N,T-N-K)
qf(0.99,N,T-N-K)


save(alfa, 
     alfa.t, 
     u, 
     u.t, 
     Sigma.u, 
     Sigma.inverso, 
     J, 
     J1, 
     JF, 
     file = "Alfa.Jensen.APT.RData")

load("Alfa.Jensen.APT.RData")

write.xlsx(apt, file = "apt.xlsx", sheetName = "apt", row.names = FALSE)
