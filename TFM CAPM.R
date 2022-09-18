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

# Directorio de trabajo
getwd()

# Cambiar Directorio de trabajo
setwd("C:/Users/Reyes/Documents/ADE MBA/TRABAJO FIN DE MASTER 48217/R")

### Datos
library(readxl)
dbtfm2 <- read_excel("dbtfm2.xlsx", sheet = "Rcarteras2")
dbtfm2 <- dbtfm2[-c(1:35), ]

# attach(dbtfm2)
# detach(dbtfm2)
# unlist(dbtfm2)
# class(dbtfm2)

dbtfm2$IBEX.neto <- dbtfm2$IBEX.r - dbtfm2$LT.r
dbtfm2$Cartera1.neto <- dbtfm2$Cartera1 - dbtfm2$LT.r
dbtfm2$Cartera2.neto <- dbtfm2$Cartera2 - dbtfm2$LT.r
dbtfm2$Cartera3.neto <- dbtfm2$Cartera3 - dbtfm2$LT.r
dbtfm2$Cartera4.neto <- dbtfm2$Cartera4 - dbtfm2$LT.r
dbtfm2$Cartera5.neto <- dbtfm2$Cartera5 - dbtfm2$LT.r
dbtfm2$Cartera6.neto <- dbtfm2$Cartera6 - dbtfm2$LT.r
dbtfm2$Cartera7.neto <- dbtfm2$Cartera7 - dbtfm2$LT.r
dbtfm2$Cartera8.neto <- dbtfm2$Cartera8 - dbtfm2$LT.r
dbtfm2$Cartera9.neto <- dbtfm2$Cartera9 - dbtfm2$LT.r
dbtfm2$Cartera10.neto <- dbtfm2$Cartera10 - dbtfm2$LT.r

### Modelo CAPM

### Cartera1
reg.cartera1 <- lm(dbtfm2$Cartera1.neto ~ dbtfm2$IBEX.neto)
summary(reg.cartera1)
apa.reg.table(reg.cartera1, filename= "reg.cartera1.doc", table.number= 2)

### Cartera2
reg.cartera2 <- lm(dbtfm2$Cartera2.neto ~ dbtfm2$IBEX.neto)
summary(reg.cartera2)
apa.reg.table(reg.cartera2, filename= "reg.cartera2.doc", table.number= 2)

### Cartera3
reg.cartera3 <- lm(dbtfm2$Cartera3.neto ~ dbtfm2$IBEX.neto)
summary(reg.cartera3)
apa.reg.table(reg.cartera3, filename= "reg.cartera3.doc", table.number= 2)

### Cartera4
reg.cartera4 <- lm(dbtfm2$Cartera4.neto ~ dbtfm2$IBEX.neto)
summary(reg.cartera4)
apa.reg.table(reg.cartera4, filename= "reg.cartera4.doc", table.number= 2)

### Cartera5
reg.cartera5 <- lm(dbtfm2$Cartera5.neto ~ dbtfm2$IBEX.neto)
summary(reg.cartera5)
apa.reg.table(reg.cartera5, filename= "reg.cartera5.doc", table.number= 2)

### Cartera6
reg.cartera6 <- lm(dbtfm2$Cartera6.neto ~ dbtfm2$IBEX.neto)
summary(reg.cartera6)
apa.reg.table(reg.cartera6, filename= "reg.cartera6.doc", table.number= 2)

### Cartera7
reg.cartera7 <- lm(dbtfm2$Cartera7.neto ~ dbtfm2$IBEX.neto)
summary(reg.cartera7)
apa.reg.table(reg.cartera7, filename= "reg.cartera7.doc", table.number= 2)

### Cartera8
reg.cartera8 <- lm(dbtfm2$Cartera8.neto ~ dbtfm2$IBEX.neto)
summary(reg.cartera8)
apa.reg.table(reg.cartera8, filename= "reg.cartera8.doc", table.number= 2)

### Cartera9
reg.cartera9 <- lm(dbtfm2$Cartera9.neto ~ dbtfm2$IBEX.neto)
summary(reg.cartera9)
apa.reg.table(reg.cartera9, filename= "reg.cartera9.doc", table.number= 2)

### Cartera10
reg.cartera10 <- lm(dbtfm2$Cartera10.neto ~ dbtfm2$IBEX.neto)
summary(reg.cartera10)
apa.reg.table(reg.cartera10, filename= "reg.cartera10.doc", table.number= 2)

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
     file = "reg.cartera.RData")


load("reg.cartera.RData")

print(reg.cartera1$coefficients[1])
print(reg.cartera1$residuals)

### Test conjunto Alfas de Jensen

T <- c(228)
T.inverso <- 1 / T
N <- c(10)
N.inverso <- 1 / N

# alfa <- rbind(reg.cartera1$coefficients[1], 
#           reg.cartera2$coefficients[1], 
#           reg.cartera3$coefficients[1], 
#           reg.cartera4$coefficients[1], 
#           reg.cartera5$coefficients[1], 
#           reg.cartera6$coefficients[1], 
#           reg.cartera7$coefficients[1], 
#           reg.cartera8$coefficients[1], 
#           reg.cartera9$coefficients[1], 
#           reg.cartera10$coefficients[1])

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

# alfa.t <- cbind(reg.cartera1$coefficients[1], 
#               reg.cartera2$coefficients[1], 
#               reg.cartera3$coefficients[1], 
#               reg.cartera4$coefficients[1], 
#               reg.cartera5$coefficients[1], 
#               reg.cartera6$coefficients[1], 
#               reg.cartera7$coefficients[1], 
#               reg.cartera8$coefficients[1], 
#               reg.cartera9$coefficients[1], 
#               reg.cartera10$coefficients[1])

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

# u <- cbind(reg.cartera1$residuals, 
              # reg.cartera2$residuals, 
              # reg.cartera3$residuals, 
              # reg.cartera4$residuals, 
              # reg.cartera5$residuals, 
              # reg.cartera6$residuals, 
              # reg.cartera7$residuals, 
              # reg.cartera8$residuals, 
              # reg.cartera9$residuals, 
              # reg.cartera10$residuals)

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

# u.t <- rbind(reg.cartera1$residuals, 
#            reg.cartera2$residuals, 
#            reg.cartera3$residuals, 
#            reg.cartera4$residuals, 
#            reg.cartera5$residuals, 
#            reg.cartera6$residuals, 
#            reg.cartera7$residuals, 
#            reg.cartera8$residuals, 
#            reg.cartera9$residuals, 
#            reg.cartera10$residuals)

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

IBEX.neto <- dbtfm2$IBEX.r - dbtfm2$LT.r
IBEX.neto

IBEX.media <- mean(IBEX.neto)
IBEX.media
print(IBEX.media^2)
IBEX.varianza <- var(IBEX.neto)
IBEX.varianza

J1 <- T * (1 + ((IBEX.media^2) / IBEX.varianza)) * alfa.t %*% Sigma.inverso %*% alfa
print(J1)
qchisq(0.95,N)
qchisq(0.99,N)

JF <- ((T - N - 1) / N) * (1 + ((IBEX.media^2) / IBEX.varianza)) * alfa.t %*% Sigma.inverso %*% alfa
print(JF)
qf(0.95,N,T-N-1)
qf(0.99,N,T-N-1)


save(alfa, 
     alfa.t, 
     u, 
     u.t, 
     Sigma.u, 
     Sigma.inverso, 
     J, 
     J1, 
     JF, 
     file = "Alfa.Jensen.RData")

load("Alfa.Jensen.RData")
