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
Rcarteras2 <- read_excel("Rcarteras2.xlsx", sheet = "Rcarteras2")
Rcarteras2 <- Rcarteras2[c(1:220), ]

# IGBM <- read_excel("IGBM rendimiento mensual.xlsx", sheet = "Hoja1")

# IGBM <- IGBM[c(117:344), ]
# IGBM <- IGBM[c(117:336), ]

FF <- read_excel("Risk Factors Spain Euros.xlsx", sheet = "Hoja1")
FF <- FF[c(37:256), c(1:3)]
class(FF)
class(FF$SMB)
class(FF$HML)

famayfrench <- cbind.data.frame(Rcarteras2[ , c(1)])

famayfrench$IBEX <- Rcarteras2$IBEX.r - Rcarteras2$LT.r
famayfrench$SMB <- FF$SMB
famayfrench$HML <- FF$HML
famayfrench$Cartera1 <- Rcarteras2$Cartera1 - Rcarteras2$LT.r
famayfrench$Cartera2 <- Rcarteras2$Cartera2 - Rcarteras2$LT.r
famayfrench$Cartera3 <- Rcarteras2$Cartera3 - Rcarteras2$LT.r
famayfrench$Cartera4 <- Rcarteras2$Cartera4 - Rcarteras2$LT.r
famayfrench$Cartera5 <- Rcarteras2$Cartera5 - Rcarteras2$LT.r
famayfrench$Cartera6 <- Rcarteras2$Cartera6 - Rcarteras2$LT.r
famayfrench$Cartera7 <- Rcarteras2$Cartera7 - Rcarteras2$LT.r
famayfrench$Cartera8 <- Rcarteras2$Cartera8 - Rcarteras2$LT.r
famayfrench$Cartera9 <- Rcarteras2$Cartera9 - Rcarteras2$LT.r
famayfrench$Cartera10 <- Rcarteras2$Cartera10 - Rcarteras2$LT.r

write.xlsx(famayfrench, file = "FamayFrench IGBM.xlsx", 
           sheetName = "famayfrench", row.names = TRUE)

save(famayfrench, 
     file = "famayfrench.RData")

### Modelo FAMA Y FRENCH IBEX

### Cartera1
reg.cartera1 <- lm(Cartera1 ~ IBEX + SMB + HML, data = famayfrench)
summary(reg.cartera1)
apa.reg.table(reg.cartera1, filename= "reg.cartera1.doc", table.number= 1)
write.xlsx(summary(reg.cartera1)$coefficients, file = "reg.cartera1.xlsx", 
           sheetName = "reg1", row.names = TRUE)

### Cartera2
reg.cartera2 <- lm(Cartera2 ~ IBEX + SMB + HML, data = famayfrench)
summary(reg.cartera2)
apa.reg.table(reg.cartera2, filename= "reg.cartera2.doc", table.number= 2)
write.xlsx(summary(reg.cartera2)$coefficients, file = "reg.cartera2.xlsx", 
           sheetName = "reg2", row.names = TRUE)

### Cartera3
reg.cartera3 <- lm(Cartera3 ~ IBEX + SMB + HML, data = famayfrench)
summary(reg.cartera3)
apa.reg.table(reg.cartera3, filename= "reg.cartera3.doc", table.number= 3)
write.xlsx(summary(reg.cartera3)$coefficients, file = "reg.cartera3.xlsx", 
           sheetName = "reg3", row.names = TRUE)

### Cartera4
reg.cartera4 <- lm(Cartera4 ~ IBEX + SMB + HML, data = famayfrench)
summary(reg.cartera4)
apa.reg.table(reg.cartera4, filename= "reg.cartera4.doc", table.number= 4)
write.xlsx(summary(reg.cartera4)$coefficients, file = "reg.cartera4.xlsx", 
           sheetName = "reg4", row.names = TRUE)

### Cartera5
reg.cartera5 <- lm(Cartera5 ~ IBEX + SMB + HML, data = famayfrench)
summary(reg.cartera5)
apa.reg.table(reg.cartera5, filename= "reg.cartera5.doc", table.number= 5)
write.xlsx(summary(reg.cartera5)$coefficients, file = "reg.cartera5.xlsx", 
           sheetName = "reg5", row.names = TRUE)

### Cartera6
reg.cartera6 <- lm(Cartera6 ~ IBEX + SMB + HML, data = famayfrench)
summary(reg.cartera6)
apa.reg.table(reg.cartera6, filename= "reg.cartera6.doc", table.number= 6)
write.xlsx(summary(reg.cartera6)$coefficients, file = "reg.cartera6.xlsx", 
           sheetName = "reg6", row.names = TRUE)

### Cartera7
reg.cartera7 <- lm(Cartera7 ~ IBEX + SMB + HML, data = famayfrench)
summary(reg.cartera7)
apa.reg.table(reg.cartera7, filename= "reg.cartera7.doc", table.number= 7)
write.xlsx(summary(reg.cartera7)$coefficients, file = "reg.cartera7.xlsx", 
           sheetName = "reg7", row.names = TRUE)

### Cartera8
reg.cartera8 <- lm(Cartera8 ~ IBEX + SMB + HML, data = famayfrench)
summary(reg.cartera8)
apa.reg.table(reg.cartera8, filename= "reg.cartera8.doc", table.number= 8)
write.xlsx(summary(reg.cartera8)$coefficients, file = "reg.cartera8.xlsx", 
           sheetName = "reg8", row.names = TRUE)

### Cartera9
reg.cartera9 <- lm(Cartera9 ~ IBEX + SMB + HML, data = famayfrench)
summary(reg.cartera9)
apa.reg.table(reg.cartera9, filename= "reg.cartera9.doc", table.number= 9)
write.xlsx(summary(reg.cartera9)$coefficients, file = "reg.cartera9.xlsx", 
           sheetName = "reg9", row.names = TRUE)

### Cartera10
reg.cartera10 <- lm(Cartera10 ~ IBEX + SMB + HML, data = famayfrench)
summary(reg.cartera10)
apa.reg.table(reg.cartera10, filename= "reg.cartera10.doc", table.number= 10)
write.xlsx(summary(reg.cartera10)$coefficients, file = "reg.cartera10.xlsx", 
           sheetName = "reg10", row.names = TRUE)

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


### Test conjunto Alfas de Jensen

# T <- c(228)
T <- c(220)
T.inverso <- 1 / T
N <- c(10)
N.inverso <- 1 / N
K <- c(3)
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
             # ncol = 228, byrow = TRUE)
             ncol = 220, byrow = TRUE)

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

factores <- as.matrix(cbind.data.frame(famayfrench$IBEX, famayfrench$SMB, famayfrench$HML))
varcovfactores <- as.matrix(cov(factores))
factores.media <- as.matrix(colMeans(data.frame(factores)))
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
     file = "Alfa.Jensen.RData")

load("Alfa.Jensen.RData")
