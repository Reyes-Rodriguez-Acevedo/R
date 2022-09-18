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
dbtfm2.apt <- read_excel("dbtfm2.apt.xlsx", sheet = "dbtfm2.apt")
class(dbtfm2.apt)

Rcarteras2 <- read_excel("Rcarteras2.xlsx", sheet = "Rcarteras2")
sum(is.na(Rcarteras2))
class(Rcarteras2)

dbtfm2.acp <- dbtfm2.apt[ -c(1:34), -c(1:1)]
sum(is.na(dbtfm2.acp))
class(dbtfm2.acp)
apply(dbtfm2.acp, 2, var)   # 1=fila, 2=columna

write.xlsx(dbtfm2.acp, file = "dbtfm2.acp.xlsx", 
           sheetName = "dbtfm2.acp", row.names = TRUE, col.names = TRUE)

T <- c(228)
T.inverso <- 1 / T
N <- c(34)
N.inverso <- 1 / N

R <- as.matrix(dbtfm2.acp)
class(R)

R.t <- t(R)
class(R.t)

Sigma.R <- T.inverso * R.t %*% R

write.xlsx(Sigma.R, file = "Sigma.R.xlsx", sheetName = "Sigma.R", row.names = FALSE, 
           col.names = FALSE)


### Analisis Componentes Principales
eigen <- eigen(Sigma.R)
autovectores <- data.frame(eigen$vectors)
class(autovectores)
autovalores <- data.frame(eigen$values)
class(autovalores)
acp34 <- as.matrix(dbtfm2.acp) %*% as.matrix(autovectores)
# acp34 <- t(R.t %*% as.matrix(autovectores))

write.xlsx(autovalores, file = "autovalores34.xlsx", 
           sheetName = "autovalores", row.names = TRUE, col.names = TRUE)
write.xlsx(acp34, file = "acp34.xlsx", 
           sheetName = "autovectores", row.names = TRUE, col.names = TRUE)


### Analisis Componentes Principales (prcomp)
acp.prcomp <- prcomp(dbtfm2.acp, center = F, scale = F)
# acp.prcomp <- prcomp(R.t, center = F, scale = F)

# acp34.prcomp <- as.matrix(dbtfm2.acp) %*% acp.prcomp$rotation
# acp34.prcomp <- acp.prcomp$rotation %*% as.matrix(dbtfm2.acp)
acp34.prcomp <- acp.prcomp$x
acp.prcomp$rotation
### rotation contiene el valor de los loadings para cada componente (eigenvector)
### https://rpubs.com/Joaquin_AR/287787


### https://rpubs.com/Cristina_Gil/PCA
### Analisis Componentes Principales (FactoMineR)
acp.factominer <- PCA(dbtfm2.acp, scale.unit = F)
# acp.factominer <- PCA(R.t, scale.unit = F)
acp.factominer$eig

plot(acp.factominer$eig[ ,3])

acp.factominer$call$X

write.xlsx(acp.factominer$eig, file = "eigenvalores34.xlsx", 
           sheetName = "eigenvalor.acp", row.names = TRUE, col.names = TRUE)


jpeg(filename = "Biplot 2 PC.jpg", width = 800, height = 600, units = "px", 
     quality = 100)

fviz_pca_biplot(acp.prcomp, col.var = "deeppink3", 
                col.ind = "dodgerblue3", 
                geom.var = c("arrow", "text"), 
                labelsize = 2, 
                repel = FALSE)
dev.off()

correlaciones <- cbind.data.frame(Rcarteras2$IBEX.r - Rcarteras2$LT.r, acp34)
colnames(correlaciones)[1] <- "IBEX.neto"
class(correlaciones)

matriz.cor <- cor(correlaciones)
write.xlsx(matriz.cor, file = "Matriz Correlaciones IBEX ACP.xlsx",
    sheetName = "Correlaciones", row.names = TRUE, col.names = TRUE)


save(dbtfm2.acp, 
     dbtfm2.apt, 
     acp.factominer, 
     acp.prcomp, 
     acp34, 
     acp34.prcomp, 
     autovalores, 
     autovectores, 
     correlaciones, 
     eigen, 
     matriz.cor, 
     R, 
     R.t, 
     Sigma.R, 
     Rcarteras2, 
     file = "ACP2.RData")

load("ACP2.RData")
