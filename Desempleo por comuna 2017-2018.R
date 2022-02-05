# Directorio de trabajo
getwd()

# Cambiar Directorio de trabajo
setwd("C:/Users/Reyes/Documents/Data Intelligence/DATA-ECONOMIA/Chile/35 Desempleo/")

# How To Solve function Rcpp_precious_remove
install.packages("Rcpp")
library(Rcpp)

# Instalar paquete readxl
install.packages("xlsx")
library(xlsx)

install.packages("openxlsx")
library(openxlsx)

# Importar archivos de proporciona una "gramatica" (particularmente verbos) para
# la manipulacion y operaciones con data frames
install.packages("dplyr")
library(dplyr)

# Importar archivos de Stata
install.packages("foreign")
library(foreign)

# Importar archivos de Stata
install.packages("tidyverse")
library(tidyverse)

# Importar archivos de Stata
install.packages("haven")
library(haven)

# a) Factor tradicional (fact): Se basa en las proyecciones de poblacion del Censo
# 2002, utilizando una metodologia de calculo que divide a la poblacion por sexo
# y segun dos tramos etarios (personas menores de 15 años y personas de 15 años
# o mas (fact = Factor de expansión trimestral)
# b) Factor calibrado (fact_cal): Utiliza las proyecciones del Censo 2017 y una
# metodologia llamada raking, de mayor precision


# guarda los objetos en un archivo de datos de R
# save(Desempleo, file="Desempleo.RData")

# append data frames
Desempleo <- rbind(FT_2018_09, FT_2018_08, FT_2018_07, FT_2018_06, FT_2018_05,
                   FT_2018_04, FT_2018_03, FT_2018_02, FT_2018_01, FT_2017_12,
                   FT_2017_11, FT_2017_10, FT_2017_09, FT_2017_08, FT_2017_07,
                   FT_2017_06, FT_2017_05, FT_2017_04, FT_2017_03, FT_2017_02,
                   FT_2017_01)

# import y append csv
Toda <- read.csv2("BD 35.csv")
bd35 <- rbind(Toda, Desempleo)
bd35 <- bd35[order(bd35$Año, bd35$Mes, bd35$Region, bd35$Comuna, decreasing = F), ]

# exportar datos a archivos con extension .csv con la funcion write.csv2()
write.csv2(x = bd35, file = "BD 35.csv", col.names = TRUE, row.names = FALSE)

# exportar datos a archivos con extension .xlsx con la funcion write.xlsx2()
write.xlsx2(x = bd35, file = "BD 35.xlsx", sheetName = "Hoja1", 
            row.names = FALSE, col.names = TRUE, append = FALSE, showNA = TRUE, 
            password = NULL)


## ene-2018-09
datos <- read_dta("ene-2018-09.dta")
# View(datos)

# aggregate(fact_cal ~ cae_especifico + r_p_c, data = datos, sum)
FT <- aggregate(datos[ , c("fact_cal")], by = list( Situacion  = datos$cae_especifico,
                                                    Region = datos$region, Comuna = datos$r_p_c), FUN = sum)

class(FT)

FT <- FT[FT$Situacion >=1 & FT$Situacion <=9, ]

Ocupados <- FT[FT$Situacion >=1 & FT$Situacion <=7, ]

Ocupados <- aggregate(Ocupados[ , c("fact_cal")], by = list( 
  Ocupados$Region, Ocupados$Comuna), FUN = sum)

names(Ocupados) = c("Region", "Comuna", "Ocupados")
# names(Ocupados)[3] = "Ocupados"

class(Ocupados)
sum(Ocupados[ , "Ocupados"])

Desocupados <- FT[FT$Situacion >=8 & FT$Situacion <=9, ]

Desocupados <- aggregate(Desocupados[ , c("fact_cal")], by = list( 
  Desocupados$Region, Desocupados$Comuna), FUN = sum)

names(Desocupados) = c("Region", "Comuna", "Desocupados")

class(Desocupados)
sum(Desocupados[ , "Desocupados"])

# FT_2018_09 <- merge(x = Ocupados, y = Desocupados, by = c("Region", "Comuna"))
FT_2018_09 <- merge(x = Ocupados, y = Desocupados, all = T)

class(FT_2018_09)
sum(na.omit(FT_2018_09[ , "Ocupados"]))
sum(na.omit(FT_2018_09[ , "Desocupados"]))

FT_2018_09$DO <- FT_2018_09$Desocupados
FT_2018_09$DO <- replace(FT_2018_09$DO, is.na(FT_2018_09$DO), 0)
FT_2018_09$Fuerza_Trabajo <- FT_2018_09$Ocupados + FT_2018_09$DO
sum(FT_2018_09[ , "Fuerza_Trabajo"])
FT_2018_09$DO <- NULL
FT_2018_09$Tasa_Desocupacion <- FT_2018_09$Desocupados / FT_2018_09$Fuerza_Trabajo
FT_2018_09$Año <- 2018
FT_2018_09$Mes <- 09


## ene-2018-08
datos <- read_dta("ene-2018-08.dta")
# View(datos)

# aggregate(fact_cal ~ cae_especifico + r_p_c, data = datos, sum)
FT <- aggregate(datos[ , c("fact_cal")], by = list( Situacion  = datos$cae_especifico,
                                                    Region = datos$region, Comuna = datos$r_p_c), FUN = sum)

class(FT)

FT <- FT[FT$Situacion >=1 & FT$Situacion <=9, ]

Ocupados <- FT[FT$Situacion >=1 & FT$Situacion <=7, ]

Ocupados <- aggregate(Ocupados[ , c("fact_cal")], by = list( 
  Ocupados$Region, Ocupados$Comuna), FUN = sum)

names(Ocupados) = c("Region", "Comuna", "Ocupados")
# names(Ocupados)[3] = "Ocupados"

class(Ocupados)
sum(Ocupados[ , "Ocupados"])

Desocupados <- FT[FT$Situacion >=8 & FT$Situacion <=9, ]

Desocupados <- aggregate(Desocupados[ , c("fact_cal")], by = list( 
  Desocupados$Region, Desocupados$Comuna), FUN = sum)

names(Desocupados) = c("Region", "Comuna", "Desocupados")

class(Desocupados)
sum(Desocupados[ , "Desocupados"])

# FT_2018_08 <- merge(x = Ocupados, y = Desocupados, by = c("Region", "Comuna"))
FT_2018_08 <- merge(x = Ocupados, y = Desocupados, all = T)

class(FT_2018_08)
sum(na.omit(FT_2018_08[ , "Ocupados"]))
sum(na.omit(FT_2018_08[ , "Desocupados"]))

FT_2018_08$DO <- FT_2018_08$Desocupados
FT_2018_08$DO <- replace(FT_2018_08$DO, is.na(FT_2018_08$DO), 0)
FT_2018_08$Fuerza_Trabajo <- FT_2018_08$Ocupados + FT_2018_08$DO
sum(FT_2018_08[ , "Fuerza_Trabajo"])
FT_2018_08$DO <- NULL
FT_2018_08$Tasa_Desocupacion <- FT_2018_08$Desocupados / FT_2018_08$Fuerza_Trabajo
FT_2018_08$Año <- 2018
FT_2018_08$Mes <- 08


## ene-2018-07
datos <- read_dta("ene-2018-07.dta")
# View(datos)

# aggregate(fact_cal ~ cae_especifico + r_p_c, data = datos, sum)
FT <- aggregate(datos[ , c("fact_cal")], by = list( Situacion  = datos$cae_especifico,
                                                    Region = datos$region, Comuna = datos$r_p_c), FUN = sum)

class(FT)

FT <- FT[FT$Situacion >=1 & FT$Situacion <=9, ]

Ocupados <- FT[FT$Situacion >=1 & FT$Situacion <=7, ]

Ocupados <- aggregate(Ocupados[ , c("fact_cal")], by = list( 
  Ocupados$Region, Ocupados$Comuna), FUN = sum)

names(Ocupados) = c("Region", "Comuna", "Ocupados")
# names(Ocupados)[3] = "Ocupados"

class(Ocupados)
sum(Ocupados[ , "Ocupados"])

Desocupados <- FT[FT$Situacion >=8 & FT$Situacion <=9, ]

Desocupados <- aggregate(Desocupados[ , c("fact_cal")], by = list( 
  Desocupados$Region, Desocupados$Comuna), FUN = sum)

names(Desocupados) = c("Region", "Comuna", "Desocupados")

class(Desocupados)
sum(Desocupados[ , "Desocupados"])

# FT_2018_07 <- merge(x = Ocupados, y = Desocupados, by = c("Region", "Comuna"))
FT_2018_07 <- merge(x = Ocupados, y = Desocupados, all = T)

class(FT_2018_07)
sum(na.omit(FT_2018_07[ , "Ocupados"]))
sum(na.omit(FT_2018_07[ , "Desocupados"]))

FT_2018_07$DO <- FT_2018_07$Desocupados
FT_2018_07$DO <- replace(FT_2018_07$DO, is.na(FT_2018_07$DO), 0)
FT_2018_07$Fuerza_Trabajo <- FT_2018_07$Ocupados + FT_2018_07$DO
sum(FT_2018_07[ , "Fuerza_Trabajo"])
FT_2018_07$DO <- NULL
FT_2018_07$Tasa_Desocupacion <- FT_2018_07$Desocupados / FT_2018_07$Fuerza_Trabajo
FT_2018_07$Año <- 2018
FT_2018_07$Mes <- 07


## ene-2018-06
datos <- read_dta("ene-2018-06.dta")
# View(datos)

# aggregate(fact_cal ~ cae_especifico + r_p_c, data = datos, sum)
FT <- aggregate(datos[ , c("fact_cal")], by = list( Situacion  = datos$cae_especifico,
                                                    Region = datos$region, Comuna = datos$r_p_c), FUN = sum)

class(FT)

FT <- FT[FT$Situacion >=1 & FT$Situacion <=9, ]

Ocupados <- FT[FT$Situacion >=1 & FT$Situacion <=7, ]

Ocupados <- aggregate(Ocupados[ , c("fact_cal")], by = list( 
  Ocupados$Region, Ocupados$Comuna), FUN = sum)

names(Ocupados) = c("Region", "Comuna", "Ocupados")
# names(Ocupados)[3] = "Ocupados"

class(Ocupados)
sum(Ocupados[ , "Ocupados"])

Desocupados <- FT[FT$Situacion >=8 & FT$Situacion <=9, ]

Desocupados <- aggregate(Desocupados[ , c("fact_cal")], by = list( 
  Desocupados$Region, Desocupados$Comuna), FUN = sum)

names(Desocupados) = c("Region", "Comuna", "Desocupados")

class(Desocupados)
sum(Desocupados[ , "Desocupados"])

# FT_2018_06 <- merge(x = Ocupados, y = Desocupados, by = c("Region", "Comuna"))
FT_2018_06 <- merge(x = Ocupados, y = Desocupados, all = T)

class(FT_2018_06)
sum(na.omit(FT_2018_06[ , "Ocupados"]))
sum(na.omit(FT_2018_06[ , "Desocupados"]))

FT_2018_06$DO <- FT_2018_06$Desocupados
FT_2018_06$DO <- replace(FT_2018_06$DO, is.na(FT_2018_06$DO), 0)
FT_2018_06$Fuerza_Trabajo <- FT_2018_06$Ocupados + FT_2018_06$DO
sum(FT_2018_06[ , "Fuerza_Trabajo"])
FT_2018_06$DO <- NULL
FT_2018_06$Tasa_Desocupacion <- FT_2018_06$Desocupados / FT_2018_06$Fuerza_Trabajo
FT_2018_06$Año <- 2018
FT_2018_06$Mes <- 06


## ene-2018-05
datos <- read_dta("ene-2018-05.dta")
# View(datos)

# aggregate(fact_cal ~ cae_especifico + r_p_c, data = datos, sum)
FT <- aggregate(datos[ , c("fact_cal")], by = list( Situacion  = datos$cae_especifico,
                                                    Region = datos$region, Comuna = datos$r_p_c), FUN = sum)

class(FT)

FT <- FT[FT$Situacion >=1 & FT$Situacion <=9, ]

Ocupados <- FT[FT$Situacion >=1 & FT$Situacion <=7, ]

Ocupados <- aggregate(Ocupados[ , c("fact_cal")], by = list( 
  Ocupados$Region, Ocupados$Comuna), FUN = sum)

names(Ocupados) = c("Region", "Comuna", "Ocupados")
# names(Ocupados)[3] = "Ocupados"

class(Ocupados)
sum(Ocupados[ , "Ocupados"])

Desocupados <- FT[FT$Situacion >=8 & FT$Situacion <=9, ]

Desocupados <- aggregate(Desocupados[ , c("fact_cal")], by = list( 
  Desocupados$Region, Desocupados$Comuna), FUN = sum)

names(Desocupados) = c("Region", "Comuna", "Desocupados")

class(Desocupados)
sum(Desocupados[ , "Desocupados"])

# FT_2018_05 <- merge(x = Ocupados, y = Desocupados, by = c("Region", "Comuna"))
FT_2018_05 <- merge(x = Ocupados, y = Desocupados, all = T)

class(FT_2018_05)
sum(na.omit(FT_2018_05[ , "Ocupados"]))
sum(na.omit(FT_2018_05[ , "Desocupados"]))

FT_2018_05$DO <- FT_2018_05$Desocupados
FT_2018_05$DO <- replace(FT_2018_05$DO, is.na(FT_2018_05$DO), 0)
FT_2018_05$Fuerza_Trabajo <- FT_2018_05$Ocupados + FT_2018_05$DO
sum(FT_2018_05[ , "Fuerza_Trabajo"])
FT_2018_05$DO <- NULL
FT_2018_05$Tasa_Desocupacion <- FT_2018_05$Desocupados / FT_2018_05$Fuerza_Trabajo
FT_2018_05$Año <- 2018
FT_2018_05$Mes <- 05


## ene-2018-04
datos <- read_dta("ene-2018-04.dta")
# View(datos)

# aggregate(fact_cal ~ cae_especifico + r_p_c, data = datos, sum)
FT <- aggregate(datos[ , c("fact_cal")], by = list( Situacion  = datos$cae_especifico,
                                                    Region = datos$region, Comuna = datos$r_p_c), FUN = sum)

class(FT)

FT <- FT[FT$Situacion >=1 & FT$Situacion <=9, ]

Ocupados <- FT[FT$Situacion >=1 & FT$Situacion <=7, ]

Ocupados <- aggregate(Ocupados[ , c("fact_cal")], by = list( 
  Ocupados$Region, Ocupados$Comuna), FUN = sum)

names(Ocupados) = c("Region", "Comuna", "Ocupados")
# names(Ocupados)[3] = "Ocupados"

class(Ocupados)
sum(Ocupados[ , "Ocupados"])

Desocupados <- FT[FT$Situacion >=8 & FT$Situacion <=9, ]

Desocupados <- aggregate(Desocupados[ , c("fact_cal")], by = list( 
  Desocupados$Region, Desocupados$Comuna), FUN = sum)

names(Desocupados) = c("Region", "Comuna", "Desocupados")

class(Desocupados)
sum(Desocupados[ , "Desocupados"])

# FT_2018_04 <- merge(x = Ocupados, y = Desocupados, by = c("Region", "Comuna"))
FT_2018_04 <- merge(x = Ocupados, y = Desocupados, all = T)

class(FT_2018_04)
sum(na.omit(FT_2018_04[ , "Ocupados"]))
sum(na.omit(FT_2018_04[ , "Desocupados"]))

FT_2018_04$DO <- FT_2018_04$Desocupados
FT_2018_04$DO <- replace(FT_2018_04$DO, is.na(FT_2018_04$DO), 0)
FT_2018_04$Fuerza_Trabajo <- FT_2018_04$Ocupados + FT_2018_04$DO
sum(FT_2018_04[ , "Fuerza_Trabajo"])
FT_2018_04$DO <- NULL
FT_2018_04$Tasa_Desocupacion <- FT_2018_04$Desocupados / FT_2018_04$Fuerza_Trabajo
FT_2018_04$Año <- 2018
FT_2018_04$Mes <- 04


## ene-2018-03
datos <- read_dta("ene-2018-03.dta")
# View(datos)

# aggregate(fact_cal ~ cae_especifico + r_p_c, data = datos, sum)
FT <- aggregate(datos[ , c("fact_cal")], by = list( Situacion  = datos$cae_especifico,
                                                    Region = datos$region, Comuna = datos$r_p_c), FUN = sum)

class(FT)

FT <- FT[FT$Situacion >=1 & FT$Situacion <=9, ]

Ocupados <- FT[FT$Situacion >=1 & FT$Situacion <=7, ]

Ocupados <- aggregate(Ocupados[ , c("fact_cal")], by = list( 
  Ocupados$Region, Ocupados$Comuna), FUN = sum)

names(Ocupados) = c("Region", "Comuna", "Ocupados")
# names(Ocupados)[3] = "Ocupados"

class(Ocupados)
sum(Ocupados[ , "Ocupados"])

Desocupados <- FT[FT$Situacion >=8 & FT$Situacion <=9, ]

Desocupados <- aggregate(Desocupados[ , c("fact_cal")], by = list( 
  Desocupados$Region, Desocupados$Comuna), FUN = sum)

names(Desocupados) = c("Region", "Comuna", "Desocupados")

class(Desocupados)
sum(Desocupados[ , "Desocupados"])

# FT_2018_03 <- merge(x = Ocupados, y = Desocupados, by = c("Region", "Comuna"))
FT_2018_03 <- merge(x = Ocupados, y = Desocupados, all = T)

class(FT_2018_03)
sum(na.omit(FT_2018_03[ , "Ocupados"]))
sum(na.omit(FT_2018_03[ , "Desocupados"]))

FT_2018_03$DO <- FT_2018_03$Desocupados
FT_2018_03$DO <- replace(FT_2018_03$DO, is.na(FT_2018_03$DO), 0)
FT_2018_03$Fuerza_Trabajo <- FT_2018_03$Ocupados + FT_2018_03$DO
sum(FT_2018_03[ , "Fuerza_Trabajo"])
FT_2018_03$DO <- NULL
FT_2018_03$Tasa_Desocupacion <- FT_2018_03$Desocupados / FT_2018_03$Fuerza_Trabajo
FT_2018_03$Año <- 2018
FT_2018_03$Mes <- 03


## ene-2018-02
datos <- read_dta("ene-2018-02.dta")
# View(datos)

# aggregate(fact_cal ~ cae_especifico + r_p_c, data = datos, sum)
FT <- aggregate(datos[ , c("fact_cal")], by = list( Situacion  = datos$cae_especifico,
                                                    Region = datos$region, Comuna = datos$r_p_c), FUN = sum)

class(FT)

FT <- FT[FT$Situacion >=1 & FT$Situacion <=9, ]

Ocupados <- FT[FT$Situacion >=1 & FT$Situacion <=7, ]

Ocupados <- aggregate(Ocupados[ , c("fact_cal")], by = list( 
  Ocupados$Region, Ocupados$Comuna), FUN = sum)

names(Ocupados) = c("Region", "Comuna", "Ocupados")
# names(Ocupados)[3] = "Ocupados"

class(Ocupados)
sum(Ocupados[ , "Ocupados"])

Desocupados <- FT[FT$Situacion >=8 & FT$Situacion <=9, ]

Desocupados <- aggregate(Desocupados[ , c("fact_cal")], by = list( 
  Desocupados$Region, Desocupados$Comuna), FUN = sum)

names(Desocupados) = c("Region", "Comuna", "Desocupados")

class(Desocupados)
sum(Desocupados[ , "Desocupados"])

# FT_2018_02 <- merge(x = Ocupados, y = Desocupados, by = c("Region", "Comuna"))
FT_2018_02 <- merge(x = Ocupados, y = Desocupados, all = T)

class(FT_2018_02)
sum(na.omit(FT_2018_02[ , "Ocupados"]))
sum(na.omit(FT_2018_02[ , "Desocupados"]))

FT_2018_02$DO <- FT_2018_02$Desocupados
FT_2018_02$DO <- replace(FT_2018_02$DO, is.na(FT_2018_02$DO), 0)
FT_2018_02$Fuerza_Trabajo <- FT_2018_02$Ocupados + FT_2018_02$DO
sum(FT_2018_02[ , "Fuerza_Trabajo"])
FT_2018_02$DO <- NULL
FT_2018_02$Tasa_Desocupacion <- FT_2018_02$Desocupados / FT_2018_02$Fuerza_Trabajo
FT_2018_02$Año <- 2018
FT_2018_02$Mes <- 02


## ene-2018-01
datos <- read_dta("ene-2018-01.dta")
# View(datos)

# aggregate(fact_cal ~ cae_especifico + r_p_c, data = datos, sum)
FT <- aggregate(datos[ , c("fact_cal")], by = list( Situacion  = datos$cae_especifico,
                                                    Region = datos$region, Comuna = datos$r_p_c), FUN = sum)

class(FT)

FT <- FT[FT$Situacion >=1 & FT$Situacion <=9, ]

Ocupados <- FT[FT$Situacion >=1 & FT$Situacion <=7, ]

Ocupados <- aggregate(Ocupados[ , c("fact_cal")], by = list( 
  Ocupados$Region, Ocupados$Comuna), FUN = sum)

names(Ocupados) = c("Region", "Comuna", "Ocupados")
# names(Ocupados)[3] = "Ocupados"

class(Ocupados)
sum(Ocupados[ , "Ocupados"])

Desocupados <- FT[FT$Situacion >=8 & FT$Situacion <=9, ]

Desocupados <- aggregate(Desocupados[ , c("fact_cal")], by = list( 
  Desocupados$Region, Desocupados$Comuna), FUN = sum)

names(Desocupados) = c("Region", "Comuna", "Desocupados")

class(Desocupados)
sum(Desocupados[ , "Desocupados"])

# FT_2018_01 <- merge(x = Ocupados, y = Desocupados, by = c("Region", "Comuna"))
FT_2018_01 <- merge(x = Ocupados, y = Desocupados, all = T)

class(FT_2018_01)
sum(na.omit(FT_2018_01[ , "Ocupados"]))
sum(na.omit(FT_2018_01[ , "Desocupados"]))

FT_2018_01$DO <- FT_2018_01$Desocupados
FT_2018_01$DO <- replace(FT_2018_01$DO, is.na(FT_2018_01$DO), 0)
FT_2018_01$Fuerza_Trabajo <- FT_2018_01$Ocupados + FT_2018_01$DO
sum(FT_2018_01[ , "Fuerza_Trabajo"])
FT_2018_01$DO <- NULL
FT_2018_01$Tasa_Desocupacion <- FT_2018_01$Desocupados / FT_2018_01$Fuerza_Trabajo
FT_2018_01$Año <- 2018
FT_2018_01$Mes <- 01


## ene-2017-12
datos <- read_dta("ene-2017-12.dta")
# View(datos)

# aggregate(fact_cal ~ cae_especifico + r_p_c, data = datos, sum)
FT <- aggregate(datos[ , c("fact_cal")], by = list( Situacion  = datos$cae_especifico,
                                                    Region = datos$region, Comuna = datos$r_p_c), FUN = sum)

class(FT)

FT <- FT[FT$Situacion >=1 & FT$Situacion <=9, ]

Ocupados <- FT[FT$Situacion >=1 & FT$Situacion <=7, ]

Ocupados <- aggregate(Ocupados[ , c("fact_cal")], by = list( 
  Ocupados$Region, Ocupados$Comuna), FUN = sum)

names(Ocupados) = c("Region", "Comuna", "Ocupados")
# names(Ocupados)[3] = "Ocupados"

class(Ocupados)
sum(Ocupados[ , "Ocupados"])

Desocupados <- FT[FT$Situacion >=8 & FT$Situacion <=9, ]

Desocupados <- aggregate(Desocupados[ , c("fact_cal")], by = list( 
  Desocupados$Region, Desocupados$Comuna), FUN = sum)

names(Desocupados) = c("Region", "Comuna", "Desocupados")

class(Desocupados)
sum(Desocupados[ , "Desocupados"])

# FT_2017_12 <- merge(x = Ocupados, y = Desocupados, by = c("Region", "Comuna"))
FT_2017_12 <- merge(x = Ocupados, y = Desocupados, all = T)

class(FT_2017_12)
sum(na.omit(FT_2017_12[ , "Ocupados"]))
sum(na.omit(FT_2017_12[ , "Desocupados"]))

FT_2017_12$DO <- FT_2017_12$Desocupados
FT_2017_12$DO <- replace(FT_2017_12$DO, is.na(FT_2017_12$DO), 0)
FT_2017_12$Fuerza_Trabajo <- FT_2017_12$Ocupados + FT_2017_12$DO
sum(FT_2017_12[ , "Fuerza_Trabajo"])
FT_2017_12$DO <- NULL
FT_2017_12$Tasa_Desocupacion <- FT_2017_12$Desocupados / FT_2017_12$Fuerza_Trabajo
FT_2017_12$Año <- 2017
FT_2017_12$Mes <- 12


## ene-2017-11
datos <- read_dta("ene-2017-11.dta")
# View(datos)

# aggregate(fact_cal ~ cae_especifico + r_p_c, data = datos, sum)
FT <- aggregate(datos[ , c("fact_cal")], by = list( Situacion  = datos$cae_especifico,
                                                    Region = datos$region, Comuna = datos$r_p_c), FUN = sum)

class(FT)

FT <- FT[FT$Situacion >=1 & FT$Situacion <=9, ]

Ocupados <- FT[FT$Situacion >=1 & FT$Situacion <=7, ]

Ocupados <- aggregate(Ocupados[ , c("fact_cal")], by = list( 
  Ocupados$Region, Ocupados$Comuna), FUN = sum)

names(Ocupados) = c("Region", "Comuna", "Ocupados")
# names(Ocupados)[3] = "Ocupados"

class(Ocupados)
sum(Ocupados[ , "Ocupados"])

Desocupados <- FT[FT$Situacion >=8 & FT$Situacion <=9, ]

Desocupados <- aggregate(Desocupados[ , c("fact_cal")], by = list( 
  Desocupados$Region, Desocupados$Comuna), FUN = sum)

names(Desocupados) = c("Region", "Comuna", "Desocupados")

class(Desocupados)
sum(Desocupados[ , "Desocupados"])

# FT_2017_11 <- merge(x = Ocupados, y = Desocupados, by = c("Region", "Comuna"))
FT_2017_11 <- merge(x = Ocupados, y = Desocupados, all = T)

class(FT_2017_11)
sum(na.omit(FT_2017_11[ , "Ocupados"]))
sum(na.omit(FT_2017_11[ , "Desocupados"]))

FT_2017_11$DO <- FT_2017_11$Desocupados
FT_2017_11$DO <- replace(FT_2017_11$DO, is.na(FT_2017_11$DO), 0)
FT_2017_11$Fuerza_Trabajo <- FT_2017_11$Ocupados + FT_2017_11$DO
sum(FT_2017_11[ , "Fuerza_Trabajo"])
FT_2017_11$DO <- NULL
FT_2017_11$Tasa_Desocupacion <- FT_2017_11$Desocupados / FT_2017_11$Fuerza_Trabajo
FT_2017_11$Año <- 2017
FT_2017_11$Mes <- 11


## ene-2017-10
datos <- read_dta("ene-2017-10.dta")
# View(datos)

# aggregate(fact_cal ~ cae_especifico + r_p_c, data = datos, sum)
FT <- aggregate(datos[ , c("fact_cal")], by = list( Situacion  = datos$cae_especifico,
                                                    Region = datos$region, Comuna = datos$r_p_c), FUN = sum)

class(FT)

FT <- FT[FT$Situacion >=1 & FT$Situacion <=9, ]

Ocupados <- FT[FT$Situacion >=1 & FT$Situacion <=7, ]

Ocupados <- aggregate(Ocupados[ , c("fact_cal")], by = list( 
  Ocupados$Region, Ocupados$Comuna), FUN = sum)

names(Ocupados) = c("Region", "Comuna", "Ocupados")
# names(Ocupados)[3] = "Ocupados"

class(Ocupados)
sum(Ocupados[ , "Ocupados"])

Desocupados <- FT[FT$Situacion >=8 & FT$Situacion <=9, ]

Desocupados <- aggregate(Desocupados[ , c("fact_cal")], by = list( 
  Desocupados$Region, Desocupados$Comuna), FUN = sum)

names(Desocupados) = c("Region", "Comuna", "Desocupados")

class(Desocupados)
sum(Desocupados[ , "Desocupados"])

# FT_2017_10 <- merge(x = Ocupados, y = Desocupados, by = c("Region", "Comuna"))
FT_2017_10 <- merge(x = Ocupados, y = Desocupados, all = T)

class(FT_2017_10)
sum(na.omit(FT_2017_10[ , "Ocupados"]))
sum(na.omit(FT_2017_10[ , "Desocupados"]))

FT_2017_10$DO <- FT_2017_10$Desocupados
FT_2017_10$DO <- replace(FT_2017_10$DO, is.na(FT_2017_10$DO), 0)
FT_2017_10$Fuerza_Trabajo <- FT_2017_10$Ocupados + FT_2017_10$DO
sum(FT_2017_10[ , "Fuerza_Trabajo"])
FT_2017_10$DO <- NULL
FT_2017_10$Tasa_Desocupacion <- FT_2017_10$Desocupados / FT_2017_10$Fuerza_Trabajo
FT_2017_10$Año <- 2017
FT_2017_10$Mes <- 10


## ene-2017-09
datos <- read_dta("ene-2017-09.dta")
# View(datos)

# aggregate(fact_cal ~ cae_especifico + r_p_c, data = datos, sum)
FT <- aggregate(datos[ , c("fact_cal")], by = list( Situacion  = datos$cae_especifico,
                                                    Region = datos$region, Comuna = datos$r_p_c), FUN = sum)

class(FT)

FT <- FT[FT$Situacion >=1 & FT$Situacion <=9, ]

Ocupados <- FT[FT$Situacion >=1 & FT$Situacion <=7, ]

Ocupados <- aggregate(Ocupados[ , c("fact_cal")], by = list( 
  Ocupados$Region, Ocupados$Comuna), FUN = sum)

names(Ocupados) = c("Region", "Comuna", "Ocupados")
# names(Ocupados)[3] = "Ocupados"

class(Ocupados)
sum(Ocupados[ , "Ocupados"])

Desocupados <- FT[FT$Situacion >=8 & FT$Situacion <=9, ]

Desocupados <- aggregate(Desocupados[ , c("fact_cal")], by = list( 
  Desocupados$Region, Desocupados$Comuna), FUN = sum)

names(Desocupados) = c("Region", "Comuna", "Desocupados")

class(Desocupados)
sum(Desocupados[ , "Desocupados"])

# FT_2017_09 <- merge(x = Ocupados, y = Desocupados, by = c("Region", "Comuna"))
FT_2017_09 <- merge(x = Ocupados, y = Desocupados, all = T)

class(FT_2017_09)
sum(na.omit(FT_2017_09[ , "Ocupados"]))
sum(na.omit(FT_2017_09[ , "Desocupados"]))

FT_2017_09$DO <- FT_2017_09$Desocupados
FT_2017_09$DO <- replace(FT_2017_09$DO, is.na(FT_2017_09$DO), 0)
FT_2017_09$Fuerza_Trabajo <- FT_2017_09$Ocupados + FT_2017_09$DO
sum(FT_2017_09[ , "Fuerza_Trabajo"])
FT_2017_09$DO <- NULL
FT_2017_09$Tasa_Desocupacion <- FT_2017_09$Desocupados / FT_2017_09$Fuerza_Trabajo
FT_2017_09$Año <- 2017
FT_2017_09$Mes <- 09


## ene-2017-08
datos <- read_dta("ene-2017-08.dta")
# View(datos)

# aggregate(fact_cal ~ cae_especifico + r_p_c, data = datos, sum)
FT <- aggregate(datos[ , c("fact_cal")], by = list( Situacion  = datos$cae_especifico,
                                                    Region = datos$region, Comuna = datos$r_p_c), FUN = sum)

class(FT)

FT <- FT[FT$Situacion >=1 & FT$Situacion <=9, ]

Ocupados <- FT[FT$Situacion >=1 & FT$Situacion <=7, ]

Ocupados <- aggregate(Ocupados[ , c("fact_cal")], by = list( 
  Ocupados$Region, Ocupados$Comuna), FUN = sum)

names(Ocupados) = c("Region", "Comuna", "Ocupados")
# names(Ocupados)[3] = "Ocupados"

class(Ocupados)
sum(Ocupados[ , "Ocupados"])

Desocupados <- FT[FT$Situacion >=8 & FT$Situacion <=9, ]

Desocupados <- aggregate(Desocupados[ , c("fact_cal")], by = list( 
  Desocupados$Region, Desocupados$Comuna), FUN = sum)

names(Desocupados) = c("Region", "Comuna", "Desocupados")

class(Desocupados)
sum(Desocupados[ , "Desocupados"])

# FT_2017_08 <- merge(x = Ocupados, y = Desocupados, by = c("Region", "Comuna"))
FT_2017_08 <- merge(x = Ocupados, y = Desocupados, all = T)

class(FT_2017_08)
sum(na.omit(FT_2017_08[ , "Ocupados"]))
sum(na.omit(FT_2017_08[ , "Desocupados"]))

FT_2017_08$DO <- FT_2017_08$Desocupados
FT_2017_08$DO <- replace(FT_2017_08$DO, is.na(FT_2017_08$DO), 0)
FT_2017_08$Fuerza_Trabajo <- FT_2017_08$Ocupados + FT_2017_08$DO
sum(FT_2017_08[ , "Fuerza_Trabajo"])
FT_2017_08$DO <- NULL
FT_2017_08$Tasa_Desocupacion <- FT_2017_08$Desocupados / FT_2017_08$Fuerza_Trabajo
FT_2017_08$Año <- 2017
FT_2017_08$Mes <- 08


## ene-2017-07
datos <- read_dta("ene-2017-07.dta")
# View(datos)

# aggregate(fact_cal ~ cae_especifico + r_p_c, data = datos, sum)
FT <- aggregate(datos[ , c("fact_cal")], by = list( Situacion  = datos$cae_especifico,
                                                    Region = datos$region, Comuna = datos$r_p_c), FUN = sum)

class(FT)

FT <- FT[FT$Situacion >=1 & FT$Situacion <=9, ]

Ocupados <- FT[FT$Situacion >=1 & FT$Situacion <=7, ]

Ocupados <- aggregate(Ocupados[ , c("fact_cal")], by = list( 
  Ocupados$Region, Ocupados$Comuna), FUN = sum)

names(Ocupados) = c("Region", "Comuna", "Ocupados")
# names(Ocupados)[3] = "Ocupados"

class(Ocupados)
sum(Ocupados[ , "Ocupados"])

Desocupados <- FT[FT$Situacion >=8 & FT$Situacion <=9, ]

Desocupados <- aggregate(Desocupados[ , c("fact_cal")], by = list( 
  Desocupados$Region, Desocupados$Comuna), FUN = sum)

names(Desocupados) = c("Region", "Comuna", "Desocupados")

class(Desocupados)
sum(Desocupados[ , "Desocupados"])

# FT_2017_07 <- merge(x = Ocupados, y = Desocupados, by = c("Region", "Comuna"))
FT_2017_07 <- merge(x = Ocupados, y = Desocupados, all = T)

class(FT_2017_07)
sum(na.omit(FT_2017_07[ , "Ocupados"]))
sum(na.omit(FT_2017_07[ , "Desocupados"]))

FT_2017_07$DO <- FT_2017_07$Desocupados
FT_2017_07$DO <- replace(FT_2017_07$DO, is.na(FT_2017_07$DO), 0)
FT_2017_07$Fuerza_Trabajo <- FT_2017_07$Ocupados + FT_2017_07$DO
sum(FT_2017_07[ , "Fuerza_Trabajo"])
FT_2017_07$DO <- NULL
FT_2017_07$Tasa_Desocupacion <- FT_2017_07$Desocupados / FT_2017_07$Fuerza_Trabajo
FT_2017_07$Año <- 2017
FT_2017_07$Mes <- 07


## ene-2017-06
datos <- read_dta("ene-2017-06.dta")
# View(datos)

# aggregate(fact_cal ~ cae_especifico + r_p_c, data = datos, sum)
FT <- aggregate(datos[ , c("fact_cal")], by = list( Situacion  = datos$cae_especifico,
                                                    Region = datos$region, Comuna = datos$r_p_c), FUN = sum)

class(FT)

FT <- FT[FT$Situacion >=1 & FT$Situacion <=9, ]

Ocupados <- FT[FT$Situacion >=1 & FT$Situacion <=7, ]

Ocupados <- aggregate(Ocupados[ , c("fact_cal")], by = list( 
  Ocupados$Region, Ocupados$Comuna), FUN = sum)

names(Ocupados) = c("Region", "Comuna", "Ocupados")
# names(Ocupados)[3] = "Ocupados"

class(Ocupados)
sum(Ocupados[ , "Ocupados"])

Desocupados <- FT[FT$Situacion >=8 & FT$Situacion <=9, ]

Desocupados <- aggregate(Desocupados[ , c("fact_cal")], by = list( 
  Desocupados$Region, Desocupados$Comuna), FUN = sum)

names(Desocupados) = c("Region", "Comuna", "Desocupados")

class(Desocupados)
sum(Desocupados[ , "Desocupados"])

# FT_2017_06 <- merge(x = Ocupados, y = Desocupados, by = c("Region", "Comuna"))
FT_2017_06 <- merge(x = Ocupados, y = Desocupados, all = T)

class(FT_2017_06)
sum(na.omit(FT_2017_06[ , "Ocupados"]))
sum(na.omit(FT_2017_06[ , "Desocupados"]))

FT_2017_06$DO <- FT_2017_06$Desocupados
FT_2017_06$DO <- replace(FT_2017_06$DO, is.na(FT_2017_06$DO), 0)
FT_2017_06$Fuerza_Trabajo <- FT_2017_06$Ocupados + FT_2017_06$DO
sum(FT_2017_06[ , "Fuerza_Trabajo"])
FT_2017_06$DO <- NULL
FT_2017_06$Tasa_Desocupacion <- FT_2017_06$Desocupados / FT_2017_06$Fuerza_Trabajo
FT_2017_06$Año <- 2017
FT_2017_06$Mes <- 06


## ene-2017-05
datos <- read_dta("ene-2017-05.dta")
# View(datos)

# aggregate(fact_cal ~ cae_especifico + r_p_c, data = datos, sum)
FT <- aggregate(datos[ , c("fact_cal")], by = list( Situacion  = datos$cae_especifico,
                                                    Region = datos$region, Comuna = datos$r_p_c), FUN = sum)

class(FT)

FT <- FT[FT$Situacion >=1 & FT$Situacion <=9, ]

Ocupados <- FT[FT$Situacion >=1 & FT$Situacion <=7, ]

Ocupados <- aggregate(Ocupados[ , c("fact_cal")], by = list( 
  Ocupados$Region, Ocupados$Comuna), FUN = sum)

names(Ocupados) = c("Region", "Comuna", "Ocupados")
# names(Ocupados)[3] = "Ocupados"

class(Ocupados)
sum(Ocupados[ , "Ocupados"])

Desocupados <- FT[FT$Situacion >=8 & FT$Situacion <=9, ]

Desocupados <- aggregate(Desocupados[ , c("fact_cal")], by = list( 
  Desocupados$Region, Desocupados$Comuna), FUN = sum)

names(Desocupados) = c("Region", "Comuna", "Desocupados")

class(Desocupados)
sum(Desocupados[ , "Desocupados"])

# FT_2017_05 <- merge(x = Ocupados, y = Desocupados, by = c("Region", "Comuna"))
FT_2017_05 <- merge(x = Ocupados, y = Desocupados, all = T)

class(FT_2017_05)
sum(na.omit(FT_2017_05[ , "Ocupados"]))
sum(na.omit(FT_2017_05[ , "Desocupados"]))

FT_2017_05$DO <- FT_2017_05$Desocupados
FT_2017_05$DO <- replace(FT_2017_05$DO, is.na(FT_2017_05$DO), 0)
FT_2017_05$Fuerza_Trabajo <- FT_2017_05$Ocupados + FT_2017_05$DO
sum(FT_2017_05[ , "Fuerza_Trabajo"])
FT_2017_05$DO <- NULL
FT_2017_05$Tasa_Desocupacion <- FT_2017_05$Desocupados / FT_2017_05$Fuerza_Trabajo
FT_2017_05$Año <- 2017
FT_2017_05$Mes <- 05


## ene-2017-04
datos <- read_dta("ene-2017-04.dta")
# View(datos)

# aggregate(fact_cal ~ cae_especifico + r_p_c, data = datos, sum)
FT <- aggregate(datos[ , c("fact_cal")], by = list( Situacion  = datos$cae_especifico,
                                                    Region = datos$region, Comuna = datos$r_p_c), FUN = sum)

class(FT)

FT <- FT[FT$Situacion >=1 & FT$Situacion <=9, ]

Ocupados <- FT[FT$Situacion >=1 & FT$Situacion <=7, ]

Ocupados <- aggregate(Ocupados[ , c("fact_cal")], by = list( 
  Ocupados$Region, Ocupados$Comuna), FUN = sum)

names(Ocupados) = c("Region", "Comuna", "Ocupados")
# names(Ocupados)[3] = "Ocupados"

class(Ocupados)
sum(Ocupados[ , "Ocupados"])

Desocupados <- FT[FT$Situacion >=8 & FT$Situacion <=9, ]

Desocupados <- aggregate(Desocupados[ , c("fact_cal")], by = list( 
  Desocupados$Region, Desocupados$Comuna), FUN = sum)

names(Desocupados) = c("Region", "Comuna", "Desocupados")

class(Desocupados)
sum(Desocupados[ , "Desocupados"])

# FT_2017_04 <- merge(x = Ocupados, y = Desocupados, by = c("Region", "Comuna"))
FT_2017_04 <- merge(x = Ocupados, y = Desocupados, all = T)

class(FT_2017_04)
sum(na.omit(FT_2017_04[ , "Ocupados"]))
sum(na.omit(FT_2017_04[ , "Desocupados"]))

FT_2017_04$DO <- FT_2017_04$Desocupados
FT_2017_04$DO <- replace(FT_2017_04$DO, is.na(FT_2017_04$DO), 0)
FT_2017_04$Fuerza_Trabajo <- FT_2017_04$Ocupados + FT_2017_04$DO
sum(FT_2017_04[ , "Fuerza_Trabajo"])
FT_2017_04$DO <- NULL
FT_2017_04$Tasa_Desocupacion <- FT_2017_04$Desocupados / FT_2017_04$Fuerza_Trabajo
FT_2017_04$Año <- 2017
FT_2017_04$Mes <- 04


## ene-2017-03
datos <- read_dta("ene-2017-03.dta")
# View(datos)

# aggregate(fact_cal ~ cae_especifico + r_p_c, data = datos, sum)
FT <- aggregate(datos[ , c("fact_cal")], by = list( Situacion  = datos$cae_especifico,
                                                    Region = datos$region, Comuna = datos$r_p_c), FUN = sum)

class(FT)

FT <- FT[FT$Situacion >=1 & FT$Situacion <=9, ]

Ocupados <- FT[FT$Situacion >=1 & FT$Situacion <=7, ]

Ocupados <- aggregate(Ocupados[ , c("fact_cal")], by = list( 
  Ocupados$Region, Ocupados$Comuna), FUN = sum)

names(Ocupados) = c("Region", "Comuna", "Ocupados")
# names(Ocupados)[3] = "Ocupados"

class(Ocupados)
sum(Ocupados[ , "Ocupados"])

Desocupados <- FT[FT$Situacion >=8 & FT$Situacion <=9, ]

Desocupados <- aggregate(Desocupados[ , c("fact_cal")], by = list( 
  Desocupados$Region, Desocupados$Comuna), FUN = sum)

names(Desocupados) = c("Region", "Comuna", "Desocupados")

class(Desocupados)
sum(Desocupados[ , "Desocupados"])

# FT_2017_03 <- merge(x = Ocupados, y = Desocupados, by = c("Region", "Comuna"))
FT_2017_03 <- merge(x = Ocupados, y = Desocupados, all = T)

class(FT_2017_03)
sum(na.omit(FT_2017_03[ , "Ocupados"]))
sum(na.omit(FT_2017_03[ , "Desocupados"]))

FT_2017_03$DO <- FT_2017_03$Desocupados
FT_2017_03$DO <- replace(FT_2017_03$DO, is.na(FT_2017_03$DO), 0)
FT_2017_03$Fuerza_Trabajo <- FT_2017_03$Ocupados + FT_2017_03$DO
sum(FT_2017_03[ , "Fuerza_Trabajo"])
FT_2017_03$DO <- NULL
FT_2017_03$Tasa_Desocupacion <- FT_2017_03$Desocupados / FT_2017_03$Fuerza_Trabajo
FT_2017_03$Año <- 2017
FT_2017_03$Mes <- 03


## ene-2017-02
datos <- read_dta("ene-2017-02.dta")
# View(datos)

# aggregate(fact_cal ~ cae_especifico + r_p_c, data = datos, sum)
FT <- aggregate(datos[ , c("fact_cal")], by = list( Situacion  = datos$cae_especifico,
                                                    Region = datos$region, Comuna = datos$r_p_c), FUN = sum)

class(FT)

FT <- FT[FT$Situacion >=1 & FT$Situacion <=9, ]

Ocupados <- FT[FT$Situacion >=1 & FT$Situacion <=7, ]

Ocupados <- aggregate(Ocupados[ , c("fact_cal")], by = list( 
  Ocupados$Region, Ocupados$Comuna), FUN = sum)

names(Ocupados) = c("Region", "Comuna", "Ocupados")
# names(Ocupados)[3] = "Ocupados"

class(Ocupados)
sum(Ocupados[ , "Ocupados"])

Desocupados <- FT[FT$Situacion >=8 & FT$Situacion <=9, ]

Desocupados <- aggregate(Desocupados[ , c("fact_cal")], by = list( 
  Desocupados$Region, Desocupados$Comuna), FUN = sum)

names(Desocupados) = c("Region", "Comuna", "Desocupados")

class(Desocupados)
sum(Desocupados[ , "Desocupados"])

# FT_2017_02 <- merge(x = Ocupados, y = Desocupados, by = c("Region", "Comuna"))
FT_2017_02 <- merge(x = Ocupados, y = Desocupados, all = T)

class(FT_2017_02)
sum(na.omit(FT_2017_02[ , "Ocupados"]))
sum(na.omit(FT_2017_02[ , "Desocupados"]))

FT_2017_02$DO <- FT_2017_02$Desocupados
FT_2017_02$DO <- replace(FT_2017_02$DO, is.na(FT_2017_02$DO), 0)
FT_2017_02$Fuerza_Trabajo <- FT_2017_02$Ocupados + FT_2017_02$DO
sum(FT_2017_02[ , "Fuerza_Trabajo"])
FT_2017_02$DO <- NULL
FT_2017_02$Tasa_Desocupacion <- FT_2017_02$Desocupados / FT_2017_02$Fuerza_Trabajo
FT_2017_02$Año <- 2017
FT_2017_02$Mes <- 02


## ene-2017-01
datos <- read_dta("ene-2017-01.dta")
# View(datos)

# aggregate(fact_cal ~ cae_especifico + r_p_c, data = datos, sum)
FT <- aggregate(datos[ , c("fact_cal")], by = list( Situacion  = datos$cae_especifico,
                                                    Region = datos$region, Comuna = datos$r_p_c), FUN = sum)

class(FT)

FT <- FT[FT$Situacion >=1 & FT$Situacion <=9, ]

Ocupados <- FT[FT$Situacion >=1 & FT$Situacion <=7, ]

Ocupados <- aggregate(Ocupados[ , c("fact_cal")], by = list( 
  Ocupados$Region, Ocupados$Comuna), FUN = sum)

names(Ocupados) = c("Region", "Comuna", "Ocupados")
# names(Ocupados)[3] = "Ocupados"

class(Ocupados)
sum(Ocupados[ , "Ocupados"])

Desocupados <- FT[FT$Situacion >=8 & FT$Situacion <=9, ]

Desocupados <- aggregate(Desocupados[ , c("fact_cal")], by = list( 
  Desocupados$Region, Desocupados$Comuna), FUN = sum)

names(Desocupados) = c("Region", "Comuna", "Desocupados")

class(Desocupados)
sum(Desocupados[ , "Desocupados"])

# FT_2017_01 <- merge(x = Ocupados, y = Desocupados, by = c("Region", "Comuna"))
FT_2017_01 <- merge(x = Ocupados, y = Desocupados, all = T)

class(FT_2017_01)
sum(na.omit(FT_2017_01[ , "Ocupados"]))
sum(na.omit(FT_2017_01[ , "Desocupados"]))

FT_2017_01$DO <- FT_2017_01$Desocupados
FT_2017_01$DO <- replace(FT_2017_01$DO, is.na(FT_2017_01$DO), 0)
FT_2017_01$Fuerza_Trabajo <- FT_2017_01$Ocupados + FT_2017_01$DO
sum(FT_2017_01[ , "Fuerza_Trabajo"])
FT_2017_01$DO <- NULL
FT_2017_01$Tasa_Desocupacion <- FT_2017_01$Desocupados / FT_2017_01$Fuerza_Trabajo
FT_2017_01$Año <- 2017
FT_2017_01$Mes <- 01

