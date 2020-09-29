#15/09/2020
#Raúl Mosco
#Escolaridad: Annlytics



setwd("C:/Users/CORU/Desktop/CORU/Analytics")


#paquetes<-c('RPostgreSQL','postGIStools','tidyverse','lubridate','tidyquant','rgdal','foreign')
#sapply(paquetes,require,character.only=T,quietly=T)
library(readr)

### Datos 


enigh <- read.csv("poblacion.csv")
enigh1<- read.csv("viviendas.csv")

tot_enigh <- left_join(enigh,enigh1)

####### ENIGH
tot_enigh$ï..folioviv <- substr(tot_enigh$ï..folioviv,1,2)

unique(tot_enigh$ï..folioviv)
table(tot_enigh1$ï..folioviv)

tot_enigh$residencia[which(is.na(tot_enigh$residencia))]<-0

unique(tot_enigh$residencia)
table(tot_enigh1$residencia)

tot_enigh1 <- tot_enigh %>%
  select(ï..folioviv,sexo,factor,edad, nivelaprob,edo_conyug,residencia)

memory.limit(9999999999)

tot_enigh1$residencia <- case_when(tot_enigh1$residencia %in% c(1) ~ 'Aguascalientes',
                                   tot_enigh1$residencia  %in% c(2) ~ 'Baja California',
                                   tot_enigh1$residencia  %in% c(3) ~ 'Baja California Sur',
                                   tot_enigh1$residencia  %in% c(4) ~ 'Campeche',
                                   tot_enigh1$residencia  %in% c(5) ~ 'Coahuila de Zaragoza',
                                   tot_enigh1$residencia  %in% c(6) ~ 'Colima',
                                    
                                   tot_enigh1$residencia  %in% c(7) ~ 'Chiapas',
                                   tot_enigh1$residencia  %in% c(8) ~ 'Chihuahua',
                                   tot_enigh1$residencia  %in% c(9) ~ 'CDMX',
                                   tot_enigh1$residencia  %in% c(10) ~ 'Durango',
                                   tot_enigh1$residencia  %in% c(11) ~ 'Guanajuato',
                                   tot_enigh1$residencia  %in% c(12) ~ 'Guerrero',
                                    
                                   tot_enigh1$residencia  %in% c(13) ~ 'Hidalgo',
                                   tot_enigh1$residencia  %in% c(14) ~ 'Jalisco',
                                   tot_enigh1$residencia  %in% c(15) ~ 'Mexico',
                                   tot_enigh1$residencia  %in% c(16) ~ 'Michoacan',
                                   tot_enigh1$residencia  %in% c(17) ~ 'Morelos',
                                   tot_enigh1$residencia  %in% c(18) ~ 'Nayarit',
                                    
                                   tot_enigh1$residencia  %in% c(19) ~ 'Nuevo Leon',
                                   tot_enigh1$residencia  %in% c(20) ~ 'Oaxaca',
                                   tot_enigh1$residencia  %in% c(21) ~ 'Puebla',
                                   tot_enigh1$residencia  %in% c(22) ~ 'Queretaro',
                                   tot_enigh1$residencia  %in% c(23) ~ 'Quintana Roo',
                                   tot_enigh1$residencia  %in% c(24) ~ 'San Luis Potosi',
                                    
                                   tot_enigh1$residencia  %in% c(25) ~ 'Sinaloa',
                                   tot_enigh1$residencia  %in% c(26) ~ 'Sonora',
                                   tot_enigh1$residencia  %in% c(27) ~ 'Tabasco',
                                   tot_enigh1$residencia  %in% c(28) ~ 'Tamaulipas',
                                   tot_enigh1$residencia  %in% c(29) ~ 'Tlaxcala',
                                   tot_enigh1$residencia  %in% c(30) ~ 'Veracruz',
                                    
                                   tot_enigh1$residencia  %in% c(31) ~ 'Yucatan',
                                   tot_enigh1$residencia  %in% c(32) ~ 'Zacatecas',
                                   tot_enigh1$residencia  %in% c(33) ~ 'EUA',
                                   tot_enigh1$residencia  %in% c(34) ~ 'Otro',
                                   tot_enigh1$residencia  %in% c(0) ~ 'Ninguno')

tot_enigh1$sexo <- case_when(tot_enigh1$sexo == 1 ~ 'Hombre',
                             tot_enigh1$sexo == 2 ~ 'Mujer')


tot_enigh1$edo_conyug <- case_when(tot_enigh1$edo_conyug == 1 ~ 'no casado',
                                   tot_enigh1$edo_conyug ==2 ~ 'casado',
                                   tot_enigh1$edo_conyug ==3 ~ 'separado',
                                   tot_enigh1$edo_conyug ==4 ~ 'divorciado',
                                   tot_enigh1$edo_conyug ==5 ~ 'viudo',
                                   tot_enigh1$edo_conyug ==6 ~ 'soltero')

tot_enigh1$nivelaprob <- case_when(tot_enigh1$nivelaprob  %in% c(0) ~ 'Sin escolaridad',
                                   tot_enigh1$nivelaprob  %in% c(1,2,3) ~'Educación básica',
                                  tot_enigh1$nivelaprob  %in% c(4,6) ~ 'Educación media superior',
                                  tot_enigh1$nivelaprob  %in% c(5,7) ~ 'Educación superior',
                                  tot_enigh1$nivelaprob  %in% c(8,9) ~ 'Posgrado')


gc()

#Factor de expansión


data_final <- data.frame()
indices <- rep(1:nrow(tot_enigh1), tot_enigh1$factor)
data_final<- tot_enigh1[indices,]



names(data_final) <- c("SEXO","factor","nivelaprob","group")










