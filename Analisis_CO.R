library(magrittr)
library(tidyverse)
library(ggplot2)
library(lubridate)
library(readxl)
library(dplyr)


AireQuito <- read_excel("~/Documents/Doctorado/Contaminación Aire/Datos/AireQuito.xlsx")
AireQuito %>% names


#Sleccionar columnas a analizar

CO_Analisis <- select(AireQuito, Fecha, Sector, CO, Dir_wind, Vel_wind, Temperatura, HumRel, Lluvia, RadSol)

#Crear un subconjunto de datos para CO (Fecha, Sector, CO, "Dir_wind",
#"Vel_wind", "Temperatura", "HumRel", "Lluvia", "RadSol")


Datos2016_CO <- Datos_CO %>% group_by(Sector) %>% 
  filter(year(Fecha) %in% c(2016)) %>% 
  filter(month(Fecha) %in% c(1:12)) %>% 
  filter(hour(Fecha) %in% c(00:07)) %>% 
  summarise(
    MEDIA= mean(CO, na.rm=TRUE), 
    MEDIA_ACOT= mean(CO, na.rm = TRUE, trim = 0.05),
    CANTIDAD= n()
  )


Datos_CO <- AireQuito[ , c("Fecha", "Sector", "CO")]


Datos_CO %>%
  group_by(Fecha, year==2008) %>%
  summarise(total = sum(value))




Datos_CO <- na.omit(Datos_CO)



#Subconjunto de datos por sector

Belisario <- Datos_CO %>% 
  filter(Sector=="Belisario")

#Seleccionar por año

Beli2016_CO <- Belisario %>% filter(year(Fecha) %in% c(2016))



p <- ggplot(data=Beli2016_CO, mapping=aes(x=Fecha, y=CO, color=CO)) + 
  geom_point() + geom_line(aes(group = 1))
p + facet_grid(facets = ~Fecha, margins = FALSE) + theme_bw()
print(p)



Beli2007_CO <- interval(start = "2008-01-01 00:00:00", end = "2008-12-31 23:00:00")

#Ordenar ascendente
Beli2016_CO <- Beli2016_CO[with(Beli2016_CO, order(Beli2016_CO$Fecha)), ] # Orden directo 


a <- ggplot(Beli2016_CO, aes(x = CO))

a + geom_line(aes(x = Fecha, y = CO))+
  xlim("Jan","Dec") + ylim(0,4)


p <- ggplot(data=Beli2016_CO, mapping=aes(x=Fecha, y=CO, color=CO)) + 
  geom_point() + geom_line(aes(group = 1))
p + facet_grid(facets = ~Fecha, margins = FALSE) + theme_bw()
print(p)

p + facet_wrap(~Fecha, ncol = 3,) + theme_bw()
print(p)

e <- ggplot(Beli2016_CO, aes(x = Fecha, y = CO))x

e + geom_line()


# Plot a subset of the data
ss <- subset(Beli2016_CO, Fecha > as.Date("2016-1-1")) 
ggplot(ss, aes(x = Fecha, y = CO)) + geom_line()



p <- ggplot(, aes(x = HumRel))

p + geom_line(aes(x = Fecha, y = HumRel))



#Seleccionar por mes

Belisario %>% filter(month(Fecha) %in% c(11,12)) %>% View()

p <- ggplot(, aes(x = HumRel))

p + geom_line(aes(x = Fecha, y = HumRel))


#Variaciones de lluvias

Lluvia_2007<- CO_Analisis %>% filter(year(Fecha) %in% c(2007))

which(is.na(Lluvia_2007$Lluvia))

#Media

Lluvia_2007 %$% mean(HumRel, na.rm = TRUE) #ojo signo de dólar para seleccionar una variable

#Boxplot simple

Lluvia_2007 %$% boxplot(HumRel, main= "Boxplot para Humedad Relativa", ylab= "Porcentaje")

#Boxplot con ggplot

a <- ggplot(Lluvia_2007, aes(x = HumRel))

a + stat_ecdf(geom = "point")

#Gráficar la serie de tiempo para el periodo completo:

p <- ggplot(Lluvia_2007, aes(x = HumRel))

p + geom_line(aes(x = Fecha, y = HumRel))






#Seleccionar por hora

CO_Analisis

with( CO_Analisis , CO_Analisis[ hour( Fecha ) >= 2 & hour( Fecha ) < 5 , ] ) %>% View()





tail(CO_2007)


CO_2007 <- selectByDate(CO_Analisis, start= "2007-06-01 01:00:00", end= "2007-12-31 23:00:00")






<- selectByDate(mydata, start = "1/1/1999", end = "31/12/1999")
head(data.1999)

roup_by(AireQuito$CO
        
        
        
        plot(AireQuito$CO)
        
        str(AireQuito)
        
        AireQuito %>% names %>% head(,n=5)
        
        #Tratamiento de los valores NAs
        
        
        A#ireQuito$CO = ifelse(is.na(AireQuito$CO),
        
        #ve(AireQuito$CO, FUN = function(x) mean(x, na.rm = TRUE)),
        # ireQuito$CO)
        esto por sector  cambia todo
        
        #Ordenar
        AireQuito[order(AireQuito$Fecha),]
        
        #Ordenar con Plyr
        library(plyr)
        library(dplyr)
        arrange(AireQuito, Sector)
        #Ordenar en orden decreciente 
        arrange(AireQuito, desc(Sector))
        
        SubSet1 <- AireQuito %>% select(Fecha, Sector, CO)
        
        SubSet1 <- filter(SubSet1$Fecha<="2007-12-31 00:00:00" )
        
        SubC2 <- filter(SubSet1, between(Fecha, as.POSIXct("2012-01-01 00:00:00"), as.POSIXct("2012-12-31 23:59:00")))
        View(SubC2)
        
        
        SubC2 <- filter(SubSet1, between(Fecha, "2007-06-01 01:00:00", "2007-12-31 23:00:00"))
        
        View(SubC2)
        
        
        
        
        
        
        
        
        str(SubSet1)
        
        AireQuito %>% select([c(1:<=2007-12-31 00:00:00),c(Sector, CO )])
        
        Analisis1 <- filter(AireQuito, Fecha<="2007-12-31 00:00:00") %>% 
          select(AireQuito, Sector, CO)
        
        AireQuito %>% filter(Fecha<=2007-12-31 00:00:00 & Sector & CO ) %>% View()
        
        AireQuito %>% filter(year(2012), CO)
        
        
        
        
        
