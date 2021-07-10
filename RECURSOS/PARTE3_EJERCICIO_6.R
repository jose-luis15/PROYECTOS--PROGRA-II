######################################################################################################
######################################## 6TA CLASE UNMSM #############################################
######################################################################################################
#install.packages("rmarkdown")
library(rmarkdown)
library(tidyverse)
library(dplyr)
#install.packages("rmarkdown", dependencies = T)
#Filtrar por el nombre para detectar el código.
dir()
setwd("C:/Users/Jose Luis/Desktop/PROGRAMACION II/MATERIAL DOCENTE/ProgramacionR-master/data")

#Definimos los archivos de trabajo y los asignamos a un elemento.
names_table <- read_csv("listRaingauge.csv")
names(names_table) #Extraemos los nombres de nuestrso archivos para observar la forma de escritura para nuestra conuslota, si estan en altas, bajas o una combinacion de estas.

estacion <- names_table %>% # Estraer el código de la estacion en base al nombre.
  dplyr::filter(NOM_EST == "SAN MIGUEL") %>%
  dplyr::select(CODIGO) %>%
  as.character()

data <- 
  read_csv("raingaugeDataset.csv")%>%
  dplyr::select(date, all_of(estacion)) %>%
  mutate(date = as.Date(date, format = "%d/%m/%Y")) %>%
  rename(pp = all_of(estacion)) %>%
  arrange(date)

#saber las ultimas opcines de un dataframe
tail(data) #con esto podre saber la fecha final(la inicial lo podemos obtener solo llamando al archivo data).
#mediante una Secuencia diaria comprombamos si la cantidadd de elementos en el dataframe estan completos.
seq(as.Date("1980-01-01"), as.Date("2013-12-31"), by = "day") %>%
length()

# CINCIDEN, EXCELENTE ...!!!!

#Determine la cantidad de missing values de la serie de tiempo a paso diario.  
(NA_diario <- 
  data %>%
  mutate(
    cantidad_NA =  sum(is.na(pp)) 
  ) %>%
  summarise(  
    cantidad_NA = unique(cantidad_NA) 
  ))

#1. determinamos los valores para las pp mensual.
pp_month <- 
  data %>%
  group_by(date = str_sub(date, 1, 7)) %>% #agrupamos los caracteres de 1 al 7 donde abarca los años y meses.
  mutate(
    valores_NA =  sum(is.na(pp))*100/n()  #indicrá nos numeros de NA en un nuevo campo llamado missval(en porcentaje). la funcion n me arroja el numero de dias.
  ) %>%
  summarise(   #Tabla resumen
    pp = sum(pp, na.rm = T), #realizamos la pp acumulada
    valores_NA = unique(valores_NA), #Genera un unico valor, la funcion unique me devuelve valores únicos
  )%>%
  mutate(
    pp = ifelse(valores_NA >= 10, NA, pp), #Los porcentajes que no cumples con el criterio se consideraran como NA
    date = as.Date(sprintf("%1$s-01", date)),
    meses = str_sub(date,6,7)#Creamos una variable que nos muestre los meses. variabilidad para cada mes por todos los años.
  )

prueba <- pp_month %>% arrange(meses)
view(prueba)

#Calcule la serie de tiempo de precipitación acumulada mensual (si el # de días con missing values, 
#en un mes, supera el 10%, la precipitación acumulada mensual será considerado como un NA).
# ggplot(pp_month, aes(date, pp)) +
#   geom_line()+
#   scale_x_discrete(
#     labels = month.abb
#   )

pp_periodo <- 
  pp_month%>%
  dplyr::filter(date >= "1980-01-01" & date < "2010-12-31")%>%
  group_by(date = str_sub(date, 6,7)) %>%
  mutate(
    valores_NA =  sum(is.na(pp))*100/n()  #indicrá nos numeros de NA en un nuevo campo llamado missval(en porcentaje). la funcion n me arroja el numero de dias.
  )%>%
  summarise(   #Tabla resumen
    pp = mean(pp, na.rm = T), #realizamos la pp acumulada
    valores_NA = unique(valores_NA), #Genera un unico valor, la funcion unique me devuelve valores únicos
  )%>%
  mutate(
    pp = ifelse(valores_NA >= 10, NA, pp), #Los porcentajes que no cumples con el criterio se consideraran como NA
    date = as.Date(sprintf("2000-%1$s-01", date)),
    meses = str_sub(date,6,7)#Creamos una variable que nos muestre los meses. variabilidad para cada mes por todos los años.
  )


ggplot(pp_periodo, aes(pp)) +
  geom_bar(color = "#048ABF")+
  scale_x_discrete(
    labels = month.abb
  )

plot(
  pp_periodo$meses, pp_periodo$pp,
  type = "o", pch = 14, xlab = "Mes",
  ylab = "Temperatura [°C]", main = "Lima, Temperatura mensual promedio",
  scale_x_discrete(
    labels = month.abb
  )
)

ggplot(pp_periodo, aes(date, pp)) +
  geom_bar(color = "#048ABF")+
  # labs(y="precipitación (mm)", x = "Años analizados")+
  # scale_x_discrete(
  #   labels = month.abb
  # )+
  ggtitle("Precipitación acumulada mensual")+
  theme(plot.title = element_text(vjust =2, hjust = 0.5))+
  theme(axis.title.y = element_text(vjust = 2.5))+
  theme(axis.title.x = element_text(vjust = -0.5))

###############################################################
ggplot(pp_month, aes(date, pp)) +
  geom_line(color = "#048ABF")+
  labs(y="precipitación (mm)", x = "Años analizados")+
  ggtitle("Precipitación acumulada mensual")+
  theme(plot.title = element_text(vjust =2, hjust = 0.5))+
  theme(axis.title.y = element_text(vjust = 2.5))+
  theme(axis.title.x = element_text(vjust = -0.5))

#Cantidad de missing values por meses donde los que tiene NA mayores al 10% se contabilizan como Na tambien.
(NA_mensual<- sum(is.na(pp_month$pp)))

#Poltear (boxplot) la variabilidad de los valores mensuales (Ene-Dic) para el período 1980-2013.
pp_month2 <- 
  pp_month%>%
  dplyr::filter(date >= "1980-01-01" & date < "2013-12-31")

ggplot(pp_month2, aes(meses, pp)) +
geom_boxplot(fill = "#048ABF") +
theme_bw() +
scale_x_discrete(
  labels = month.abb
) +
ggtitle("Variabilidad de la precipitación mensual - periodo 1980-2013")+
  theme(plot.title = element_text(vjust =2, hjust = 0.5))+
labs(y="precipitación (mm)") +
theme(axis.title.y = element_text(vjust = 2.5))


