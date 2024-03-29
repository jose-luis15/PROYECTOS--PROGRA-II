# Para esta serie de ejercicios usaremos el data frame stars que nos indica 
# atributos de una serie de estrellas como su temperatura, tipo y magnitud. 
# La columna magnitude es la magnitud absoluta, donde un valor m�s negativo 
# de una estrella respecto a otra quiere decir que es m�s luminosa.
names(estrellas) <- c("temp", "lumin", "radius", "magnitude", "type", "color", "clase")

# 1. La temperatura est� en grados Kelvin. Agrega la columna temp_celsius 
# usando la siguiente f�rmula: C = K - 273.15. Reporta un gr�fico de 
# dispersi�n de temperatura versus magnitud coloreando seg�n el clase de estrella. 
# Adem�s, cambia la escala del eje-x a logar�tmica en base 10.

estrellas_n <- estrellas %>%
  mutate(temp_celsius = temp - 273.15)

ggplot(estrellas_n, aes(x=temp_celsius, y=magnitude, color=clase)) +
  geom_point() +
  scale_x_log10()

# 2. Dado que los valores positivos indican que hay menos brillo, invierte 
# los valores del gr�fico anterior usando la capa scale_y_reverse().

ggplot(estrellas_n, aes(x=temp_celsius, y=magnitude, color=clase)) +
  geom_point() +
  scale_x_log10() +
  scale_y_reverse()

# El Sol es clasificado como una estrella clase G.
# �Son las estrellas de clase G las m�s luminosas? 
# Crea un gr�fico de cajas para comparar medianas de la magnitud y determinar 
# qu� clase de estrellas son las m�s luminosas.

ggplot(estrellas_n, aes(x=clase, y=magnitude)) +
  geom_boxplot() +
  scale_y_reverse()
