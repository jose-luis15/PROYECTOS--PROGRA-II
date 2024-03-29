# Cargar las librerias ggplot2 y dplyr
library(ggplot2)
library(dplyr)

# Para tener una primera idea de ggplot2, intentemos ejecutar algunos 
# comandos b�sicos de ggplot2. 

# El conjunto de datos mtcars contiene informaci�n sobre 32 autom�viles 
# de un n�mero de 1973 de la revista Motor Trend. 
# Este conjunto de datos es peque�o, intuitivo y contiene una variedad 
# de variables continuas y categ�ricas.

# ------------------------------------------------------------------------
# 1. Cargue el paquete ggplot2 usando library().
# 2. Use str() para explorar la estructura del conjunto de datos mtcars.
# 3. Genera un gr�fico de dispersi�n con los campos cyl y mpg

str(mtcars)
ggplot(mtcars, aes(x = cyl, y = mpg)) + 
  geom_point()

# ------------------------------------------------------------------------
# Para que la columna cyl se convierta en una variable categ�rica, 
# es necesario convertirla a factor
# 1. En la creaci�n del gr�fico, convertir cyl a factor con factor() 
# y volver a generar el gr�fico

ggplot(mtcars, aes(x = factor(cyl), y = mpg)) + 
  geom_point()

# ------------------------------------------------------------------------
# Grammar of graphics (Leland Wikinson, 1999)
# https://www.springer.com/gp/book/9780387245447
# 3 elementos gramaticales esenciales:
# - Data: La data en s� que necesita ser ploteada
# - Aesthetics: Escala en la que mapeamos nuestros datos
# - Geometries: Elementos visuales que usa nuestra data
# No esenciales
# - Themes: Lo externo no relacionado con los datos
# - Statistics: Representaciones de nuestra data que facilitan el entedimiento
# - Coordinates: El espacio que la data necesita para ser ploteada
# - Facets: Ploteo multiple

# ------------------------------------------------------------------------
# Agregue una est�tica de color asignada al desplazamiento del motor (disp): 
# dentro de aes(), agregue un argumento de color igual a disp.

ggplot(mtcars, aes(x = factor(cyl), y = mpg, color=disp)) + 
  geom_point()

# Ahora cambia color por tama�o (size)

ggplot(mtcars, aes(x = factor(cyl), y = mpg, size=disp)) + 
  geom_point()

# ------------------------------------------------------------------------
# ------------------------------------------------------------------------
# Dataset IRIS

ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_jitter() +
  labs(x = "Longitud de sepalo (cm)", y = "Ancho de sepalo (cm)") +
  theme_classic()

# ------------------------------------------------------------------------
# ------------------------------------------------------------------------
# Dataset Diamonds

# El conjunto de datos de diamantes contiene detalles de 1,000 diamantes. 
# Entre las variables incluidas est�n el quilate (una medida del tama�o del diamante) y el precio.

# Utilizar� dos funciones comunes de capa geom:
# geom_point () agrega puntos (como en un diagrama de dispersi�n).
# geom_smooth () agrega una curva de tendencia suave.
# Como viste anteriormente, estos se agregan usando el operador +.

# ggplot (datos, aes (x, y)) +
#   geom_<LA GEOMETRIA DESEADA>()

# ------------------------------------------------------------------------
# 1. Explore los datos de diamantes con str()
# 2. Edite el c�digo de trazado para agregar un punto geom. 
#    Use el operador + para agregar geom_point () al comando ggplot ().
# 3. Agregue una geom suave a la trama. Use el operador + para agregar geom_smooth().

# <MODIFICAR CODIGO AQUI>
ggplot(diamonds, aes(carat, price)) ___
  ___ +
  ___
# <MODIFICAR CODIGO AQUI>

# ------------------------------------------------------------------------
# Si tiene m�ltiples geometrias, entonces asignar una est�tica a una variable de 
# datos dentro de la llamada a ggplot() cambiar� todas las geometrias. 
# geom_point() tiene un argumento alfa que controla la opacidad de los puntos. 
# Un valor de 1 (el valor predeterminado) significa que los puntos son totalmente opacos; 
# un valor de 0 significa que los puntos son totalmente transparentes 

# 1. Agregar el color segun la columna "clarity"
# 2. Haga los puntos transl�cidos estableciendo el argumento alfa en 0.4

# <MODIFICAR CODIGO AQUI>
ggplot(diamonds, aes(carat, price)) +
  geom_point() +
  geom_smooth()
# <MODIFICAR CODIGO AQUI>

# ------------------------------------------------------------------------
# Los gr�ficos se pueden guardar como variables, que se pueden agregar 
# dos m�s tarde utilizando el operador +. 
# Esto es realmente �til si desea hacer m�ltiples trazados relacionados desde una base com�n.

# 1. Utilizando el conjunto de datos de diamantes, trace el "price" (eje y) 
#    frente al "carat" (eje x), asignando a la variable "plt_price_vs_carat"
#    Usando geom_point(), agregue una capa de puntos a plt_price_vs_carat.
# 2. Agregue un argumento alfa a la capa de puntos para hacer que los puntos 
#    sean 20% opacos, asignando a plt_price_vs_carat_transparent.
# 3. Dentro de geom_point (), llame a aes () y asigne color a "clarity"
# Muestre su resultado

# ------------------------------------------------------------------------
# ------------------------------------------------------------------------
# Estetica visible
# x: columna X
# y: columna Y
# fill: Color de llenado
# color: Color de los puntos o lineas
# size: Area o radio de los puntos, grosor de lineas
# alpha: Transparencia
# linetype: Patr�n de tipo de linea
# labels: Texto en un plot o ejes
# shape: Forma de los puntos
# ------------------------------------------------------------------------
# ------------------------------------------------------------------------

# 1. Usando mtcars, cree un grafico de nombre plt_mpg_vs_wt. Mapea mpg en "y" y wt en "x".
#    Agregue una capa de puntos de cilindros. Dale un tama�o seg�n cyl.
# 2. Cambiar el mapeo. Esta vez, cyl debe mapearse en alfa.
# 3. Cambiar el mapeo. Esta vez, cyl debe mapearse en shape.
# 4. Intercambie la capa de geom: cambie los puntos a texto.
#    Cambiar el mapeo. Esta vez, ycl debe asignarse a la etiqueta.

# <ESCRIBIR CODIGO AQUI>

# ------------------------------------------------------------------------
# ------------------------------------------------------------------------
# Funciones de escala
# scale_x_*() / scale_x_continuous()
# scale_y_*()
# scale_color_*() / # scale_colour_*() / scale_color_discrete()
# scale_fill_*()
# scale_shape_*()
# scale_linetype_*()
# scale_size_*()

# ------------------------------------------------------------------------
# ------------------------------------------------------------------------
# Actualizaci�n de etiquetas est�ticas
# labs() permite establecer las etiquetas de los ejes x e y.
# scale_color_manual() define las propiedades de la escala de colores. 

# ------------------------------------------------------------------------
# 1. Establezca la etiqueta del eje x en "N�mero de cilindros" y la etiqueta del eje y en "Cuenta" 
#    utilizando los argumentos x e y de labs(), respectivamente.
# 2. Implemente una escala de color de relleno personalizada usando scale_fill_manual().
#    Establezca el primer argumento en "Transmisi�n" y los valores en paleta.
# 3. Modifique el c�digo para establecer la posici�n para esquivar de modo que las barras 
#    para las transmisiones se muestren una al lado de la otra.


# <ESCRIBIR CODIGO AQUI>
palette <- c(automatic = "#377EB8", manual = "#E41A1C")

ggplot(mtcars, aes(cyl, fill = gear)) +
  geom_bar()
# <ESCRIBIR CODIGO AQUI>

# ------------------------------------------------------------------------
# ------------------------------------------------------------------------
head(iris,3)
iris %>%
  group_by(Species) %>%
  summarise_all(mean) -> iris.summary

ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width, col=Species)) +
  geom_point() +
  geom_point(data=iris.summary, shape=15, size=5)

ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width, col=Species)) +
  geom_point() +
  geom_point(data=iris.summary, shape=21, size=5, fill="black", stroke=2)


# ------------------------------------------------------------------------
# ------------------------------------------------------------------------
# Histogram
# Recuerde que los histogramas cortan una variable continua en contenedores discretos y,
# de manera predeterminada. 
# Se puede acceder a una variable interna llamada notaci�n, es decir, ..densidad.. 
# Se mostrar� la frecuencia relativa, que es la altura por el width de cada bin.

ggplot(mtcars, aes(mpg)) +
  geom_histogram()

# Posiciones en histogramas
# Aqu�, examinaremos las diversas formas de aplicar posiciones a los histogramas. 
# geom_histogram(), un caso especial de geom_bar(), 
# tiene un argumento de posici�n que puede tomar los siguientes valores:
#
# stack (predeterminado): Se apilan una encima de la otra.
# dodge: Se colocan una al lado de la otra.
# fill: Se muestran como proporciones.
# identity: trazar los valores tal como aparecen en el conjunto de datos.

# ------------------------------------------------------------------------
# 1. Actualice la est�tica para que el color de relleno de las barras est� determinado por gear.
# 2. Actualice la capa de histograma para colocar las barras una al lado de la otra, es decir, "dodge".
# 3. Actualice la capa de histograma para que las posiciones de las barras "fill" el eje y.
# 4. Actualice la capa de histograma para que las barras est�n una encima de la otra, 
#    utilizando la posici�n de "identity". Para que se pueda ver cada barra, establezca alfa en 0.4.

# <ESCRIBIR CODIGO AQUI>
ggplot(mtcars, aes(mpg)) +
  geom_histogram(binwidth = 1)
# <ESCRIBIR CODIGO AQUI>

# ------------------------------------------------------------------------
# ------------------------------------------------------------------------
# Gr�ficos de barras y columnas
# Veamos c�mo cambia el argumento de posici�n geom_bar().
# 
# Tenemos tres opciones de posici�n:
# 
# stack: Predeterminado
# dodge: Al costado del otro
# fill: para mostrar proporciones
# Si bien usaremos geom_bar() aqu�, tenga en cuenta que la funci�n geom_col() es simplemente geom_bar(), 
# donde los argumentos de posici�n y estad�stica se establecen en "identity". 
# Se utiliza cuando queremos que las alturas de las barras representen los valores exactos en los datos.
# 
# En este ejercicio, dibujar� el recuento total de autom�viles que tienen un 
# n�mero determinado de cilindros (cyl), seg�n el tipo de transmisi�n manual o autom�tica (gear).

# ------------------------------------------------------------------------
# 1. Usando mtcars, trace cyl, lleno por gear.
# 2. Agregue una capa de barra usando geom_bar().
# 3. Establezca el argumento de la posici�n de la barra en "fill".

# ------------------------------------------------------------------------
# Superposici�n de los barplots
# 1. Use la forma funcional de la posici�n de la barra: reemplace "dodge" con una llamada a position_dodge().
#    Establezca su width en 0.2.

mtcars$fam = as.character(mtcars$gear)

ggplot(mtcars, aes(cyl, fill = gear)) +
    geom_bar(position = "dodge")

ggplot(mtcars, aes(cyl, fill = fam)) +
  geom_bar(position = position_dodge(width=0.2), alpha=0.6)


# ------------------------------------------------------------------------
# ------------------------------------------------------------------------
# Gr�ficos de barras: paleta de colores secuenciales
# En este diagrama de barras, llenaremos cada segmento de acuerdo con una variable ordinal. 
# La mejor manera de hacerlo es con una paleta de colores secuencial.
# 
# Aqu� hay un ejemplo del uso de una paleta de colores secuencial con el conjunto de datos mtcars:
#   ggplot(mtcars, aes(cyl, fill = fam)) +
#   geom_bar() +
#   scale_fill_brewer(palette = "Set1")

install.packages("carData")
library(carData)
data(Vocab)

head(Vocab)
# ------------------------------------------------------------------------
# 1. Genere un grafico con el dataset de Vocab, mapeando la "education" en x y "vocabulary" en el fill
# 2. Agregue una geometria de barra, estableciendo la posici�n "fill"
# 3. Agregue una escala de relleno (scale_fill_brewer()), utilizando la paleta predeterminada. 
#    Observe c�mo esto genera un mensaje de advertencia y un diagrama incompleto.

# <ESCRIBIR CODIGO AQUI>

# ------------------------------------------------------------------------
# ------------------------------------------------------------------------
# Trazados de l�neas b�sicas
# Aqu�, usaremos el conjunto de datos "economics" para hacer algunos gr�ficos de l�neas. 
# El conjunto de datos contiene una serie temporal de estad�sticas de desempleo y 
# poblaci�n del Banco de la Reserva Federal de St. Louis en los Estados Unidos. 
# Los datos est�n contenidos en el paquete ggplot2.
# 
# Se puede ver c�mo la mediana del tiempo de desempleo y la tasa de desempleo 
# (el n�mero de personas desempleadas como proporci�n de la poblaci�n) cambian con el tiempo.

# ------------------------------------------------------------------------
# 1. Imprima el "head" del conjunto de datos "economics".
# 2. Trace "unemploy" vs. "date" como un grafico lineal.
# 3. Ajuste el aes del eje y de la parcela para que represente la proporci�n 
#    de la poblaci�n que est� desempleada.

# <ESCRIBIR CODIGO AQUI>

# ------------------------------------------------------------------------
# M�ltiples series de tiempo
# Ya vimos c�mo la forma de sus datos afecta c�mo puede trazarlos. 
# Exploremos eso m�s a fondo con m�ltiples series de tiempo. Aqu�, es importante 
# que todas las l�neas est�n en la misma escala y, si es posible, en la misma trama.
# 
# Se tiene un dataset de series de tiempo de evapotranspiracion de 1981 al 2017. 
# Cada variable (columna) es una estacion diferente y cada observaci�n (fila) es de un a�o. 

# ------------------------------------------------------------------------
# 1. Use str() para examinar la estructura del dataset.
# 2. Trace una serie temporal de la fecha con la columna Rainbow con geom_line()



getwd()

ggsave("nombre.png", width = 30, height = 16, units = "cm")
