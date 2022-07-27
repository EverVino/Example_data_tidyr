library("pacman") # Biblioteca para gestionar paquetes abiertos

p_load("readr") # Biblioteca para abrir archivos .cvs
p_load("tidyr") # Biblioteca para manipular la forma de nuestra tabla
p_load("ggplot2") # Biblioteca para realizar las gráficas del ejemplo
p_load("dplyr")  # Biblioteca para filtrar datos de la tabla
p_load("zoo") # Biblioteca para transformar fechas a trimestres
p_load("scales") # Biblioteca para dar formato de miles en el eje y
setwd("/home/ever/rpractice")
getwd()

# Importación de datos
datos_turistas <- read_csv("turistas.csv")
head(datos_turistas)

# Agrupamos los datos usando gather()
# Queremos almacenar las fechas en una nueva columna llamada Fecha
datos_turistas <-
  gather(data = datos_turistas,
         key = Fecha,
         value = Turistas,
         2:ncol(datos_turistas))

# Separar la fecha en días, mes y año
datos_turistas <-
  separate(data = datos_turistas,
           col = Fecha,
           into = c("día", "mes", "año"),
           sep = "/",
           remove = FALSE
  )

# Convertirmos los datos de año a entero
# Añadimos "20" antes de convertirlo
datos_turistas$año <-
  as.integer(paste("20", datos_turistas$año, sep = ""))

# Queremos saber de cuales países existen más visitas en los cinco últimos años

datos_turistas_2016_2021 <- 
  datos_turistas %>% filter(as.integer(año) >= 2016, na.rm =TRUE)

# Agrupando por Nacionalidad y año
datos_turistas_2016_2021 <- 
  datos_turistas_2016_2021 %>%
  group_by(Nacionalidad, año) %>%
  summarise(turistas_anuales = sum(Turistas))

#Extrayendo los top turistas visitantes de Bolivia
top_turistas_2016_2021 <- 
  datos_turistas_2016_2021 %>% 
  arrange(desc(turistas_anuales)) %>%
  group_by(año) %>% 
  slice(1:7)

#Graficando top_turistas 2016 - 2021
ggplot(data = top_turistas_2016_2021) +
  geom_bar(
    aes(x = año, y = turistas_anuales, fill = Nacionalidad),
    stat = "identity",
    alpha = 0.8
  ) +
  theme_minimal() + scale_fill_brewer(palette = "Paired") +
  scale_x_continuous(breaks = c(2016:2021)) +
  scale_y_continuous(labels = comma_format(big.mark = " ")) +
  labs(
    x = "",
    y = "",
    fill = "Nacionalidad",
    title = "Viajeros llegados por avión a Bolivia 2016-2021",
    subtitle = "Top 7 nacionalidades por año",
    caption = "Fuente: INE Bolivia"
  )

#Guardando imagen generada
ggsave(
  filename = "Viajeros_2016_2021.jpeg",
  path = "./",
  scale = 1,
  device = "png",
  dpi = 300,
  bg = "white"
)

# Unimos las columnas mes y año en una sola
datos_turistas_trimestre <- datos_turistas %>%
  unite(mes_año, c("mes", "año"), sep = "-")


datos_turistas_trimestre <- datos_turistas_trimestre %>%
  mutate(trimestre = as.yearqtr(mes_año, format = "%m-%Y")) %>%
  group_by(Nacionalidad, trimestre) %>%
  summarise(turistas_trimestre = sum(Turistas))

# Dividimos en varias columnas
datos_turistas_trimestre <-
  spread(
    data = datos_turistas_trimestre, 
    key = trimestre, 
    value = turistas_trimestre)

#Extraemos top turistas en el trimestre 1 2019
top_turistas_trimestre_1_2019 <-
  datos_turistas_trimestre %>% 
  select(Nacionalidad, `2019 Q1`) %>%
  rename(trimestre1_2019= `2019 Q1`) %>%
  arrange(desc(trimestre1_2019)) %>% as.data.frame() %>%
  slice_head(n = 10)

# Graficamos
ggplot(data = top_turistas_trimestre_1_2019) +
  geom_bar(aes(x=Nacionalidad, y=trimestre1_2019, fill=Nacionalidad), stat="identity", alpha = 0.8)+
  theme_minimal() + scale_fill_brewer(palette = "Paired")+
  scale_y_continuous(labels = comma_format(big.mark=" ")) +
  labs(
    x = "",
    y = "",
    fill = "Nacionalidad",
    title = "Viajeros llegados por avión a Bolivia Primer Trimestre 2019",
    subtitle = "Top 10 nacionalidades",
    caption = "Fuente: INE Bolivia"
  )

# Guardamos la imagen generada
ggsave(
  filename = "Viajeros_trimestre_1_2019.jpeg",
  path = "./",
  scale = 1,
  device = "png",
  dpi = 300,
  bg = "white"
)