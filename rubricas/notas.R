#######################################################################
####### Este es el script para calcular notas en ESOD299, puede #######
#######    ser utilizado, distribuido y modificado libremente   #######
#######################################################################
# Cargar paquetes requeridos
library(googlesheets)
library(tidyverse)
# Importa las evaluaciones de la base de datos
gs_ls()
db = 'poTEOHWvyPDFCyiGXHwLqr0f2hAqmojz'
gs_ls(db)
df <- gs_title(db)
(DF_KEY <- gs_gap_key())
third_party_gap <- DF_KEY %>%
  gs_key()
(DF_URL <- gs_gap_url())
extract_key_from_url(DF_URL)
df <- df %>%
  gs_gs()
df <- gs_read(df, check.names = F)
glimpse(df)
# Ordena los datos
grades <- df %>% 
  gather(nstudent,student,'Estudiante 1':'Estudiante 2') %>% 
# Reemplaza las categorías por números (puntaje)
  mutate_at(vars(5:15),
    funs(recode(.,
      '4. No requiere cambios en su forma actual' = 4,
      '3. Requiere cambios menores' = 3,
      '2. Requiere cambios mayores' = 2,
      '1. No cumple con lo mínimo' = 1,
      .default = 0
    ))) %>% 
# Agrega una nueva columna (score) con el puntaje total de cada evaluador
  mutate(score = rowSums(.[5:15])) %>% 
# Agrega una nueva columna (grades) con la nota de cada evaluador
  mutate(grade = (score * 7)/44) %>% 
# Muestra la nota para cada estudiante
  group_by(student) %>% 
  summarize(total = mean(grade))
# Guarda el archivo
write.table(grades, 'notas.csv', sep = ';', dec = ',')
