#############################################################
## Este es el script para calcular notas en ESOD299, puede ##
##    ser utilizado, distribuido y modificado libremente   ##
#############################################################
# Cargar paquetes requeridos
library(tidyverse)
library(here)
# Importa todas las evaluaciones (https://github.com/diegohalabi/ESOD299/blob/master/rubricas/rubrica_evaluacion_prof_Informante.pdf)
here()
df <- read.csv('eval.csv',sep=',')
str(df)
# Ordena los datos
grades <- df %>% 
  gather(nstudent,student,Estudiante.1:Estudiante.2) %>% 
  # Reemplaza las categorías por números (puntaje)
  # Note for DH of the future: create a function to make this code less gross.
  mutate(R..brica..Titulo.=recode(R..brica..Titulo.,
                                  '4. No requiere cambios en su forma actual' = 4,
                                  '3. Requiere cambios menores' = 3,
                                  '2. Requiere cambios mayores' = 2,
                                  '1. No cumple con lo mínimo' = 1
  )) %>% 
  mutate(R..brica..Resumen.=recode(R..brica..Resumen.,
                                  '4. No requiere cambios en su forma actual' = 4,
                                  '3. Requiere cambios menores' = 3,
                                  '2. Requiere cambios mayores' = 2,
                                  '1. No cumple con lo mínimo' = 1
  )) %>% 
  mutate(R..brica..Introducci..n.=recode(R..brica..Introducci..n.,
                                  '4. No requiere cambios en su forma actual' = 4,
                                  '3. Requiere cambios menores' = 3,
                                  '2. Requiere cambios mayores' = 2,
                                  '1. No cumple con lo mínimo' = 1
  )) %>% 
  mutate(R..brica..Material.y.M..todos.A.=recode(R..brica..Material.y.M..todos.A.,
                                  '4. No requiere cambios en su forma actual' = 4,
                                  '3. Requiere cambios menores' = 3,
                                  '2. Requiere cambios mayores' = 2,
                                  '11. No cumple con lo mínimo' = 1
  )) %>% 
  mutate(R..brica..Material.y.M..todos.B.=recode(R..brica..Material.y.M..todos.B.,
                                  '4. No requiere cambios en su forma actual' = 4,
                                  '3. Requiere cambios menores' = 3,
                                  '2. Requiere cambios mayores' = 2,
                                  '1. No cumple con lo mínimo' = 1
  )) %>% 
  mutate(R..brica..Resultados.A.=recode(R..brica..Resultados.A.,
                                  '4. No requiere cambios en su forma actual' = 4,
                                  '3. Requiere cambios menores' = 3,
                                  '2. Requiere cambios mayores' = 2,
                                  '1. No cumple con lo mínimo' = 1
  )) %>% 
  mutate(R..brica..Resultados.B.=recode(R..brica..Resultados.B.,
                                  '4. No requiere cambios en su forma actual' = 4,
                                  '3. Requiere cambios menores' = 3,
                                  '2. Requiere cambios mayores' = 2,
                                  '1. No cumple con lo mínimo' = 1
  )) %>% 
  mutate(R..brica..Discusi..n.A.=recode(R..brica..Discusi..n.A.,
                                  '4. No requiere cambios en su forma actual' = 4,
                                  '3. Requiere cambios menores' = 3,
                                  '2. Requiere cambios mayores' = 2,
                                  '1. No cumple con lo mínimo' = 1
  )) %>% 
  mutate(R..brica..Discusi..n.B.=recode(R..brica..Discusi..n.B.,
                                  '4. No requiere cambios en su forma actual' = 4,
                                  '3. Requiere cambios menores' = 3,
                                  '2. Requiere cambios mayores' = 2,
                                  '1. No cumple con lo mínimo' = 1
  )) %>% 
  mutate(R..brica..Referencias.=recode(R..brica..Referencias.,
                                  '4. No requiere cambios en su forma actual' = 4,
                                  '3. Requiere cambios menores' = 3,
                                  '2. Requiere cambios mayores' = 2,
                                  '1. No cumple con lo mínimo' = 1
  )) %>% 
  mutate(R..brica..Escritura.=recode(R..brica..Escritura.,
                                  '4. No requiere cambios en su forma actual' = 4,
                                  '3. Requiere cambios menores' = 3,
                                  '2. Requiere cambios mayores' = 2,
                                  '1. No cumple con lo mínimo' = 1
  )) %>%
  # Agrega una nueva columna (score) con el puntaje total de cada evaluador
  mutate(score = rowSums(.[5:15])) %>% 
  # Agrega una nueva columna (grades) con la nota de cada evaluador
  mutate(grade = (score * 7)/44) %>% 
  # Muestra la nota para cada estudiante
  group_by(student) %>% 
  summarize(total=mean(grade))
# Guarda el archivo
write.csv(grades,'notas.csv')
