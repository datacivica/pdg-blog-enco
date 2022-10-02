#
# Autor: Adrián Lara
# 08-2022
# ------------------------------------------------------------------------------------
# Blog Economía Cheyenne / clean-enco.R

if(!require(pacman)){install.packages("pacman")}
pacman::p_load(tidyverse, foreign, janitor, data.table,
       here, cumstats, scales)


# Archivero
files <- list(
    socioec = here("input/microdatos_enco_jul22/encocs_0722.DBF"),
    cuestbas = here("input/microdatos_enco_jul22/encocb_0722.DBF"),
    clean_cuestbas = here("clean/clean-cuestbas.rds")
    )


# Abrimos

openDBF <- function(db){
  db <- read.dbf(db, as.is = T) %>% clean_names()
}

socioec <- openDBF(files$socioec)
cuestbas <- openDBF(files$cuestbas)

# Ojo: la base de socioeconomicos tiene 2 factores:
# factor y fac_p18 no para expandir a la población completa (incluyendo menores de 18)
names(socioec)[str_starts(names(socioec), "fac")] 

# A pesar de parecer contraintuitivo, el fac_p18 de la base de socioec es igual a
# la variable "factor" del cuestionario básico

if(sum(socioec$fac_p18) == sum(cuestbas$factor)){
  print("fac_p18 en socioeconómicos es igual a factor en cuestionario básico")
}

# Limpieza socioeconomicos

clean_socioec <- socioec %>% 
  unite("id", per:n_ren) %>% 
  select(id, sex, eda, upm)

rm(socioec)

# Decodificación de respuestas del cuestionario básico:

r1_6 <- function(x){
  case_when(
    x <= 2 ~ "Mejor o mucho mejor", 
    x == 3 ~ "Igual",
    x <= 5 ~ "Peor o mucho peor",
    x == 6 ~ "No sabe",
    T ~ "check decoding")
}

r7 <- function(x){
  case_when(
    x == "1" ~ "Sí",
    x == "2" ~ "Igual posibilidad",
    x == "3" ~ "No",
    x == "4" ~ "No sabe",
    T ~ "check decoding"
  )
}

r8 <- function(x){
  case_when(
    x == "1" ~ "Mayores",
    x == "2" ~ "Iguales",
    x == "3" ~ "Menores",
    x == "4" ~ "No sabe",
    T ~ "check decoding"
  )
}

r9 <- function(x){
  case_when(
    x == "1" ~ "Sí",
    x == "2" ~ "No",
    x == "3" ~ "No sabe",
    T ~ "check decoding"
    )
}

r10 <- function(x){
  case_when(
    x == "1" ~ "Sí",
    x == "2" ~ "No",
    x == "3" ~ "No sabe",
    x == "4" ~ "No tiene ingresos",
    T ~ "check decoding"
  )
}

r11 <- function(x){
  case_when(
    x == "1" ~ "Muy buenas",
    x == "2" ~ "Buenas",
    x == "3" ~ "Iguales",
    x == "4" ~ "Malas",
    x == "5" ~ "Muy malas",
    x == "6" ~ "No sabe",
    T ~ "check decoding"
  )
}

r12 <- function(x){
  case_when(
    x == "1" ~ "Disminuirán mucho",
    x == "2" ~ "Disminuirán poco",
    x == "3" ~ "Permanecerán igual",
    x == "4" ~ "Aumentarán poco",
    x == "5" ~ "Aumentarán igual",
    x == "6" ~ "Aumentarán mucho",
    x == "7" ~ "No sabe",
    T ~ "check decoding"
  )
}

r13 <- function(x){
  case_when(
    x == "1" ~ "Aumentará mucho",
    x == "2" ~ "Aumentará poco",
    x == "3" ~ "Permanecerá igual",
    x == "4" ~ "Disminuirá poco",
    x == "5" ~ "Disminuirá mucho",
    x == "6" ~ "No sabe",
    T ~ "check decoding"
  )
}

r14_15 <- function(x){
  case_when(
    x == "1" ~ "Sí",
    x == "2" ~ "Probablemente",
    x == "3" ~ "No",
    x == "4" ~ "No sabe",
    T ~ "check decoding")
}

# Procesamos

clean_cuestbas <- cuestbas %>% 
  unite("id", per:n_ren) %>% 
  mutate(across(p1:p6, r1_6),
         p7 = r7(p7), p8 = r8(p8),
         p9 = r9(p9), p10 = r10(p10),
         p11 = r11(p11), p12 = r12(p12),
         p13 = r13(p13),
         across(p14:p15, r14_15)) %>% 
  mutate() %>% 
  left_join(., clean_socioec, by = "id") 

saveRDS(clean_cuestbas, files$clean_cuestbas)
