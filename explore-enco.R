require(pacman)
p_load(tidyverse, foreign, janitor, data.table, cumstats, scales)

# Abrimos

openDBF <- function(db){
  db <- read.dbf(db, as.is = T) %>% clean_names()
}

socioec <- openDBF("/Users/adrix-lg/Repos/blog-cheyenne/microdatos_enco_jul22/encocs_0722.DBF") 
cuestbas <- openDBF("/Users/adrix-lg/Repos/blog-cheyenne/microdatos_enco_jul22/encocb_0722.DBF") 

# Ojo: la base de socioeconomicos tiene 2 factores:
# factor y fac_p18 no para expandir a la población completa (incluyendo menores de 18)
names(socioec)[str_starts(names(socioec), "fac")] 

# A pesar de parecer contraintuitivo, el fac_p18 de la base de socioec es igual a
# la variable "factor" del cuestionario básico

if(sum(socioec$fac_p18) == sum(cuestbas$factor)){
  print("fac_p18 en socioeconómicos es igual a factor en cuestionario básico")
}

# Limpieza

clean_socioec <- socioec %>% 
  unite("id", per:n_ren) %>% 
  select(id, sex, eda)

rm(socioec)
# Decodifcación de respuestas:

r1_6 <- function(x){
  case_when(
    x == 1 ~ "Mucho mejor", 
    x == 2 ~ "Mejor" ,
    x == 3 ~ "Igual",
    x == 4 ~ "Peor",
    x == 5 ~  "Mucho peor",
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

rm(cuestbas)
# Grafs

# 1.- Función general para preparación de porcentajes y porcentajes promedios 

make_data <- function(db, colname){
  tempo <- db %>% 
    group_by(eda) %>% 
    mutate(tot_etario = sum(factor, na.rm = T)) %>% 
    ungroup() %>% 
    group_by(across(c(eda, matches(paste0(colname, "$")), tot_etario))) %>% 
    summarise(tot_p = sum(factor, na.rm = T)) %>%
    mutate(porcentaje = round(tot_p / tot_etario, digits = 2)) %>%
    ungroup() %>% 
    arrange(across(c(starts_with(colname), eda))) %>%
    group_by(across(c(starts_with(colname)))) %>%
    mutate(cummean = dplyr::cummean(porcentaje)) %>% 
    pivot_longer(porcentaje:cummean, names_to = "var_stat", 
                 values_to = "value") %>% 
    mutate(var_stat = ifelse(var_stat == "porcentaje", "Porcentaje",
                             "Porcentaje promedio acumulado"))
}

# 2.- Función general para visualización sencilla y wrappeada

plot_meanline <- function(db, colname, title, compara){
  db <- make_data(db, colname)
  
  if(compara == TRUE){
    wrapvars <- c("Porcentaje", "Porcentaje promedio acumulado")
    ifelse(!dir.exists(file.path("/Users/adrix-lg/Repos/blog-cheyenne/grafs/prom-compare")),
           dir.create(file.path("/Users/adrix-lg/Repos/blog-cheyenne/grafs/prom-compare")), 
           FALSE)
    savingdir <- "prom-compare/"
  } else{
    wrapvars <- c("Porcentaje promedio acumulado")
    ifelse(!dir.exists(file.path("/Users/adrix-lg/Repos/blog-cheyenne/grafs/prom-acum")),
           dir.create(file.path("/Users/adrix-lg/Repos/blog-cheyenne/grafs/prom-acum")), 
           FALSE)
    savingdir <- "prom-acum/"
  }
  
 plot <- ggplot(db %>% filter(get(colname) != "No sabe",
                              var_stat %in% wrapvars), 
           aes_string(x = "eda", y = "value", group = colname, color = colname)) +
      geom_line(size = 1.4) +
      geom_point(size = 2.6) +
      labs(title = title, y = "", x = "Edad", 
           caption = "Fuente: Elaboración propia - ENCO Julio 2022", color = "") +
      scale_y_continuous(limits=c(0,1), breaks = seq(0,1,0.25), labels = scales::percent) +
      facet_wrap(~var_stat) +
      theme_bw() +
      theme(legend.position = "top", 
            text = element_text(size = 15),
            plot.title = element_text(size = 24), 
            legend.text = element_text(size = 18), 
            strip.text.x = element_text(size = 19),
            axis.text.x = element_text(size=11)) +
      guides(color = guide_legend(reverse=TRUE))
  
 # print(plot)
 
  ggsave(paste0("/Users/adrix-lg/Repos/blog-cheyenne/grafs/", savingdir,colname, ".png"), plot,
  width = 16, height = 10)
}

# Muestra 
plot_meanline(clean_cuestbas, "p14", "¿Algún miembro de este hogar o usted están planeando comprar un automóvil nuevo o usado
en los próximos 2 años?", compara = FALSE)

# Loop
preguntas <- paste0("p", c(1:15))
titulos <- c("¿Cómo describiría su situación económica comparada con la de hace 12 meses?",
             "¿Cómo cree que será su situación económica en 12 meses respecto a la actual?",
             "¿Cómo cree que es la situación de los miembros de este hogar\nen este momento comparada hace 12 meses?",
             "¿Cómo será la situación económica de los miembros de este hogar en 12 meses?",
             "¿Cómo considera la situación económica del país hoy respecto a la de hace 12 meses?",
             "¿Cuál será la condición económica del país en 12 meses?",
             "¿Actualmente tiene más posibilidades de comprar\n ropa, zapatos, o alimentos que hace un año?",
             "En comparación hace un año, ¿cómo son las posibilidades de que usted\no alguno de los integrantes de este hogar compre\nmuebles, televisor, lavadora, etc.?",
             "¿Considera usted que durante el próximo año usted\no alguno de los integrantes de este hogar tendrán posibilidadesd de\nsalir de vacaciones?",
             "¿Actualmente usted tiene posibilidades de ahorrar alguna parte de sus ingresos?",
             "¿Cómo considera usted que serán sus condiciones económicas para ahorrar en 12 meses?",
             "¿Cómo cree usted que se comporten los precios en el país en los siguientes 12 meses?",
             "¿Cree usted que el empleo en el país en los próximos 12 meses",
             "¿Algún miembro de este hogar o usted están planeando comprar\n un automóvil nuevo o usado en los próximos 2 años?",
             "¿Algún miembro de este hogar o usted están planeando\ncomprar, construir o remodelar una casa en los próximos 2 años?"
             )

pb <- txtProgressBar(min = 1, max = length(preguntas), style = 3)
for (i in 1:length(preguntas)){
  plot_meanline(clean_cuestbas, preguntas[i], titulos[i], compara = TRUE)
  plot_meanline(clean_cuestbas, preguntas[i], titulos[i], compara = FALSE)
  
  setTxtProgressBar(pb, i)
}
close(pb)
rm(pb,i)

