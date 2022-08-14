require(pacman)
p_load(tidyverse, foreign, janitor, data.table, cumstats)

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

clean_cuestbas <- cuestbas %>% 
  unite("id", per:n_ren) %>% 
  mutate(p14 = case_when(
    p14 == "1" ~ "Sí",
    p14 == "2" ~ "Probablemente",
    p14 == "3" ~ "No",
    p14 == "4" ~ "No sabe"),
    p10 = case_when(
      p10 == "1" ~ "Sí",
      p10 == "2" ~ "No",
      p10 == "3" ~ "No sabe",
      p10 == "4" ~ "No tiene ingresos")) %>% 
  left_join(., clean_socioec, by = "id") 

# Grafs

# Función general para visualizar promedios acumulados 

make_data <- function(db, colname){
  tempo <- db %>% 
    group_by(eda) %>% 
    mutate(tot_etario = sum(factor, na.rm = T)) %>% 
    ungroup() %>% 
    group_by(across(c(eda, starts_with(colname), tot_etario))) %>% 
    summarise(tot_p = sum(factor, na.rm = T)) %>%
    mutate(porcentaje = round(tot_p / tot_etario, digits = 2)) %>%
    ungroup() %>% 
    arrange(across(c(starts_with(colname), eda))) %>%
    group_by(across(c(starts_with(colname)))) %>%
    mutate(cummean = dplyr::cummean(porcentaje))
}


plot_meanline <- function(colname, title){
  db <- make_data(clean_cuestbas, colname)
  
 plot <- ggplot(db %>% filter(across(c(starts_with(colname)))), 
           aes_string(x = "eda", y = "cummean", group = colname, color = colname)) +
      geom_line() +
      geom_point() +
      labs(title = title, y = "Promedio acumulado", x = "Edad", 
           caption = "Fuente: Elaboración propia - ENCO Julio 2022", color = "") +
      scale_y_continuous(limits=c(0,1), breaks = seq(0,1,0.25)) +
      theme_bw() +
      theme(legend.position = "top") +
      guides(color = guide_legend(reverse=TRUE))
  
 plot
 
  ggsave(paste0("/Users/adrix-lg/Repos/blog-cheyenne/grafs/", colname, ".png"), plot,
         width = 15, height = 9)
}

# Muestra 
plot_meanline("p14", "¿Algún miembro de este hogar o usted están planeando comprar un automóvil nuevo o usado
en los próximos 2 años?")

# Loop
preguntas <- paste0("p", c(1:15))
titulos <- c("¿Cómo describiría usted su situación económica comparada con la de hace 12 meses?",
             "¿Cómo cree que será su situación económica en 12 meses respecto a la actual?",
             "¿Cómo cree que es su situación en este momento comparada hace 12 meses?",
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
  plot_meanline(preguntas[i], titulos[i])
  
  setTxtProgressBar(pb, i)
}
close(pb)
rm(pb,i)

