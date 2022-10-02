#
# Autor: Adrián Lara
# 08-2022
# ------------------------------------------------------------------------------------
# Blog Economía Cheyenne / grafs-exploratorias.R
rm(list = ls())

if(!require(pacman)){install.packages("pacman")}
p_load(tidyverse, data.table, here, cumstats, scales, extrafont)

# Archivero

files <- list(clean_cuestbas = here("clean/clean-cuestbas.rds"))

# Abrimos 

clean_cuestbas <- readRDS(files$clean_cuestbas) 


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
  
  ifelse(!dir.exists(file.path(here("grafs"))),
         dir.create(file.path(here("grafs"))), 
         FALSE)
  
  ifelse(!dir.exists(file.path(here("grafs/exploratorias"))),
         dir.create(file.path(here("grafs/exploratorias"))), 
         FALSE)
  
  if(compara == TRUE){
    wrapvars <- c("Porcentaje", "Porcentaje promedio acumulado")
    ifelse(!dir.exists(file.path(here("grafs/exploratorias/prom-compare"))),
           dir.create(file.path(here("grafs/exploratorias/prom-compare"))), 
           FALSE)
    savingdir <- "prom-compare/"
  } else{
    wrapvars <- c("Porcentaje promedio acumulado")
    ifelse(!dir.exists(file.path(here("grafs/exploratorias/prom-acum"))),
           dir.create(file.path(here("grafs/exploratorias/prom-acum"))), 
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
  
  ggsave(paste0(here("grafs/exploratorias/"), savingdir,colname, ".png"), plot,
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
