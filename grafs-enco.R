#
# Autor: Adrián Lara
# 08-2022
# ------------------------------------------------------------------------------------
# Blog Economía Cheyenne / grafs-enco.R
rm(list = ls())

if(!require(pacman)){install.packages("pacman")}
p_load(tidyverse, data.table, here, cumstats, scales, srvyr, extrafont)

# Archivero 

files <- list(
  clean_cuestbas = here("clean/clean-cuestbas.rds")
)

# Tema gráfico
loadfonts(quiet = T)

adrix_theme <- theme_bw() +
  theme(legend.position = "top", 
        text = element_text(size = 15, family = "Roboto Slab"),
        plot.title = element_text(size = 24, hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5),
        legend.text = element_text(size = 18), 
        strip.text.x = element_text(size = 19),
        axis.text.x = element_text(size=11)) 

# Títulos
titulos <- c(
  "¿Cómo describiría su situación económica comparada con la de hace 12 meses?",
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

# Funciones

mk_ages <- function(x){ 
    # Fn de grupos de edades
    case_when(
      x %in% 18:29 ~ "18 a 29",
      x %in% 30:39 ~ "30 a 39",
      x %in% 40:49 ~ "40 a 49",
      x %in% 50:59 ~ "50 a 59",
      x %in% 60:69 ~ "60 a 69",
      x >= 70 ~ "70 o más",
      T ~ "nope"
    )
  }
  
mk_data_intervalos <- function(db, inciso, age_col = "eda"){
    # Prepararmos configuración INEGI
    options(survey.lonely.psu="adjust")
    
    x <- db %>%
      mutate(across(c(age_col), mk_ages, .names = "grupo_edad")) %>% 
      as_survey_design(ids = upm, weights = factor) %>%
      srvyr::group_by(across(c(grupo_edad, matches(paste0(inciso, "$"))))) %>%
      summarise(mean = survey_mean(), total_grupo = survey_total()) %>% 
      mutate(lower_ic = mean - 1.96 * mean_se, 
             upper_ic = mean + 1.96 * mean_se) %>% 
      arrange(across(c(grupo_edad, matches(paste0(inciso, "$"))))) %>%
      mutate(pregunta = inciso)  %>% 
      rename_with(~(x = "respuesta"), matches(inciso, "$")) %>% 
      relocate(pregunta, .before = respuesta)
  }
  

# Abrimos y nombramos grupos de edades

clean_cuestbas <- readRDS(files$clean_cuestbas) 

# Graf A - Vehículos
tempo <- mk_data_intervalos(clean_cuestbas, "p14") %>% 
  filter(respuesta %in% c("Sí", "No")) %>% 
  mutate(respuesta = factor(respuesta, levels=c("Sí", "No")))

ggplot(tempo, aes(x = grupo_edad, y = mean, group = respuesta, fill = respuesta)) +
  geom_line(size = 1.4, color="light gray") +
  geom_point(size = 2.6, color="navy blue") +
  geom_text(aes(label = paste0(c(as.character(round(mean, digits = 2)*100)), "%")),
            family = "Roboto Slab", vjust = -1.2, color =  "black") +
  geom_ribbon(aes(ymin = lower_ic,ymax = upper_ic),alpha=0.3) +
  labs(title =  "¿Alguien en este hogar o usted están planeando comprar un automóvil nuevo o usado\nen los próximos 2 años?",
       subtitle = "Porcentajes por grupo edad con intervalos de confianza", y = "", x = "Edades", 
       caption = "Fuente: Elaboración propia - ENCO Julio 2022", color = "", fill = "") +
  scale_color_manual(c("aquamarine3", "blueviolet")) +
  scale_fill_manual(values = c("aquamarine3", "blueviolet")) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  guides(color = guide_legend(reverse=TRUE)) +
  adrix_theme 

# Graf aspiraciones
  
tempo <- mk_data_intervalos(clean_cuestbas, "p14")  %>% 
  bind_rows(mk_data_intervalos(clean_cuestbas, "p9")) %>%
  bind_rows(mk_data_intervalos(clean_cuestbas, "p15")) %>% 
  filter(respuesta %in% c("No")) %>% 
  mutate(pregunta = case_when(
    pregunta == "p14" ~ "Comprar un automóvil nuevo o usado\nen los próximos 2 años",
    pregunta == "p15" ~ "Comprar, construir o remodelar una\ncasa en los próximos 2 años",
    pregunta == "p9" ~ "Salir de vacaciones el próximo año"))

ggplot(tempo, aes(x = grupo_edad, y = mean, group = pregunta, color = pregunta, fill = pregunta)) +
  geom_line(size = 1.4) +
  geom_point(size = 2.6) +
  geom_text(aes(label = paste0(c(as.character(round(mean, digits = 2)*100)), "%")),
            family = "Roboto Slab", vjust = -1.2, color =  "black") +
  geom_ribbon(aes(ymin = lower_ic,ymax = upper_ic), alpha=0.1) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_color_manual(values = c("aquamarine3", "blueviolet", "coral")) +
  scale_fill_manual(values = c("aquamarine3", "blueviolet", "coral")) +
  facet_wrap(~ pregunta) +
  labs(title =  "¿Qué porcentaje de personas en México piensan que NO podrán...?",
       subtitle = "Porcentajes por grupo edad (con intervalos de confianza)", y = "", x = "Edades", 
       caption = "Fuente: Elaboración propia - ENCO Julio 2022") +
  adrix_theme +
  theme(strip.text.x = element_text(size = 10),
        legend.position = "none")
  

# Gráfica de ahorros 
tempo <- mk_data_intervalos(clean_cuestbas, "p10")  %>% 
  filter(respuesta %in% c("Sí"))  

ggplot(tempo, aes(x = grupo_edad, y = mean, group = pregunta, color = pregunta, fill = pregunta)) +
  geom_line(size = 1.4) +
  geom_point(size = 2.6) +
  geom_text(aes(label = paste0(c(as.character(round(mean, digits = 2)*100)), "%")),
            family = "Roboto Slab", vjust = -1.2, color =  "black") +
  geom_ribbon(aes(ymin = lower_ic,ymax = upper_ic), alpha=0.1) +
  labs(title =  "¿Qué porcentaje de personas en México afirma poder ahorrar una parte de sus ingresos?",
       subtitle = "Porcentajes por grupo edad (con intervalos de confianza)", y = "", x = "Edades", 
       caption = "Fuente: Elaboración propia - ENCO Julio 2022") +
  adrix_theme +
  theme(legend.position = "none")


# Grafs expectativas

tempo <- mk_data_intervalos(clean_cuestbas, "p2")  %>% 
  bind_rows(mk_data_intervalos(clean_cuestbas, "p4")) %>% 
  bind_rows(mk_data_intervalos(clean_cuestbas, "p6")) %>% 
  filter(respuesta != "No sabe") %>% 
  mutate(pregunta = case_when(
           pregunta == "p2" ~ "Su situación económica personal",
           pregunta == "p4" ~ "La situación económica de los otros miembros de su hogar",
           pregunta == "p6" ~ "La situación económica del país"),
         respuesta = case_when(
           respuesta == "Peor o mucho peor" ~ "Será peor o mucho peor",
           respuesta == "Mejor o mucho mejor" ~ "Será mejor o mucho mejor",
           respuesta == "Igual" ~ "Será igual")) %>% 
  mutate(respuesta = factor(respuesta, levels = c("Será peor o mucho peor", "Será igual", "Será mejor o mucho mejor")),
         pregunta = factor(pregunta, levels = c("Su situación económica personal",
                                                "La situación económica de los otros miembros de su hogar", 
                                                "La situación económica del país")))

ggplot(tempo, aes(x = grupo_edad, y = mean, group = respuesta, fill = respuesta)) +
  geom_col() +
  facet_wrap(~pregunta)  +
  geom_text(aes(label = paste0(c(as.character(round(mean, digits = 2)*100)), "%")),
            family = "Roboto Slab", color =  "black",
            position = position_stack(vjust = .5)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_manual(values = c("aquamarine3", "blueviolet", "coral")) +
  labs(title =  "¿Cuál es la expectativa de las personas para...?",
       subtitle = "Porcentajes por grupo edad (con intervalos de confianza)", y = "", x = "Edades", 
       caption = "Fuente: Elaboración propia - ENCO Julio 2022") +
  adrix_theme +
  theme(strip.text.x = element_text(size = 10),
        legend.title = element_blank())

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
