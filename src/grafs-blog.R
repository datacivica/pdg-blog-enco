#
# Autor: Adrián Lara
# 08-2022
# ------------------------------------------------------------------------------------
# Blog Economía Cheyenne / grafs-blog.R
rm(list = ls())

if(!require(pacman)){install.packages("pacman")}
p_load(tidyverse, data.table, here, cumstats, scales, srvyr, extrafont, ggrepel)

# Archiveros 
ifelse(!dir.exists(file.path(here("grafs"))),
       dir.create(file.path(here("grafs"))), 
       FALSE)

ifelse(!dir.exists(file.path(here("grafs/blog"))),
       dir.create(file.path(here("grafs/blog"))), 
       FALSE)


files <- list(clean_cuestbas = here("clean/clean-cuestbas.rds"),
              graf_aspiraciones_general = here("grafs/blog/general-aspiraciones.jpg"),
              graf_aspiraciones_edades = here("grafs/blog/aspiraciones-edades.jpg"),
              graf_ahorro = here("grafs/blog/ahorro.jpg"),
              graf_expectativas = here("grafs/blog/expectativas.jpg"),
              graf_ahorro_tipos = here("grafs/blog/ahorro-tipo.jpg"))

# Tema gráfico
loadfonts(quiet = T)

adrix_theme <- theme_bw() +
  theme(legend.position = "top", 
        text = element_text(size = 15, family = "Roboto Slab"),
        plot.title = element_text(size = 24, hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5),
        legend.text = element_text(size = 18), 
        strip.text.x = element_text(size = 19),
        axis.text.x = element_text(size=12.5)) 

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


mk_data_noage <- function(db, inciso){
  # Prepararmos configuración INEGI
  options(survey.lonely.psu="adjust")
  
  x <- db %>%
    as_survey_design(ids = upm, weights = factor) %>%
    srvyr::group_by(across(matches(paste0(inciso, "$"))))%>%
    summarise(mean = survey_mean(), total_grupo = survey_total()) %>% 
    mutate(lower_ic = mean - 1.96 * mean_se, 
           upper_ic = mean + 1.96 * mean_se) %>% 
    mutate(pregunta = inciso)  %>% 
    rename_with(~(x = "respuesta"), matches(inciso, "$")) %>% 
    relocate(pregunta, .before = respuesta)
}
  

# Abrimos y nombramos grupos de edades

clean_cuestbas <- readRDS(files$clean_cuestbas) 

# Graf General 
tempo <- mk_data_noage(clean_cuestbas, "p14") %>% 
  bind_rows(mk_data_noage(clean_cuestbas, "p9")) %>%
  bind_rows(mk_data_noage(clean_cuestbas, "p15")) %>% 
  filter(respuesta %in% c("Sí","No")) %>% 
  mutate(pregunta = case_when(
    pregunta == "p14" ~ "Comprar un automóvil nuevo o usado\nen los próximos 2 años",
    pregunta == "p15" ~ "Comprar, construir o remodelar una\ncasa en los próximos 2 años",
    pregunta == "p9" ~ "Salir de vacaciones el próximo año"))

ggplot(tempo, aes(x = respuesta, y = mean, fill = respuesta)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin = lower_ic, ymax = upper_ic), width=.2,
                position=position_dodge(.9)) +
  geom_text(aes(label = paste0(c(as.character(round(mean, digits = 2)*100)), "%")),
            family = "Roboto Slab", hjust = -0.95, vjust = -0.2, color =  "black") +
  scale_fill_manual(values = c("#9BE8E6", "#CFA6F2")) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  facet_grid(~pregunta) +
  labs(title =  "¿Alguien en este hogar o usted están planeando...?",
       subtitle = "Porcentajes con intervalos de confianza", y = "Porcentaje", x = "", 
       caption = "Fuente: Elaboración propia - ENCO Septiembre 2022", color = "", fill = "") +
  adrix_theme +
  theme(legend.position = "none",
        strip.text.x = element_text(size = 10))

ggsave(files$graf_aspiraciones_general, width = 16, height = 10)

# Graf aspiraciones
  
tempo <- mk_data_intervalos(clean_cuestbas, "p14")  %>% 
  bind_rows(mk_data_intervalos(clean_cuestbas, "p9")) %>%
  bind_rows(mk_data_intervalos(clean_cuestbas, "p15")) %>% 
  filter(respuesta %in% c("Sí", "No")) %>% 
  mutate(pregunta = case_when(
    pregunta == "p14" ~ "Comprar un automóvil nuevo o usado\nen los próximos 2 años",
    pregunta == "p15" ~ "Comprar, construir o remodelar una\ncasa en los próximos 2 años",
    pregunta == "p9" ~ "Salir de vacaciones el próximo año")) 

ggplot(tempo, aes(x = grupo_edad, y = mean, group = respuesta,
                  color = respuesta, fill = respuesta)) +
  geom_line(size = 1.4) +
  geom_point(size = 2.6) +
  geom_text(aes(label = paste0(c(as.character(round(mean, digits = 2)*100)), "%")),
            family = "Roboto Slab", size = 6.5, vjust = -1.2, color =  "black") +
  geom_ribbon(aes(ymin = lower_ic,ymax = upper_ic), alpha=0.1) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_color_manual(values = c("aquamarine3", "blueviolet", "coral")) +
  scale_fill_manual(values = c("aquamarine3", "blueviolet", "coral")) +
  facet_wrap(~ pregunta) +
  labs(title =  "¿Qué porcentaje de personas en México piensan que\n alguien de su hogar o ellas mismas podrán...?",
       subtitle = "Porcentajes por grupo edad (con intervalos de confianza)", y = "", x = "Grupo de edad", 
       caption = "Fuente: Elaboración propia - ENCO Septiembre 2022") +
  adrix_theme +
  guides(fill = guide_legend(reverse = TRUE), color = guide_legend(reverse = TRUE)) +
  theme(strip.text.x = element_text(size = 16),
        legend.title = element_blank())
  
ggsave(files$graf_aspiraciones_edades, width = 16, height = 10)

# Gráfica de ahorros 
tempo <- mk_data_intervalos(clean_cuestbas, "p10")  %>% 
  filter(respuesta %in% c("Sí"))  

ggplot(tempo, aes(x = grupo_edad, y = mean, group = pregunta, color = pregunta, fill = pregunta)) +
  geom_line(size = 1.4) +
  geom_point(size = 2.6) +
  geom_text(aes(label = paste0(c(as.character(round(mean, digits = 2)*100)), "%")),
            family = "Roboto Slab", size = 6, vjust = -1.2, color =  "black") +
  geom_ribbon(aes(ymin = lower_ic,ymax = upper_ic), alpha=0.1) +
  scale_color_manual(values = c("aquamarine3")) +
  scale_fill_manual(values = c("aquamarine3")) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title =  "¿Qué porcentaje de personas en México afirma poder ahorrar una parte de sus ingresos?",
       subtitle = "Porcentajes por grupo edad (con intervalos de confianza)", y = "", x = "Grupo de edad", 
       caption = "Fuente: Elaboración propia - ENCO Septiembre 2022") +
  adrix_theme +
  theme(legend.position = "none")

ggsave(files$graf_ahorro, width = 16, height = 10)


# Grafs expectativas global
tempo <- mk_data_noage(clean_cuestbas, "p2")  %>% 
  bind_rows(mk_data_noage(clean_cuestbas, "p4")) %>% 
  bind_rows(mk_data_noage(clean_cuestbas, "p6")) %>% 
  filter(respuesta != "No sabe") %>% 
  mutate(pregunta = case_when(
    pregunta == "p2" ~ "Su situación económica personal",
    pregunta == "p4" ~ "La situación económica de los otros miembros de su hogar",
    pregunta == "p6" ~ "La situación económica del país"),
    respuesta = case_when(
      respuesta == "Peor o mucho peor" ~ "Será peor o mucho peor",
      respuesta == "Mejor o mucho mejor" ~ "Será mejor o mucho mejor",
      respuesta == "Igual" ~ "Será igual")) %>% 
  mutate(respuesta = factor(respuesta, levels = c( "Será mejor o mucho mejor","Será peor o mucho peor", "Será igual")),
         pregunta = factor(pregunta, levels = c("Su situación económica personal",
                                                "La situación económica de los otros miembros de su hogar", 
                                                "La situación económica del país")))


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
  geom_col(color = "black") +
  geom_text(aes(label = paste0(c(as.character(round(mean, digits = 2)*100)), "%")),
                  family = "Roboto Slab", color =  "white", size = 6, position = position_stack(vjust = 0.5)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  facet_wrap(~ pregunta, ncol = 3) +
  scale_fill_manual(values = c("blueviolet", "#CFA6F2", "aquamarine3")) +
  labs(title =  "En un año, ¿Cómo cree que será..?",
       subtitle = "Porcentajes por grupo edad", y = "", x = "Grupo de edad", 
       caption = "Fuente: Elaboración propia - ENCO Septiembre 2022") +
  adrix_theme +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme(strip.text.x = element_text(size = 16),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 16))

ggsave(files$graf_expectativas, width = 16, height = 10)

# Ahorros general
tempo <- mk_data_noage(clean_cuestbas %>% filter(p14 == "No"), "p10")  %>% 
  mutate(p ="p14") %>% 
  bind_rows(., mk_data_noage(clean_cuestbas %>% filter(p15 == "No"), "p10")%>%  mutate(p ="p15")) %>%
  bind_rows(., mk_data_noage(clean_cuestbas %>% filter(p9 == "No"), "p10") %>%  mutate(p="p9")) %>% 
  bind_rows(., mk_data_noage(clean_cuestbas, "p10") %>% mutate(p="p10")) %>% 
  filter(respuesta %in% c("Sí", "No"))  %>% 
  mutate(etiqueta = factor(p,
                           levels = c("p10", "p14", "p15", "p9"),
                           labels = c("Población en general", "Personas que no piensan comprar un coche",
                                      "Personas que no piensan comprar, remodelar o construir una casa",
                                      "Personas que no piensan salir de vacaciones")))


ggplot(tempo, aes(x = fct_rev(etiqueta), y = mean, group = respuesta, fill = respuesta)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = paste0(c(as.character(round(mean, digits = 2)*100)), "%")),
            family = "Roboto Slab", size = 6, color =  "black",
            position = position_dodge(width = 1), hjust = -.25) +
  coord_flip(ylim = c(0, 0.90)) +
  scale_fill_manual(values = c("#9BE8E6", "#CFA6F2")) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title =  "¿Qué porcentaje de personas en México afirma\npoder ahorrar una parte de sus ingresos?",
       subtitle = "Porcentajes por tipo de expectativas", y = "", x = "", 
       caption = "Fuente: Elaboración propia - ENCO Septiembre 2022") +
  adrix_theme 
  
ggsave(files$graf_ahorro_tipos, width = 16, height = 10)
