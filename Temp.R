###########################################
#
# Cercles climatiques des villes françaises (2023)
# sources : meteo.gouv.fr, inspiré par : Dr. Dominic Royé, réalisation : Jarry Justine, nov. 2024
#
###########################################

# 1. Chargement des librairies

if(!require("tidyverse")) install.packages("tidyverse")
if(!require("fs")) install.packages("fs")
if(!require("lubridate")) install.packages("lubridate")

library(tidyverse)
library(lubridate)
library(fs)
library(readr)
library(dplyr)
library(ggplot2)

# 2. Import des données

rm(list = ls())
setwd("C:/Users/jarry/OneDrive/Documents/R/Temperatures/data")
getwd()
list.files()

# Jointure entre plusieurs csv
files <- list.files(path = "C:/Users/jarry/OneDrive/Documents/R/Temperatures/data2", 
                    pattern = "*.csv", 
                    full.names = TRUE) 
list_data <- lapply(files, read_csv)
meteo <- bind_rows(list_data)

meteo <- meteo %>% 
  mutate(nom_usuel = case_when(
    nom_usuel == "LUXEMBOURG" ~ "Paris", 
    nom_usuel == "RENNES GALLET" ~ "Rennes", 
    nom_usuel == "VILLENEUVE-LES-MAG-INRAE" ~ "Montpellier",
    nom_usuel == "LILLE-LESQUIN" ~ "Lille",
    nom_usuel == "STRASBOURG - BOTANIQUE" ~ "Strasbourg",
    nom_usuel == "CRAN-GEVRIER" ~ "Annecy",
    TRUE ~ nom_usuel ))

stats_names <- unique(meteo$nom_usuel)
stats_names

meteo <- meteo %>% 
  mutate(date = as.Date(as.character(aaaammjj), format = "%Y%m%d")) %>% 
  rename(nom = nom_usuel)

colnames(meteo)
str(meteo)

meteo <- meteo %>% 
  mutate(tmm = (tx + tn) / 2)
meteo2<- meteo %>% 
  select(nom, date, tx, tn, tmm, rr) 

col_temp <- c("#830f74","#974ea8",
              "#0b144f","#0e2680","#223b97","#1c499a","#2859a5",
              "#1b6aa3","#1d9bc4","#1ca4bc","#64c6c7","#86cabb",
              "#91e0a7","#c7eebf","#ebf8da","#f6fdd1","#fdeca7",
              "#f8da77","#fcb34d","#fc8c44","#f85127","#f52f26",
              "#d10b26","#9c042a","#760324","#18000c")

grid_x <- tibble(x = seq(min(meteo2$date), max(meteo2$date), by = "month"), 
                 y = rep(-10, length.out = length(seq(min(meteo2$date), max(meteo2$date), by = "month"))), 
                 xend = seq(min(meteo2$date), max(meteo2$date), by = "month"), 
                 yend = rep(41, length.out = length(seq(min(meteo2$date), max(meteo2$date), by = "month"))))
print(grid_x)

theme_cc <- function(){ 
  theme_minimal(base_family = "sans") %+replace% 
    theme( 
      plot.title = element_text(hjust = 0.5, 
                                colour = "white", 
                                size = 30, margin = margin(b = 20)), 
      plot.caption = element_text(colour = "white",
                                  size = 9, hjust = .5, 
                                  vjust = -30), 
      plot.background = element_rect(fill = "black"), 
      plot.margin = margin(0.5, 0.5, 0.5, 0.5, unit = "cm"), 
      
      axis.text.x = element_text(face = "italic", colour = "white"), 
      axis.title.y = element_blank(), 
      axis.text.y = element_blank(), 
      
      legend.title = element_text(colour = "white"), 
      legend.position = "bottom", 
      legend.justification = "center",
      legend.text = element_text(colour = "white"), 
      
      strip.text = element_text(colour = "white", 
                                face = "bold", size = 14), 
      
      panel.spacing.y = unit(1, "lines"), 
      panel.background = element_rect(fill = "black"), 
      panel.grid = element_blank() 
      ) 
  }

Annecy <- filter(meteo2, nom == "Annecy") 
ggplot(Annecy) + 
  geom_linerange(aes(date, 
                     ymax = tx, 
                     ymin = tn, 
                     colour = tmm),
                 size=0.5, 
                 alpha = .7) + 
  scale_y_continuous(breaks = seq(-30, 50, 10), 
                     limits = c(-11, 42), 
                     expand = expansion()) +
  scale_colour_gradientn(colours = col_temp, 
                         limits = c(-12, 35), 
                         breaks = seq(-12, 34, 5)) + 
  scale_x_date(date_breaks = "month",
               date_labels = "%b") +
  labs(title = "Temperatures à Annecy en 2023", 
       colour = "Moyenne journalière
des temperatures") 

Montpellier <- filter(meteo2, nom == "Montpellier") 


ggplot(Montpellier) + 
  geom_linerange(aes(date, 
                     ymax = tx, 
                     ymin = tn, 
                     colour = tmm),
                 size=0.5, 
                 alpha = .7) + 
  scale_y_continuous(breaks = seq(-30, 50, 10), 
                     limits = c(-11, 42), 
                     expand = expansion()) +
  scale_colour_gradientn(colours = col_temp, 
                         limits = c(-12, 35), 
                         breaks = seq(-12, 34, 5)) + 
  scale_x_date(date_breaks = "month",
               date_labels = "%b") +
  coord_polar()+
  labs(title = "Temperatures à Montpellier en 2023", 
       colour = "Moyenne journalière
des temperatures") 

ggplot(meteo2) + 
  geom_hline(yintercept = c(-10, 0, 10, 20, 30, 40), 
             colour = "white", 
             size = .4) +
  geom_segment(data = grid_x , 
               aes(x = x, 
                   y = y, 
                   xend = xend, 
                   yend = yend), 
               linetype = "dashed", 
               colour = "white", 
               size = .2) +
  geom_linerange(aes(date, 
                     ymax = tx, 
                     ymin = tn, 
                     colour = tmm),
                 size=0.5, 
                 alpha = .7) + 
  scale_y_continuous(breaks = seq(-30, 50, 10), 
                     limits = c(-11, 42), 
                     expand = expansion())+
  scale_colour_gradientn(colours = col_temp, 
                         limits = c(-12, 35), 
                         breaks = seq(-12, 34, 5)) + 
  scale_x_date(date_breaks = "month", 
               date_labels = "%b") +
  guides(colour = guide_colourbar(barwidth = 15,
                                  barheight = 0.5, 
                                  title.position = "top",
                                  title.hjust = 0.5)
  ) +
  facet_wrap(~nom, nrow = 3) +
  coord_polar() + 
  labs(title = "Cercles climatiques", 
       colour = "Température moyenne journalière")+
  theme_cc()

ggsave("graphique.png", width = 12, height = 10, dpi = 300)