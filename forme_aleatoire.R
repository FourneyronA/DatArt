# . -------------------------------------------------------------------------- =============
# 1 - Librairie ====
# . -------------------------------------------------------------------------- =============

pkgs <-  c("dplyr","tidyverse","concaveman","gganimate", "mapview", "ggplot2", "raster","sf", "tmap", "gifski") # "rgdal", "tidyr", "tiff"

if (length(setdiff(pkgs, rownames(installed.packages()))) > 0) {
  # installation des packages 
  install.packages(setdiff(pkgs, rownames(installed.packages())))  
} 
# chargement des packages 
lapply(pkgs, library, character.only = TRUE)
rm(pkgs)

# . -------------------------------------------------------------------------- =============
# 2 - Generation des formes ====
# . -------------------------------------------------------------------------- =============

# lecture d'un polygone 

REG <- st_read("C:/Users/fa101525/Desktop/GitHub/analyse_festosh/data/AURA_L93.shp")

# Fonction génératrice de forme 
# Emprise = référence géographique 
# step = nombre de point dans la zone d'emprise 
# nb_forme = nombre de forme différente pour chaque point

generation_form <- function(emprise, step, nb_form){
  # emprise <- REG
  # step <- 3
  # nb_form <- 15
  
  geom_final <- data.frame()
  
  for(z in 1:nb_form){
    # selection d'une zone aleatoire
    p_exact = emprise %>%
      st_buffer( dist =  -20000) %>%
      st_sample(1, exact = TRUE) %>%
      st_as_sf()  %>%
      st_buffer( dist =  20000)
    
    print(paste("Creation de la forme :", z))
    
    for(i in 1:step){
      geom_form <- st_sample(p_exact, 5, exact = TRUE) %>%
        st_as_sf() %>% 
        concaveman() %>%
        st_buffer( dist = 2000 ) %>%
        mutate( "id" = z,
                "step" = i)
      print(paste("Step :", i))
      
      geom_final <- geom_final %>%
        rbind(geom_form)
      
     
      
    }
  }
  return(geom_final)
  
}

# Génération de polygone aléatoire 

Data_geom <- generation_form(REG, 1, 300)

# . -------------------------------------------------------------------------- =============
# 3 - Data Visualisation ====
# . -------------------------------------------------------------------------- =============

ggplot(Data_geom) +
  geom_sf( fill = "#575756", color = "#e7237f", lwd = 1, alpha = 0.5 ) + #aes(fill = as.character(id)
  theme_void()

