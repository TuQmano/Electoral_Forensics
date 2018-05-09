########### TUCUMAN. 1999 Provincial Election ##################
########### 
########### Created: 16 September 2017
########### Modify: 9 May  2018
########### By: @TuQmano
########### 


#### NEEDED PACKAGES
library(readr)  # To import data
library(ggplot2) # To plot data
library(hexbin) # For hexagon geom in plot


setwd("~/Desktop/TUC_99/")   # Set working directory


### IMPORT DATA
data <- read_delim("~/Desktop/TUC_99/data.csv",";", escape_double = FALSE, trim_ws = TRUE)

View(data) # Open data frame viewer

# VARIABLE TRANSFORMATIONS

data$TURNOUT <- TOTAL/ELECTORES   #### (re)compute Turnout


data$VALIDOS <- FR+AP+PH+PU+FUT+ALIANZA+FFT+BLANCO  # VALID VOTES (for party lists and blank)


data$FFT_pc <- FFT/data$VALIDOS  ## Frente Fundacional (PJ) percent share vote



###
####
#####     PLOTS
####      
### 

#    (1)  Incumbent & Turnout


ggplot(data, aes(TURNOUT, FFT_pc)) + 
  geom_hex(bins=50) +
  geom_vline(xintercept = .75, colour="red", linetype = "dotdash") +
  geom_hline(yintercept = .5, colour="red", linetype = "dotdash") +
  theme_bw(base_size = 15, base_family = "Times") + 
  scale_fill_gradient(low = "blue", high = "yellow", name="Mesas") +
  labs(title="TUCUMAN 1999", subtitle="GOBERNADOR",
     x="PARTICIPACION (%)", 
     y="Frente Fundacional (lista ganadora)", 
     caption="Cuadrante superior izquierdo = Zona de Riesgo. Seguimos idea de CIPPEC -
     OEAR y definimos como zona sospechosa cuando se trata de mesas con participacion menor al 75% y la lista ganadora obtiene mas de 50%")



#    (2) Runnerer up & blank_vote


# VARIABLE TRANSFORMATIONS
data$FR_pc <- FR/data$VALIDOS  ## Fuerza Republicana percent share vote

data$B_pc <- BLANCO/data$VALIDOS  ## Blank vores share

attach(data)
data$diff <- abs(FFT_pc-FR_pc)  # Absolute distance of 2 main party lists


ggplot(data) + geom_hex(aes(B_pc, diff), bins=50) + 
  geom_vline(xintercept = .05, colour="red", linetype = "dotdash")+
  geom_hline(yintercept = .25, colour="red", linetype = "dotdash")+
  theme_bw(base_size = 15, base_family = "Times") + 
  scale_fill_gradient(low = "blue", high = "yellow", name="Mesas")+
  labs(title="TUCUMAN 1999", subtitle="GOBERNADOR",
       x="Voto BLANCO", y="Diferencia listas principales (FFT & FR)", caption="Cuadrante superior derecho = Zona de Riesgo. Seguimos idea de CIPPEC -
       OEAR y definimos como zona sospechosa cuando se trata de mesas que superan el 5% de voto BLANCO 
       y la  diferencia entre listas es mayor a 25%", family="Times")






