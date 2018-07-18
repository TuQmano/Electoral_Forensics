#
#    Aportes privados de camapaña en la Provincia de Buenos Aires (2017)
#    TuQmano con datos de la CNE
#    www.github.com/tuqmano
#




# CARGAR PAQUETES NECESARIOS

library(readxl)
library(tidyverse) 
library(ggalluvial)
library(ggrepel)

##########################


aportes <- read_excel("~/Desktop/AportesPrivados.xls") # LEER BASE DE DATOS


aportes %>% 
  filter(Eleccion == "Elección Nacional 2017") %>%  ### FILTRAMOS DATOS DE ELECCION 2017
  filter(Distrito == "BUENOS AIRES") %>%             ### FILTRAMOS DATOS DEL DISTRITO BUENOS AIRES  
  group_by(Agrupacion, TipoAporte) %>%               ###  AGRUPAMOS LOS DATOS POR LISTAS Y TIPO DE APORES
  select(9, 19, 22) %>%                              ### SELECCIONAMOS VARIABLES RELEVANTES
  summarise_all(funs(sum))-> e2017                   ### SUMAMOS APORTES Y GENERAMOS UNA NUEVA BASE DE DATOS (e2017)


e2017$ImporteTot <- round(e2017$ImporteTot, digits = 0)  # REDONDEAMOS VALORES


# CONVERTIMOS VARIABLES A TIPO "FACTOR"
e2017$Agrupacion <- as.factor(e2017$Agrupacion)
e2017$TipoAporte <- as.factor(e2017$TipoAporte)



e2017 %>% 
  select(1,3) %>% 
  group_by(Agrupacion) %>% 
  summarise_all(funs(sum)) -> total # CALCULAMOS TOTALES POR AGRUOACION (SIN DIFERENCIAR POR TIPO DE APORTE)


###### PLOT 1

total$ImporteTot <- total$ImporteTot/1000  # PARA CALCULAR POR "MILES DE PESOS"


# GRAFICO DE APORTES PRIVADOS - TOTALES POR AGRUPACION

ggplot(total) +
  geom_col(aes(Agrupacion, ImporteTot, fill=Agrupacion)) +
  scale_y_continuous(breaks = c(30000,70000)) + 
  coord_flip() +
  labs(title = "Aportes privados de campaña", 
       subtitle = "Provincia de Buenos Aires - 2017", 
       x = "", 
       y = "Importe total (miles de pesos)", 
       caption = "@TuQmano con datos de la CNE - https://www.electoral.gov.ar/financiamiento/aportes-privados.php ") +
  theme_bw() +
  theme(legend.position = "none")



###### PLOT 2


e2017$ImporteTot <- e2017$ImporteTot/1000 # PARA CALCULAR POR "MILES DE PESOS"


# GRAFICO DE APORTES PRIVADOS - TOTALES POR AGRUPACION Y TIPO DE FINANCIAMIENTO

ggplot(e2017) +
  geom_col(aes(TipoAporte,ImporteTot, fill = TipoAporte)) +
  scale_y_continuous(breaks = c(10000,70000)) +
  facet_wrap(~Agrupacion) + 
  coord_flip() + 
  labs(title = "Aportes privados de campaña (por tipo de aporte)", 
       subtitle = "Provincia de Buenos Aires - 2017", 
       fill = "Tipo de aporte", 
       x = "", 
       y = "Importe total (miles de pesos)", 
       caption = "@TuQmano con datos de la CNE - https://www.electoral.gov.ar/financiamiento/aportes-privados.php ") +
  theme_bw()
  

#### ALLUVIAL 

sankey <- e2017


sankey %>% 
  filter(Agrupacion %in% c("CAMBIEMOS BUENOS AIRES", "FRENTE DE IZQUIERDA Y DE LOS TRABAJADORES",
                            "FRENTE JUSTICIALISTA","UNIDAD CIUDADANA", "1 PAIS")) -> sankey


colnames(sankey) <-  c("lista","aporte","monto")


sankey$monto <- sankey$monto/1000 # PARA CALCULAR POR "MILES DE PESOS"


ggplot((sankey), aes(y = monto, axis1 = lista, axis2 = aporte)) +
  geom_alluvium(aes(fill = lista), width = 1/6) +
  geom_stratum(width = 1/48, fill = "grey",  color = "black") +
  scale_fill_manual(breaks = c("1 PAIS","CAMBIEMOS BUENOS AIRES", "FRENTE DE IZQUIERDA Y DE LOS TRABAJADORES",
                                 "FRENTE JUSTICIALISTA","UNIDAD CIUDADANA"), 
                      values= c("black", "yellow","red","blue","lightblue"))+
  geom_text_repel(stat = "stratum", label.strata = TRUE, size = 2.5, color = "black", fontface = "bold")  +
  scale_x_discrete(limits = c("Lista", "Tipo de Aporte"), expand = c(.05, .05)) +
  ggtitle("Aportes privados por lista y tipo de aporte") +
  labs(title = "Aportes privados de campaña (por tipo de aporte)", 
       subtitle = "Provincia de Buenos Aires - 2017", 
       fill = "Espacio político", 
       x = "", 
       y = "Importe total (miles de pesos)", 
       caption = "@TuQmano con datos de la CNE - https://www.electoral.gov.ar/financiamiento/aportes-privados.php ") +
  theme_bw()
