#Guia 3-------------------------------------------------------------------------
#Ramo: Estadistica IV
#Estudiantes: Josefa Espinoza y Karla Medina
#Profesora: Maria Constanza Ayala
#Ayudantes: Charo Astorga y Lucas Galanakis
#Fecha: 14- Noveimbre-2022

#Carga de paquetes y ajustes iniciales------------------------------------------
pacman::p_load(haven, 
               lm.beta, 
               dplyr,sjPlot,texreg,sjmisc, tidyverse, table1, psych, moments)
options(scipen=999) 
rm(list=(ls()))

#Carga de datos y exploracion---------------------------------------------------
CEP <- read_dta("input/data/base_85.dta")
names(CEP)
head(CEP)

#Seleccion de variables---------------------------------------------------------
#interes_pol_1_b: ¿Cuan interesado está Ud.en la politica?
#confianza_6_c: A continuacion, le voy a leer los nombres de algunas instituciones.
#De acuerdo con las alternativas de la tarjeta,¿Cuanta confianza tiene Ud. en cada
#una de ellas? PARTIDOS POLITICOS.
#confianza_6_d: A continuacion, le voy a leer los nombres de algunas instituciones.
#De acuerdo con las alternativas de la tarjeta,¿Cuanta confianza tiene Ud. en cada
#una de ellas? TRIBUNALES DE JUSTICIA.
#confianza_6_h: A continuacion, le voy a leer los nombres de algunas instituciones.
#De acuerdo con las alternativas de la tarjeta,¿Cuanta confianza tiene Ud. en cada
#una de ellas? CARABINEROS.
#confianza_6_i: A continuacion, le voy a leer los nombres de algunas instituciones.
#De acuerdo con las alternativas de la tarjeta,¿Cuanta confianza tiene Ud. en cada
#una de ellas? Gobierno.
#confianza_6_k: A continuacion, le voy a leer los nombres de algunas instituciones.
#De acuerdo con las alternativas de la tarjeta,¿Cuanta confianza tiene Ud. en cada
#una de ellas? CONGRESO.

frq(CEP$interes_pol_1_b)
frq(CEP$confianza_6_c)
frq(CEP$confianza_6_d)
frq(CEP$confianza_6_h)
frq(CEP$confianza_6_i)
frq(CEP$confianza_6_k)

#Recodificacion de variables----------------------------------------------------
#Ver NA
table(CEP$interes_pol_1_b, exclude = F)
table(CEP$confianza_6_c, exclude = F)
table(CEP$confianza_6_d, exclude = F)
table(CEP$confianza_6_h, exclude = F)
table(CEP$confianza_6_i, exclude = F)
table(CEP$confianza_6_k, exclude = F)
#Eliminar NA
CEP<- CEP %>%
  select(confianza_6_c, confianza_6_d, confianza_6_h, confianza_6_i, confianza_6_k,
         interes_pol_1_b)  %>%
  mutate_all(., ~(as.numeric(.))) %>%
  mutate_all(.,~case_when(.==88 | .==99 ~ NA_real_, TRUE ~ .))
CEP <- CEP %>%
  drop_na()
dim(CEP) #1361 obs

#Analisis descriptivos----------------------------------------------------------
table1(~ . ,
       data = CEP)

#Evaluacion de supuestos--------------------------------------------------------
skewness(CEP,na.rm=T) #recomendable valores entre -2 y +2
kurtosis(CEP,na.rm=T) #recomendable valores entre -2 y +2

#Histogramas
hist(CEP$interes_pol_1_b)
hist(CEP$confianza_6_c)
hist(CEP$confianza_6_d)
hist(CEP$confianza_6_h)
hist(CEP$confianza_6_i)
hist(CEP$confianza_6_k)

#Shapiro-Wilk
shapiro.test(CEP$interes_pol_1_b) 
#Kolmogorov-Smirnov
ks.test(CEP$interes_pol_1_b, "pnorm") #rechazo H0, es decir, no puedo asegurar una distribución normal
ks.test(CEP$confianza_6_c, "pnorm") 
ks.test(CEP$confianza_6_d, "pnorm") 
ks.test(CEP$confianza_6_h, "pnorm") 
ks.test(CEP$confianza_6_i, "pnorm") 
ks.test(CEP$confianza_6_k, "pnorm") 

#Correlacion lineal entre variables---------------------------------------------
cor_CEP<- cor(CEP)
print(cor_CEP)

#Determinar matriz de correlacion
det(cor(CEP)) #0,3026475

#Prueba de esfericidad barlett
cortest.bartlett(CEP)

#Prueba KMO
KMO(CEP)

#Analisis de componentes principales--------------------------------------------
#Numero de componentes
PCA <- principal(CEP, #la data
                 nfactors = 6, #n componentes=n de variables
                 rotate = "none") #esto lo vamos a profundizar la próxima cápsula
PCA

#Gráfico de Cattell
plot(princomp(CEP, scores=T,cor=T), type="lines")

#Apartir de lo mostrado por el grafico de cattell, hemos decidido quedarnos con el
#primer componente. 

#Rotacion-----------------------------------------------------------------------
#varimax
PCA_varimax <- principal(CEP, #la data
                         nfactors = 1, #n componentes=componentes seleccionados
                         rotate = "varimax") #rotación
PCA_varimax


#Oblimin
PCA_oblimin <- principal(CEP, #la data
                         nfactors = 1, #n componentes=componentes seleccionados
                         rotate = "oblimin") #rotación
PCA_oblimin

#Para  ordenar los resultados
print.psych(PCA_oblimin, cut = 0.3, sort = TRUE)

#Guardar puntuacion------------------------------------------------------------
PCA_scores <- principal(CEP, #la data
                        nfactors = 1, #n componentes=componentes seleccionados
                        rotate = "oblimin", #rotación
                        scores=T, #se generan las puntuaciones
                        method="regression")  #el método para generar puntuaciones

CEP<- cbind(CEP, PCA_scores$scores)
names(CEP)
