
rm(list=ls())
options(scipen=999)

library(pacman)

p_load('abind',
       'vars',
       'forecast',
       'readr',
       'dplyr',
       'tidyverse', 
       'vars',
       'forecast',
       'MASS',
       'leaps',
       'caret',
       'purrr')


#Base de datos 

df_metas <- read.csv('C:/Users/diego/OneDrive/Escritorio/Modelo_SAT/SAT/Modelo PCA/20210830_BaseMetas2021.csv')


dplyr::glimpse(df_metas)

list(colnames(df_metas))


missings <-   df_metas  %>% 
              summarise_all(~ sum((is.na(.)/sum(n()))*100))   %>% 
              gather(key=colnames, value =value)



missings




#8 areas generadoras
#adr_interno, adr_externo, aggc_interno, aggc_externo
#agh_interno, agh_externo, agh_interno_sinpemex, agh_externo_sinpemex


v_var_y <- c( "adr_interno"    ,                                                             
              "adr_externo"    ,                                                           
              "aggc_interno"   ,                                                           
              "aggc_externo"   ,                                                           
              "agh_interno"    ,                                                           
              "agh_externo"    ,                                                           
              "agh_interno_sinpemex" ,                                                       
              "agh_externo_sinpemex",
              "pemex_interno" ,
              "pemex_externo",
               "OTROS")


####  1: Datos: Data Frame´s, tranformaciones, etc #### 

#Variables Independientes (series de tiempo)
v_Y     <-  df_metas[,v_var_y] %>% 
            mutate_all(~ ts(. , start = 2011, frequency =  12))

v_X      <- df_metas[27:82] %>% 
            mutate_all(~ ts(. , start = 2011, frequency =  12))

#Variables Y y X en logaritmo
v_X_In  <-  df_metas[83:138] %>% 
            mutate_all(~ ts(. , start = 2011, frequency =  12))



v_Y_In  <-  df_metas[,16:26]  %>% 
            mutate_all(~ ts(. , start = 2011, frequency =  12))


#Agrupamos las variables indepedientes con las dependientes logaritmos
Datos_s <- cbind(v_Y,v_Y_In, v_X, v_X_In)


#Variables independientes en nivel y log 
v_X_In   <- cbind(v_X , v_X_In)



#### 2. Stepwise Regression #### 

#Mejoras (valdria la pena ver otros metodos de reduccion de dimensiones)

#Función de Stepwise


Step_reg_coef <- function( df_X,  df_Y, m = "forward"){
   
 
 df_X <- as.data.frame(df_X)
 df_Y <- as.data.frame(df_Y) 
 df_results <- data.frame()
   
 for (i in 1:length(df_Y)){  
 
    X_fwd             <- regsubsets(df_Y[,i] ~ . , data = df_X,
                                    nvmax = 20, method = m)
    #Summary of step-wise
    v_r_sw_sum        <- summary(X_fwd)
    
    #Modelo con el mejor BIC
    num               <- which.min(summary(X_fwd)[[6]])
    
    #Variables seleccionadas por step-wise
    v_variables_step  <- names(coef(X_fwd,num ))[1:num + 1]
 
    
    v_variables_step  <- (v_variables_step %>%
                                         str_remove("v_X_ln.")) 
    v_variables_step  <- gsub(",",",",toString(v_variables_step))
  
    #Creamos Data frame con mejores variables X para cada Y
    Y <- names(df_Y)
    df_results[i,'Y']  <- Y[i]
    df_results[i,'Min_BIC'] <- c(which.min(summary(X_fwd)$bic))
    df_results[i,'Min_Cp' ] <- c(which.min(summary(X_fwd)$cp))
    df_results[i,'R_sq' ]   <- c(which.max(summary(X_fwd)$rsq))
    df_results[i,'R_sq_adj' ] <- c(which.max(summary(X_fwd)$adjr2))
    
    df_results[i, 'v_var_selec'] <- c(v_variables_step )
   
 }
 
  return(df_results)
}

#Implementamos función Step_reg ---------------------------------------------

#Data frame con las variables discriminadas para cada variable independiente por SW
df_step_coef_back       <- Step_reg_coef(df_X = v_X_In , df_Y =  v_Y_In ,  m = 'backward')
df_step_coef_forward    <- Step_reg_coef(df_X = v_X_In , df_Y =  v_Y_In ,  m = 'forward')
df_step_coef_seqrep     <- Step_reg_coef(df_X = v_X_In , df_Y =  v_Y_In ,  m = "seqrep")



#### 3. Discriminación estadístiva y económica para cada una de las   Áreas Recaudadoras #### 

#### 3.1 Administración Desconcentrada de Recaudación interna: log(ln_adr_interno) ####


#Variable dependiente
Tau          <- ts(v_Y_In[,"ln_adr_interno"], start = 2011, frequency = 12)

#Obtenemos variables del stepwise  para Y  -----------------------------------

v_name_X_sw <- strsplit(df_step_coef_seqrep[df_step_coef_seqrep$Y=="ln_adr_interno", c('v_var_selec')],
                ",") 

v_name_X_sw <- gsub(" ","",c(v_name_X_sw[[1]]))

print(v_name_X_sw)


#Variables full SW
X_adr_sw <- v_X_In[ , c(v_name_X_sw)]


#Variables económicas 

X_adr <- v_X_In[, c("ln_igaee_sec_const" ,
                    "ln_igaee_sec_manuf" ,
                    "ln_igaee_ter"       , 
                    "ln_imp_total_nopetro"    ,
                    "ln_exp_no_petro"    ,
                    "ln_remesas"          ,
                    "ln_lg3_tasa_desoc")]



#Componentes Principales (tomamos el primer componente)
X_cp_adr    <- princomp(scale(X_adr))$scores[,1]
X_cp_adr_sw <- princomp(scale(X_adr_sw))$scores[,1]

#Juntamos datos

Datos_adr_int    <- cbind(Tau, X_cp_adr)
Datos_adr_int_sw <- cbind(Tau , X_cp_adr_sw)

colnames(Datos_adr_int) <- c("ln_adr_interno", colnames(Datos_adr_int)[2])
colnames(Datos_adr_int_sw) <- c("ln_adr_interno_sw", colnames(Datos_adr_int_sw)[2])

#### 3.2 Administración  General de Grandes contribuyentes interna: log(ln_aggc_interno) ####

#Variable dependiente
Tau <- ts(v_Y_In[,"ln_aggc_interno"], start = 2011, frequency = 12)

#Obtenemos variables del stepwise  para Y

v_name_X_sw <- strsplit(df_step_coef_seqrep[df_step_coef_seqrep$Y=="ln_aggc_interno", c('v_var_selec')],
                        ",") 

v_name_X_sw <- gsub(" ","",c(v_name_X_sw[[1]]))

print(v_name_X_sw)

X_aggc_sw <- v_X_In[ , c(v_name_X_sw)]

#Variables económicas 

X_aggc <- v_X_In[,  c("ln_igae",
                      "ln_inv_fija_bruta",
                      "ln_inpc")]


#Componentes Principales (tomamos el primer componente)
X_cp_aggc    <- princomp(scale(X_aggc))$scores[,1]
X_cp_aggc_sw <- princomp(scale(X_aggc_sw))$scores[,1]

#Juntamos datos
Datos_aggc_int <- cbind(Tau, X_cp_aggc)
Datos_aggc_int_sw <- cbind(Tau, X_cp_aggc_sw)

colnames(Datos_aggc_int)    <- c("ln_aggc_interno", colnames(Datos_aggc_int)[2])
colnames(Datos_aggc_int_sw) <- c("ln_aggc_interno_sw", colnames(Datos_aggc_int_sw)[2])

#### 3.3 ln_agh_interno_sinpemex #### 

Tau <- ts(v_Y_In[,"ln_agh_interno_sinpemex"], start = 2011, frequency = 12)

#Obtenemos variables del stepwise  para Y

v_name_X_sw <- strsplit(df_step_coef_seqrep[df_step_coef_seqrep$Y=="ln_agh_interno_sinpemex", c('v_var_selec')],
                        ",") 

v_name_X_sw <- gsub(" ","",c(v_name_X_sw[[1]]))

print(v_name_X_sw)

#Discriminación Step Wise 
X_agh_sw <- v_X_In[ , c(v_name_X_sw)]


#Discrimianción económica
X_agh <- v_X_In[,c("ln_igae",
                   "lg_tipo_cambio",
                   "ln_igaee_sec_ener")] 

#Componentes Principales (tomamos el primer componente)
X_cp_agh    <- princomp(scale(X_agh))$scores[,1]
X_cp_agh_sw <- princomp(scale(X_agh_sw))$scores[,1]
#Juntamos datos
Datos_agh_int    <- cbind(Tau, X_cp_agh)
Datos_agh_int_sw <- cbind(Tau, X_cp_agh_sw)

colnames(Datos_agh_int) <- c("ln_agh_interno_sinpemex", colnames(Datos_agh_int)[2])
colnames(Datos_agh_int_sw) <- c("ln_agh_interno_sinpemex_sw", colnames(Datos_agh_int_sw)[2])

#### 3.4 ln_pemex_interno  #### 

Tau <- ts( v_Y_In[,"ln_pemex_interno"], start = 2011, frequency = 12)

#Obtenemos variables del stepwise  para Y

v_name_X_sw <- strsplit(df_step_coef_seqrep[df_step_coef_seqrep$Y=="ln_pemex_interno", c('v_var_selec')],
                        ",") 

v_name_X_sw <- gsub(" ","",c(v_name_X_sw[[1]]))

print(v_name_X_sw)

#Discriminación Step Wise 
X_pemex_sw <- v_X_In[ , c(v_name_X_sw)]


#Discriminaión ecnómica

X_pemex <- v_X_In[,c("ln_igaee_sec_ener",
                     "lg_tipo_cambio",
                     "Promedio_combustible",
                     "ln_combustibles")]

#Componentes Principales (tomamos el primer componente)
X_cp_pemex <- princomp(scale(X_pemex))$scores[,1]

X_cp_pemex_sw <- princomp(scale(X_pemex_sw))$scores[,1]

#Juntamos datos
Datos_pemex_int <- cbind(Tau, X_cp_pemex)
Datos_pemex_int_sw <- cbind(Tau, X_cp_pemex_sw)

colnames(Datos_pemex_int) <- c("ln_pemex_interno", colnames(Datos_pemex_int)[2])
colnames(Datos_pemex_int_sw) <- c("ln_pemex_interno_sw", colnames(Datos_pemex_int_sw)[2])




#### 3.5 ln_adr_externo ####

Tau <- ts(v_Y_In[,"ln_adr_externo"], start = 2011, frequency = 12)


#Obtenemos variables del stepwise  para Y

v_name_X_sw <- strsplit(df_step_coef_seqrep[df_step_coef_seqrep$Y=="ln_adr_externo", c('v_var_selec')],
                        ",") 

v_name_X_sw <- gsub(" ","",c(v_name_X_sw[[1]]))

print(v_name_X_sw)

#Discriminación Step Wise 
X_adr_ext_sw <- v_X_In[ , c(v_name_X_sw)]

#Discrminación económica 
X_adr_ext <-      v_X_In[,c("ln_igae"
                  , "ln_tipo_cambio"
                  , "ln_exp_no_petro")]

#Componentes Principales (tomamos el primer componente)
X_cp_adr_ext <- princomp(scale(X_adr_ext))$scores[,1]
X_cp_adr_ext_sw <- princomp(scale(X_adr_ext_sw))$scores[,1]

#Juntamos datos
Datos_adr_ext <- cbind(Tau, X_cp_adr_ext)
Datos_adr_ext_sw <- cbind(Tau, X_cp_adr_ext_sw)

colnames(Datos_adr_ext) <- c("ln_adr_externo", colnames(Datos_adr_ext)[2])
colnames(Datos_adr_ext) <- c("ln_adr_externo_sw", colnames(Datos_adr_ext_sw)[2])

#### 3.6 ln_aggc_externo #### 

Tau <- ts(v_Y_In[,"ln_aggc_externo"], start = 2011, frequency = 12)

#Obtenemos variables del stepwise  para Y

v_name_X_sw <- strsplit(df_step_coef_seqrep[df_step_coef_seqrep$Y=="ln_aggc_externo", c('v_var_selec')],
                        ",") 

v_name_X_sw <- gsub(" ","",c(v_name_X_sw[[1]]))

print(v_name_X_sw)
#Discriminación Step Wise 
X_aggc_sw <- v_X_In[ , c(v_name_X_sw)]


#Discriminación Económica 
X_aggc <- v_X_In[, c("ln_igaee_ter"
                   , "ln_exp_no_petro"
                   , "ln_tipo_cambio",
                      "ln_igae_prim")]

#Componentes Principales (tomamos el primer componente)
X_cp_aggc    <- princomp(scale(X_aggc))$scores[,1]
X_cp_aggc_sw <- princomp(scale(X_aggc_sw))$scores[,1]

#Juntamos datos
Datos_aggc_ext    <- cbind(Tau, X_cp_aggc)
Datos_aggc_ext_sw <- cbind(Tau, X_cp_aggc_sw)

colnames(Datos_aggc_ext) <- c("ln_aggc_externo", colnames(Datos_aggc_ext)[2])

colnames(Datos_aggc_ext_sw) <- c("ln_aggc_externo_sw", colnames(Datos_aggc_ext_sw)[2])

#### 3.7 ln_agh_externo_sinpemex ####

Tau   <- ts(v_Y_In[,"ln_agh_externo_sinpemex"], start = 2011, frequency = 12)


#Obtenemos variables del stepwise  para Y
v_name_X_sw <- strsplit(df_step_coef_seqrep[df_step_coef_seqrep$Y=="ln_agh_externo_sinpemex", c('v_var_selec')],
                        ",") 

v_name_X_sw <- gsub(" ","",c(v_name_X_sw[[1]]))

print(v_name_X_sw)

#Discriminación Step Wise 
X_agh_sw <- v_X_In[ , c(v_name_X_sw)]

#Discriminación Económica
X_agh <- v_X_In[,c("ln_igae"
                  , "lg_tipo_cambio"
                  , "Promedio_combustible")]



#Componentes Principales (tomamos el primer componente)
X_cp_agh    <- princomp(scale(X_agh))$scores[,1]
X_cp_agh_sw <- princomp(scale(X_agh_sw))$scores[,1]

#Juntamos datos
Datos_agh_ext    <- cbind(Tau, X_cp_agh)
Datos_agh_ext_sw <- cbind(Tau, X_cp_agh_sw)

colnames(Datos_agh_ext) <- c("ln_agh_externo_sinpemex", colnames(Datos_agh_ext)[2])
colnames(Datos_agh_ext_sw) <- c("ln_agh_externo_sinpemex_sw", colnames(Datos_agh_ext_sw)[2])

#### 3.8 ln_pemex_externo ####

Tau        <- ts( v_Y_In[,"ln_pemex_externo"], start = 2011, frequency = 12)

v_name_X_sw <- strsplit(df_step_coef_seqrep[df_step_coef_seqrep$Y=="ln_pemex_externo",
                                            c('v_var_selec')], ",") 

v_name_X_sw <- gsub(" ","",c(v_name_X_sw[[1]]))

print(v_name_X_sw)

#Discriminación Step Wise 
X_pemex_sw <- v_X_In[ , c(v_name_X_sw)]

#Discriminación ecnómica 
X_pemex    <-   v_X_In[,c("ln_igae"
                    , "lg_tipo_cambio"
                    , "ln_precio_prom_petr_crudo")]

#Componentes Principales (tomamos el primer componente)
X_cp_pemex    <- princomp(scale(X_pemex))$scores[,1]
X_cp_pemex_sw <- princomp(scale(X_pemex_sw))$scores[,1]
#Juntamos datos
Datos_pemex_ext    <- cbind(Tau, X_cp_pemex)

Datos_pemex_ext_sw <- cbind(Tau, X_cp_pemex_sw)


colnames(Datos_pemex_ext)    <- c("ln_pemex_externo", colnames(Datos_pemex_ext)[2])

colnames(Datos_pemex_ext_sw) <- c("ln_pemex_externo_sw", colnames(Datos_pemex_ext_sw)[2])


#### 4. MODELO VAR ####



#### 4.1 Interno ####
#Lag optimo
L_adr   <- VARselect(Datos_adr_int, type = "const", season = 12)$selection["AIC(n)"] 
L_aggc  <- VARselect(Datos_aggc_int, type = "const", season = 12)$selection["AIC(n)"] 
L_agh   <- VARselect(Datos_agh_int, type = "const", season = 12)$selection["AIC(n)"] 
L_pemex <- VARselect(Datos_pemex_int, type = "const", season = 12)$selection["AIC(n)"] 

#Modelo VAR
VAR_adr_int   <- VAR(Datos_adr_int, season = 12, p = L_adr)
VAR_aggc_int  <- VAR(Datos_aggc_int, season = 12, p = L_aggc)
VAR_agh_int   <- VAR(Datos_agh_int, season = 12, p = L_agh)
VAR_pemex_int <- VAR(Datos_pemex_int, season = 12, p = L_pemex)

# 4.2 Interno SW --------------------

#Lag optimo Step wise 
L_adr_sw   <- VARselect(Datos_adr_int_sw  , type = "const", season = 12)$selection["AIC(n)"] 
L_aggc_sw  <- VARselect(Datos_aggc_int_sw , type = "const", season = 12)$selection["AIC(n)"] 
L_agh_sw   <- VARselect(Datos_agh_int_sw  , type = "const", season = 12)$selection["AIC(n)"] 
L_pemex_sw <- VARselect(Datos_pemex_int_sw , type = "const", season = 12)$selection["AIC(n)"] 

#Modelo VAR
VAR_adr_int_sw   <- VAR(Datos_adr_int_sw  , season = 12, p = L_adr)
VAR_aggc_int_sw  <- VAR(Datos_aggc_int_sw , season = 12, p = L_aggc)
VAR_agh_int_sw   <- VAR(Datos_agh_int_sw  , season = 12, p = L_agh)
VAR_pemex_int_sw <- VAR(Datos_pemex_int_sw, season = 12, p = L_pemex)

#### 4.3 Externo ####

#Lag optimo
L_adr   <- VARselect(Datos_adr_ext,   type = "const", season = 12)$selection["AIC(n)"] 
L_aggc  <- VARselect(Datos_aggc_ext,  type = "const", season = 12)$selection["AIC(n)"] 
L_agh   <- VARselect(Datos_agh_ext,   type = "const", season = 12)$selection["AIC(n)"] 
L_pemex <- VARselect(Datos_pemex_ext, type = "const", season = 12)$selection["AIC(n)"] 



#Modelo VAR
VAR_adr_ext   <- VAR(Datos_adr_ext, season = 12,   p = L_adr )
VAR_aggc_ext  <- VAR(Datos_aggc_ext, season = 12,  p = L_aggc )
VAR_agh_ext   <- VAR(Datos_agh_ext, season = 12,   p = L_agh  )
VAR_pemex_ext <- VAR(Datos_pemex_ext, season = 12, p = L_pemex)

# 4.4 Externo SW -------------------------------------------------------------------
#Lag optimo
L_adr_sw  <- VARselect(Datos_adr_ext_sw,   type = "const", season = 12)$selection["AIC(n)"] 
L_aggc_sw <- VARselect(Datos_aggc_ext_sw,  type = "const", season = 12)$selection["AIC(n)"] 
L_agh_sw  <- VARselect(Datos_agh_ext_sw,   type = "const", season = 12)$selection["AIC(n)"] 
L_pemex_sw<- VARselect(Datos_pemex_ext_sw, type = "const", season = 12)$selection["AIC(n)"] 



#Modelo VAR
VAR_adr_ext_sw   <- VAR(Datos_adr_ext_sw, season = 12,   p = L_adr_sw  )
VAR_aggc_ext_sw  <- VAR(Datos_aggc_ext_sw, season = 12,  p = L_aggc_sw )
VAR_agh_ext_sw   <- VAR(Datos_agh_ext_sw, season = 12,   p = L_agh_sw)
VAR_pemex_ext_sw <- VAR(Datos_pemex_ext_sw, season = 12, p = L_pemex_sw)

#### 5.Prónóstico #### 

h <- 12

#### 5.1 Interno ####
Pred_adr_int   <- predict(VAR_adr_int,  n.ahead = h, interval="confidence", level=.80)
Pred_aggc_int  <- predict(VAR_aggc_int,  n.ahead = h, interval="confidence", level=.80)
Pred_agh_int   <- predict(VAR_agh_int,  n.ahead = h, interval="confidence", level=.80)
Pred_pemex_int <- predict(VAR_pemex_int,  n.ahead = h, interval="confidence", level=.80)

#### 5.2 Externo ####
Pred_adr_ext   <- predict(VAR_adr_ext,  n.ahead = h, interval="confidence", level=.80)
Pred_aggc_ext  <- predict(VAR_aggc_ext,  n.ahead = h, interval="confidence", level=.80)
Pred_agh_ext   <- predict(VAR_agh_ext,  n.ahead = h, interval="confidence", level=.80)
Pred_pemex_ext <- predict(VAR_pemex_ext,  n.ahead = h, interval="confidence", level=.80)
#5.3 Interno sw -----------------------------------------------------------

Pred_adr_int_sw  <- predict(VAR_adr_int_sw, n.ahead = h, interval="confidence", level=.80)
Pred_aggc_int_sw <- predict(VAR_aggc_int_sw,  n.ahead = h, interval="confidence", level=.80)
Pred_agh_int_sw  <- predict(VAR_agh_int_sw, n.ahead = h, interval="confidence", level=.80)
Pred_pemex_int_sw<- predict(VAR_pemex_int_sw,  n.ahead = h, interval="confidence", level=.80)

#5.2 Externo -----------------------------------------------------------------

Pred_adr_ext_sw   <- predict(VAR_adr_ext_sw,    n.ahead = h, interval="confidence", level=.80)
Pred_aggc_ext_sw  <- predict(VAR_aggc_ext_sw,  n.ahead = h, interval="confidence", level=.80)
Pred_agh_ext_sw   <- predict(VAR_agh_ext_sw,    n.ahead = h, interval="confidence", level=.80)
Pred_pemex_ext_sw <- predict(VAR_pemex_ext_sw,n.ahead = h, interval="confidence", level=.80)




###### LOG ####

a_int <-       tibble("adr_interno"  = (Pred_adr_int$fcst$ln_adr_interno[,1])
                      ,        "aggc_interno" = (Pred_aggc_int$fcst$ln_aggc_interno[,1])
                      ,        "agh_interno_sinpemex"  = (Pred_agh_int$fcst$ln_agh_interno_sinpemex[,1])
                      ,        "pemex_interno" = (Pred_pemex_int$fcst$ln_pemex_interno[,1]))




a_ext <-       tibble(  "adr_externo" = (Pred_adr_ext$fcst$ln_adr_externo[,1])
                        ,  "aggc_externo" = (Pred_aggc_ext$fcst$ln_aggc_externo[,1])
                        ,  "agh_externo_sinpemex"  = (Pred_agh_ext$fcst$ln_agh_externo_sinpemex[,1])
                        ,  "pemex_externo"=(Pred_pemex_ext$fcst$ln_pemex_externo[,1]))


##### Data Frame estimaciones antecedente (Especificación de la presentación equipo PrisVisMa)
df_predict_area_ln  <- cbind(a_int, a_ext)

#Resultados de predicción del stepwise

a_int_SW <-  tibble("adr_interno"  = (Pred_adr_int_sw$fcst$ln_adr_interno_sw[,1])
                      ,        "aggc_interno" = (Pred_aggc_int_sw$fcst$ln_aggc_interno_sw[,1])
                      ,        "agh_interno_sinpemex"  = (Pred_agh_int_sw$fcst$ln_agh_interno_sinpemex_sw[,1])
                      ,        "pemex_interno" = (Pred_pemex_int_sw$fcst$ln_pemex_interno_sw[,1]))




a_ext_SW <-    tibble(  "adr_externo" = (Pred_adr_ext_sw$fcst$Tau[,1])
                        ,  "aggc_externo" = (Pred_aggc_ext_sw$fcst$ln_aggc_externo_sw[,1])
                        ,  "agh_externo_sinpemex"  = (Pred_agh_ext_sw$fcst$ln_agh_externo_sinpemex_sw[,1])
                        ,  "pemex_externo"=(Pred_pemex_ext_sw$fcst$ln_pemex_externo_sw[,1]))


# Data Frame estimaciones Full SW 
df_predict_area_ln_SW  <- cbind(a_int_SW, a_ext_SW)






# TOTAL sin OTROS  ------------------------------------------------------------------
df_predict_area <- df_predict_area %>%  
                   mutate(total_rec =  rowSums(.))  

df_predict_area_SW <- df_predict_area_SW %>%  
                       mutate(total_rec =  rowSums(.))  
            
v_Y <- v_Y  %>%  
       select(-c("agh_interno" , "agh_externo")) %>% 
       mutate(total_rec =  rowSums(.))  

million <- 1000000000


