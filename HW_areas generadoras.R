

options(scipen=999, pillar.sigfig = 7)

#Librerias ----

if (!require(pacman)) install.packages("pacman") 


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
       'purrr',
       'reshape',
       'tseries', 
       'feasts',
       'imputeTS')

# Datos -------------------------------------------------------------------

##### NOTA: Cargar datos a partir de codigo "Tratamiento_datos" #####

#DF variables Y 
Datos_Y <- Datos_s[,colnames(Datos_s[,1:9])] 


#Interpolación NA en AGH Interno 
#Code for visualization

Datos_Y[,"AGH_interno_impute"] <- na_interpolation(Datos_Y[,"AGH_interno"])

#PlOT  del valor imputado
ggplot_na_imputations(x_with_na = Datos_Y[,"AGH_interno"],
                      x_with_imputations = Datos_Y[,"AGH_interno_impute"])

# #Probamos si el valor NA afecta nuestro modelo --------------
# ts_na      <- ts(Datos_Y[,"AGH_interno"], start = c(2015, 1), frequency = 12)
# ts_impute  <- ts(Datos_Y[,"AGH_interno_impute"], start = c(2015, 1), frequency = 12)
#
# fit_na     <- hw(ts_na , seasonal = "multiplicative"  )
# fit_impute <- hw(ts_impute,seasonal = "multiplicative")
#
# results_impute_kalman        <- fit_impute$model
# results_impute_interpolation <- fit_impute$model


#Datos variables de áreas generadoras (ADR)

Areas  <- c("ADR_interno", "AGGC_interno",  "AGH_interno", "AGH_interno_impute", "Pemex_interno", 
            "ADR_externo", "AGGC_externo", "AGH_externo", "Pemex_externo", "Otros")


# Pronósticos -------------------------------------------------------------

for (i in Areas) {
  
     aust <- ts(Datos_Y[,i], start = c(2015, 1), frequency = 12)
 
    fit2 <- hw(aust,
               seasonal="multiplicative",
               h = 12,
               exponential = T)
  
    nam <- paste("HW_", i, sep = "")
    assign(nam, fit2)
  
}

#Df Predicciones 
  Pred_HW <- tibble("ADR_interno"  = HW_ADR_interno$mean,
                    "AGGC_interno" = HW_AGGC_interno$mean, 
                    "AGH_interno_impute"  = HW_AGH_interno_impute$mean,
                    "Pemex_interno"= HW_Pemex_interno$mean,
                    "ADR_externo"  = HW_ADR_externo$mean, 
                    "AGGC_externo" = HW_AGGC_externo$mean,
                    "AGH_externo"  = HW_AGH_externo$mean, 
                    "Pemex_externo"= HW_Pemex_externo$mean, 
                    "Otros"        = HW_Otros$mean)
  
  #Quitamos interno con NA 
Datos_Y <- Datos_Y %>%  dplyr::select(-c("AGH_interno"))
#Fecha (modificar si se actualiza)
fecha    <- (c(seq(from = as.Date("2015-01-01"), 
                   to = as.Date("2022-12-01"), 
                   by = 'month')))


#DF con predicción y fechas correctas 
Datos_Y_w <- rbind(Datos_Y, Pred_HW)

Datos_Y_w <- cbind(fecha, Datos_Y_w)


# Estimación para 2022  -------  
million <- 1000000

Datos_result_nivel_2022 <-  Datos_Y_w  %>%     
                            filter(fecha >="2022-01-01")    %>% 
                            mutate_at(vars(-one_of("fecha")), exp) %>% 
                            mutate_at(vars(-one_of("fecha")), ~./ million)

#Recaudación total estimada -----

n_recaudación_2022 <- Datos_result_nivel_2022 %>% 
                      dplyr::select( -(fecha)) %>%  
                      mutate(total_rec =  rowSums(.)) %>% 
                      
                      summarise(total = sum(total_rec))

#DFIntervalos de confianza


Pred_HW_ic <- cbind(   HW_ADR_interno$upper[,2],       HW_ADR_interno$lower[,2]
                    ,  HW_AGGC_interno$upper[,2],      HW_AGGC_interno$lower[,2]
                    ,  HW_AGH_interno_impute$upper[,2],HW_AGH_interno_impute$lower[,2]
                    ,  HW_Pemex_interno$upper[,2],     HW_Pemex_interno$lower[,2]
                    ,  HW_ADR_externo$upper[,2],       HW_ADR_externo$lower[,2]
                    ,  HW_AGGC_externo$upper[,2],      HW_AGGC_externo$lower[,2]
                    ,  HW_AGH_externo$upper[,2],       HW_AGH_externo$lower[,2]
                    ,  HW_Pemex_externo$upper[,2],     HW_Pemex_externo$lower[,2]
                    ,  HW_Otros$upper[,2],             HW_Otros$lower[,2])



#PLOT ----
                    
# for (i in c(unique(df_long$"Áreas Generadoras")))  {
name <- paste("Área Generadora: ", "ADR_externo", sep ="")

plot <- Datos_Y_w %>%  
          mutate_at(vars(-one_of("fecha")), exp) %>% 
          mutate_at(vars(-one_of("fecha")), ~./ million) %>% 
  
          mutate(forecast_id = if_else(fecha >="2021-08-01" , "Pronóstico", "Observado")) %>%  
          
          ggplot( aes(x = fecha, y =  ADR_externo)) + 
          geom_path(aes(group = 1)) +
          geom_point(aes(color =forecast_id)) +
          geom_line(aes(linetype = forecast_id, color = forecast_id))+ 
          geom_ribbon(aes(ymin = c(rep(NA,length(Datos_Y_w$fecha)-12),exp(HW_ADR_externo$upper[,2])/million), 
                          ymax = c(rep(NA,length(Datos_Y_w$fecha)-12),exp(HW_ADR_externo$lower[,2])/million)),
                      alpha = 0.3, fill = "darkgreen")  +
         geom_vline(xintercept = as.Date(c('2021-08-01')),colour = "darkgreen" ,linetype="dotdash") + 

          labs(title = name,
               subtitle = "Millones de pesos",
               y = "",
               x = "")+
          #ylim(0,200000)+
           ggthemes::theme_tufte() + 
           scale_x_date(date_labels = "%m-%Y", breaks = "3 month",  limits = as.Date(c('2015-01-01','2022-12-01'))) + 

          theme(plot.title = element_text(hjust = 0.5, lineheight = 1, size=16),
                plot.subtitle = element_text(hjust = 0.5, lineheight = 1, size=12),
                plot.caption = element_text(hjust = 0, size=10),
                                   legend.position = c("bottom"),
                                   text=element_text(size=13, family= "Optima"),
                axis.text.x = element_text(angle = 80, hjust = 1)) +
         scale_color_manual(name = "", values = c( "#691C32", "#6F7271")) +
         scale_linetype_manual(name = "", values = c( 1, 2))
       

ggsave("plot_ADR_externo.png", plot = plot)


  