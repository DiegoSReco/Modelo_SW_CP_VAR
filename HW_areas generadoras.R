#
#     Metodología Holt Winters
#

library(forecast)
library(imputeTS)

# Datos -------------------------------------------------------------------

#Cargar datos a partir de codigo "Tratamiento_datos"
#Restringimos los datos a julio 2021 proque no tenemos Y's completas todavía

Datos_Y <- Datos_s[1:79,1:9]

Datos_Y <- cbind(Datos_Y[,1:2], na_interpolation(Datos_Y[,3]), Datos_Y[,4:length(Datos_Y)])
colnames(Datos_Y) <- names(Datos_s)[1:length(Datos_Y)]

#Datos_Y[is.na(Datos_Y)] = 0.1



# Holt Winters ------------------------------------------------------------


# Alfa optima -------------------------------------------------------------

Areas  <- c("ADR_interno", "AGGC_interno", "AGH_interno", "Pemex_interno", 
            "ADR_externo", "AGGC_externo", "AGH_externo", "Pemex_externo", "Otros"
            )

for (i in Areas) {
  e <- 1
  alfa <- c()
  print(i)

  for (j in seq(0.05,0.95,0.05)) {
    aust <- ts(Datos_Y[,i], start = c(2015, 1), frequency = 12)
    #fit1 <- hw(aust,seasonal="additive")
    fit2 <- hw(aust, seasonal = "multiplicative", alpha = j)
    
    # nam <- paste("HW_", i, sep = "")
    # assign(nam, fit2)
    
    if (sum(fit2[["fitted"]]-Datos_Y[,i])/nrow(Datos_Y) < e) {
      a <- j
      e <- sum(fit2[["fitted"]]-Datos_Y[,i])/nrow(Datos_Y)
      
      print(a)
      nam_a <- paste("Alfa_", i, sep = "")
      assign(nam_a, a)
    }
    # autoplot(aust) +
    #   autolayer(fit1, series="HW additive forecasts", PI=FALSE) +
    #   autolayer(fit2, series="HW multiplicative forecasts",
    #             PI=FALSE) +
    #   xlab("Year") +
    #   ylab("Visitor nights (millions)") +
    #   ggtitle("International visitors nights in Australia") +
    #   guides(colour=guide_legend(title="Forecast")) 
  }
}

Alfas <- c(Alfa_ADR_interno, Alfa_AGGC_interno, Alfa_AGH_interno, Alfa_Pemex_interno,
           Alfa_ADR_externo, Alfa_AGGC_externo, Alfa_AGH_externo, Alfa_Pemex_externo,
           Alfa_Otros)
           

# Pronosticos -------------------------------------------------------------

for (i in Areas) {
   aust <- ts(Datos_Y[,i], start = c(2015, 1), frequency = 12)
   #fit1 <- hw(aust,seasonal="additive")
   fit2 <- hw(aust,seasonal="multiplicative"
              #, alpha = Alfas[i]
              )
   
   nam <- paste("HW_", i, sep = "")
   assign(nam, fit2)
   # autoplot(aust) +
   #   autolayer(fit1, series="HW additive forecasts", PI=FALSE) +
   #   autolayer(fit2, series="HW multiplicative forecasts",
   #             PI=FALSE) +
   #   xlab("Year") +
   #   ylab("Visitor nights (millions)") +
   #   ggtitle("International visitors nights in Australia") +
   #   guides(colour=guide_legend(title="Forecast"))
   
}



aust <- ts(Datos_Y[,"Otros"], start = c(2015, 1), frequency = 12)

HW_Otros <- hw(aust,seasonal="multiplicative") #no fittea con alfa optima

Pred_HW <- cbind(HW_ADR_interno$mean, HW_AGGC_interno$mean, HW_AGH_interno$mean, HW_Pemex_interno$mean,
  HW_ADR_externo$mean, HW_AGGC_externo$mean, HW_AGH_externo$mean, HW_Pemex_externo$mean, 
  HW_Otros$mean)


Pred_HW_ic <- cbind(HW_ADR_interno$mean, HW_ADR_interno$upper[,2], HW_ADR_interno$lower[,2]
                    , HW_AGGC_interno$mean, HW_AGGC_interno$upper[,2], HW_AGGC_interno$lower[,2]
                    , HW_AGH_interno$mean, HW_AGH_interno$upper[,2], HW_AGH_interno$lower[,2]
                    , HW_Pemex_interno$mean, HW_Pemex_interno$upper[,2], HW_Pemex_interno$lower[,2]
                    , HW_ADR_externo$mean, HW_ADR_externo$upper[,2], HW_ADR_externo$lower[,2]
                    , HW_AGGC_externo$mean, HW_AGGC_externo$upper[,2], HW_AGGC_externo$lower[,2]
                    , HW_AGH_externo$mean, HW_AGH_externo$upper[,2], HW_AGH_externo$lower[,2]
                    , HW_Pemex_externo$mean, HW_Pemex_externo$upper[,2], HW_Pemex_externo$lower[,2]
                    , HW_Otros$mean, HW_Otros$upper[,2], HW_Otros$lower[,2]
                    )
