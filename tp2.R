#install.packages("quantmod")
#install.packages("tidyquant")
library(tidyquant)
library(tidyverse)
library(quantmod)
library(dplyr)
library(ggplot2)

#Obtención de Datos
#Bolsas
Merval <- c("^MERV") %>%
  tq_get(get = "stock.prices",
         from = "2014-01-01",
         to = "2019-12-01",
         complete_cases = T)
#EEUU
SP500 <-  c("^GSPC") %>%
  tq_get(get = "stock.prices",
         from = "2014-01-01",
         to = "2019-12-02",
         complete_cases = T)

#JapÃ³n
Nikkei225 <- c("^N225") %>%
  tq_get(get = "stock.prices",
         from = "2014-01-01",
         to = "2019-12-02",
         complete_cases = T)

#UK
FTSE100 <- c("^FTSE") %>%
  tq_get(get = "stock.prices",
         from = "2014-01-01",
         to = "2019-12-02",
         complete_cases = T)

#Alemania
DAX <- c("^GDAXI") %>%
  tq_get(get = "stock.prices",
         from = "2014-01-01",
         to = "2019-12-02",
         complete_cases = T)

#España
IBEX35 <- c("^IBEX") %>%
  tq_get(get = "stock.prices",
         from = "2014-01-01",
         to = "2019-12-02",
         complete_cases = T)

#China
SHANGAI <- c("000001.SS") %>%
  tq_get(get = "stock.prices",
         from = "2014-01-01",
         to = "2019-12-02",
         complete_cases = T)

#Mexico
IPCMEX <- c("^MXX") %>%
  tq_get(get = "stock.prices",
         from = "2014-01-01",
         to = "2019-12-02",
         complete_cases = T)

#India
SENSEX <- c("^BSESN") %>%
  tq_get(get = "stock.prices",
         from = "2014-01-01",
         to = "2019-12-02",
         complete_cases = T)

#Brasil
IBOVESPA <- c("^BVSP") %>%
  tq_get(get = "stock.prices",
         from = "2014-01-01",
         to = "2019-12-02",
         complete_cases = T)

#Acciones
stockMerval = c("ALUA.BA", "BMA.BA", "BBAR.BA", "BYMA.BA", "CVH.BA", "CEPU.BA",
                "CRES.BA", "EDN.BA", "GGAL.BA", "VALO.BA", "SUPV.BA", "HARG.BA",
                "PAMP.BA", "COME.BA", "TECO2.BA", "TXAR.BA", "TRAN.BA", "TGNO4.BA",
                "TGSU2.BA", "YPFD.BA") %>%
  tq_get(get = "stock.prices", complete_cases = T)

#write.csv(stockMerval, "stockMerval")

# Visualización de los primeros 6 datos
#head(YPF)
#plot(Merval, na.rm = T)
#write.csv(Merval, "")



#Obtener exchange_rates
usd_ars = c("USDARS=X") %>%
  tq_get(get = "stock.prices",
         from = "2014-01-01",
         to = "2019-12-02",
         complete_cases = T)


#Sustitución de valores nulos para el loop
usd_ars <- usd_ars %>%
  mutate(adjusted = replace(adjusted,
                            is.na(adjusted),
                            0))

for (i in 1:nrow(Merval))
{
  date = Merval[i, "date"]
  exchange_rate = usd_ars %>%
    select(adjusted) %>%
    filter(usd_ars$date == date)
  if(as.numeric(exchange_rate) == 0)
  {
    Merval[i, "adjusted"] = NA
  }
  else
    Merval[i, "adjusted"] = Merval[i, "adjusted"]/exchange_rate
}

#Grafico del merval en dolares
Merval %>%
  ggplot(aes(x = date, y = adjusted)) +
  geom_line() +
  labs(title = "Merval en USD", y = "Puntos", x = "") + 
  theme_tq()

merval_returns_monthly <- Merval %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn, 
               period     = "monthly", 
               col_rename = "Returns")
merval_returns_monthly

merval_returns_monthly %>%
  ggplot(aes(x = date, y = merval_returns_monthly$Returns)) +
  geom_line() +
  labs(title = "Merval", y = "Retornos en U$D", x = "") + 
  theme_tq()

