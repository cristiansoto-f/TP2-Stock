#install.packages("quantmod")
#install.packages("tidyquant")
#install.packages("tidyverse")
#install.packages("plotly")
library(tidyquant)
library(tidyverse)
library(quantmod)
library(dplyr)
library(ggplot2)
library(plotly)

fecha.comienzo = "2014-01-01"
fecha.fin = "2019-12-01"

#####FUNCIONES####

delete_na_values = function(c_exchglist)
{
  if(is_tibble(c_exchglist))
  {
  
  c_exchglist <- c_exchglist %>%
    mutate(adjusted = replace(adjusted,
                              is.na(adjusted),
                              0))
  }
  else return("Parámetros mal ingresados")
}

actualizar_precios = function(index, c_exchglist)
{
  
  for (i in 1:nrow(index))
  {
    date = index[i, "date"]
    exchange_rate = c_exchglist %>%
      select(adjusted) %>%
      filter(c_exchglist$date == date)
    if(as.numeric(exchange_rate) == 0)
    {
      index[i, "adjusted"] = NA
    }
    else
      index[i, "adjusted"] = index[i, "adjusted"]/exchange_rate
  }
}

graficar_precios = function(index, title, y, x)
{
  #Procurar utilizar datos actualizados
  index %>%
    ggplot(aes(x = date, y = adjusted)) +
    geom_line() +
    labs(title = title, y = y, x = x) + 
    theme_tq()
  
}

graph_index_returns_monthly <- function(index, title, y, x)
{
  index_returns_monthly <- index %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn, 
               period     = "monthly", 
               col_rename = "returns")
  
  p = index_returns_monthly %>%
    ggplot(aes(x = date, y = index_returns_monthly$returns)) +
    #geom_line(color="steelblue") +
    geom_area(color="steelblue") +
    #geom_point() +
    labs(title = title, y = y, x = x, caption="Source: YahooFinance", subtitle = "Hola") + 
    theme_tq()
  ggplotly(p)
}
####FIN FUNCIONES####
####Recopilación de Datos####

#####.Bolsas#####
Merval <- c("^MERV") %>%
  tq_get(get = "stock.prices",
         from = fecha.comienzo,
         to = fecha.fin,
         complete_cases = T)
#EEUU
SP500 <-  c("^GSPC") %>%
  tq_get(get = "stock.prices",
         from = fecha.comienzo,
         to = fecha.fin,
         complete_cases = T)

#JapÃ³n
Nikkei225 <- c("^N225") %>%
  tq_get(get = "stock.prices",
         from = fecha.comienzo,
         to = fecha.fin,
         complete_cases = T)

#UK
FTSE100 <- c("^FTSE") %>%
  tq_get(get = "stock.prices",
         from = fecha.comienzo,
         to = fecha.fin,
         complete_cases = T)

#Alemania
DAX <- c("^GDAXI") %>%
  tq_get(get = "stock.prices",
         from = fecha.comienzo,
         to = fecha.fin,
         complete_cases = T)

#España
IBEX35 <- c("^IBEX") %>%
  tq_get(get = "stock.prices",
         from = fecha.comienzo,
         to = fecha.fin,
         complete_cases = T)

#China
SHANGAI <- c("000001.SS") %>%
  tq_get(get = "stock.prices",
         from = fecha.comienzo,
         to = fecha.fin,
         complete_cases = T)

#Mexico
IPCMEX <- c("^MXX") %>%
  tq_get(get = "stock.prices",
         from = fecha.comienzo,
         to = fecha.fin,
         complete_cases = T)

#India
SENSEX <- c("^BSESN") %>%
  tq_get(get = "stock.prices",
         from = fecha.comienzo,
         to = fecha.fin,
         complete_cases = T)

#Brasil
IBOVESPA <- c("^BVSP") %>%
  tq_get(get = "stock.prices",
         from = fecha.comienzo,
         to = fecha.fin,
         complete_cases = T)

#####.Acciones#####
stockMerval = c("ALUA.BA", "BMA.BA", "BBAR.BA", "BYMA.BA", "CVH.BA", "CEPU.BA",
                "CRES.BA", "EDN.BA", "GGAL.BA", "VALO.BA", "SUPV.BA", "HARG.BA",
                "PAMP.BA", "COME.BA", "TECO2.BA", "TXAR.BA", "TRAN.BA", "TGNO4.BA",
                "TGSU2.BA", "YPFD.BA") %>%
  tq_get(get = "stock.prices",
         from = fecha.comienzo,
         to = fecha.fin,
         complete_cases = T)

#write.csv(stockMerval, "stockMerval")

# Visualización de los primeros 6 datos
#head(YPF)
#plot(Merval, na.rm = T)
#write.csv(Merval, "")

#####.Exchange rates#####
usd_ars = c("USDARS=X") %>%
  tq_get(get = "stock.prices",
         from = fecha.comienzo,
         to = fecha.fin,
         complete_cases = T)
usd_ars = delete_na_values(usd_ars)

#####Graficos####
#Grafico del merval en dolares

Merval = actualizar_precios(Merval, usd_ars)
graficar_precios(Merval, "Merval en Dólares", "Puntos", "")
graph_index_returns_monthly(Merval, "Merval", "Retornos", "")

graph_index_returns_monthly(SP500, "S&P", "Retornos", "")
