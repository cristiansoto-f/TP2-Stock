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

fecha.comienzo = "2009-12-01"
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
    exchange_rate = c_exchglist %>%
      select(adjusted) %>%
      filter(as.Date(c_exchglist$date) == as.Date(index$date[i]))
    if(as.numeric(exchange_rate) == 0)
    {
      index[i, "adjusted"] = NA
    }
    else
      index[i, "adjusted"] = index[i, "adjusted"]/exchange_rate
  }
  return(index)
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

graph_density_returns = function(indexframe, title, x, y)
{
  markets.returns <- indexframe
  p = markets.returns %>%
    ggplot(aes(x = returns, fill = symbol)) +
    labs(title = title, x = x, y = y) +
    #xlim(c(-0.4, 0.3)) +
    #ylim(c(0,15)) +
    geom_density(alpha = 0.3)
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

Merval <- actualizar_precios(Merval, usd_ars)
graficar_precios(Merval, "Merval en Dólares", "Puntos", "")
graph_index_returns_monthly(Merval, "Merval", "Retornos", "")

graph_index_returns_monthly(SP500, "S&P", "Retornos", "Tiempo")
###.Graficos de densidad###
sp500_returns_monthly <- SP500 %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn, 
               period     = "monthly", 
               col_rename = "returns")
sp500_returns_monthly <- mutate(sp500_returns_monthly, symbol = "S&P 500")

merval_returns_monthly <- Merval %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn, 
               period     = "monthly", 
               col_rename = "returns")
merval_returns_monthly <- mutate(merval_returns_monthly, symbol = "Merval")

nikkei_returns_monthly <- Nikkei225 %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn, 
               period     = "monthly", 
               col_rename = "returns")
nikkei_returns_monthly <- mutate(nikkei_returns_monthly, symbol = "Nikkei")

graph_density_returns(rbind(sp500_returns_monthly,nikkei_returns_monthly,merval_returns_monthly), "Distribución de retornos mensuales", 
                      "Retornos", "Densidad")
## Modelado
## Idea: Retorno diario del Merval en función de los retornos del día anterior de los otros índices.
merval_returns_daily <- Merval %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn, 
               period     = "daily", 
               col_rename = "returns")
