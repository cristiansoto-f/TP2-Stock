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
library(chron)
library(caret)
library(e1071)
library(quantmod)
library(PerformanceAnalytics)
library(PortfolioAnalytics)
library(ROI)
require(ROI.plugin.glpk)
require(ROI.plugin.quadprog)

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
                 type = "log",
                 col_rename = "returns")
  
  p = index_returns_monthly %>%
    ggplot(aes(x = date, y = index_returns_monthly$returns)) +
    #geom_line(color="steelblue") +
    geom_area(fill="#4fe8c5", alpha=0.7) +
    #geom_point() +
    labs(title = title, y = y, x = x, caption="Source: YahooFinance") + 
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

periodic_returns <- function(index, period) {
  return (index %>%
    tq_transmute(select     = adjusted, 
                 mutate_fun = periodReturn, 
                 period     = period, 
                 col_rename = "returns"))
}

ticker_history <- function(ticker, start, end) {
  ticker %>%
    tq_get(get = "stock.prices",
           from = start,
           to = end,
           complete_cases = T)
}

dataMatch <- function(baseIndexReturns, matchingIndexReturns, baseIndexName, matchingIndexName){
  dataConsolidada <- data.frame(baseIndexReturns)
  colnames(dataConsolidada)[2] <- baseIndexName 
  for(i in 1:nrow(dataConsolidada)){
    if (wday(dataConsolidada$date[i]) == 2) {
      #Si el día es lunes, se obtienen los datos del viernes.
      if (length(filter(matchingIndexReturns, (dataConsolidada$date[i]-3) == date)$returns) !=0){
        
        dataConsolidada$index2[i] <- select(filter(matchingIndexReturns, (dataConsolidada$date[i]-3) == date), returns)
        
      } else {dataConsolidada$index2[i] <- NA}
      
    } else {
      
      if (length(filter(matchingIndexReturns, (dataConsolidada$date[i]-1) == date)$returns) != 0){
        
        dataConsolidada$index2[i] <- select(filter(matchingIndexReturns, (dataConsolidada$date[i]-1) == date), returns)
        
      } else {dataConsolidada$index2[i] <- NA}
    }
  }
  
  dataConsolidada <- filter(dataConsolidada, !is.na(index2))
  dataConsolidada$index2 <- unlist(dataConsolidada$index2)
  colnames(dataConsolidada)[ncol(dataConsolidada)] <- matchingIndexName
  return(dataConsolidada)
}
####FIN FUNCIONES####
####Recopilación de Datos####

#####.Bolsas#####
Merval <- ticker_history("^MERV", fecha.comienzo, fecha.fin)

#EEUU
SP500 <-  ticker_history("^GSPC", fecha.comienzo, fecha.fin)

#JapÃ³n
Nikkei225 <- ticker_history("^N225", fecha.comienzo, fecha.fin)

#UK
FTSE100 <- ticker_history("^FTSE", fecha.comienzo, fecha.fin)

#Alemania
DAX <- ticker_history("^GDAXI", fecha.comienzo, fecha.fin)

#España
IBEX35 <- ticker_history("^IBEX", fecha.comienzo, fecha.fin)

#China
SHANGHAI <- ticker_history("000001.SS", fecha.comienzo, fecha.fin)

#Mexico
IPCMEX <- ticker_history("^MXX", fecha.comienzo, fecha.fin)

#India
SENSEX <- ticker_history("^BSESN", fecha.comienzo, fecha.fin)

#Brasil
IBOVESPA <- ticker_history("^BVSP", fecha.comienzo, fecha.fin)

#DOWJONES
DOWJONES <- ticker_history("^DJI", fecha.comienzo, fecha.fin)

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
usd_ars = ticker_history("USDARS=X", fecha.comienzo, fecha.fin)
usd_ars = delete_na_values(usd_ars)

usd_jpy = ticker_history("USDJPY=X", fecha.comienzo, fecha.fin)
usd_jpy = delete_na_values(usd_jpy)

#####Graficos####
#Grafico del merval en dolares

Merval <- actualizar_precios(Merval, usd_ars)
graficar_precios(Merval, "Merval en Dólares", "Puntos", "")
graph_index_returns_monthly(Merval, "Merval", "Retornos", "")

graph_index_returns_monthly(SP500, "S&P", "Retornos", "Tiempo")

#Graficos nikkei
Nikkei225 <- actualizar_precios(Nikkei225, usd_jpy)
graficar_precios(Nikkei225, "Nikkei225 en dólares", "Puntos", "")
graph_index_returns_monthly(Nikkei225, "Nikkei225", "Retornos", "")

###.Graficos de densidad###
sp500_returns_monthly <- periodic_returns(SP500, "monthly")
sp500_returns_monthly <- mutate(sp500_returns_monthly, symbol = "S&P 500")

merval_returns_monthly <- periodic_returns(Merval, "monthly")
merval_returns_monthly <- mutate(merval_returns_monthly, symbol = "Merval")

nikkei_returns_monthly <- periodic_returns(Nikkei225, "monthly")
nikkei_returns_monthly <- mutate(nikkei_returns_monthly, symbol = "Nikkei")

graph_density_returns(rbind(sp500_returns_monthly,nikkei_returns_monthly,merval_returns_monthly), "Distribución de retornos mensuales", 
                      "Retornos", "Densidad")
ohlc=Merval[,"adjusted"]
ohlc=na.omit(ohlc)
v1=volatility(ohlc,calc = "close")
ohlc2=SP500[,c("open","high","low","close")]
ohlc2=na.omit(ohlc2)
v2=volatility(ohlc2,calc = "close")
volatilidades=data.frame(tail(v1),tail(v2))
names(volatilidades)=c("Vol.Merval","Vol.SP500")
View(volatilidades)
Retornos=data.frame(tail(ROC(ohlc)),tail(ROC(ohlc2)))
names(Retornos)=c("Ret.Merval","Ret.SP500")


## Modelado
## Idea: Retorno diario del Merval en función de los retornos del día anterior de los otros índices.
merval_returns_daily <- periodic_returns(Merval, "daily")
sp500_returns_daily <- periodic_returns(SP500, "daily")

dataConsolidada <- dataMatch(merval_returns_daily, sp500_returns_daily, "merval", "sp500")
dataConsolidada <- dataConsolidada[-c(1,2),]

set.seed(6)
trainIndex=createDataPartition(dataConsolidada$merval, p=0.75)$Resample1

d_merval_train=dataConsolidada[trainIndex, ]
d_merval_test= dataConsolidada[-trainIndex, ]

modelo<- lm(merval ~ sp500, data = d_merval_train)
summary(modelo)
d_merval_test$pred<-predict(modelo, d_merval_test, type= "response")
plot(d_merval_test$merval, col="blue")
points(d_merval_test$pred,col = "red")



#Optimizacion de portafolio
stockMerval = c("BMA.BA", "BBAR.BA", "CVH.BA",
                "CRES.BA", "EDN.BA", "GGAL.BA", "VALO.BA",
                "PAMP.BA",  "TECO2.BA", "TRAN.BA",
                "TGSU2.BA", "YPFD.BA") %>%
  tq_get(get = "stock.prices",
         from = fecha.comienzo,
         to = fecha.fin,
         complete_cases = T)

portfolioReturns <- na.omit(ROC(stockMerval))


tickers <- c("BMA.BA", "CRES.BA", "EDN.BA", "GGAL.BA", "VALO.BA",
             "PAMP.BA",  "TECO2.BA", "TRAN.BA", "TGSU2.BA", "YPFD.BA")

portfolioPrices <- NULL
for(ticker in tickers) {
  portfolioPrices <- cbind(portfolioPrices,
                           getSymbols(ticker, from='2009-01-03', periodicity = 'daily', auto.assign=FALSE)[,4])
}

portfolioReturns <- na.omit(ROC(portfolioPrices))

portf <- portfolio.spec(colnames(portfolioReturns))

portf <- add.constraint(portf, type="weight_sum", min_sum=1, max_sum=1)
portf <- add.constraint(portf, type="box", min=0, max=.50)
portf <- add.objective(portf, type="return", name="mean")
portf <- add.objective(portf, type="risk", name="StdDev")

optPort <- optimize.portfolio(portfolioReturns, portf, optimize_method = "ROI", trace=TRUE)

chart.Weights(optPort)

ef <- extractEfficientFrontier(optPort, match.col = "StdDev", n.portfolios = 25,
                               risk_aversion = NULL)

chart.EfficientFrontier(ef,
                        match.col = "StdDev", n.portfolios = 25, xlim = NULL, ylim = NULL,
                        cex.axis = 0.8, element.color = "darkgray", main = "Efficient Frontier",
                        RAR.text = "SR", rf = 0, tangent.line = TRUE, cex.legend = 0.8,
                        chart.assets = TRUE, labels.assets = TRUE, pch.assets = 21,
                        cex.assets = 0.8)



getSymbols("NFLX", from='2009-01-03', periodicity = 'daily', auto.assign=FALSE)
getSymbols("GOOG", from='2009-01-03', periodicity = 'daily', auto.assign=FALSE)
getSymbols("MCD", from='2009-01-03', periodicity = 'daily', auto.assign=FALSE)
precios<-data.frame(NFLX$NFLX.Close,GOOG$GOOG.Close,MCD$MCD.Close)
names(precios)<-c("NETFLIX","GOOGLE","MACDONALDS")
retornos<-na.omit(ROC(precios))
portf2 <- portfolio.spec(colnames(retornos))

portf2 <- add.constraint(portf2, type="weight_sum", min_sum=1, max_sum=1)
portf2 <- add.constraint(portf2, type="box", min=0, max=.50)
portf2 <- add.objective(portf2, type="return", name="mean")
portf2 <- add.objective(portf2, type="risk", name="StdDev")
optPort2 <- optimize.portfolio(retornos, portf2, optimize_method = "ROI", trace=TRUE)

ef2 <- extractEfficientFrontier(optPort2, match.col = "StdDev", n.portfolios = 25,
                               risk_aversion = NULL)

chart.EfficientFrontier(ef2,
                        match.col = "StdDev", n.portfolios = 25, xlim = NULL, ylim = NULL,
                        cex.axis = 0.8, element.color = "darkgray", main = "Efficient Frontier",
                        RAR.text = "SR", rf = 0, tangent.line = TRUE, cex.legend = 0.8,
                        chart.assets = TRUE, labels.assets = TRUE, pch.assets = 21,
                        cex.assets = 0.8)
rendimientos<-c(optPort$objective_measures$mean,optPort2$objective_measures$mean)
desvios<-c(optPort$objective_measures$StdDev,optPort2$objective_measures$StdDev)
sharpe<-c((rendimientos[1]-0)/desvios[1],(rendimientos[2]-0)/desvios[2])
tabla<-cbind(rendimientos,desvios,sharpe)
rownames(tabla)<-c("merval","cartera")
