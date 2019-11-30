#install.packages("quantmod")
#install.packages("tidyquant")
library(tidyquant)
library(quantmod)

#Obtención de Datos
#Bolsas
Merval <- getSymbols("^MERV", src="yahoo", from = "2010-01-01", 
                     to = "2019-11-28", auto.assign = FALSE)
#EEUU
SP500 <- getSymbols("^GSPC", src="yahoo", from = "2010-01-01", 
                     to = "2019-11-28", auto.assign = FALSE)
#Japón
Nikkei225 <- getSymbols("^N225", src="yahoo", from = "2010-01-01", 
                    to = "2019-11-28", auto.assign = FALSE)
#UK
FTSE100 <- getSymbols("^FTSE", src="yahoo", from = "2010-01-01", 
                   to = "2019-11-28", auto.assign = FALSE)

#Alemania
DAX <- getSymbols("^GDAXI", src="yahoo", from = "2010-01-01", 
                      to = "2019-11-28", auto.assign = FALSE)

#España
IBEX35 <- getSymbols("^IBEX", src="yahoo", from = "2010-01-01", 
                  to = "2019-11-28", auto.assign = FALSE)

#China
SHANGAI <- getSymbols("000001.SS", src="yahoo", from = "2010-01-01", 
                     to = "2019-11-28", auto.assign = FALSE)

#Mexico
IPCMEX <- getSymbols("^MXX", src="yahoo", from = "2010-01-01", 
                      to = "2019-11-28", auto.assign = FALSE)

#India
SENSEX <- getSymbols("^BSESN", src="yahoo", from = "2010-01-01", 
                      to = "2019-11-28", auto.assign = FALSE)

#Brasil
IBOVESPA <- getSymbols("^BVSP", src="yahoo", from = "2010-01-01", 
                      to = "2019-11-28", auto.assign = FALSE)

#Acciones
stockMerval = c("ALUA.BA", "BMA.BA", "BBAR.BA", "BYMA.BA", "CVH.BA", "CEPU.BA",
                "CRES.BA", "EDN.BA", "GGAL.BA", "VALO.BA", "SUPV.BA", "HARG.BA",
                "PAMP.BA", "COME.BA", "TECO2.BA", "TXAR.BA", "TRAN.BA", "TGNO4.BA",
                "TGSU2.BA", "YPFD.BA") %>%
  tq_get(get = "stock.prices", complete_cases = T)

write.csv(stockMerval, "stockMerval")

# Visualización de los primeros 6 datos
head(YPF)
plot(YPF)
