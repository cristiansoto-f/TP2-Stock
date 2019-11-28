#install.packages("quantmod")
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
YPF <- getSymbols("YPFD.BA", src="yahoo", from = "2010-01-01", 
                   to = "2019-11-28", auto.assign = FALSE)

# Visualización de los primeros 6 datos
head(YPF)
plot(YPF)
