install.packages("quantmod")
library(quantmod)

YPF <- getSymbols("YPFD.BA", src="yahoo", from = "2018-01-01", 
                   to = "2019-11-28", auto.assign = FALSE)

# Visualización de los primeros 6 datos
head(YPF)
plot(YPF)
