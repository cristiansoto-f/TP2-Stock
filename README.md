# TP2-Stock
Trabajo práctico 2 - Computación 
# Últimos cambios
    02/12 Agregadas variables fecha.comienzo y fecha.fin para definir las fechas desde donde se obtendran los datos.
          Agregadas funciones para graficar: 
            Para hacer una comparación entre dos mercados conviene que estos esten en la misma moneda, es por eso que 
            se deben obtener los tipo de cambio, ej usd_ars = c("USDARS=X") (Yahoo finance)
            Luego se debe utilizar la funcion delete_na_values para eliminar los valores nulos.
            Finalmente la funcion actualizar_precios para obtener los adjusted prices de la acción/indice en un tipo de cambio.
            Luego se pueden graficar los precios y los rendimientos mensuales mediante graficar_precios y      graph_index_returns_monthly
    30/11 Obtención de datos completa
# Información
    ■ Se puede obtener información sobre las acciones en Yahoo Finance.
    ■ Para obtener información sobre los tickets del merval: https://www.spindices.com/documents/additional-material/sp-merval-index-ars-constituent-data.pdf
    ■ Información sobre el paquete quantmod: https://synergy.vision/corpus/inversion/2017-08-21-quantmod.html
    ■ TidyQuant guia: https://cran.r-project.org/web/packages/tidyquant/vignettes/TQ01-core-functions-in-tidyquant.html
    ■ Introduccion al analisis de portafolios: https://rpubs.com/DanielSLee/IntroPortfolioAnalysis
    ■ Videos que podrían iluminarnos:
      • Portfolio Optimization with R https://www.youtube.com/watch?v=5gmhZEl0kI8
      • Quant Finance with R Part 3: Portfolio Optimization https://www.youtube.com/watch?v=6Pi0fjARtUI&feature=youtu.be
      • Quant Finance with R Part 3: Portfolio Optimization https://www.youtube.com/watch?v=6Pi0fjARtUI
