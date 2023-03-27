### Combinação de forecasts para o Consumo do Varejo no Maranhão com forecastHybrid
### Elaboração: Paulo Roberto Carneiro de Sá

library(BETS)
# Pegando as séries a partir do site do Banco Central do Brasil
# Índice de volume de vendas no varejo Total de Maranhão
# mensal a partir de jan/2000 até fev/2020 
# 242 observações mensais

varejoma <- BETSget(1463) 
print(varejoms)
class(varejoma)
dput(varejoma)  # opção para ter os dados como na structure abaixo

#A rotina de dados obtidos pelo BETS já retorna a série em formato ts, ou seja, série temporal. 
#Farei então a criação de uma série em diferenças para observar o comportamento da série em nível e em diferenças.

#Inicialmente olharei as estatísticas descritivas da série. 
#Em seguida farei um plot básico da série e o plot pelo pacote dygraphs, 
#útil para ver os pontos de picos e momentos específicos.

dvarejo <- diff(varejoma)
# estatisticas basicas
summary(varejoma)

# plot basico lembrar que em class(), ele já indicou que era ts = serie
# temporal
plot(varejoma)

# pelo pacote dygraph dá mais opções
library(dygraphs)
dygraph(varejoma, main = "Índice de volume de vendas no varejo total do Maranhão <br> (Mensal)  (2011=100) BCB 1479") %>% 
  dyAxis("x", drawGrid = TRUE) %>% dyEvent("2005-1-01", "2005", labelLoc = "bottom") %>% 
  dyEvent("2015-1-01", "2015", labelLoc = "bottom") %>% dyEvent("2018-1-01", 
                                                                "2018", labelLoc = "bottom") %>% dyEvent("2019-2-20", "2019", labelLoc = "bottom") %>% 
  dyOptions(drawPoints = TRUE, pointSize = 2)
#É possivel visualizar nos plots acima: sazonalidade (por exemplo, picos em dezembro de cada ano); 
#a tendência aparentemente crescente até 2014 e decresce com a “crise” brasileira; 
#e uma aparente não-estacionariedade (média e variância mudam no tempo).
##############################################################################################################################################################

### Análise da série
#Farei a estimação para o modelo rápido usando o forecastHybrid. 
#Portanto, especificarei a série de varejoms, sem me preocupar de início com a sazonalidade e a não-estacionariedade da série, 
#imaginando que os modelos conseguirão resolver essa situação.

# aplicação do forecastHybrid
#pacotes necessários para este passo:
library(forecastHybrid)
library(forecast)
library(fpp2)
library(zoo)
set.seed(12345)
quickModel <- hybridModel(varejoma)
# estima os modelos: auto.arima, ets, thetam, nnetar, stlm e tbats
fcst.res <- forecast(quickModel, h = 60, PI.combination = c("mean"))
class(quickModel)

print(quickModel)

accuracy(quickModel)

autoplot(varejoma) + autolayer(fcst.res$mean, series = "Combinação") + xlab("Ano") + 
  ylab("Índice (2011=100)") + ggtitle("Varejo MA: Forecast híbrido 
     considerando modelos SARIMA, ETS, Thetam, NNETAR, STLM e TBATS: 
     Período Fev/2020-Julho/2024")

matriz.res <- cbind(varejoms, fcst.res$mean, ts(fcst.res$lower[, 2], start = c(2019, 
                                                                               3), frequency = 12), ts(fcst.res$upper[, 2], start = c(2019, 3), frequency = 12))

#############################################################################################
# grafico com os modelos SARIMA, ETS, Thetam, NNETAR, STLM e TBATS:

library(writexl)

matriz.res <- cbind(varejoma, fcst.res$mean, ts(fcst.res$lower[, 2], start = c(2020, 
                                                                               1), frequency = 12), ts(fcst.res$upper[, 2], start = c(2020, 1), frequency = 12))
writexl::write_xlsx(as.data.frame(matriz.res), "resid.xlsx")
autoplot(varejoma) + autolayer(matriz.res[, 2], series = "Combinação") + autolayer(matriz.res[, 
                                                                                              3], series = "PI inferior 95%") + autolayer(matriz.res[, 4], series = "PI superior 95%") + 
  xlab("Ano") + ylab("Índice (2011=100)") + ggtitle("Varejo MA: Forecast híbrido 
     considerando modelos SARIMA, ETS, Thetam, NNETAR, STLM e TBATS: 
     Período Fev/2020-Julho/2024")

#Vamos aperfeiçoar o modelo ARIMA para permitir mais defasagens que o padrão.

set.seed(12345)
#### OPCAO PELOS PESOS IGUAIS
hmi <- hybridModel(y = varejoma, models = "aefnstz", a.args = list(stepwise = FALSE, 
                                                                   approximation = FALSE))
(acuraciai <- accuracy(hmi, individual = TRUE))

#Separando algumas medidas de acurácia de interesse.

RMSE = c(ETS = acuraciai$ets[, "RMSE"], ARIMA = acuraciai$auto.arima[, "RMSE"], `STL-ETS` = acuraciai$stlm[, 
                                                                                                           "RMSE"], NNAR = acuraciai$nnetar[, "RMSE"], TBATS = acuraciai$tbats[, "RMSE"], 
         THETA = acuraciai$thetam[, "RMSE"], SNAIVE = acuraciai$snaive[, "RMSE"], Combinacao = accuracy(hmi)[, 
                                                                                                             "RMSE"])

MAPE = c(ETS = acuraciai$ets[, "MAPE"], ARIMA = acuraciai$auto.arima[, "MAPE"], `STL-ETS` = acuraciai$stlm[, 
                                                                                                           "MAPE"], NNAR = acuraciai$nnetar[, "MAPE"], TBATS = acuraciai$tbats[, "MAPE"], 
         THETA = acuraciai$thetam[, "MAPE"], SNAIVE = acuraciai$snaive[, "MAPE"], Combinacao = accuracy(hmi)[, 
                                                                                                             "MAPE"])

MAE = c(ETS = acuraciai$ets[, "MAE"], ARIMA = acuraciai$auto.arima[, "MAE"], `STL-ETS` = acuraciai$stlm[, 
                                                                                                        "MAE"], NNAR = acuraciai$nnetar[, "MAE"], TBATS = acuraciai$tbats[, "MAE"], THETA = acuraciai$thetam[, 
                                                                                                                                                                                                             "MAE"], SNAIVE = acuraciai$snaive[, "MAE"], Combinacao = accuracy(hmi)[, "MAE"])

tabelai <- cbind(RMSE, MAE, MAPE)
knitr::kable(tabelai)

# estima os modelos: auto.arima, ets, thetam, nnetar, stlm, tbats e snaive
fcst.resi <- forecast(hmi, h = 12, PI.combination = c("mean"))
class(hmi)

print(hmi)

accuracy(hmi)

######################################################################################################
### Comparação com dados novos ### 
#Agora que já temos as informações até fev/2020, 
#podemos comparar as estimativas com os dados reais. 
#Faremos a comparação relativamente ao forecast do forecast::thetaf.

######################################################################################################
# a partir deste ponto, deu erro

dadosnovos<-BETS::BETSget(1463)
require(zoo)
print(cbind.zoo(fcst.resi$mean, dadosnovos)[236:242])
































































































































































































































