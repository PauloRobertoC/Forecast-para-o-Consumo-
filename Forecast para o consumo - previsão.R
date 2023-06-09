### Combina��o de forecasts para o Consumo do Varejo no Maranh�o com forecastHybrid
### Elabora��o: Paulo Roberto Carneiro de S�

library(BETS)
# Pegando as s�ries a partir do site do Banco Central do Brasil
# �ndice de volume de vendas no varejo Total de Maranh�o
# mensal a partir de jan/2000 at� fev/2020 
# 242 observa��es mensais

varejoma <- BETSget(1463) 
print(varejoms)
class(varejoma)
dput(varejoma)  # op��o para ter os dados como na structure abaixo

#A rotina de dados obtidos pelo BETS j� retorna a s�rie em formato ts, ou seja, s�rie temporal. 
#Farei ent�o a cria��o de uma s�rie em diferen�as para observar o comportamento da s�rie em n�vel e em diferen�as.

#Inicialmente olharei as estat�sticas descritivas da s�rie. 
#Em seguida farei um plot b�sico da s�rie e o plot pelo pacote dygraphs, 
#�til para ver os pontos de picos e momentos espec�ficos.

dvarejo <- diff(varejoma)
# estatisticas basicas
summary(varejoma)

# plot basico lembrar que em class(), ele j� indicou que era ts = serie
# temporal
plot(varejoma)

# pelo pacote dygraph d� mais op��es
library(dygraphs)
dygraph(varejoma, main = "�ndice de volume de vendas no varejo total do Maranh�o <br> (Mensal)  (2011=100) BCB 1479") %>% 
  dyAxis("x", drawGrid = TRUE) %>% dyEvent("2005-1-01", "2005", labelLoc = "bottom") %>% 
  dyEvent("2015-1-01", "2015", labelLoc = "bottom") %>% dyEvent("2018-1-01", 
                                                                "2018", labelLoc = "bottom") %>% dyEvent("2019-2-20", "2019", labelLoc = "bottom") %>% 
  dyOptions(drawPoints = TRUE, pointSize = 2)
#� possivel visualizar nos plots acima: sazonalidade (por exemplo, picos em dezembro de cada ano); 
#a tend�ncia aparentemente crescente at� 2014 e decresce com a �crise� brasileira; 
#e uma aparente n�o-estacionariedade (m�dia e vari�ncia mudam no tempo).
##############################################################################################################################################################

### An�lise da s�rie
#Farei a estima��o para o modelo r�pido usando o forecastHybrid. 
#Portanto, especificarei a s�rie de varejoms, sem me preocupar de in�cio com a sazonalidade e a n�o-estacionariedade da s�rie, 
#imaginando que os modelos conseguir�o resolver essa situa��o.

# aplica��o do forecastHybrid
#pacotes necess�rios para este passo:
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

autoplot(varejoma) + autolayer(fcst.res$mean, series = "Combina��o") + xlab("Ano") + 
  ylab("�ndice (2011=100)") + ggtitle("Varejo MA: Forecast h�brido 
     considerando modelos SARIMA, ETS, Thetam, NNETAR, STLM e TBATS: 
     Per�odo Fev/2020-Julho/2024")

matriz.res <- cbind(varejoms, fcst.res$mean, ts(fcst.res$lower[, 2], start = c(2019, 
                                                                               3), frequency = 12), ts(fcst.res$upper[, 2], start = c(2019, 3), frequency = 12))

#############################################################################################
# grafico com os modelos SARIMA, ETS, Thetam, NNETAR, STLM e TBATS:

library(writexl)

matriz.res <- cbind(varejoma, fcst.res$mean, ts(fcst.res$lower[, 2], start = c(2020, 
                                                                               1), frequency = 12), ts(fcst.res$upper[, 2], start = c(2020, 1), frequency = 12))
writexl::write_xlsx(as.data.frame(matriz.res), "resid.xlsx")
autoplot(varejoma) + autolayer(matriz.res[, 2], series = "Combina��o") + autolayer(matriz.res[, 
                                                                                              3], series = "PI inferior 95%") + autolayer(matriz.res[, 4], series = "PI superior 95%") + 
  xlab("Ano") + ylab("�ndice (2011=100)") + ggtitle("Varejo MA: Forecast h�brido 
     considerando modelos SARIMA, ETS, Thetam, NNETAR, STLM e TBATS: 
     Per�odo Fev/2020-Julho/2024")

#Vamos aperfei�oar o modelo ARIMA para permitir mais defasagens que o padr�o.

set.seed(12345)
#### OPCAO PELOS PESOS IGUAIS
hmi <- hybridModel(y = varejoma, models = "aefnstz", a.args = list(stepwise = FALSE, 
                                                                   approximation = FALSE))
(acuraciai <- accuracy(hmi, individual = TRUE))

#Separando algumas medidas de acur�cia de interesse.

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
### Compara��o com dados novos ### 
#Agora que j� temos as informa��es at� fev/2020, 
#podemos comparar as estimativas com os dados reais. 
#Faremos a compara��o relativamente ao forecast do forecast::thetaf.

######################################################################################################
# a partir deste ponto, deu erro

dadosnovos<-BETS::BETSget(1463)
require(zoo)
print(cbind.zoo(fcst.resi$mean, dadosnovos)[236:242])
































































































































































































































