library(dplyr)
library(tidyr)
library(ggplot2)
library(zoo)
library(plotly)
library(tibble)
library(forecast)
library(tseries)
library(data.table)
library(tidyverse)
library(ROracle)
library(keyring)
library(dynlm)
library(timetk)
library(MASS)
#Extraemos data
drv<-dbDriver("Oracle")

db_con <- dbConnect(drv, "estudios", "Mantecado_Amargo7315", dbname='riesgo')

fetch_solvencia <- dbSendQuery(db_con,"select ano, mes, solvencia
from indicadores_finan_riesgo_cred
where tipo_entidad=0
order by ano, mes")
solvencia <- fetch(fetch_solvencia)
solvencia <-solvencia%>%select(SOLVENCIA)%>%ts(start = c(2007,1),frequency = 12)
solvencia <-solvencia%>%as.zoo()


fetch_tasa_interbancaria <- dbSendQuery(db_con,"select ano, mes, tipm_interbancaria
from bcrd_tipm
order by ano, mes")
tasa_interbancaria <- fetch(fetch_tasa_interbancaria)
tasa_interbancaria <-tasa_interbancaria%>%select(TIPM_INTERBANCARIA)%>%ts(start = c(2008,1),frequency = 12)
tasa_interbancaria <-tasa_interbancaria%>%as.zoo()

fetch_m2 <- dbSendQuery(db_con,"select ano, mes, m2_l1
from bcrd_ag_mon
order by ano, mes")
m2 <- fetch(fetch_m2)
m2 <-m2%>%select(M2_L1)%>%ts(start = c(1996,1),frequency = 12)
m2 <-m2%>%as.zoo()

fetch_imae <- dbSendQuery(db_con,"select ano, mes, imae
from bcrd_imae
order by ano, mes")
imae <- fetch(fetch_imae)
imae <-imae%>%select(IMAE)%>%ts(start = c(2007,3),frequency = 12)
imae <-imae%>%as.zoo()

fetch_ipc <- dbSendQuery(db_con,"select ano, mes, ipc
from bcrd_ipc
order by ano, mes")
ipc <- fetch(fetch_ipc)
ipc <-ipc%>%select(IPC)%>%ts(start = c(1984,1),frequency = 12)
ipc <-ipc%>%as.zoo()


data <- merge(solvencia,tasa_interbancaria,m2,imae,ipc)
data <- data[complete.cases(data),]


fig <- plot_ly(y=solvencia[,1],x=index(solvencia), type = 'scatter', mode = 'lines')%>%
  layout(title="Solvencia")
fig

fig <- plot_ly(y=tasa_interbancaria[,1],x=index(tasa_interbancaria), type = 'scatter', mode = 'lines')%>%
  layout(title="Tasa Interbancaria")
fig

fig <- plot_ly(y=m2[,1],x=index(m2), type = 'scatter', mode = 'lines')%>%
  layout(title="M2")
fig

fig <- plot_ly(y=imae[,1],x=index(imae), type = 'scatter', mode = 'lines')%>%
  layout(title="IMAE")
fig

fig <- plot_ly(y=ipc[,1],x=index(ipc), type = 'scatter', mode = 'lines')%>%
  layout(title="IPC")
fig

nrow(data)

adf.test(solvencia)
pp.test(solvencia)
kpss.test(solvencia)


adf.test(diff(log(solvencia)))
pp.test(diff(log(solvencia)))
kpss.test(diff(log(solvencia)))


adf.test(tasa_interbancaria)
pp.test(tasa_interbancaria)
kpss.test(tasa_interbancaria)


adf.test(diff(log(tasa_interbancaria)))
pp.test(diff(log(tasa_interbancaria)))
kpss.test(diff(log(tasa_interbancaria)))

stationarity_tests(diff(tasa_interbancaria))

stationarity_tests(m2)
stationarity_tests(diff(log(m2)))


fig <- plot_ly(y=diff(log(m2[,1])),x=index(diff(log(m2))), type = 'scatter', mode = 'lines')%>%
  layout(title="M2 Growth Rate" )
fig

stationarity_tests(imae)
stationarity_tests(diff(log(imae)))

stationarity_tests(ipc)
stationarity_tests(diff(log(ipc)))


fig <- plot_ly(y=diff(log(ipc[,1])),x=index(diff(log(ipc))), type = 'scatter', mode = 'lines')%>%
  layout(title="Intermonth Inflation" )
fig

options(scipen = 999)

stationary_data <- apply(data,2,function(x) {diff(log(x))*100})
stationary_data <- stationary_data%>%as.zoo()
index(stationary_data) <- index(data[-1,])
fin_stat_data <- stationary_data[,1:3]
purge_stat_data <- stationary_data[,4:5]

lag_imae_1 <- lag_vec(stationary_data$IMAE)
lag_imae_2 <- lag_vec(stationary_data$IMAE,2)

lag_ipc_1 <- lag_vec(stationary_data$IPC)
lag_ipc_2 <- lag_vec(stationary_data$IPC,2)

complete_data <- stationary_data <- merge(stationary_data,lag_imae_1,lag_imae_2,lag_ipc_1,lag_ipc_2)


purging_solvencia <- lm(SOLVENCIA ~ lag_imae_1+lag_imae_2+lag_ipc_1+lag_ipc_2, data=complete_data)
purging_solvencia_res <- residuals(purging_solvencia)


purging_interbancaria <- lm(TIPM_INTERBANCARIA ~ lag_imae_1+lag_imae_2+lag_ipc_1+lag_ipc_2, data=complete_data)
purging_interbancaria_res <- residuals(purging_interbancaria)


purging_m2 <- lm(M2_L1 ~ lag_imae_1+lag_imae_2+lag_ipc_1+lag_ipc_2, data=complete_data)
purging_m2_res <- residuals(purging_m2)

pca_data <- cbind(purging_solvencia_res,purging_interbancaria_res,purging_m2_res)

pca_data <- scale(pca_data)

pcas <- princomp(pca_data)
summary(pcas)


icf <- pca_data%*%pcas$loadings[,1]
icf <- icf %>%as.zoo()
index(icf) <- index(data[-1:-3,])
fig <- plot_ly(y=icf%>%as.numeric(),x=index(icf), type = 'scatter', mode = 'lines')%>%
  layout(title="ICF" )
fig

icf <- rollmean(icf,6,fill  = F)
icf <- icf%>%head(-1)


fig <- plot_ly(y=icf%>%as.numeric(),x=index(icf), type = 'scatter', mode = 'lines')%>%
  layout(title="ICF" )
fig
