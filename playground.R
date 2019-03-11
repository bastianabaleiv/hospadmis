library(tidyverse)
library(tsibble)
library(reshape2)
library(forecast)

source('paths2.R')
load(file = paste(data.path, 'resp_disease.RData'))

resp.fit <-
    as.ts(resp.train$admis) %>% Arima(order = c(0, 1, 1), seasonal = c(0, 1, 7))

plot(resp.fit$x, type = 'l')
lines(resp.fit$fitted, col = 'red')

resp.arima.fit <- resp.train$admis %>% auto.arima()
lines(resp.arima.fit$fitted, col = 'blue')

legend('topleft', legend=c('Actual', 'Sarima', 'Arima'),
       col=c('black','red','blue'), lty=1:3, cex=0.8)

as.ts(resp.train$admis) %>% diff(lag = 7) %>% ggtsdisplay(xlab = 'Year',
                                                                       main = 'Diff week')

resp.train %>% count_gaps(.full = TRUE)
