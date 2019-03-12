library(tidyverse)
library(forecast)
library(lubridate)
library(furrr)
library(tsibble)

source('paths2.R')
load(file = paste(data.path, 'resp_disease.RData'))

resp.train <- resp %>% select(date, admis) %>% filter(year(date) < 2018) %>% 
    group_by(date) %>% pull(admis) %>% ts(.)

resp.test <- resp %>% select(date, admis) %>% filter(year(date) == 2018) %>% pull(admis) %>% ts(.)
                                                             
order.list <- list('p' = seq(0, 3),
                   'd' = seq(0, 1),
                   'q' = seq(0, 3)) %>% cross() %>% map(lift(c))

season.list <- list(
    'P' = seq(0, 3),
    'D' = seq(0, 1),
    'Q' = seq(0, 3),
    'period' = 7
)  %>%
    cross() %>%
    map(lift(c))

order.df <- tibble('order' = order.list)
season.df <- tibble('season' = season.list)

hp.grid <- crossing(order.df,season.df)                                            
hp.grid <- hp.grid[1:10,]
n.rows <- nrow(hp.grid)

plan(multiprocess, workers = 3)

tic <- Sys.time()
resp.models <- hp.grid %>%
    mutate(models = future_map2(
        .x = order,
        .y = season,
        ~possibly(arima, otherwise = NULL)(
            x = resp.train,
            order = .x,
            seasonal = .y
        )
    ))
running.time <- Sys.time() - tic

resp.models

######### 
tic <- Sys.time()
test.sarima <- arima(resp.train, order = c(1,1,1), seasonal = list(order = c(1,1,1), period = 7))
running.time <- Sys.time() - tic
