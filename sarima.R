library(tidyverse)
library(forecast)
library(lubridate)
library(furrr)
library(tsibble)

source('paths3.R')
load(file = paste(data.path, 'resp_disease.RData'))

resp.train <- resp %>% select(date, admis) %>% filter(year(date) < 2018) %>% 
    group_by(date) %>% pull(admis) %>% msts(., seasonal.periods = c(7, 365.25))

resp.test <- resp %>% select(date, admis) %>% filter(year(date) == 2018) %>% pull(admis) %>% msts(., seasonal.periods = c(7, 365.25))
                                                             
order.list <- list('p' = seq(0, 1),
                   'd' = seq(0, 1),
                   'q' = seq(0, 1)) %>% cross() %>% map(lift(c))

season.list <- list(
    'P' = seq(0, 1),
    'D' = seq(0, 1),
    'Q' = seq(0, 1),
    'period' = 7
)  %>%
    cross() %>%
    map(lift(c))

order.df <- tibble('order' = order.list)
season.df <- tibble('season' = season.list)

hp.grid <- crossing(order.df,season.df)                                            

plan(multiprocess, workers = 3)

tic <- Sys.time()
resp.models <- hp.grid %>%
    mutate(models = future_map2(
        .x = order,
        .y = season,
        ~ possibly(arima, otherwise = NULL)(
            x = resp.train,
            order = .x,
            seasonal = .y
        )
    ))
running.time <- Sys.time() - tic