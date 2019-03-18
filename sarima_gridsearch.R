library(tidyverse)
library(forecast)
library(lubridate)
library(furrr)
library(tsibble)

source('paths2.R')
load(file = paste(data.path, 'resp_disease.RData'))

resp.train <-
    resp %>% select(date, admis) %>% filter(year(date) < 2018) %>%
    group_by(date) %>% pull(admis) %>% ts(.)

resp.test <-
    resp %>% select(date, admis) %>% filter(year(date) == 2018) %>% pull(admis) %>% ts(.)

order.list <- list('p' = seq(0, 3),
                   'd' = seq(0, 2),
                   'q' = seq(0, 3)) %>% cross() %>% map(lift(c))

season.list <- list(
    'P' = seq(0, 3),
    'D' = seq(0, 2),
    'Q' = seq(0, 3),
    'period' = 7
)  %>%
    cross() %>%
    map(lift(c))

order.df <- tibble('order' = order.list)
season.df <- tibble('season' = season.list)

hp.grid <-
    crossing(order.df, season.df)
hp.grid <- hp.grid[1341:1351, ]

n.rows <- nrow(hp.grid)

resp.sarima <- function(x, order, seasonal) {
    tic <- Sys.time()
    sarima.fit <- arima(x, order = order, seasonal = seasonal)
    sarima.fit$runtime <- Sys.time() - tic
    return(sarima.fit)
}

plan(multiprocess, workers = 3)

all.tic <- Sys.time()
resp.models <- hp.grid %>%
    mutate(models = future_map2(
        .x = order,
        .y = season,
        ~ possibly(resp.sarima, otherwise = NULL)(
            x = resp.train,
            order = .x,
            seasonal = .y
        )
    ))
all.runtime <- difftime(Sys.time(), all.tic)

resp.models <-
    resp.models %>% mutate(aic = map(models, ~ possibly(AIC, otherwise = NULL)(.)))

sarima.modelset <-
    resp.models %>% mutate(akaike = map_lgl(aic, ~ !is.null(.x))) %>%
    filter(akaike) %>% select(-akaike)

sarima.modelset <-
    sarima.modelset %>% mutate(d = unlist(order)[2]) %>%
    mutate(D = unlist(season)[2])

sarima.model <-
    sarima.modelset %>% mutate(aic = unlist(resp.models[[4]])) %>% filter(aic == min(aic))

sarima.model <- sarima.model$models[[1]]

save.image(file = paste(data.path, 'sarima_gridsearch.RData'))

txt.file <- file(paste(
    'sarima_gridsearch_',
    format(Sys.time(), '%Y-%m-%d_%H-%M'),
    '.txt',
    sep = ''
))

writeLines(
    c(
        'Sarima Grid Search Parameter Optimization',
        '-----------------------------------------',
        '',
        paste0('Total runtime: ',
               all.runtime, ' ', units(all.runtime)),
        paste0('Total models: ', nrow(hp.grid)),
        paste0(
            'Non-Null models: ',
            nrow(sarima.modelset),
            ' (',
            round(nrow(sarima.modelset) / nrow(hp.grid) * 100),
            ')%'
        ),
        paste0('Null-models: ', nrow(hp.grid) - nrow(sarima.modelset)),
        #paste0(names(arimaorder(sarima.model))),
        #paste0(as.character(arimaorder(sarima.model))),
        paste0('AIC: ', sarima.model$aic),
        paste0('Best model specification:')
    )
        ,
        txt.file
)
close(txt.file)

sink(paste(
    'sarima_gridsearch_',
    format(Sys.time(), '%Y-%m-%d_%H-%M'),
    '.txt',
    sep = ''
), append = TRUE)
print(sarima.model)
cat("\n")
print(Arima(y = resp.train, model = sarima.model))
sink()




