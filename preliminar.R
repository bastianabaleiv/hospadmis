library(dplyr)
library(purrr)
library(furrr)
library(caret)
library(forecast)

load("/media/sf_Google_Drive/Hospital_Forecast/hospadmis/data/ resp_disease.RData")
load("/media/sf_Google_Drive/Hospital_Forecast/hospadmis/data/sarima_grid.RData")

resp.train <-
    resp %>% filter(date >= "2014-01-01", date <= "2017-12-31")

resp.test <- resp %>% filter(date >= "2018-01-01")



resp.models2 <- resp.models %>% mutate(aic = map(models, ~possibly(AIC, otherwise = NULL)(.)))
resp.models2 <- resp.models %>% mutate(aic = map(models,function(x){if(!is.null(x)){AIC(x)}}))
sarima.mods <- resp.models2 %>% mutate(akaike = map_lgl(aic, ~ !is.null(.x))) %>% 
    filter(akaike) %>% select(-akaike)
sarima.fit <- sarima.mods %>% mutate(aic = unlist(sarima.mods[[4]])) %>% filter(aic == min(aic))
sarima.fit <- sarima.fit$models[[1]]

sarima.fitted <- resp.train$admis + sarima.fit$residuals
library(Metrics)

sarima.mse <- mse(resp.train$admis, sarima.fitted)
sarima.rmse <- rmse(resp.train$admis, sarima.fitted)
sarima.mape <- mape(resp.train$admis, sarima.fitted)*100

# out sample 
resp.sets <- createTimeSlices(resp$admis,
                              initialWindow = nrow(resp.train),
                              horizon = 1,
                              fixedWindow = TRUE) %>% as_tibble()

resp.hat <- tibble(y = double(), yhat = double())
for (i in 1:nrow(resp.sets)) {
    i <-1 
    sarima.rfit <- Arima(ts(resp[resp.sets$train[[i]],]$admis), model = sarima.fit)
    
    sarima.forecast <- as.numeric(forecast(sarima.rfit, h = 1, lambda = NULL)$mean)
    
    sarima.actual <- as.numeric(ts(resp[resp.sets$test[[i]],]$admis))
    
    resp.forecast <- list(y = sarima.actual, yhat = sarima.forecast)
    
    resp.hat <- bind_rows(resp.hat, resp.forecast)
    
}

plot(resp.hat$y, type = "l")
lines(resp.hat$yhat, col = "red")
