library('tidyverse')
library('readxl')
library('tsibble')

source('paths.R')

# CSV cache snapshot ------------------------------------------------------

resp.raw <- paste0(data.path, bd.path) %>%
    read_excel(sheet = 'Página1_1',
               range = 'B17:DHK19',
               col_names = FALSE) %>% t()

resp <-
    tsibble(admis = as.numeric(resp.raw[, 3]),
        date = as.Date(resp.raw[, 1]),
        key = id(),
        index = date
    )

resp %>% 
   write_csv(paste(data.path, 'resp_disease.csv'))

rm(resp.raw)

save.image(file = paste(data.path, 'resp_disease.RData'))
