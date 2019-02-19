library('tidyverse')
library('readxl')
library('tsibble')

source('paths.R')

# CSV cache snapshot ------------------------------------------------------
raw.files <- c(hosp = paste0(hospbd.path, hosp.fpath),
                  sapu = paste0(sapubd.path, sapu.fpath),
                  sar = paste0(sarbd.path, sar.fpath))

resp.raw <-  raw.files[1] %>% read_excel(sheet = 'Página1_1',
                        range = 'B17:BRG19',
                        col_names = FALSE) %>% t()

resp.raw <- lapply(raw.files, function(x) read_excel(x, sheet = 'Página1_1',
                                                     range = 'B17:BRG19',
                                                     col_names = FALSE) %>% t())

resp <- lapply(resp.raw, function(x)
    tsibble(admis = as.numeric(x[, 3]),
        date = as.Date(x[, 1]),
        key = id(),
        index = date
    ))

resp %>% 
   write_csv(paste(data.path, 'resp_disease.csv'))

rm(resp.raw)

save.image(file = paste(data.path, 'resp_disease.RData'))
