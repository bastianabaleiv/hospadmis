library('tidyverse')
library('readxl')
library('tsibble')

source('paths2.R')

# CSV cache snapshot ------------------------------------------------------
raw.files <- c(
    hosp = paste0(hospbd.path, hosp.fpath),
    sapu = paste0(sapubd.path, sapu.fpath),
    sar = paste0(sarbd.path, sar.fpath)
)

resp.raw <-  raw.files[1] %>% read_excel(sheet = 'Página1_1',
                                         range = 'B17:BRG19',
                                         col_names = FALSE) %>% t()

resp.raw <-
    lapply(raw.files, function(x)
        read_excel(
            x,
            sheet = 'Página1_1',
            range = 'B17:BRG19',
            col_names = FALSE
        ) %>% t())

resp.urserv <- lapply(resp.raw, function(x)
    tsibble(
        admis = as.numeric(x[, 3]),
        date = as.Date(x[, 1]),
        key = id(),
        index = date
    ))

resp <- tsibble(
    admis =
        rowSums(
            cbind(
                resp.urserv$hosp$admis,
                resp.urserv$sapu$admis,
                resp.urserv$sar$admis
            ),
            na.rm = TRUE
        ),
    date = resp.urserv$hosp$date,
    key = id(),
    index = date
)

resp <- resp %>% mutate(
    year = lubridate::year(date),
    month = lubridate::month(date, label = TRUE),
    month.day = lubridate::day(date),
    day = lubridate::wday(
        date,
        label = TRUE,
        abbr = FALSE,
        week_start =
            getOption("lubridate.week.start", 1)
    ),
    week.day = lubridate::wday(
        date,
        label = FALSE,
        abbr = FALSE,
        week_start =
            getOption("lubridate.week.start", 1)
    ),
    time = rep(0:47, length.out = nrow(resp))
) %>%
    mutate(weekend = if_else(day %in% c("Saturday", "Sunday"), 1, 0))

resp %>% 
   write_csv(paste(data.path, 'resp_disease.csv'))

rm(resp.raw, resp.urserv)

save.image(file = paste(data.path, 'resp_disease.RData'))
