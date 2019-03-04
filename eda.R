library('tidyverse')
library('cowplot')
library('astsa')
library('forecast')
library('reshape2')
library('tsibble')
library('plotly')
library('ggTimeSeries')

source('paths2.R')
load(file = paste(data.path, 'resp_disease.RData'))

# Exploratory Data Analysis -----------------------------------------------

resp.full.plot <-
    resp %>% ggplot(aes(x = date, y = admis)) + geom_line() +
    background_grid(minor = "xy")

save_plot(paste0(fig.path, 'resp_full.pdf'),
          resp.full.plot,
          base_aspect_ratio = 1.8)

htmlwidgets::saveWidget(as_widget(ggplotly(resp.full.plot)),
                        paste0(fig.path, 'resp_full.html'))

resp.train <-
    resp %>% filter(date >= "2014-01-01", date <= "2017-12-31")

resp.train.plot <-
    resp.train %>% ggplot(aes(x = date, y = admis)) + geom_line() +
    background_grid(major = "xy", minor = "none")

save_plot(paste0(fig.path, 'resp_train.pdf'),
          resp.train.plot,
          base_aspect_ratio = 1.8)

htmlwidgets::saveWidget(as_widget(ggplotly(resp.train.plot)),
                        paste0(fig.path, 'resp_train.html'))
resp.acf <- tibble(
    lag = seq(1:(7 * 8)),
    acf = astsa::acf2(resp$admis, max.lag = 7 * 8, plot = FALSE)[, 1],
    pacf = astsa::acf2(resp$admis, max.lag = 7 * 8, plot = FALSE)[, 2]
)

resp.acf %>%
    write_csv(paste(data.path, 'resp_acf_pacf.csv'))


resp.acf.plot <-
    resp.acf %>% ggplot(mapping = aes(x = lag, y = acf)) +
    coord_cartesian(ylim = c(-1, 1)) + background_grid(major = "xy", minor = "none") +
    scale_x_continuous(breaks = seq(0, nrow(resp.acf), 7)) +
    geom_segment(mapping = aes(xend = lag, yend = 0)) + labs(x = "Lag (days)", y = "ACF")


resp.pacf.plot  <-
    resp.acf %>% ggplot(mapping = aes(x = lag, y = pacf)) +
    coord_cartesian(ylim = c(-1, 1)) +  background_grid(major = "xy", minor = "none") +
    scale_x_continuous(breaks = seq(0, nrow(resp.acf), 7)) +
    geom_segment(mapping = aes(xend = lag, yend = 0)) + labs(x = "Lag (days)", y = "PACF")


save_plot(paste0(fig.path, 'acf_plot.pdf'),
          resp.acf.plot)

save_plot(paste0(fig.path, 'pacf_plot.pdf'),
          resp.pacf.plot)

resp.acf.semiplot <- resp.acf.plot + labs(y = "ACF") +
    theme(
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank()
    )

save_plot(
    paste0(fig.path, 'acf_pacf_plot.pdf'),
    plot_grid(
        resp.acf.semiplot,
        resp.pacf.plot,
        nrow = 2,
        align = "v"
    )
)

resp.loess.plot <-
    resp.train.plot + geom_smooth(method = "loess",
                                  size = 1.5,
                                  colour = "red")

save_plot(paste0(fig.path, 'loess_plot.pdf'),
          resp.loess.plot,
          base_aspect_ratio = 2)

resp.ksmooth <-
    ksmooth(seq(1:nrow(resp.train)), resp$admis, "normal", bandwidth = 6)

resp.ksmooth <- resp.train %>% mutate(ksmooth = resp.ksmooth$y)

resp.ksmooth.plot <-
    resp.ksmooth %>% ggplot(aes(x = date)) + geom_line(aes(y = admis)) +
    geom_line(aes(y = ksmooth), colour = "red") +
    background_grid(major = "xy", minor = "none")

save_plot(paste0(fig.path, 'ksmooth_plot.pdf'),
          resp.ksmooth.plot,
          base_aspect_ratio = 2)

resp.monthly <-
    resp.train %>% index_by(year.month = yearmonth(date)) %>% summarize(admis.avg.month = mean(admis))

resp.monthly.plot <-
    resp.monthly %>% ggplot(aes(x = year.month, y = admis.avg.month)) + geom_line() +
    background_grid(major = "xy", minor = "none")

save_plot(paste0(fig.path, 'resp_monthly_plot.pdf'),
          resp.monthly.plot,
          base_aspect_ratio = 1.5)

resp.density.plot <- resp.train %>% ggplot(aes(admis)) +
    geom_histogram(aes(y = stat(density)),
                   binwidth = 60,
                   col = "black",
                   fill = "gray90") +
    geom_density(aes(y = ..density..), col = 'red')

save_plot(paste0(fig.path, 'resp_density_plot.pdf'),
          resp.density.plot,
          base_aspect_ratio = 1.3)

resp.heatmap <- resp.train %>% ggplot_calendar_heatmap('date',
                                                       'admis') +
    xlab(NULL) +
    ylab(NULL) +
    scale_fill_continuous(low = 'green', high = 'red') +
    facet_wrap( ~ Year, ncol = 1)

save_plot(paste0(fig.path, 'resp_heatmap.pdf'),
          resp.heatmap,
          base_aspect_ratio = 2)

resp.train <- resp.train %>% mutate(
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
    time = rep(0:47, length.out = nrow(resp.train))
) %>%
    mutate(weekend = if_else(day %in% c("Saturday", "Sunday"), 1, 0))

resp.train %>%
    write_csv(paste(data.path, 'resp_full.csv'))

resp.train %>% tsibble::index_by(week.day = day) %>% summarize(admis.avg = mean(admis),
                                                                    admis.sd = sd(admis)) %>% ggplot(aes(
                                                                        x = week.day,
                                                                        y = admis.avg,
                                                                        ymin = admis.avg - admis.sd,
                                                                        ymax = admis.avg + admis.sd
                                                                    )) + geom_errorbar(width = 0.3) + geom_pointrange()

resp.bp <-
    melt(resp.train,
         id.vars = c("week.day", "day"),
         measure.vars = "admis")

resp.box.plot <- resp.bp %>% ggplot(aes(x = day, y = value)) + geom_boxplot() + background_grid(minor = "xy")

save_plot(paste0(fig.path, 'resp_box_plot.pdf'),
          resp.box.plot,
          base_aspect_ratio = 1.5)

