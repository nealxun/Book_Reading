# objective: exploring facebook prophet package
# reference: https://github.com/facebook/prophet

# install the package
devtools::install_github("facebook/prophet", subdir="R", ref="8306ae3")

# load package
library(prophet)
library(tidyverse)

# read data
df <- read_csv("./data/example_wp_log_peyton_manning.csv")
ggplot(df, aes(x = ds, y = y)) +
        geom_line()

# modeling
m <- prophet(df)
future <- make_future_dataframe(m, periods = 365)
tail(future)
forecast <- predict(m, future)
tail(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])

# plotting
plot(m, forecast)
prophet_plot_components(m, forecast)

# another example
df <- canadian_gas %>%
        filter(year(Month) >= 1995) %>%
        as_tibble() %>%
        mutate(ds = as.Date(Month), y = Volume) %>%
        select(ds, y)
m <- prophet(df)
future <- make_future_dataframe(m, periods = 60, freq = "month")
forecast <- predict(m, future)
plot(m, forecast)



