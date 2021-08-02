# objective: efficient r programming book reading

# preparation
rm(list = ls())
# obtain the current source file work directory
# wd <- dirname(rstudioapi::getSourceEditorContext()$path)
# setwd(wd)

# load necessary packages
library(tidyverse)
library(fpp3)
library(broom)
library(patchwork)
library(fable.prophet)



#-----------------------------------------------#
########## chapter 1 getting started ############
#-----------------------------------------------#

#-----------------------------------------------#
########## chapter 2 time series graphics #######
#-----------------------------------------------#
# tsibble object
y <- tsibble(Year = 2015:2019, Observation = c(123,39,78,52,110), index = Year)
olympic_running
PBS %>%
        filter(ATC2=="A10") %>%
        select(Month, Concession, Type, Cost) %>%
        summarise(TotalC = sum(Cost)) %>%
        mutate(Cost = TotalC/1e6) -> a10

prison <- readr::read_csv("https://OTexts.com/fpp3/extrafiles/prison_population.csv")
prison <- prison %>%
        mutate(quarter = yearquarter(date)) %>%
        select(-date) %>%
        as_tsibble(key = c(state, gender, legal, indigenous), index = quarter)
prison

# time plots
melsyd_economy <- ansett %>%
        filter(Airports == "MEL-SYD", Class=="Economy")
melsyd_economy %>%
        autoplot(Passengers) +
        labs(title = "Ansett economy class passengers", subtitle = "Melbourne-Sydney") +
        xlab("Year")

a10 %>% autoplot(Cost) +
        ggtitle("Antidiabetic drug sales") +
        ylab("$ million") + xlab("Year")

# seasonal plot
a10 %>% gg_season(Cost, labels = "both") +
        ylab("$ million") +
        ggtitle("Seasonal plot: antidiabetic drug sales")

vic_elec %>% gg_season(Demand, period="day") + theme(legend.position = "none")
vic_elec %>% gg_season(Demand, period="week") + theme(legend.position = "none")
vic_elec %>% gg_season(Demand, period="year") + theme(legend.position = "right")

# seasonal subseries plot
a10 %>%
        gg_subseries(Cost) +
        ylab("$ million") +
        xlab("Year") +
        ggtitle("Seasonal subseries plot: antidiabetic drug sales")

# scatter plot
vic_elec %>%
        filter(year(Time) == 2014) %>%
        ggplot(aes(x = Temperature, y = Demand)) +
        geom_point() +
        ylab("Demand (GW)") + xlab("Temperature (Celsius)")

visitors <- tourism %>%
        group_by(State) %>%
        summarise(Trips = sum(Trips))
visitors %>%
        ggplot(aes(x = Quarter, y = Trips)) +
        geom_line() +
        facet_grid(vars(State), scales = "free_y") +
        ylab("Number of visitor nights each quarter (millions)")
# visitors %>%
#         spread(State, Trips) %>%
#         GGally::ggpairs(columns = 2:9)

# autocorrelation
recent_production <- aus_production %>%
        filter(year(Quarter) >= 1992)
recent_production %>% ACF(Beer, lag_max = 9)
recent_production %>% ACF(Beer) %>% autoplot()

# simulated series with increase trend, always pay attention to the definition
h <- 30
ls_months <- seq.Date(from = as.Date("2000-01-01"), by = "month", length.out = h)
value <- seq(h) + rnorm(30)
tsbl_trend <- tibble(date = ls_months, value = value) %>%
        mutate(month = yearmonth(date)) %>%
        select(-date) %>%
        as_tsibble(index = month)
autoplot(tsbl_trend, value)
tsbl_trend %>% ACF(value)
tsbl_trend %>% ACF(value) %>% autoplot()

# white noise
set.seed(30)
y <- tsibble(sample = 1:50, wn = rnorm(50), index = sample)
y %>% autoplot(wn) + ggtitle("White noise")
y %>% ACF(wn) %>% autoplot()




#----------------------------------------------------#
########## chapter 3 time series decomposition #######
#----------------------------------------------------#
# adjustment and transformations
global_economy %>%
        filter(Country == "Australia") %>%
        autoplot(GDP / Population)

# box-cos transformation
lambda <- aus_production %>%
        features(Gas, features = guerrero) %>%
        pull(lambda_guerrero)
autoplot(aus_production, Gas)
aus_production %>% autoplot(box_cox(Gas, lambda))

# time series components
us_retail_employment <- us_employment %>%
        filter(year(Month) >= 1990, Title == "Retail Trade") %>%
        select(-Series_ID)
us_retail_employment %>%
        autoplot(Employed) +
        xlab("Year") + ylab("Persons (thousands)") +
        ggtitle("Total employment in US retail")
dcmp <- us_retail_employment %>%
        model(STL(Employed))
components(dcmp)

us_retail_employment %>%
        autoplot(Employed, color='gray') +
        autolayer(components(dcmp), trend, color='red') +
        xlab("Year") + ylab("Persons (thousands)") +
        ggtitle("Total employment in US retail")
components(dcmp) %>% autoplot() + xlab("Year")

# moving averages
aus_exports <- global_economy %>%
        filter(Country == "Australia") %>%
        mutate(
                `5-MA` = slide_dbl(Exports, mean, .size = 5, .align = "center")
        )
aus_exports %>%
        autoplot(Exports) +
        autolayer(aus_exports, `5-MA`, color='red') +
        xlab("Year") + ylab("Exports (% of GDP)") +
        ggtitle("Total Australian exports") +
        guides(colour=guide_legend(title="series"))

# moving average of moving averages
beer <- aus_production %>%
        filter(year(Quarter) >= 1992) %>%
        select(Quarter, Beer)
beer_ma <- beer %>%
        mutate(
                `4-MA` = slide_dbl(Beer, mean, .size = 4, .align = "center-left"),
                `2x4-MA` = slide_dbl(`4-MA`, mean, .size = 2, .align = "center-right")
        )

us_retail_employment_ma <- us_retail_employment %>%
        mutate(
                `12-MA` = slide_dbl(Employed, mean, .size = 12, .align = "cr"),
                `2x12-MA` = slide_dbl(`12-MA`, mean, .size = 2, .align = "cl")
        )
us_retail_employment_ma %>%
        autoplot(Employed, color='gray') +
        autolayer(us_retail_employment_ma, vars(`2x12-MA`), color='red') +
        xlab("Year") + ylab("Persons (thousands)") +
        ggtitle("Total employment in US retail")

# classical decomposition
us_retail_employment %>%
        model(classical_decomposition(Employed, type = "additive")) %>%
        components() %>%
        autoplot() + xlab("Year") +
        ggtitle("Classical additive decomposition of total US retail employment")

# X11 decomposition
x11_dcmp <- us_retail_employment %>%
        model(x11 = feasts:::X11(Employed, type = "additive")) %>%
        components()
autoplot(x11_dcmp) + xlab("Year") +
        ggtitle("Additive X11 decomposition of US retail employment in the US")

x11_dcmp %>%
        ggplot(aes(x = Month)) +
        geom_line(aes(y = Employed, colour = "Data")) +
        geom_line(aes(y = season_adjust, colour = "Seasonally Adjusted")) +
        geom_line(aes(y = trend, colour = "Trend")) +
        xlab("Year") + ylab("Persons (thousands)") +
        ggtitle("Total employment in US retail") +
        scale_colour_manual(values=c("gray","blue","red"),
                            breaks=c("Data","Seasonally Adjusted","Trend"))

# SEATS decomposition
seats_dcmp <- us_retail_employment %>%
        model(seats = feasts:::SEATS(Employed)) %>%
        components()
autoplot(seats_dcmp) + xlab("Year") +
        ggtitle("SEATS decomposition of total US retail employment")

# STL decomposition, trend window and season window need to be an odd number to achieve symmetric
us_retail_employment %>%
        model(STL(Employed ~ trend(window=7) + season(window='periodic'),
                  robust = TRUE)) %>%
        components() %>%
        autoplot()




#-----------------------------------------------#
########## chapter 4 timer series features ############
#-----------------------------------------------#
# some simple statistics
head(tourism)
tourism %>% features(Trips, list(mean = mean)) %>% arrange(mean)
tourism %>% features(Trips, list(mean = mean, max = max, quantile)) %>% arrange(mean)

# acf features
tourism %>% features(Trips, feat_acf)
tourism %>% features(Trips, .period = 4, feat_acf)

# stl features
tourism %>%
        features(Trips, feat_stl)
tourism %>%
        features(Trips, feat_stl) %>%
        ggplot(aes(x=trend_strength, y=seasonal_strength_year, col=Purpose)) +
        geom_point() + facet_wrap(vars(State))

# most seasonal serie
tourism %>%
        features(Trips, feat_stl) %>%
        filter(seasonal_strength_year == max(seasonal_strength_year)) %>%
        left_join(tourism, by = c("State","Region","Purpose")) %>%
        ggplot(aes(x = Quarter, y = Trips)) + geom_line() +
        facet_grid(vars(State,Region,Purpose))
# most trended serie
tourism %>%
        features(Trips, feat_stl) %>%
        filter(trend_strength == max(trend_strength)) %>%
        left_join(tourism, by = c("State","Region","Purpose")) %>%
        ggplot(aes(x = Quarter, y = Trips)) + geom_line() +
        facet_grid(vars(State,Region,Purpose))

# other features
tourism_features <- tourism %>%
        features(Trips, feature_set(pkgs="feasts"))
tourism_features

# tourism_features %>%
#         select_at(vars(contains("season"), Purpose)) %>%
#         mutate(
#                 seasonal_peak_year = glue::glue("Q{seasonal_peak_year+1}"),
#                 seasonal_trough_year = glue::glue("Q{seasonal_trough_year+1}"),
#         ) %>%
#         GGally::ggpairs(mapping = aes(colour=Purpose))

# principal component analysis
pcs <- tourism_features %>%
        select(-State, -Region, -Purpose) %>%
        prcomp(scale=TRUE) %>%
        augment(tourism_features)
pcs %>%
        ggplot(aes(x=.fittedPC1, y=.fittedPC2, col=Purpose)) +
        geom_point() + theme(aspect.ratio=1)

# identify outliers
outliers <- pcs %>%
        filter(.fittedPC1 > 10.5) %>%
        select(Region, State, Purpose, .fittedPC1, .fittedPC2)
outliers
outliers %>%
        left_join(tourism, by = c("State", "Region", "Purpose")) %>%
        mutate(Series = glue::glue("{State}", "{Region}", "{Purpose}", .sep = "\n\n")) %>%
        ggplot(aes(x = Quarter, y = Trips)) +
        geom_line() +
        facet_grid(Series ~ ., scales='free') +
        ggtitle("Outlying time series in PC space")





#--------------------------------------------------------#
########## chapter 5 the forecaster's toolbox ############
#--------------------------------------------------------#
# a tidy forecasting working flow
head(global_economy)
global_economy %>%
        filter(Country=="Sweden") %>%
        autoplot(GDP) +
        ggtitle("GDP for Sweden") + ylab("$US billions")
fit <- global_economy %>%
        model(trend_model = TSLM(GDP ~ trend()))
fit %>% forecast(h = "3 years")
fit %>% forecast(h = "3 years") %>%
        filter(Country=="Sweden") %>%
        autoplot(global_economy) +
        ggtitle("GDP for Sweden") + ylab("$US billions")

# some simple forecasting methods
bricks <- aus_production %>% filter_index(1970 ~ 2004)
# average method
fit <- bricks %>% model(MEAN(Bricks))
autoplot(forecast(fit, h = 10), bricks)
# naive method
bricks %>% model(NAIVE(Bricks))
# RW(Bricks) is an equivalent alternative
# seasonal naive method
bricks %>% model(SNAIVE(Bricks ~ lag("year")))
# drift model
bricks %>% model(RW(Bricks ~ drift()))

# a comprehensive example
# Set training data from 1992 to 2006
train <- aus_production %>% filter_index("1992 Q1" ~ "2006 Q4")
# Fit the models
beer_fit <- train %>%
        model(
                Mean = MEAN(Beer),
                `Naïve` = NAIVE(Beer),
                `Seasonal naïve` = SNAIVE(Beer)
        )
# Generate forecasts for 14 quarters
beer_fc <- beer_fit %>% forecast(h=14)
# Plot forecasts against actual values
beer_fc %>%
        autoplot(train, level = NULL) +
        autolayer(filter_index(aus_production, "2007 Q1" ~ .), color = "black") +
        ggtitle("Forecasts for quarterly beer production") +
        xlab("Year") + ylab("Megalitres") +
        guides(colour=guide_legend(title="Forecast"))

# fitted and residuals values
augment(beer_fit)

# residuals diagnostics
# Re-index based on trading days
google_stock <- gafa_stock %>%
        filter(Symbol == "GOOG") %>%
        mutate(day = row_number()) %>%
        update_tsibble(index = day, regular = TRUE)
# Filter the year of interest
google_2015 <- google_stock %>% filter(year(Date) == 2015)
google_2015 %>% autoplot(Close) +
        xlab("Day") + ylab("Closing Price (US$)") +
        ggtitle("Google Stock in 2015")
aug <- google_2015 %>% model(NAIVE(Close)) %>% augment()
aug %>% autoplot(.resid) + xlab("Day") + ylab("") +
        ggtitle("Residuals from naïve method")
aug %>%
        ggplot(aes(x = .resid)) +
        geom_histogram() +
        ggtitle("Histogram of residuals")
aug %>% ACF(.resid) %>% autoplot() + ggtitle("ACF of residuals")
google_2015 %>% model(NAIVE(Close)) %>% gg_tsresiduals()

# formal statistics test
aug %>% features(.resid, box_pierce, lag=10, dof=0)
aug %>% features(.resid, ljung_box, lag=10, dof=0)

# prediction intervals
google_2015 %>%
        model(NAIVE(Close)) %>%
        forecast(h = 10) %>%
        hilo()
google_2015 %>%
        model(NAIVE(Close)) %>%
        forecast(h = 10) %>%
        autoplot(google_2015, level = c(80, 95))

# bootstrapped confidence interval
fit <- google_2015 %>%
        model(NAIVE(Close))
sim <- fit %>% generate(h = 30, times = 5, bootstrap = TRUE)
sim
google_2015 %>%
        ggplot(aes(x = day)) +
        geom_line(aes(y = Close)) +
        geom_line(aes(y = .sim, colour = as.factor(.rep)), data = sim) +
        ggtitle("Google closing stock price") +
        guides(col = FALSE)
fc <- fit %>% forecast(h = 30, bootstrap = TRUE)
fc
fc %>% autoplot(google_2015) +
        ggtitle("Google closing stock price")

# forecast within a specified interval
scaled_logit <- new_transformation(
        transformation = function(x, lower=0, upper=1){
                log((x-lower)/(upper-x))
        },
        inverse = function(x, lower=0, upper=1){
                (upper-lower)*exp(x)/(1+exp(x)) + lower
        }
)

eggs <- as_tsibble(fma::eggs)
fit <- eggs %>% model(RW(log(value) ~ drift()))
fc <- fit %>% forecast(h=50) %>%
        mutate(Forecast = "Bias adjusted")
fc_biased <- fit %>% forecast(h=50, bias_adjust = FALSE) %>%
        mutate(Forecast = "Simple back transformation")
eggs %>% autoplot(value) +
        autolayer(fc_biased, level = 80) +
        autolayer(fc, level = NULL, color = "red")

# forecasting with decomposition
us_retail_employment <- us_employment %>%
        filter(year(Month) >= 1990, Title == "Retail Trade")
dcmp <- us_retail_employment %>%
        model(STL(Employed ~ trend(window = 7), robust=TRUE)) %>%
        components() %>%
        select(-.model)
dcmp %>%
        model(ETS(season_adjust ~ season("N"))) %>%
        forecast() %>%
        autoplot(dcmp) + ylab("New orders index") +
        ggtitle("Naive forecasts of seasonally adjusted data")

us_retail_employment %>%
        model(stlf = decomposition_model(
                STL(Employed ~ trend(window = 7), robust = TRUE),
                ETS(season_adjust ~ season("N")),
                SNAIVE(season_year)
        )) %>%
        forecast() %>%
        autoplot(us_retail_employment)

# forecast accuracy
# subset observations from each key
aus_retail %>%
        group_by(State, Industry) %>%
        slice(1:12)

a <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 100)
a_median <- median(a)
a_mean <- mean(a)
sum(abs(a - a_median))
sum(abs(a - a_mean))
sum(abs(a - a_median)) < sum(abs(a - a_mean))
sum((a - a_median) ^ 2)
sum((a - a_mean) ^ 2)
sum((a - a_median) ^ 2) < sum((a - a_mean) ^ 2)

# example, beer production
recent_production <- aus_production %>% filter(year(Quarter) >= 1992)
beer_train <- recent_production %>% filter(year(Quarter) <= 2007)

beer_fit <- beer_train %>%
        model(
                Mean = MEAN(Beer),
                `Naïve` = NAIVE(Beer),
                `Seasonal naïve` = SNAIVE(Beer),
                Drift = RW(Beer ~ drift())
        )

beer_fc <- beer_fit %>%
        forecast(h = 10)
accuracy(beer_fc, recent_production)

beer_fc %>%
        autoplot(filter(aus_production, year(Quarter) >= 1992), level = NULL) +
        xlab("Year") + ylab("Megalitres") +
        ggtitle("Forecasts for quarterly beer production") +
        guides(colour=guide_legend(title="Forecast"))

# time series cross-validation
# Time series cross-validation accuracy
google_2015_tr <- google_2015 %>%
        slice(1:(n()-1)) %>%
        stretch_tsibble(.init = 3, .step = 1)
fc <- google_2015_tr %>%
        model(RW(Close ~ drift())) %>%
        forecast(h=1)
fc %>% accuracy(google_2015)
# Residual accuracy
google_2015 %>% model(RW(Close ~ drift())) %>% accuracy()

# multi-setp cross validation
google_2015_tr <- google_2015 %>%
        slice(1:(n()-8)) %>%
        stretch_tsibble(.init = 3, .step = 1)

fc <- google_2015_tr %>%
        model(RW(Close ~ drift())) %>%
        forecast(h=8) %>%
        group_by(.id) %>%
        mutate(h = row_number()) %>%
        ungroup()

fc %>%
        accuracy(google_2015, by = "h") %>%
        ggplot(aes(x = h, y = RMSE)) + geom_point()




#-----------------------------------------------#
########## chapter 6 judgmental forecasts ############
#-----------------------------------------------#


#-------------------------------------------------------------#
########## chapter 7 time series regression models ############
#-------------------------------------------------------------#
us_change %>%
        ggplot(aes(x = Quarter)) +
        geom_line(aes(y = Consumption, colour = "Consumption")) +
        geom_line(aes(y = Income, colour = "Income")) +
        ylab("% change") + xlab("Year") +
        guides(colour=guide_legend(title="series"))
us_change %>%
        ggplot(aes(x=Income, y=Consumption)) +
        ylab("Consumption (quarterly % change)") +
        xlab("Income (quarterly % change)") +
        geom_point() +
        geom_smooth(method="lm", se=FALSE)
# us_change %>%
#         GGally::ggpairs(columns = 2:6)

# multi-linear regression
fit.consMR <- us_change %>%
        model(
                tslm = TSLM(Consumption ~ Income + Production + Unemployment + Savings)
        )
report(fit.consMR)
augment(fit.consMR) %>%
        ggplot(aes(x = Quarter)) +
        geom_line(aes(y = Consumption, colour = "Data")) +
        geom_line(aes(y = .fitted, colour = "Fitted")) +
        xlab("Year") + ylab(NULL) +
        ggtitle("Percent change in US consumption expenditure") +
        guides(colour=guide_legend(title=NULL))
augment(fit.consMR) %>%
        ggplot(aes(x=Consumption, y=.fitted)) +
        geom_point() +
        ylab("Fitted (predicted values)") +
        xlab("Data (actual values)") +
        ggtitle("Percent change in US consumption expenditure") +
        geom_abline(intercept=0, slope=1)

# residual diagnostics
fit.consMR %>% gg_tsresiduals()
augment(fit.consMR) %>% features(.resid, ljung_box, lag = 10, dof = 5)
df <- left_join(us_change, residuals(fit.consMR), by = "Quarter")
p1 <- ggplot(df, aes(x=Income, y=.resid)) +
        geom_point() + ylab("Residuals")
p2 <- ggplot(df, aes(x=Production, y=.resid)) +
        geom_point() + ylab("Residuals")
p3 <- ggplot(df, aes(x=Savings, y=.resid)) +
        geom_point() + ylab("Residuals")
p4 <- ggplot(df, aes(x=Unemployment, y=.resid)) +
        geom_point() + ylab("Residuals")
(p1 | p2) / (p3 | p4)
augment(fit.consMR) %>%
        ggplot(aes(x=.fitted, y=.resid)) +
        geom_point() +
        labs(x = "Fitted", y = "Residuals")

# spurious regression
fit <- aus_airpassengers %>%
        left_join(guinea_rice, by = "Year") %>%
        model(TSLM(Passengers ~ Production))
report(fit)

# trend and seasonal dummy variables
recent_production <- aus_production %>%
        filter(year(Quarter) >= 1992)
recent_production %>%
        autoplot(Beer) +
        labs(x = "Year", y = "Megalitres")
fit_beer <- recent_production %>%
        model(TSLM(Beer ~ trend() + season()))
report(fit_beer)
augment(fit_beer) %>%
        ggplot(aes(x = Quarter)) +
        geom_line(aes(y = Beer, colour = "Data")) +
        geom_line(aes(y = .fitted, colour = "Fitted")) +
        labs(x = "Year", y = "Megalitres",
             title = "Quarterly Beer Production")
augment(fit_beer) %>%
        ggplot(aes(x = Beer, y = .fitted,
                   colour = factor(quarter(Quarter)))) +
        geom_point() +
        ylab("Fitted") + xlab("Actual values") +
        ggtitle("Quarterly beer production") +
        scale_colour_brewer(palette="Dark2", name="Quarter") +
        geom_abline(intercept=0, slope=1)

# fourier terms
fourier_beer <- recent_production %>%
        model(TSLM(Beer ~ trend() + fourier(K=2)))
report(fourier_beer)
augment(fourier_beer) %>%
        ggplot(aes(x = Quarter)) +
        geom_line(aes(y = Beer, colour = "Data")) +
        geom_line(aes(y = .fitted, colour = "Fitted")) +
        labs(x = "Year", y = "Megalitres",
             title = "Quarterly Beer Production")

# forecasting with regression
recent_production <- aus_production %>% filter(year(Quarter) >= 1992)
fit_beer <- recent_production %>%
        model(TSLM(Beer ~ trend() + season()))
fc_beer <- forecast(fit_beer)
fc_beer %>%
        autoplot(recent_production) +
        ggtitle("Forecasts of beer production using regression") +
        xlab("Year") + ylab("megalitres")

# scenario based forecasting
fit_consBest <- us_change %>%
        model(lm = TSLM(Consumption ~ Income + Savings + Unemployment))
up_future <- new_data(us_change, 4) %>%
        mutate(Income = 1, Savings = 0.5, Unemployment = 0)
down_future <- new_data(us_change, 4) %>%
        mutate(Income = -1, Savings = -0.5, Unemployment = 0)
fc_up <- forecast(fit_consBest, new_data = up_future) %>%
        mutate(Scenario = "Increase") %>%
        as_fable(response=Consumption, key = Scenario)
fc_down <- forecast(fit_consBest, new_data = down_future) %>%
        mutate(Scenario = "Decrease") %>%
        as_fable(response=Consumption, key = Scenario)
us_change %>%
        autoplot(Consumption) +
        autolayer(rbind(fc_up, fc_down)) +
        ylab("% change in US consumption")

# nonlinear regression
boston_men <- boston_marathon %>%
        filter(Event == "Men's open division") %>%
        mutate(Minutes = as.numeric(Time)/60)
fit_trends <- boston_men %>%
        model(
                linear = TSLM(Minutes ~ trend()),
                exponential = TSLM(log(Minutes) ~ trend()),
                piecewise = TSLM(Minutes ~ trend(knots = c(1940, 1980)))
        )
fc_trends <- fit_trends %>% forecast(h=10)
boston_men %>%
        autoplot(Minutes) +
        geom_line(aes(y=.fitted, colour=.model), data = fitted(fit_trends)) +
        autolayer(fc_trends, alpha = 0.5, level = 95) +
        xlab("Year") +  ylab("Winning times in minutes") +
        ggtitle("Boston Marathon") +
        guides(colour=guide_legend(title="Model"))

# dummy variable trap
df <- read_csv("dummy_varialbe_trap.csv")
fit <- lm(formula = y ~ ., data = df)
summary(fit)





#-----------------------------------------------------#
########## chapter 8 exponential smoothing ############
#-----------------------------------------------------#
## simple exponential smoothing
algeria_economy <- global_economy %>%
        filter(Country == "Algeria")
algeria_economy %>%
        autoplot(Exports) +
        ylab("Exports (% of GDP)") + xlab("Year")
# Estimate parameters
fit <- algeria_economy %>%
        model(ETS(Exports ~ error("A") + trend("N") + season("N"), opt_crit = "mse"))
report(fit) # see model parameters
fc <- fit %>%
        forecast(h = 5)
fc %>%
        autoplot(algeria_economy) +
        geom_line(aes(y = .fitted, colour = "Fitted"), data = augment(fit)) +
        ylab("Exports (% of GDP)") + xlab("Year")

# choose a small alpha, the fitted value series will be smoother
fit <- algeria_economy %>%
        model(ETS(Exports ~ error("A") + trend("N", alpha = 0.2) + season("N"), opt_crit = "mse"))
report(fit) # see model parameters
fc <- fit %>%
        forecast(h = 5)
fc %>%
        autoplot(algeria_economy) +
        geom_line(aes(y = .fitted, colour = "Fitted"), data = augment(fit)) +
        ylab("Exports (% of GDP)") + xlab("Year")

## methods with trend
aus_economy <- global_economy %>%
        filter(Code == "AUS") %>%
        mutate(Pop = Population / 1e6)
fit <- aus_economy %>%
        model(AAN = ETS(Pop ~ error("A") + trend("Ad", phi = 0.9) + season("N")))
report(fit)
fc <- fit %>% forecast(h = 20)
autoplot(select(aus_economy, Year, Pop)) +
        autolayer(fc) +
        geom_line(aes(y = .fitted, colour = "Fitted"), data = augment(fit))

# damped method
aus_economy %>%
        model(
                `Holt's method` = ETS(Pop ~ error("A") + trend("A") + season("N")),
                `Damped Holt's method` = ETS(Pop ~ error("A") + trend("Ad", phi = 0.9) + season("N"))
        ) %>%
        forecast(h = 15) %>%
        autoplot(aus_economy, level = NULL) +
        ggtitle("Forecasts from Holt's method") + xlab("Year") +
        ylab("Population of Australia (millions)") +
        guides(colour = guide_legend(title = "Forecast"))

# internet usage
www_usage <- as_tsibble(WWWusage)
www_usage %>% autoplot(value) +
        xlab("Minute") + ylab("Number of users")
www_usage %>%
        # rolling over time series with an initial windown size, output id means number of time series for cross validation
        stretch_tsibble(.init = 10) %>%
        model(
                SES = ETS(value ~ error("A") + trend("N") + season("N")),
                Holt = ETS(value ~ error("A") + trend("A") + season("N")),
                Damped = ETS(value ~ error("A") + trend("Ad") + season("N"))
        ) %>%
        forecast(h = 1) %>%
        accuracy(www_usage)
fit <- www_usage %>%
        model(Damped = ETS(value ~ error("A") + trend("Ad") + season("N")))
# Estimated parameters:
tidy(fit)
fit %>%
        forecast(h = 10) %>%
        autoplot(www_usage, level = c(80, 95)) +
        xlab("Minute") + ylab("Number of users")

## methods with seasonality
aus_holidays <- tourism %>%
        filter(Purpose == "Holiday") %>%
        summarise(Trips = sum(Trips))
fit <- aus_holidays %>%
        model(
                additive = ETS(Trips ~ error("A") + trend("A") + season("A")),
                multiplicative = ETS(Trips ~ error("M") + trend("A") + season("M"))
        )
fc <- fit %>% forecast(h = "3 years")

fc %>%
        autoplot(aus_holidays, level = NULL) + xlab("Year") +
        ylab("Overnight trips (millions)") +
        scale_color_brewer(type = "qual", palette = "Dark2")

# daily data
sth_cross_ped <- pedestrian %>%
        filter(Sensor == "Southern Cross Station", yearmonth(Date) == yearmonth("2016 July")) %>%
        index_by(Date) %>%
        summarise(Count = sum(Count))
fit <- sth_cross_ped %>%
        model(hw = ETS(Count ~ error("M") + trend("Ad") + season("M")))
fit %>% 
        forecast(h = "5 weeks") %>%
        autoplot(sth_cross_ped)


## estimation and model selection
aus_holidays <- tourism %>%
        filter(Purpose == "Holiday") %>%
        summarise(Trips = sum(Trips))
fit <- aus_holidays %>%
        model(ETS(Trips, ic = "aicc"))
report(fit)
components(fit) %>%
        autoplot() +
        ggtitle("ETS(M,N,M) components")
autoplot(aus_holidays) +
        autolayer(forecast(fit, h = 12))




#-----------------------------------------------#
########## chapter 9 ARIMA models ############
#-----------------------------------------------#
google_stock <- gafa_stock %>%
        filter(Symbol == "GOOG") %>%
        mutate(day = row_number()) %>%
        update_tsibble(index = day, regular = TRUE)
google_2015 <- google_stock %>% filter(year(Date) == 2015)
# H0 of ljung box test is the series is a white noise, therefore, large p value cannot reject H0
google_2015 %>%
        mutate(diff_close = difference(Close)) %>%
        features(diff_close, ljung_box, lag = 10)

# difference
PBS %>%
        filter(ATC2 == "H02") %>%
        summarise(Cost = sum(Cost)/1e6) %>%
        transmute(
                `Sales ($million)` = Cost,
                `Log sales` = log(Cost),
                `Annual change in log sales` = difference(log(Cost), 12),
                `Doubly differenced log sales` = difference(difference(log(Cost), 12), 1)
        ) %>%
        gather("Type", "Sales", !!!syms(measured_vars(.)), factor_key = TRUE) %>%
        ggplot(aes(x = Month, y = Sales)) +
        geom_line() +
        facet_grid(vars(Type), scales = "free_y") +
        labs(title = "Corticosteroid drug sales", x = "Year", y = NULL)

# unit root test, test whether a series is stationary
google_2015 %>%
        features(Close, unitroot_kpss)
google_2015 %>%
        mutate(diff_close = difference(Close)) %>%
        features(diff_close, unitroot_kpss)
# number of first differences needed
google_2015 %>%
        features(Close, unitroot_ndiffs)
# number of seasonal differences needed
google_2015 %>%
        features(Close, unitroot_nsdiffs)

# non-seasonal ARIMA models
us_change <- readr::read_csv("https://otexts.com/fpp3/extrafiles/us_change.csv") %>%
        mutate(Time = yearquarter(Time)) %>%
        as_tsibble(index = Time)
us_change %>% autoplot(Consumption) +
        labs(x = "Year", y = "Quarterly percentage change", title = "US consumption")
fit <- us_change %>%
        model(ARIMA(Consumption ~ 1 + pdq(0,1,0) + PDQ(0,0,0)))
report(fit)
fcst <- fit %>% forecast(h=100)
# suppress confidence interval
autoplot(fcst, us_change, level = NULL) +
        geom_line(aes(y = .fitted, colour = "Fitted"), data = augment(fit))

# ARIMA modeling in R
# seasonally adjusted electrical equipment orders
elec_equip <- as_tsibble(fpp2::elecequip)
elec_dcmp <- elec_equip %>%
        model(STL(value ~ season(window="periodic"))) %>%
        components() %>%
        select(-.model) %>%
        as_tsibble()
ggplot(elec_dcmp) +
        geom_line(aes(x = index, y = value, color = "actual")) +
        geom_line(aes(x = index, y = season_adjust, color = "seasonal_adjust"))
# first order difference once
elec_dcmp %>%
        gg_tsdisplay(difference(season_adjust), plot_type='partial')
fit <- elec_dcmp %>%
        model(
                arima = ARIMA(season_adjust ~ 1 + pdq(1,1,1) + PDQ(0,0,0))
        )
report(fit)
# residual analysis
fit %>% gg_tsresiduals()
augment(fit) %>%
        features(.resid, ljung_box, lag = 24, dof = 4)
fit %>% forecast() %>% autoplot(elec_dcmp)
# inverse unit root plot
gg_arma(fit)


## seasonal ARIMA models
eu_retail <- as_tsibble(fpp2::euretail)
eu_retail %>% autoplot(value) + ylab("Retail index") + xlab("Year")
eu_retail %>% gg_tsdisplay(difference(value, 4), plot_type='partial')
eu_retail %>% gg_tsdisplay(value %>% difference(4) %>% difference(),
                           plot_type='partial')

# modeling
fit <- eu_retail %>%
        model(arima = ARIMA(value ~ pdq(0,1,1) + PDQ(0,1,1)))
fit %>% gg_tsresiduals()
fit <- eu_retail %>%
        model(ARIMA(value ~ pdq(0,1,3) + PDQ(0,1,1)))
fit %>% gg_tsresiduals()

# generating forecast
fit %>% forecast(h=60) %>% autoplot(eu_retail, level = NULL)

# auto arima model
eu_retail %>%
        model(ARIMA(value))

# Corticosteroid monthly drug sales in Australia
h02 <- PBS %>%
        filter(ATC2 == "H02") %>%
        summarise(Cost = sum(Cost)/1e6)
h02 %>%
        # logarithms to stabilize the variance
        mutate(log(Cost)) %>%
        gather() %>%
        ggplot(aes(x = Month, y = value)) +
        geom_line() +
        facet_grid(key ~ ., scales = "free_y") +
        xlab("Year") + ylab("") +
        ggtitle("Cortecosteroid drug scripts (H02)")
h02 %>% gg_tsdisplay(difference(log(Cost), 12), plot_type='partial', lag_max = 24)

# manual model selection
fit <- h02 %>%
        model(ARIMA(log(Cost) ~ 0 + pdq(3,0,1) + PDQ(0,1,2)))
report(fit)
fit %>% gg_tsresiduals(lag_max=36)
augment(fit) %>%
        features(.resid, ljung_box, lag = 36, dof = 6)

# auto model selection
fit <- h02 %>%
        model(ARIMA(log(Cost)))
report(fit)

# generate the forecast
h02 %>%
        model(ARIMA(log(Cost) ~ 0 + pdq(3,0,1) + PDQ(0,1,2))) %>%
        forecast() %>%
        autoplot(h02) +
        ylab("H02 sales (million scripts)") + xlab("Year")

# Example: Comparing ARIMA() and ETS() on non-seasonal data
aus_economy <- global_economy %>% filter(Code == "AUS") %>%
        mutate(Population = Population/1e6)

aus_economy %>%
        slice(-n()) %>% # remove last row
        stretch_tsibble(.init = 10) %>% # prepare data for cross-validation, see chapter 5.9
        model(
                ETS(Population),
                ARIMA(Population)
        ) %>%
        forecast(h = 1) %>%
        accuracy(aus_economy)

# generate forecast
aus_economy %>%
        model(ets = ETS(Population), 
              arima = ARIMA(Population ~ pdq(1,1,2))) %>%
        forecast(h = "150 years") %>%
        autoplot(aus_economy, level = NULL)

# Example: Comparing ARIMA() and ETS() on seasonal data
# Consider the cement data beginning in 1988
cement <- aus_production %>%
        filter(year(Quarter) >= 1988)
autoplot(cement, .vars = Cement)
cement %>% features(Cement, feat_stl)

# Use 20 years of the data as the training set
train <- cement %>%
        filter(year(Quarter) <= 2007)

# arima model
fit_arima <- train %>% model(ARIMA(Cement))
report(fit_arima)
gg_tsresiduals(fit_arima, lag_max = 16)
augment(fit_arima) %>%
        features(.resid, ljung_box, lag = 16, dof = 6)

# ets model
fit_ets <- train %>% model(ETS(Cement ~ error("A") + trend("A") + season("A")))
report(fit_ets)
augment(fit_ets) %>%
        features(.resid, ljung_box, lag = 16, dof = 6)

# Generate forecasts and compare accuracy over the test set
bind_rows(
        fit_arima %>% accuracy(),
        fit_ets %>% accuracy(),
        fit_arima %>% forecast(h = "2 years 6 months") %>%
                accuracy(cement),
        fit_ets %>% forecast(h = "2 years 6 months") %>%
                accuracy(cement)
)

# Generate forecasts from an ARIMA model
train %>% 
        model(arima = ARIMA(Cement), ets = ETS(Cement ~ error("A") + trend("A") + season("A"))) %>% 
        forecast(h="3 years") %>% 
        autoplot(cement, level = NULL)

# entire data set
cement %>% 
        model(arima = ARIMA(Cement), ets = ETS(Cement ~ error("A") + trend("A") + season("A"))) %>% 
        forecast(h="3 years") %>% 
        autoplot(cement, level = NULL)




#-----------------------------------------------#
########## chapter 10 dynamic regression models ############
#-----------------------------------------------#
# regression with ARIMA errors in R
us_change %>%
        gather("var", "value", Consumption, Income) %>%
        ggplot(aes(x = Quarter, y = value)) +
        geom_line() +
        facet_grid(vars(var), scales = "free_y") +
        xlab("Year") + ylab(NULL) +
        ggtitle("Quarterly changes in US consumption and personal income")
fit <- us_change %>%
        model(ARIMA(Consumption ~ Income))
report(fit)
# ARIMA errors (should be white noise) and regression errors
df <- bind_rows(
        `Regression Errors` = as_tibble(residuals(fit, type="regression")),
        `ARIMA Errors` = as_tibble(residuals(fit, type="innovation")),
        .id = "type"
)
ggplot(df, aes(x = Quarter, y = .resid)) +
        geom_line() +
        facet_grid(vars(type), scales = "free_y") +
        xlab("Year") + ylab(NULL)

# ARIMA errors
fit %>% gg_tsresiduals()
augment(fit) %>%
        features(.resid, ljung_box, dof = 5, lag = 8)

# regression errors
ts_regression_erros <- df %>% 
        filter(type == "Regression Errors") %>% 
        as_tsibble(key = "type")
ACF(ts_regression_erros, .resid) %>% autoplot()
PACF(ts_regression_erros, .resid) %>% autoplot()
ts_regression_erros %>% features(.resid, ljung_box)

# forecasting
# Example: US personal consumption and income
us_change_future <- new_data(us_change, 8) %>% mutate(Income = mean(us_change$Income))
forecast(fit, new_data = us_change_future) %>%
        autoplot(us_change) + xlab("Year") +
        ylab("Percentage change")

# example: forecasting electricity demand
vic_elec_daily <- vic_elec %>%
        filter(year(Time) == 2014) %>%
        index_by(Date = date(Time)) %>%
        summarise(
                Demand = sum(Demand)/1e3,
                Temperature = max(Temperature),
                Holiday = any(Holiday)
        ) %>%
        mutate(Day_Type = case_when(
                Holiday ~ "Holiday",
                wday(Date) %in% 2:6 ~ "Weekday",
                TRUE ~ "Weekend"
        ))

vic_elec_daily %>%
        ggplot(aes(x=Temperature, y=Demand, colour=Day_Type)) +
        geom_point() +
        ylab("Electricity demand (GW)") +
        xlab("Maximum daily temperature")

# fit a quadratic regression model
fit <- vic_elec_daily %>%
        model(ARIMA(Demand ~ Temperature + I(Temperature^2) + (Day_Type=="Weekday")))
report(fit)
fit %>% gg_tsresiduals()
augment(fit) %>%
        features(.resid, ljung_box, dof = 8, lag = 14)

# generate forecast
vic_elec_future <- new_data(vic_elec_daily, 14) %>%
        mutate(
                Temperature = 26,
                Holiday = c(TRUE, rep(FALSE, 13)),
                Day_Type = case_when(
                        Holiday ~ "Holiday",
                        wday(Date) %in% 2:6 ~ "Weekday",
                        TRUE ~ "Weekend"
                )
        )
forecast(fit, vic_elec_future) %>%
        autoplot(vic_elec_daily) + ylab("Electricity demand (GW)")

# stochastic and deterministic trends
aus_visitors <- as_tsibble(fpp2::austa)
aus_visitors %>%
        autoplot(value) +
        labs(x = "Year", y = "millions of people",
             title = "Total annual international visitors to Australia")
fit_deterministic <- aus_visitors %>%
        model(ARIMA(value ~ trend() + pdq(d = 0)))
report(fit_deterministic)
fit_stochastic <- aus_visitors %>%
        model(ARIMA(value ~ pdq(d=1)))
report(fit_stochastic)

fcst_deterministic <- fit_deterministic %>% forecast(h = 10)# %>% mutate(.model = "Deterministic")
fcst_stochastic <- fit_stochastic %>% forecast(h = 10)# %>% mutate(.model = "Stochastic")

bind_rows(fcst_deterministic, fcst_stochastic) %>%
        autoplot(aus_visitors) +
        labs(x = "Year", y = "Visitors to Australia (millions)",
             title = "Forecasts from trend models")

# dynamic harmonic regression
aus_cafe <- aus_retail %>%
        filter(
                Industry == "Cafes, restaurants and takeaway food services",
                year(Month) %in% 2004:2018
        ) %>%
        summarise(Turnover = sum(Turnover))

fit <- aus_cafe %>%
        model(
                `K = 1` = ARIMA(log(Turnover) ~ fourier(K = 1) + PDQ(0,0,0)),
                `K = 2` = ARIMA(log(Turnover) ~ fourier(K = 2) + PDQ(0,0,0)),
                `K = 3` = ARIMA(log(Turnover) ~ fourier(K = 3) + PDQ(0,0,0)),
                `K = 4` = ARIMA(log(Turnover) ~ fourier(K = 4) + PDQ(0,0,0)),
                `K = 5` = ARIMA(log(Turnover) ~ fourier(K = 5) + PDQ(0,0,0)),
                `K = 6` = ARIMA(log(Turnover) ~ fourier(K = 6) + PDQ(0,0,0))
        )

fit %>%
        forecast(h = "2 years") %>%
        autoplot(aus_cafe) +
        facet_wrap(vars(.model), ncol = 2) +
        guides(colour = FALSE) +
        geom_label(
                aes(x = yearmonth("2007 Jan"), y = 4250, label = paste0("AICc = ", format(AICc))),
                data = glance(fit)
        )

# lagged predictors
insurance <- as_tsibble(fpp2::insurance, pivot_longer = FALSE)

insurance %>%
        gather("key", "value", Quotes, TV.advert) %>%
        ggplot(aes(x = index, y = value, color = key)) +
        geom_line() +
        #facet_grid(vars(key), scales = "free_y") +
        labs(x = "Year", y = NULL,
             title = "Insurance advertising and quotations")

# model selection, based on AICc.
fit <- insurance %>%
        # Restrict data so models use same fitting period
        mutate(Quotes = c(NA,NA,NA,Quotes[4:40])) %>%
        # Estimate models
        model(
                lag0 = ARIMA(Quotes ~ pdq(d = 0) + TV.advert),
                lag1= ARIMA(Quotes ~ pdq(d = 0) + TV.advert + lag(TV.advert)),
                lag2= ARIMA(Quotes ~ pdq(d = 0) + TV.advert + lag(TV.advert) + lag(TV.advert, 2)),
                lag3= ARIMA(Quotes ~ pdq(d = 0) + TV.advert + lag(TV.advert) + lag(TV.advert, 2) + lag(TV.advert, 3))
        )
glance(fit)

# refit the entire data set
fit_best <- insurance %>%
        model(ARIMA(Quotes ~ pdq(d = 0) + TV.advert + lag(TV.advert)))
report(fit_best)

# generate forecast
insurance_future <- new_data(insurance, 20) %>%
        mutate(TV.advert = 8)

fit_best %>%
        forecast(insurance_future) %>%
        autoplot(insurance) + ylab("Quotes") +
        ggtitle("Forecast quotes with future advertising set to 8")




#-------------------------------------------------------------#
########## chapter 12 advanced forecasting methods ############
#-------------------------------------------------------------#
## complex seasonality
load("./Data/bank_calls.Rda")
bank_calls %>%
        fill_gaps() %>%
        autoplot(Calls) +
        ylab("Call volume") + xlab("Date")

# re-index to avoid the missing values
calls <- bank_calls %>%
        mutate(t = row_number()) %>%
        update_tsibble(index = t, regular = TRUE)

calls %>%
        model(STL(sqrt(Calls) ~ season(period=169) + season(period=5*169),
                  robust=TRUE)) %>%
        components() %>%
        autoplot() + xlab("Observation")

# Forecasts from STL+ETS decomposition
my_dcmp_spec <- decomposition_model(
        STL(sqrt(Calls) ~ season(period = 169) + season(period = 5 * 169),
            robust = TRUE),
        ETS(season_adjust ~ season("N"))
)
fc <- calls %>%
        model(my_dcmp_spec) %>%
        forecast(h = 5 * 169)

# Add correct time stamps to fable
fc_with_times <- bank_calls %>%
        new_data(n = 7 * 24 * 60 / 5) %>%
        mutate(time = format(DateTime, format = "%H:%M:%S")) %>%
        filter(
                time %in% format(bank_calls$DateTime, format = "%H:%M:%S"),
                wday(DateTime, week_start = 1) <= 5 # remove weekend
        ) %>%
        mutate(t = row_number() + max(calls$t)) %>%
        left_join(fc, by = "t") %>%
        as_fable(response = "Calls", distribution = Calls)

# Plot results with last 3 weeks of data
fc_with_times %>%
        fill_gaps() %>%
        mutate(Calls = .mean) %>% 
        autoplot(.vars = Calls) +
        autoplot(bank_calls %>% tail(14 * 169) %>% fill_gaps()) +
        labs(x="Date", y="Call volume")

# example, electricity demand
elec <- vic_elec %>%
        mutate(
                DOW = wday(Date, label=TRUE),
                Working_Day = !Holiday & !(DOW %in% c("Sat","Sun")),
                Cooling = pmax(Temperature, 18)
        )
elec %>%
        ggplot(aes(x=Temperature, y=Demand, col=Working_Day)) +
        geom_point(alpha=0.6) +
        labs(x="Temperature (degrees Celsius)", y="Demand (MWh)")

# takes 20 mins to train the model, be catious.
fit <- elec %>%
        model(
                ARIMA(Demand ~ PDQ(0, 0, 0) + Temperature + Cooling + Working_Day +
                              fourier(period = "day", K = 10) +
                              fourier(period = "week", K = 5) +
                              fourier(period = "year", K = 3))
        )
temps <- tail(elec, 2*48)
fc <- fit %>%
        forecast(new_data = tail(elec, 2*48))
fc %>%
        autoplot(elec %>% tail(10*48)) +
        labs(x="Date", y="Demand (MWh)")


## prophet model
# example: aus cement production
cement <- aus_production %>%
        filter(year(Quarter) >= 1988)
train <- cement %>%
        filter(year(Quarter) <= 2007) %>% 
        select(Quarter, Cement)
# the following code will return c++ exception (unknown reason), cannot be resolved at the moment
# please refer to https://discourse.mc-stan.org/t/dealing-with-catalina-iv/13502/29
# another reference: https://cran.r-project.org/web/packages/fable.prophet/vignettes/intro.html
# fit <- train %>%
#         model(
#                 #arima = ARIMA(Cement),
#                 #ets = ETS(Cement),
#                 #prophet = fable.prophet::prophet(Cement ~ season("year", type="multiplicative"))
#                 prophet_ml = prophet(Cement)
#         )
# fc <- fit %>% forecast(h = "2 years 6 months")
# fc %>% autoplot(cement)
# 
# a <- select(as_tibble(train), ds = Quarter, y = Cement) %>% 
#         mutate(ds = as.Date(ds))
# prophet::prophet(a)

# example, half-hourly electricity demand
elec <- vic_elec %>%
        mutate(
                DOW = wday(Date, label=TRUE),
                Working_Day = !Holiday & !(DOW %in% c("Sat","Sun")),
                Cooling = pmax(Temperature, 18)
        )
autoplot(elec)
elec %>%
        ggplot(aes(x=Temperature, y=Demand, col=Working_Day)) +
        geom_point(alpha=0.6) +
        labs(x="Temperature (degrees Celsius)", y="Demand (MWh)")
fit <- elec %>%
        model(
                prophet(Demand ~ Temperature + Cooling + Working_Day +
                                season(period = "day", order=10) +
                                season(period = "week", order=5) +
                                season(period = "year", order=3))
        )
fit %>% components() %>% autoplot()
temps <- tail(elec, 2*48)
fc <- fit %>%
        forecast(new_data = tail(elec, 2*48))
fc %>%
        autoplot(elec %>% tail(10*48)) +
        labs(x="Date", y="Demand (MWh)")



#-------------------------------------------------------------#
########## chapter 13 some practical forecasting issues ############
#-------------------------------------------------------------#
us_gasoline %>%
        model(STL(Barrels)) %>%
        components() %>%
        autoplot() + xlab("Observation")

my_dcmp_spec <- decomposition_model(
        STL(Barrels),
        ETS(season_adjust ~ season("N"))
)

us_gasoline %>%
        model(stl_ets = my_dcmp_spec) %>%
        forecast(h = "2 years") %>%
        autoplot(us_gasoline)




