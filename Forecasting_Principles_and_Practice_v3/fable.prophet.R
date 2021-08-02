# fable.prophet package testing

library(tsibble)
library(fable.prophet)
library(tidyverse)
library(lubridate)

# Read in the data
lax_passengers <- read_csv("https://raw.githubusercontent.com/mitchelloharawild/fable.prophet/master/data-raw/lax_passengers.csv")

# Tidy and summarise the data for comparison of international and domestic passenger counts
lax_passengers <- lax_passengers %>%
        mutate(datetime = mdy_hms(ReportPeriod)) %>%
        group_by(month = yearmonth(datetime), type = Domestic_International) %>%
        summarise(passengers = sum(Passenger_Count)) %>%
        ungroup()

lax_passengers <- as_tsibble(lax_passengers, index = month, key = type)
lax_passengers

lax_passengers %>% 
        autoplot(passengers)

prophet(passengers ~ growth("logistic") + season("year", type = "multiplicative"))
#> <prophet model definition>

lax_passengers %>% 
        as_tibble() %>% 
        group_by(type) %>% 
        summarise(my_range = range(passengers))
        
fit <- lax_passengers %>% 
        filter(type == "Domestic") %>%
        model(
                mdl = prophet(passengers ~ growth(type = "logistic", capacity = 7000000, floor = 4000000) 
                              + season("year", type = "additive"))
        )
fit

components(fit)

components(fit) %>% filter(type == "Domestic") %>% autoplot()

fc <- fit %>% 
        forecast(h = "6 years")
fc
fc %>% 
        autoplot(lax_passengers)
accuracy(fit)



# Rcpp
Rcpp::sourceCpp(code = 
                        '
#include <Rcpp.h>

// [[Rcpp::export]]
int throw_exception() { 
  std::stringstream errmsg; errmsg << "this is the expected behavior";
  throw std::domain_error(errmsg.str()); 
  return 0;
}
'
)

throw_exception()

dlls <- getLoadedDLLs()
paths <- vapply(dlls, `[[`, "path", FUN.VALUE = character(1))
invisible(lapply(paths, function(path) {
        
        if (!file.exists(path))
                return(FALSE)
        
        output <- system(paste("otool -L", shQuote(path), "| grep libc++ || true"),
                         intern = TRUE)
        if (length(output) == 0)
                return(FALSE)
        
        writeLines(paste0(path, ":"))
        writeLines(output)
        
}))

example(stan_model, package = "rstan", run.dontrun = TRUE, verbose = TRUE)

library(rstan)
Sys.unsetenv("PKG_CXXFLAGS")

Rcpp::sourceCpp(code = 
                        '
#include <RcppArmadillo.h>

// [[Rcpp::depends(RcppArmadillo)]]

// [[Rcpp::export]]
void hello_world() {
  Rcpp::Rcout << "Hello World!" << std::endl;
}

/*** R
hello_world()
*/

'
)

hello_world()


history <- data.frame(ds = seq(as.Date('2015-01-01'), as.Date('2016-01-01'), by = 'd'),
                      y = sin(1:366/200) + rnorm(366)/10)

m <- prophet::prophet(history)
m
