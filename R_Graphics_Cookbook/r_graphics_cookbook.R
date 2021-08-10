# objective: notes of R graphics cookbook

# housekeeping
rm(list = ls())

# load necessary packages
library(tidyverse) # data wranglings
library(gcookbook)
packageVersion("tidyverse")
packageVersion("gcookbook")




#------------------------------------------------------------------------------#
########## chapter 0 preface ###########
#------------------------------------------------------------------------------#




#------------------------------------------------------------------------------#
########## chapter 1 R basics ###########
#------------------------------------------------------------------------------#
# pipe operator
# Michelson speed of light data (km/sec, with 299,000 subtracted)
morley %>% 
        filter(Expt == 1) %>% 
        summary()




#------------------------------------------------------------------------------#
########## chapter 2 quickly exploring data ###########
#------------------------------------------------------------------------------#
## scatter plot
# base R
plot(mtcars$wt, mtcars$mpg)
# ggplot2
ggplot(mtcars, aes(x = wt, y = mpg)) +
        geom_point()


## line graph
# base R
plot(pressure$temperature, pressure$pressure, type = "l")
points(pressure$temperature, pressure$pressure)
# ggplot2
ggplot(pressure, aes(x = temperature, y = pressure)) +
        geom_line() +
        geom_point()

## bar plot
# base R
barplot(BOD$demand, names.arg = BOD$Time)
barplot(table(mtcars$cyl)) # count of each category
# ggplot2
ggplot(BOD, aes(x = Time, y = demand)) +
        geom_col()
ggplot(BOD, aes(x = factor(Time), y = demand)) +
        geom_col() # discrete x variable
ggplot(mtcars, aes(x = factor(cyl))) +
        geom_bar() # count of each category

## histogram
ggplot(mtcars, aes(x = mpg)) +
        geom_histogram(binwidth = 4)

## box plot
ggplot(ToothGrowth, aes(x = interaction(supp, dose), y = len)) +
        geom_boxplot()

## function curve
myfun <- function(xvar) {
        1 / (1 + exp(-xvar + 10))
}
ggplot(data.frame(x = c(0, 20)), aes(x = x)) +
        stat_function(fun = myfun, geom = "line") +
        stat_function(fun = myfun, geom = "point")
# smooth out the curve with a large n
ggplot(data.frame(x = c(0, 20)), aes(x = x)) +
        stat_function(fun = myfun, geom = "line", n = 200) +
        stat_function(fun = myfun, geom = "point", n = 200)

p <- ggplot(data.frame(x = c(-3, 3)), aes(x = x))
p +
        stat_function(aes(color = "norm-dist"), fun = dnorm) +
        stat_function(aes(color = "t-dist"), fun = dt, args = list(df = 2))


