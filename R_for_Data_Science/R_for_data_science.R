# R for data science lectures

# housekeeping
rm(list = ls())
# obtain the current source file work directory
wd <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(wd)

# load necessary packages
library(tidyverse)
library(nycflights13)
library(gapminder)
library(Lahman)
library(hexbin)
library(modelr)
library(stringr)
library(forcats)
library(lubridate)
library(splines)
library(ggrepel)
library(viridis)

#------------------------------------------------------#
#### 3 data visualization ##################
#------------------------------------------------------#
ggplot(data = mpg) + 
        geom_point(mapping = aes(x = displ, y = hwy))
# exercise
ggplot(data = mpg)
dim(mpg)
# drv, f = front-wheel drive, r = rear wheel drive, 4 = 4wd.
ggplot(data = mpg) + 
        geom_point(mapping = aes(x = cyl, y = hwy))
ggplot(data = mpg) + 
        geom_point(mapping = aes(x = class, y = drv))

# aesthetitc mappings
ggplot(data = mpg) +
        geom_point(mapping = aes(x = displ, y = hwy, color = class))

# facet
ggplot(data = mpg) + 
        geom_point(mapping = aes(x = displ, y = hwy)) + 
        facet_wrap(~ class, nrow = 2)
ggplot(data = mpg) + 
        geom_point(mapping = aes(x = displ, y = hwy)) + 
        facet_grid(cyl ~ class)
ggplot(data = mpg) + 
        geom_point(mapping = aes(x = displ, y = hwy)) + 
        facet_grid(. ~ class)

# geometric objects
ggplot(data = mpg) +
        geom_smooth(
                mapping = aes(x = displ, y = hwy, color = drv),
                show.legend = TRUE
        )

# multiple layers with customized aesthetics
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
        geom_point(mapping = aes(color = class)) + 
        geom_smooth(data = filter(mpg, class == "subcompact"), se = FALSE)

# statistical transformation
ggplot(data = diamonds) + 
        geom_bar(mapping = aes(x = cut))
demo <- tribble(
        ~cut,         ~freq,
        "Fair",       1610,
        "Good",       4906,
        "Very Good",  12082,
        "Premium",    13791,
        "Ideal",      21551
)
# change stat, default is count
ggplot(data = demo) +
        geom_bar(mapping = aes(x = cut, y = freq), stat = "identity")
ggplot(data = diamonds) + 
        geom_bar(mapping = aes(x = cut, y = ..prop.., group = 1))
ggplot(data = diamonds) + 
        stat_summary(
                mapping = aes(x = cut, y = depth),
                fun.ymin = min,
                fun.ymax = max,
                fun.y = median
        )

# position adjustment
ggplot(data = diamonds, mapping = aes(x = cut, colour = clarity)) + 
        geom_bar(fill = NA, position = "dodge")
ggplot(data = mpg) + 
        geom_point(mapping = aes(x = displ, y = hwy), position = "jitter")


#------------------------------------------------------#
#### 4 Workflow: basics ##################
#------------------------------------------------------#
# what's in a name
this_is_a_really_long_name <- 2.5


#------------------------------------------------------#
#### 5 data transformation ##################
#------------------------------------------------------#
# use select function to choose a subset of variables
select(flights, year, month, day)
select(flights, year:day)
select(flights, -(year:day))
rename(flights, tail_num = tailnum)
select(flights, time_hour, air_time, everything())
vars <- c("year", "month", "day", "dep_delay", "arr_delay")
select(flights, one_of(vars))
# change from case insensitive to case sensitive
select(flights, contains("TIME"))
select(flights, contains("TIME", ignore.case = FALSE))

# group by and summarise function
by_day <- group_by(flights, year, month, day)
summarise(by_day, delay = mean(dep_delay, na.rm = TRUE))
# study the flights with highest delays
not_cancelled <- flights %>% 
        filter(!is.na(dep_delay), !is.na(arr_delay))
delays <- not_cancelled %>% 
        group_by(tailnum) %>% 
        summarise(
                delay = mean(arr_delay, na.rm = TRUE),
                n = n()
        )
ggplot(data = delays, mapping = aes(x = n, y = delay)) + 
        geom_point(alpha = 1/10)
# select only the flights with more than 25 flight times
delays %>% 
        filter(n > 25) %>% 
        ggplot(mapping = aes(x = n, y = delay)) + 
        geom_point(alpha = 1/10)
# with logic subsetting
not_cancelled %>% 
        group_by(year, month, day) %>% 
        summarise(
                avg_delay1 = mean(arr_delay),
                avg_delay2 = mean(arr_delay[arr_delay > 0]) # the average positive delay
        )
# What proportion of flights are delayed by more than an hour?
not_cancelled %>% 
        group_by(year, month, day) %>% 
        summarise(hour_perc = mean(arr_delay > 60))
# ungrouping
daily <- group_by(flights, year, month, day)
daily %>% 
        ungroup() %>%             # no longer grouped by date
        summarise(flights = n())  # all flights


#------------------------------------------------------#
#### 6 work flow: scripts ##################
#------------------------------------------------------#


#------------------------------------------------------#
#### 7 exploratory data analysis ##################
#------------------------------------------------------#
# 7.3 variation
ggplot(data = diamonds) +
        geom_histogram(mapping = aes(x = carat), binwidth = 0.5)
diamonds %>% 
        count(cut_width(carat, 0.5))
# different binwidths
smaller <- diamonds %>% 
        filter(carat < 3)
ggplot(data = smaller, mapping = aes(x = carat)) +
        geom_histogram(binwidth = 0.1)
ggplot(data = smaller, mapping = aes(x = carat, colour = cut)) +
        geom_freqpoly(binwidth = 0.1)
ggplot(data = smaller, mapping = aes(x = carat)) +
        geom_histogram(binwidth = 0.01)
# zoom y axis
ggplot(diamonds) + 
        geom_histogram(mapping = aes(x = y), binwidth = 0.5) +
        coord_cartesian(ylim = c(0, 50))

# 7.5 covariation
# one categorical, one continous
ggplot(data = diamonds, mapping = aes(x = price, y = ..density..)) + 
        geom_freqpoly(mapping = aes(colour = cut), binwidth = 500)
ggplot(data = diamonds, mapping = aes(x = cut, y = price)) +
        geom_boxplot()
# fair diamonds have more weight than ideal ones
diamonds %>% 
        group_by(cut) %>% 
        dplyr::summarise(caratMedian = median(carat, na.rm = TRUE), 
                         caratMean = mean(carat, na.rm = TRUE), 
                         count = n())
# reorder categorical variable
ggplot(data = mpg) +
        geom_boxplot(mapping = aes(x = class, y = hwy))
ggplot(data = mpg) +
        geom_boxplot(mapping = aes(x = reorder(class, hwy, FUN = median), y = hwy))
ggplot(data = mpg) +
        geom_boxplot(mapping = aes(x = reorder(class, hwy, FUN = median), y = hwy)) +
        coord_flip()

# two categorical variables
ggplot(data = diamonds) +
        geom_count(mapping = aes(x = cut, y = color))
diamonds %>% 
        dplyr::count(color, cut) %>%  
        ggplot(mapping = aes(x = cut, y = color)) +
        geom_tile(mapping = aes(fill = n))

# two continuous variables
ggplot(data = diamonds) + 
        geom_point(mapping = aes(x = carat, y = price), alpha = 1 / 5)
ggplot(data = smaller) +
        geom_bin2d(mapping = aes(x = carat, y = price))
ggplot(data = smaller) +
        geom_hex(mapping = aes(x = carat, y = price))
# varwidth make the width of the boxplot proportional to the number of points within
ggplot(data = smaller, mapping = aes(x = carat, y = price)) + 
        geom_boxplot(mapping = aes(group = cut_width(carat, 0.2)), varwidth = TRUE)
# display the approximately same number of points in each bin
ggplot(data = smaller, mapping = aes(x = carat, y = price)) + 
        geom_boxplot(mapping = aes(group = cut_number(carat, 20)))
ggplot(data = diamonds) +
        geom_point(mapping = aes(x = x, y = y)) +
        coord_cartesian(xlim = c(4, 11), ylim = c(4, 11))
ggplot(data = diamonds) +
        geom_hex(mapping = aes(x = x, y = y))

# patterns and models
# remove effect of carat, then explore the relationship between cut and price.
mod <- lm(log(price) ~ log(carat), data = diamonds)
diamonds2 <- diamonds %>% 
        add_residuals(mod) %>% 
        mutate(resid = exp(resid))
ggplot(data = diamonds2) + 
        geom_point(mapping = aes(x = carat, y = resid))
ggplot(data = diamonds2) + 
        geom_boxplot(mapping = aes(x = cut, y = resid))

# ggplot2 calls
ggplot(faithful, aes(eruptions)) + 
        geom_freqpoly(binwidth = 0.25)


#------------------------------------------------------#
#### 8 workflow projects ##################
#------------------------------------------------------#

#------------------------------------------------------#
#### 10 tibble ##################
#------------------------------------------------------#
vignette("tibble")
myTibble <- tibble(
        a = lubridate::now() + runif(1e3) * 86400,
        b = lubridate::today() + runif(1e3) * 30,
        c = 1:1e3,
        d = runif(1e3),
        e = sample(letters, 1e3, replace = TRUE)
)
print(myTibble, n = 20, width = Inf)


#------------------------------------------------------#
#### 11 data import ##################
#------------------------------------------------------#
myDiamonds <- read_csv("diamonds.csv")
read_csv("The first line of metadata
  The second line of metadata
         x,y,z
         1,2,3", skip = 2)
read_csv("1,2,3\n4,5,6", col_names = c("x", "y", "z"))
# specify na values
read_csv("a,b,c\n1,2,.", na = ".")

# parsing a vector
str(parse_logical(c("TRUE", "FALSE", "NA")))
# encoding issue
x1 <- "El Ni\xf1o was particularly bad this year"
x2 <- "\x82\xb1\x82\xf1\x82\xc9\x82\xbf\x82\xcd"
parse_character(x1, locale = locale(encoding = "Latin1"))
parse_character(x2, locale = locale(encoding = "Shift-JIS"))
guess_encoding(charToRaw(x1))
guess_encoding(charToRaw(x2))
# dates
parse_date("1 janvier 2015", "%d %B %Y", locale = locale(date_names = "fr"))
# parsing a file
challenge <- read_csv(readr_example("challenge.csv"))
problems(challenge)
challenge <- read_csv(
        readr_example("challenge.csv"), 
        col_types = cols(
                x = col_double(),
                y = col_date()
        )
)





#------------------------------------------------------#
#### 12 tidy data ##################
#------------------------------------------------------#
stocks <- tibble(
        year   = c(2015, 2015, 2016, 2016),
        half  = c(   1,    2,     1,    2),
        return = c(1.88, 0.59, 0.92, 0.17)
)
stocks %>% 
        spread(year, return, convert = TRUE) %>% 
        gather("year", "return", `2015`:`2016`, convert = TRUE)
# missing values
stocks <- tibble(
        year   = c(2015, 2015, 2015, 2015, 2016, 2016, 2016),
        qtr    = c(   1,    2,    3,    4,    2,    3,    4),
        return = c(1.88, 0.59, 0.35,   NA, 0.92, 0.17, 2.66)
)
# case study
who %>%
        gather(key, value, new_sp_m014:newrel_f65, na.rm = TRUE) %>% 
        mutate(key = stringr::str_replace(key, "newrel", "new_rel")) %>%
        separate(key, c("new", "var", "sexage")) %>% 
        select(-new, -iso2, -iso3) %>% 
        separate(sexage, c("sex", "age"), sep = 1)




#------------------------------------------------------#
#### 13 relational data ##################
#------------------------------------------------------#
# check for primary key uniqueness
planes %>% 
        count(tailnum) %>% 
        filter(n > 1)
# filtering joins
top_dest <- flights %>%
        count(dest, sort = TRUE) %>%
        head(10)
flights %>% 
        semi_join(top_dest)
# find flights that don't have a match in planes
flights %>%
        anti_join(planes, by = "tailnum") %>%
        count(tailnum, sort = TRUE)




#------------------------------------------------------#
#### 14 strings ##################
#------------------------------------------------------#
# help on quotes
?"'"
# combining strings
name <- "Hadley"
time_of_day <- "morning"
birthday <- FALSE
str_c(
        "Good ", time_of_day, " ", name,
        if (birthday) " and HAPPY BIRTHDAY",
        "."
)
# regular expressions
x <- c("apple", "banana", "pear")
str_view(x, "an")
str_view(c("grey", "gray"), "gr(e|a)y")
x <- "1888 is the longest year in Roman numerals: MDCCCLXXXVIII"
str_view(x, "CC?")
str_view(x, "CC{0,1}")
str_view(x, "CC{0,1}?")
str_view(x, "C{1,3}")
str_view("aaaa", "(.)\\1\\1")
# finds all fruits that have a repeated pair of letters.
str_view(fruit, "(..)\\1", match = TRUE)
# extract nouns from the sentences
noun <- "(a|the) ([^ ]+)"
has_noun <- sentences %>%
        str_subset(noun) %>%
        head(10)
has_noun %>% 
        str_extract(noun)
has_noun %>% 
        str_match(noun)
tibble(sentence = sentences) %>% 
        tidyr::extract(
                sentence, c("article", "noun"), "(a|the) ([^ ]+)", 
                remove = FALSE
        )
# regex with comments
phone <- regex("
               \\(?     # optional opening parens
               (\\d{3}) # area code
               [) -]?   # optional closing parens, space, or dash
               (\\d{3}) # another three numbers
               [ -]?    # optional space or dash
               (\\d{3}) # three more numbers
               ", comments = TRUE)

str_match("514-791-8141", phone)




#------------------------------------------------------#
#### 15 factors ##################
#------------------------------------------------------#
# creating factors
x1 <- c("Dec", "Apr", "Jan", "Mar")
x2 <- c("Dec", "Apr", "Jam", "Mar")
month_levels <- c(
        "Jan", "Feb", "Mar", "Apr", "May", "Jun", 
        "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
)
y1 <- factor(x1, levels = month_levels)
sort(y1)
y2 <- factor(x2, levels = month_levels)
y2

# general social survey data
?gss_cat
count(gss_cat, race)
ggplot(gss_cat, aes(race)) +
        geom_bar() +
        scale_x_discrete(drop = FALSE)
relig_summary <- gss_cat %>%
        group_by(relig) %>%
        summarise(
                age = mean(age, na.rm = TRUE),
                tvhours = mean(tvhours, na.rm = TRUE),
                n = n()
        )
# reorder the levels
ggplot(relig_summary, aes(tvhours, fct_reorder(relig, tvhours))) +
        geom_point()
# line plot, legend alignment
by_age <- gss_cat %>%
        filter(!is.na(age)) %>%
        count(age, marital) %>%
        group_by(age) %>%
        mutate(prop = n / sum(n))
ggplot(by_age, aes(age, prop, colour = marital)) +
        geom_line(na.rm = TRUE)
# legend alignment, colour based on the y values associated with the largest x values
ggplot(by_age, aes(age, prop, colour = fct_reorder2(marital, age, prop))) +
        geom_line() +
        labs(colour = "marital")

# bar plot
gss_cat %>%
        mutate(marital = marital %>% fct_infreq() %>% fct_rev()) %>%
        ggplot(aes(marital)) +
        geom_bar()

# change level value of a factor variable
gss_cat %>%
        mutate(partyid = fct_recode(partyid,
                                    "Republican, strong"    = "Strong republican",
                                    "Republican, weak"      = "Not str republican",
                                    "Independent, near rep" = "Ind,near rep",
                                    "Independent, near dem" = "Ind,near dem",
                                    "Democrat, weak"        = "Not str democrat",
                                    "Democrat, strong"      = "Strong democrat",
                                    "Other"                 = "No answer",
                                    "Other"                 = "Don't know",
                                    "Other"                 = "Other party"
        )) %>%
        count(partyid)
# lump small groups together
gss_cat %>%
        mutate(relig = fct_lump(relig, n = 10)) %>%
        count(relig, sort = TRUE) %>%
        print(n = Inf)
# exercises
gss_cat %>%
        mutate(partyid =
                       fct_collapse(partyid,
                                    other = c("No answer", "Don't know", "Other party"),
                                    rep = c("Strong republican", "Not str republican"),
                                    ind = c("Ind,near rep", "Independent", "Ind,near dem"),
                                    dem = c("Not str democrat", "Strong democrat"))) %>%
        count(year, partyid)  %>%
        group_by(year) %>%
        mutate(p = n / sum(n)) %>%
        ggplot(aes(x = year, y = p,
                   colour = fct_reorder2(partyid, year, p))) +
        geom_point() +
        geom_line() +
        labs(colour = "Party ID.")
gss_cat %>%
        mutate(rincome =
                       fct_collapse(
                               rincome,
                               `Unknown` = c("No answer", "Don't know", "Refused", "Not applicable"),
                               `Lt $5000` = c("Lt $1000", str_c("$", c("1000", "3000", "4000"),
                                                                " to ", c("2999", "3999", "4999"))),
                               `$5000 to 10000` = str_c("$", c("5000", "6000", "7000", "8000"),
                                                        " to ", c("5999", "6999", "7999", "9999"))
                       )) %>%
        ggplot(aes(x = rincome)) +
        geom_bar() +
        coord_flip()




#------------------------------------------------------#
#### 16 date and times ##################
#------------------------------------------------------#
flights %>% 
        select(year, month, day, hour, minute) %>% 
        mutate(departure = make_datetime(year, month, day, hour, minute))
# combine multiple columns into date-time
make_datetime_100 <- function(year, month, day, time) {
        make_datetime(year, month, day, time %/% 100, time %% 100)
}
flights_dt <- flights %>% 
        filter(!is.na(dep_time), !is.na(arr_time)) %>% 
        mutate(
                dep_time = make_datetime_100(year, month, day, dep_time),
                arr_time = make_datetime_100(year, month, day, arr_time),
                sched_dep_time = make_datetime_100(year, month, day, sched_dep_time),
                sched_arr_time = make_datetime_100(year, month, day, sched_arr_time)
        ) %>% 
        select(origin, dest, ends_with("delay"), ends_with("time"))
flights_dt %>% 
        ggplot(aes(dep_time)) + 
        geom_freqpoly(binwidth = 86400) # 86400 seconds = 1 day

# date-time components
datetime <- ymd_hms("2016-07-08 12:34:56")
year(datetime)
month(datetime, label = TRUE)
wday(datetime, label = TRUE, abbr = FALSE)
# more flights depart on weekday than the weekend
flights_dt %>% 
        mutate(wday = wday(dep_time, label = TRUE)) %>% 
        ggplot(aes(x = wday)) +
        geom_bar()
# depart at the nice time
flights_dt %>% 
        mutate(minute = minute(dep_time)) %>% 
        group_by(minute) %>% 
        summarise(
                avg_delay = mean(arr_delay, na.rm = TRUE),
                n = n()) %>% 
        ggplot(aes(minute, avg_delay)) +
        geom_line()
flights_dt %>% 
        mutate(minute = minute(dep_time)) %>% 
        group_by(minute) %>% 
        summarise(
                avg_delay = mean(arr_delay, na.rm = TRUE),
                n = n()) %>% 
        ggplot(aes(minute, n)) +
        geom_line()
sched_dep <- flights_dt %>% 
        mutate(minute = minute(sched_dep_time)) %>% 
        group_by(minute) %>% 
        summarise(
                avg_delay = mean(arr_delay, na.rm = TRUE),
                n = n())
ggplot(sched_dep, aes(minute, avg_delay)) +
        geom_line()
ggplot(sched_dep, aes(minute, n)) +
        geom_line()
# update date
update(datetime, year = 2020, month = 2, mday = 2, hour = 2)
# flight distribution for each hour within a day
flights_dt %>% 
        mutate(dep_hour = update(dep_time, yday = 1)) %>% 
        ggplot(aes(dep_hour)) +
        geom_freqpoly(binwidth = 300)
# fix the issue of overnight flights that arr_time < dep_time
flights_dt <- flights_dt %>% 
        mutate(
                overnight = arr_time < dep_time,
                arr_time = arr_time + days(overnight * 1),
                sched_arr_time = sched_arr_time + days(overnight * 1)
        )




#------------------------------------------------------#
#### 18 pipes ##################
#------------------------------------------------------#
# lazy evaluation
try(if(iter > 10) stop("too many iterations"))
tryCatch(stop("!"), error = function(e) "An error")
stop("!") %>% 
        tryCatch(error = function(e) "An error")



#------------------------------------------------------#
#### 19 function ##################
#------------------------------------------------------#
df <- tibble::tibble(
        a = rnorm(10),
        b = rnorm(10),
        c = rnorm(10),
        d = rnorm(10)
)
# before using a function
df$a <- (df$a - min(df$a, na.rm = TRUE)) / 
        (max(df$a, na.rm = TRUE) - min(df$a, na.rm = TRUE))
df$b <- (df$b - min(df$b, na.rm = TRUE)) / 
        (max(df$b, na.rm = TRUE) - min(df$a, na.rm = TRUE))
df$c <- (df$c - min(df$c, na.rm = TRUE)) / 
        (max(df$c, na.rm = TRUE) - min(df$c, na.rm = TRUE))
df$d <- (df$d - min(df$d, na.rm = TRUE)) / 
        (max(df$d, na.rm = TRUE) - min(df$d, na.rm = TRUE))
# after using a function
rescale01 <- function(x) {
        rng <- range(x, na.rm = TRUE)
        (x - rng[1]) / (rng[2] - rng[1])
}
df$a <- rescale01(df$a)
df$b <- rescale01(df$b)
df$c <- rescale01(df$c)
df$d <- rescale01(df$d)

df <- tibble(
        x = runif(5),
        y = rnorm(5)
)
df
# extract columns, subsetting
df$x
df[["x"]]
df[[1]]
df %>% .$x
df %>% .[[1]]

# conditions
# a function to describe whether or not each element of a vector is named
has_name <- function(x) {
        nms <- names(x)
        if (is.null(nms)) {
                rep(FALSE, length(x))
        } else {
                !is.na(nms) & nms != ""
        }
}
a <- c("a", "b")
names(a) <- c("myName1", "myName2")
has_name(a)

# function arguments
# check pre-conditions
wt_mean <- function(x, w) {
        if (length(x) != length(w)) {
                stop("`x` and `w` must be the same length", call. = FALSE)
        }
        sum(w * x) / sum(w)
}

# environments, redefine the function of +
`+` <- function(x, y) {
        if (runif(1) < 0.1) {
                sum(x, y)
        } else {
                sum(x, y) * 1.1
        }
}
table(replicate(1000, 1+2))
rm(`+`)




#----------------------------------#
#### 20 vectors   ##################
#----------------------------------#
# lists
y <- list("a", 1L, 1.5, TRUE)
str(y)
z <- list(list(1, 2), list(3, 4))
str(z)

# attributes
x <- 1:10
attr(x, "greeting")
#> NULL
attr(x, "greeting") <- "Hi!"
attr(x, "farewell") <- "Bye!"
attributes(x)

# augmented vector
tb <- tibble::tibble(x = 1:5, y = 5:1)
typeof(tb)
#> [1] "list"
attributes(tb)




#----------------------------------#
#### 21 iteration  #################
#----------------------------------#
# for loops
df <- tibble(
        a = rnorm(10),
        b = rnorm(10),
        c = rnorm(10),
        d = rnorm(10)
)

# compute the mean of each columns in mtcars
output <- vector("double", ncol(mtcars))
for (i in seq_along(mtcars)) {
        output[[i]] <- mean(mtcars[[i]])
}
output
# modifying an existing object
for (i in seq_along(df)) {
        df[[i]] <- scale(df[[i]])
}

# a while loop to find how many tries it takes to get three heads in a row
flip <- function() sample(c("T", "H"), 1)
flips <- 0
nheads <- 0
while (nheads < 3) {
        if (flip() == "H") {
                nheads <- nheads + 1
        } else {
                nheads <- 0
        }
        flips <- flips + 1
}
flips

# map function
map_dbl(mtcars, mean)
map_dbl(df, sd)

# map over multiple arguments
set.seed(2018)
mu <- list(5, 10, -3)
mu %>% 
        map(rnorm, n = 5) %>% 
        str()

# walk
plots <- mtcars %>% 
        split(.$cyl) %>% 
        map(~ggplot(., aes(mpg, wt)) + geom_point())
paths <- stringr::str_c(names(plots), ".pdf")
pwalk(list(paths, plots), ggsave, path = tempdir())




#----------------------------------#
#### 23 model basics  ##############
#----------------------------------#
ggplot(sim1, aes(x, y)) + 
        geom_point()
# a linear model
model1 <- function(a, data) {
        a[1] + data$x * a[2]
}
# distance calculation
measure_distance <- function(mod, data) {
        diff <- data$y - model1(mod, data)
        sqrt(mean(diff ^ 2))
}
# find the optimal solution
best <- optim(c(0, 0), measure_distance, data = sim1)
best$par
ggplot(sim1, aes(x, y)) + 
        geom_point(size = 2, colour = "grey30") + 
        geom_abline(intercept = best$par[1], slope = best$par[2])

# visualizing models
sim1_mod <- lm(y ~ x, data = sim1)
grid <- sim1 %>% 
        data_grid(x) %>% 
        add_predictions(sim1_mod)
ggplot(sim1, aes(x)) +
        geom_point(aes(y = y)) +
        geom_line(aes(y = pred), data = grid, colour = "red", size = 1)
# reisduals
sim1 <- sim1 %>% 
        add_residuals(sim1_mod)
ggplot(sim1, aes(x, resid)) + 
        geom_ref_line(h = 0) +
        geom_point() 
# categorical variables
ggplot(sim2) + 
        geom_point(aes(x = x, y = y))
mod2 <- lm(y ~ x, data = sim2)
grid <- sim2 %>% 
        data_grid(x) %>% 
        add_predictions(mod2)
grid
# categorical variable and continuous variable
sim3
ggplot(sim3, aes(x1, y)) + 
        geom_point(aes(colour = x2))
mod1 <- lm(y ~ x1 + x2, data = sim3)
mod2 <- lm(y ~ x1 * x2, data = sim3)
grid <- sim3 %>% 
        data_grid(x1, x2) %>% 
        gather_predictions(mod1, mod2)
ggplot(sim3, aes(x1, y, colour = x2)) + 
        geom_point() + 
        geom_line(data = grid, aes(y = pred)) + 
        facet_wrap(~ model)
sim3 <- sim3 %>% 
        gather_residuals(mod1, mod2)
ggplot(sim3, aes(x1, resid, colour = x2)) + 
        geom_point() + 
        facet_grid(model ~ x2)
# two continous variables
mod1 <- lm(y ~ x1 + x2, data = sim4)
mod2 <- lm(y ~ x1 * x2, data = sim4)
grid <- sim4 %>% 
        data_grid(
                x1 = seq_range(x1, 5), 
                x2 = seq_range(x2, 5) 
        ) %>% 
        gather_predictions(mod1, mod2)
# analyze residuals
sim4 <- sim4 %>% 
        gather_residuals(mod1, mod2)
ggplot(sim4, aes(y, resid)) + 
        geom_point() + 
        facet_grid(model ~ .)
# extroploate experiment
sim5 <- tibble(
        x = seq(0, 3.5 * pi, length = 50),
        y = 4 * sin(x) + rnorm(length(x))
)

# polynomial models
ggplot(sim5, aes(x, y)) +
        geom_point()
mod1 <- lm(y ~ ns(x, 1), data = sim5)
mod2 <- lm(y ~ ns(x, 2), data = sim5)
mod3 <- lm(y ~ ns(x, 3), data = sim5)
mod4 <- lm(y ~ ns(x, 4), data = sim5)
mod5 <- lm(y ~ ns(x, 5), data = sim5)
mod1b <- lm(y ~ poly(x, 1), data = sim5)
mod2b <- lm(y ~ poly(x, 2), data = sim5)
mod3b <- lm(y ~ poly(x, 3), data = sim5)
mod4b <- lm(y ~ poly(x, 4), data = sim5)
mod5b <- lm(y ~ poly(x, 5), data = sim5)
grid <- sim5 %>% 
        data_grid(x = seq_range(x, n = 50, expand = 0.1)) %>% 
        gather_predictions(mod1, mod2, mod3, mod4, mod5, mod1b, mod2b, mod3b, mod4b,
                           mod5b, .pred = "y")
ggplot(sim5, aes(x, y)) + 
        geom_point() +
        geom_line(data = grid, colour = "red") +
        facet_wrap( ~ model)




#----------------------------------#
#### 24 model building  ############
#----------------------------------#
options(na.action = na.warn)
diamonds2 <- diamonds %>% 
        filter(carat <= 2.5) %>% 
        mutate(lprice = log2(price), lcarat = log2(carat))
# model
mod_diamond2 <- lm(lprice ~ lcarat + color + cut + clarity, data = diamonds2)
# visualization
grid <- diamonds2 %>% 
        data_grid(clarity, .model = mod_diamond2) %>% 
        add_predictions(mod_diamond2)
ggplot(grid, aes(clarity, pred)) + 
        geom_point()
# residual analysis
diamonds2 <- diamonds2 %>% 
        add_residuals(mod_diamond2, "lresid2")
ggplot(diamonds2, aes(lprice, lresid2)) + 
        geom_hex(bins = 50)

# nyc flights
daily <- flights %>% 
        mutate(date = make_date(year, month, day)) %>% 
        group_by(date) %>% 
        summarise(n = n())
ggplot(daily, aes(date, n)) + 
        geom_line()




#----------------------------------#
#### 25 many models  ############
#----------------------------------#
# how does life expectancy (lifeExp) change over time (year) for each country (country)?
library(gapminder)
head(gapminder)
gapminder %>% 
        ggplot(aes(year, lifeExp, group = country)) +
        geom_line(alpha = 1/3)
# nested data
by_country <- gapminder %>% 
        group_by(country, continent) %>% 
        nest()
country_model <- function(df) {
        lm(lifeExp ~ year, data = df)
}
by_country <- by_country %>% 
        mutate(model = map(data, country_model))
by_country <- by_country %>% 
        mutate(resids = map2(data, model, add_residuals))
resids <- unnest(by_country, resids)
resids %>% 
        ggplot(aes(year, resid)) +
        geom_line(aes(group = country), alpha = 1 / 3) + 
        geom_smooth(se = FALSE)
resids %>% 
        ggplot(aes(year, resid, group = country)) +
        geom_line(alpha = 1 / 3) + 
        facet_wrap(~continent)
# check r squred
glance <- by_country %>% 
        mutate(glance = map(model, broom::glance)) %>% 
        unnest(glance, .drop = TRUE)
glance %>% 
        ggplot(aes(continent, r.squared)) + 
        geom_jitter(width = 0.5)
bad_fit <- filter(glance, r.squared < 0.25)
gapminder %>% 
        semi_join(bad_fit, by = "country") %>% 
        ggplot(aes(year, lifeExp, colour = country)) +
        geom_line()




#----------------------------------#
#### 28 graphics for communiation  ############
#----------------------------------#
# labels
ggplot(mpg, aes(displ, hwy)) +
        geom_point(aes(color = class)) +
        geom_smooth(se = FALSE) +
        labs(
                title = "Fuel efficiency generally decreases with engine size",
                subtitle = "Two seaters (sports cars) are an exception because of their light weight",
                caption = "Data from fueleconomy.gov",
                x = "Engine displacement (L)",
                y = "Highway fuel economy (mpg)",
                colour = "Car type"
        )

# annotations
best_in_class <- mpg %>%
        group_by(class) %>%
        filter(row_number(desc(hwy)) == 1)
ggplot(mpg, aes(displ, hwy)) +
        geom_point(aes(colour = class)) +
        geom_point(size = 3, shape = 1, data = best_in_class) +
        ggrepel::geom_label_repel(aes(label = model), data = best_in_class)
# put legends into the graph
class_avg <- mpg %>%
        group_by(class) %>%
        summarise(
                displ = median(displ),
                hwy = median(hwy)
        )
ggplot(mpg, aes(displ, hwy, colour = class)) +
        ggrepel::geom_label_repel(aes(label = class),
                                  data = class_avg,
                                  size = 6,
                                  label.size = 0,
                                  segment.color = NA
        ) +
        geom_point() +
        theme(legend.position = "none")
# a single label on the top right
label <- mpg %>%
        summarise(
                displ = max(displ),
                hwy = max(hwy),
                label = "Increasing engine size is \nrelated to decreasing fuel economy."
        )
ggplot(mpg, aes(displ, hwy)) +
        geom_point() +
        geom_text(aes(label = label), data = label, vjust = "top", hjust = "right")
# scales
ggplot(mpg, aes(displ, hwy)) +
        geom_point() +
        scale_y_continuous(breaks = seq(15, 40, by = 5))
# axes, president start and end date
presidential %>%
        mutate(id = 33 + row_number()) %>%
        ggplot(aes(start, id, colour = party)) +
        geom_point() +
        geom_segment(aes(xend = end, yend = id)) +
        scale_colour_manual(values = c(Republican = "red", Democratic = "blue"))
# legends
ggplot(mpg, aes(displ, hwy)) +
        geom_point(aes(colour = class)) +
        geom_smooth(se = FALSE) +
        theme(legend.position = "bottom") +
        guides(colour = guide_legend(nrow = 2, override.aes = list(size = 4)))
# replace scales
ggplot(diamonds, aes(carat, price)) +
        geom_bin2d() + 
        scale_x_log10() + 
        scale_y_log10()
ggplot(mpg, aes(displ, hwy)) +
        geom_point(aes(color = drv, shape = drv)) +
        scale_colour_brewer(palette = "Set1")
# override scales
ggplot(diamonds, aes(carat, price)) +
        geom_point(aes(colour = cut), alpha = 1/20)  +
        theme(legend.position = "bottom") +
        guides(colour = guide_legend(nrow = 1, override.aes = list(alpha = 1))) 

# zoom
suv <- mpg %>% filter(class == "suv")
compact <- mpg %>% filter(class == "compact")

x_scale <- scale_x_continuous(limits = range(mpg$displ))
y_scale <- scale_y_continuous(limits = range(mpg$hwy))
col_scale <- scale_colour_discrete(limits = unique(mpg$drv))

ggplot(suv, aes(displ, hwy, colour = drv)) +
        geom_point() +
        x_scale +
        y_scale +
        col_scale

ggplot(compact, aes(displ, hwy, colour = drv)) +
        geom_point() +
        x_scale +
        y_scale +
        col_scale
