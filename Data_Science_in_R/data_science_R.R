# objective: data science in R book reading notes

# preparation
rm(list = ls())
# obtain the current source file work directory
wd <- getwd()

# load necessary packages
library(tidyverse)
library(fpp3)
library(broom)
library(patchwork)
library(cranlogs)
library(gghighlight)
library(cowplot)
library(ggforce)
library(ggridges)
library(latex2exp)
library(colorspace)
library(infer)
library(lme4)
# library(broom.mixed)
library(modelr)
library(ggrepel)
# library(showtext)
# library(tidymodels)
# library(rstan)
# library(tidybayes)
# library(brms)




#--------------------------------------------------#
########## chapter 1 data science and R ############
#--------------------------------------------------#
#--------------------------------------------------#
########## chapter 2 R basics ############
#--------------------------------------------------#
1 == "1"




#--------------------------------------------------#
########## chapter 2 extract elements ############
#--------------------------------------------------#
# list
l <- list("one" = c("a", "b", "c"), 
          "two" = c(1:5), 
          "three" = c(TRUE, FALSE)
)
l[1]
l[[1]]

# matrix
a <- matrix(1:9, nrow = 3)
a[1, 1:2]




#--------------------------------------------------#
########## chapter 3 element extract ############
#--------------------------------------------------#
df <- data.frame(x = 1:4,
                 y = 4:1,
                 z = c("a", "b", "c", "d")
)
df
df["x"]
df[["x"]]
df$x




#-----------------------------------------------------#
########## chapter 4 reproducible research ############
#-----------------------------------------------------#
# R markdown




#-----------------------------------------------------#
########## chapter 5 read data ############
#-----------------------------------------------------#




#-----------------------------------------------------#
########## chapter 6 data pre-processing ############
#-----------------------------------------------------#
df <- tibble(
        name = c("Alice", "Alice", "Bob", "Bob", "Carol", "Carol"),
        type = c("english", "math", "english", "math", "english", "math")
)
score2020 <- c(80.2, 90.5, 92.2, 90.8, 82.5, 84.6)
mutate(.data = df, score4 = score2020)




#---------------------------------------------------------------#
########## chapter 7 legacy row and column operation ############
#---------------------------------------------------------------#
df <- tibble(
        grp = rep(c("a", "b"), each = 5),
        x = c(rnorm(5, -0.25, 1), rnorm(5, 0, 1.5)),
        y = c(rnorm(5, 0.25, 1), rnorm(5, 0, 0.5))
)
df
df %>%
        group_by(grp) %>%
        summarise(
                x = quantile(x, c(0.25, 0.5, 0.75)),
                q = c(0.25, 0.5, 0.75)
        )
mtcars %>%
        group_by(cyl) %>%
        summarise(
                broom::tidy(lm(mpg ~ wt))
        )
mtcars %>%
        group_by(cyl, vs) %>%
        summarise(cyl_n = n()) %>%
        group_vars()

homeworld_species <- starwars %>% 
        group_by(homeworld, species) %>% 
        summarise(n = n())

# change column sequence
df %>% relocate(grp, .after = y)
df %>% relocate(grp, .after = last_col())

# across function
std <- function(x) {
        # standardize a numeric vector
        (x - mean(x)) / sd(x)
}

iris %>%
        group_by(Species) %>%
        summarise(
                across(starts_with("Sepal"), std)
        )

# multiple functions
iris %>%
        group_by(Species) %>%
        summarise(
                across(starts_with("Petal"), list(min = min, max = max))
                # across(starts_with("Petal"), list(min = min, max = max), .names = "{fn}_{col}")
        )




#---------------------------------------------------------------#
########## chapter 7 data visualization ############
#---------------------------------------------------------------#
d <- cran_downloads(package = c("forecast", "ggplot2", "lubridate", "dplyr"), from = "2019-01-01", to = "2019-12-31")
d %>% group_by(package) %>% summarise(count = sum(count))

mpg %>%
        select(displ, hwy, class) %>%
        head(4)
ggplot(mpg, aes(x = displ, y =  hwy)) +
        geom_point()
ggplot(data = mpg, aes(x = displ, y = hwy, color = class)) +
        geom_point()
ggplot(data = mpg, aes(x = displ, y = hwy, size = class)) +
        geom_point()
ggplot(data = mpg, aes(x = displ, y = hwy, shape = class)) +
        geom_point()
ggplot(data = mpg, aes(x = displ, y = hwy, alpha = class)) +
        geom_point()

# global vs. local
ggplot(mpg, aes(x = displ, y = hwy, color = class)) +
        geom_point() +
        geom_smooth(method = lm)

ggplot(mpg, aes(x = displ, y = hwy)) +
        geom_point(aes(color = class)) +
        geom_smooth(method = lm)


#---------------------------------------------------------------#
########## chapter 8 strings ############
#---------------------------------------------------------------#
# basics
str_c(c("x", "y", "z"), c("a", "b", "c"), collapse = "|")

# pattern match
x <- c("apple", "banana", "pear")
str_view(x, ".a.")
str_view(x, "^a")
str_view(x, "a$")

x <- "Roman numerals: MDCCCCLXXXVIII"
str_view(x, "CC?")

x <- "cacaca"
str_view(x, "(ca){2}")

ft <- fruit %>% head(10)
ft

str_view(ft, "(.)\\1", match = TRUE) # aa, bb match
str_view(ft, "(..)\\1", match = TRUE) # abab, cdcd match
str_view(ft, "(.)(.)\\2\\1", match = TRUE) # abba, cddc match

# pattern detection
stringr::words %>% head()
# proportion of common words end with a vowel?
mean(str_detect(words, "[aeiou]$"))

str_view("abcd", "ab|cd")
str_view("abc", "a[bc]d")

win <- c("Windows2000", "Windows", "Windows3.1")
str_view(win, "Windows(?=95|98|NT|2000)")
win <- c("2000Windows", "Windows", "3.1Windows")
str_view(win, "(?<=95|98|NT|2000)Windows")

# examples
# extract all the numbers from string and then make a sum
df <- tibble(
        x = c("1234", "B246", "217C", "2357f", "21WD4")
)
df %>%
        mutate(num = str_match_all(x, "\\d")) %>%
        unnest(num) %>%
        mutate_at(vars(num), as.numeric) %>%
        group_by(x) %>%
        summarise(sum = sum(num))



#--------------------------------------------------#
########## chapter 9 tidy data ############
#--------------------------------------------------#
plant_heigt <- data.frame(
        Day = 1:5,
        A = c(0.7, 1.0, 1.5, 1.8, 2.2),
        B = c(0.5, 0.7, 0.9, 1.3, 1.8)
)
plant_heigt

# to longer
long <- plant_heigt %>%
        pivot_longer(
                cols = c(A, B),
                names_to = "plant",
                values_to = "height"
        )
long

# to wider
wide <- long %>% 
        pivot_wider(
                names_from = "plant",
                values_from = "height"
        )
wide



#--------------------------------------------------#
########## chapter 10 factors ############
#--------------------------------------------------#
income <- c("low", "high", "medium", "medium", "low", "high",  "high")
factor(income, levels = c("low", "high", "medium") )

# change levels
x <- income
x %>% fct_relevel(levels = c("high", "medium", "low"))

# application
d <- tibble(
        x = c("a","a", "b", "b", "c", "c"),
        y = c(2, 2, 1, 5,  0, 3)
        
)
d
d %>% 
        ggplot(aes(x = x, y = y)) +
        geom_point(alpha = 0.5)
d %>% 
        mutate(x = fct_reorder(x, y, .fun = median)) %>% 
        ggplot(aes(x = fct_reorder(x, y, .fun = median), y = y)) +
        geom_point(alpha = 0.5)




#-------------------------------------------------------#
########## chapter 11 functional programming ############
#-------------------------------------------------------#
a_list <- list(
        num = c(8, 9),
        log = TRUE,
        cha = c("a", "b", "c")
)
a_list
a_list["num"] # will return a list that contains 1 element
a_list[["num"]] # will return the element
a_list$num

# purrr package
exams <- list(
        student1 = round(runif(10, 50, 100)),
        student2 = round(runif(10, 50, 100)),
        student3 = round(runif(10, 50, 100)),
        student4 = round(runif(10, 50, 100)),
        student5 = round(runif(10, 50, 100))
)
exams
exams %>% map(mean) # map is a function to apply a function to each element of the list.
exams %>% map_dbl(mean)
exams %>% map_df(mean)

my_fun <- function(x){
        x - mean(x)
}
exams %>% map_df(my_fun)
exams %>% map_df(~ .x - mean(.x))
exams %>% map_df(~ . - mean(.)) # . represents each element in exams

# applications on modeling
mtcars %>%
        group_by(cyl) %>%
        nest() %>%
        mutate(model = purrr::map(data, ~ lm(mpg ~ wt, data = .))) %>%
        mutate(result = purrr::map(model, ~ broom::tidy(.))) %>%
        unnest(result)




#--------------------------------------------------#
########## chapter 12 tibble ############
#--------------------------------------------------#
df <- data.frame(
        x = 1:3,
        y = x + 2
)
tb <- tibble(
        x = 1:3,
        y = x + 2
)
tb

# covert a named vector to a tibble
enframe(c(a = 5, b = 7, c = 9))

# row names
df <- mtcars[1:3, 1:3]
df
rownames_to_column(df, var = "rowname")
rowid_to_column(df, var = "rowid")

# fix column names
tibble(`year 1` = 1, `year 2` = 2) %>%
        janitor::clean_names()





#--------------------------------------------------#
########## chapter 13-17 ggplot ############
#--------------------------------------------------#
# ch13 geom objects ------------------------------------------------------------
df <- datasauRus::datasaurus_dozen
df %>%
        group_by(dataset) %>%
        summarize(
                mean_x = mean(x),
                mean_y = mean(y),
                std_dev_x = sd(x),
                std_dev_y = sd(y),
                corr_x_y = cor(x, y)
        )
ggplot(df, aes(x = x, y = y, colour = dataset)) +
        geom_point() +
        # geom_smooth(method = lm) +
        theme(legend.position = "none") +
        facet_wrap(~dataset, ncol = 3)

# gap minder data set example
gapdata <- read_csv("./demo_data/gapminder.csv")
gapdata

# check NA
gapdata %>%
        summarise(
                across(everything(), ~ sum(is.na(.)))
        )

## bar chart
gapdata %>%
        ggplot(aes(x = continent)) +
        geom_bar()

# reorder the continent based on the number of countries within each continent
gapdata %>%
        ggplot(aes(x = fct_reorder(continent, continent, .fun = length))) +
        geom_bar()
gapdata %>%
        ggplot(aes(x = fct_reorder(continent, continent, .fun = length, .desc = TRUE))) +
        geom_bar()
gapdata %>%
        ggplot(aes(x = reorder(continent, continent, length))) +
        geom_bar() +
        coord_flip()
gapdata %>%
        distinct(continent, country) %>%
        group_by(continent) %>%
        summarise(num = n()) %>%
        ggplot(aes(x = continent, y = num)) +
        geom_bar(stat = "identity")


## histogram
gapdata %>%
        ggplot(aes(x = lifeExp)) +
        geom_histogram()
gapdata %>%
        filter(continent != "Oceania") %>%
        ggplot(aes(x = lifeExp, fill = continent)) +
        geom_histogram() +
        facet_grid(continent ~ .)
gapdata %>%
        ggplot(aes(x = lifeExp, color = continent)) +
        geom_freqpoly()
gapdata %>%
        ggplot(aes(x = lifeExp, color = continent)) +
        geom_density()
gapdata %>%
        filter(continent != "Oceania") %>%
        ggplot(aes(x = lifeExp, y = stat(density))) +
        geom_histogram(aes(fill = continent)) +
        geom_density() +
        facet_grid(continent ~ .)


## box plot
gapdata %>%
        ggplot(aes(x = year, y = lifeExp)) +
        geom_boxplot(aes(group = year))


## jitter
gapdata %>% ggplot(aes(x = continent, y = lifeExp)) +
        geom_point()
gapdata %>% ggplot(aes(x = continent, y = lifeExp)) +
        geom_jitter()
gapdata %>%
        ggplot(aes(x = continent, y = lifeExp)) +
        geom_jitter() +
        stat_summary(fun.y = median, colour = "red", geom = "point", size = 5)


## ridge plot
gapdata %>%
        ggplot(aes(
                x = lifeExp,
                y = continent,
                fill = continent
        )) +
        ggridges::geom_density_ridges() +
        scale_fill_manual(
                values = c("#003f5c", "#58508d", "#bc5090", "#ff6361", "#ffa600")
        )


## scatter plot
# relationship between gdp per cap and life expansion
gapdata %>%
        ggplot(aes(x = log(gdpPercap), y = lifeExp, color = continent)) +
        geom_point(alpha = 1/2) +
        geom_smooth(lwd = 2, se = FALSE, method = "lm")

gapdata %>%
        ggplot(aes(x = gdpPercap, y = lifeExp, color = continent)) +
        geom_point(show.legend = FALSE) +
        facet_wrap(~continent)

jCountries <- c("Canada", "Rwanda", "Cambodia", "Mexico")
gapdata %>%
        filter(country %in% jCountries) %>%
        ggplot(aes(x = year, y = lifeExp, color = country)) +
        geom_line() +
        geom_point()

# highlight each legend based on last observation point
d1 <- gapdata %>%
        filter(country %in% jCountries) %>%
        group_by(country) %>%
        mutate(end_label = if_else(year == max(year), country, NA_character_))
d1 %>% ggplot(aes(
        x = year, y = lifeExp, color = country
)) +
        geom_line() +
        geom_point() +
        geom_label(aes(label = end_label)) +
        theme(legend.position = "none")
gapdata %>%
        filter(country %in% jCountries) %>%
        ggplot(aes(
                x = year, y = lifeExp, color = country
        )) +
        geom_line() +
        geom_point() +
        gghighlight::gghighlight()


## point and line plot
gapdata %>%
        filter(continent == "Asia" & year == 2007) %>%
        ggplot(aes(
                x = lifeExp,
                y = reorder(country, lifeExp)
        )) +
        geom_point(color = "blue", size = 2) +
        geom_segment(aes(
                x = 40,
                xend = lifeExp,
                y = reorder(country, lifeExp),
                yend = reorder(country, lifeExp)
        ),
        color = "lightgrey"
        ) +
        labs(
                x = "Life Expectancy (years)",
                y = "",
                title = "Life Expectancy by Country",
                subtitle = "GapMinder data for Asia - 2007"
        ) +
        theme_minimal() +
        theme(
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank()
        )


## annotation
gapdata %>%
        ggplot(aes(x = gdpPercap, y = lifeExp)) +
        geom_point() +
        ggforce::geom_mark_ellipse(aes(
                filter = gdpPercap > 70000,
                label = "Rich countries",
                description = "Who are they?"
        ))


# error bar
avg_gapdata <- gapdata %>%
        group_by(continent) %>%
        summarise(
                mean = mean(lifeExp),
                sd = sd(lifeExp)
        )
avg_gapdata %>%
        ggplot(aes(continent, mean)) +
        # geom_col(alpha = 0.5) +
        geom_point() +
        geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.25) +
        labs(y = "Average Life Expectancy")

## 2D density
gapdata %>%
        ggplot(aes(x = gdpPercap, y = lifeExp)) +
        geom_hex()

# Life expectancy for Each Continent Over Time
gapdata %>%
        group_by(continent, year) %>%
        summarise(mean_lifeExp = mean(lifeExp)) %>%
        ggplot(aes(x = year, y = continent, size = mean_lifeExp)) +
        geom_point(shape = 21, color = "red", fill = "white") +
        scale_size_continuous(range = c(7, 15)) +
        geom_text(aes(label = round(mean_lifeExp, 2)), size = 3, color = "black") +
        theme(legend.position = "none") +
        labs(title = "Life Expectancy for Each Continent Over Time")

# themes
gapdata %>%
        ggplot(aes(x = gdpPercap, y = lifeExp, color = continent)) +
        geom_point() +
        geom_smooth(lwd = 3, se = FALSE, method = "lm") +
        ggthemes::theme_calc() +
        ggtitle("ggthemes::theme_calc()")
gapdata %>%
        ggplot(aes(x = gdpPercap, y = lifeExp, color = continent)) +
        geom_point() +
        geom_smooth(lwd = 3, se = FALSE, method = "lm") +
        ggthemes::theme_economist_white() +
        ggtitle("ggthemes::theme_economist_white()")
gapdata %>%
        ggplot(aes(x = gdpPercap, y = lifeExp, color = continent)) +
        geom_point() +
        geom_smooth(lwd = 3, se = FALSE, method = "lm") +
        ggthemes::theme_wsj() +
        ggtitle("ggthemes::theme_wsj()")


# ch14 theme --------------------------------------------------------------
glimpse(mpg)
df <- mpg %>% 
        as_tibble() %>% 
        filter(class != "2seater", manufacturer %in% c("toyota", "volkswagen"))
df
df %>%
        ggplot(aes(x = displ, y = hwy, color = factor(cyl))) +
        geom_point() +
        facet_grid(vars(manufacturer), vars(class)) +
        ggtitle("My Title") +
        labs(x = "x_displ", y = "y_hwy")

# graph element
df %>%
        ggplot(aes(x = displ, y = hwy, color = factor(cyl))) +
        geom_point() +
        facet_grid(vars(manufacturer), vars(class)) +
        ggtitle("My Title") +
        labs(x = "x_displ", y = "y_hwy") +
        theme(
                plot.background = element_rect(fill = "orange", color = "black", size = 10),
                plot.title = element_text(hjust = 1, color = "red", face = "italic"),
                plot.margin = margin(t = 20, r = 20, b = 20, l = 20, unit = "pt")
        )

# axis element
df %>%
        ggplot(aes(x = displ, y = hwy, color = factor(cyl))) +
        geom_point() +
        facet_grid(vars(manufacturer), vars(class)) +
        ggtitle("My Title") +
        labs(x = "x_displ", y = "y_hwy") +
        theme(
                axis.line = element_line(color = "orange", size = 2),
                axis.title = element_text(color = "red", face = "italic"),
                axis.ticks = element_line(color = "purple", size = 3),
                axis.text = element_text(color = "blue"),
                axis.text.x = element_text(angle = 45, hjust = 1)
        )

# panel element
df %>%
        ggplot(aes(x = displ, y = hwy, color = factor(cyl))) +
        geom_point() +
        facet_grid(vars(manufacturer), vars(class)) +
        ggtitle("My Title") +
        labs(x = "x_displ", y = "y_hwy") +
        theme(
                panel.background = element_rect(fill = "orange", color = "blue"),
                panel.grid = element_line(color = "grey80", size = 0.5)
        )

# legend element
df %>%
        ggplot(aes(x = displ, y = hwy, color = factor(cyl))) +
        geom_point() +
        facet_grid(vars(manufacturer), vars(class)) +
        ggtitle("My Title") +
        labs(x = "x_displ", y = "y_hwy") +
        theme(
                legend.background = element_rect(fill = "orange"),
                legend.title = element_text(color = "blue", size = 10),
                legend.key = element_rect(fill = "grey80"),
                legend.text = element_text(color = "red"),
                legend.margin = margin(t = 20, r = 20, b = 20, l = 20, unit = "pt"),
                legend.position = "bottom"
        )

# strip element
df %>%
        ggplot(aes(x = displ, y = hwy, color = factor(cyl))) +
        geom_point() +
        facet_grid(vars(manufacturer), vars(class)) +
        ggtitle("My Title") +
        labs(x = "x_displ", y = "y_hwy") +
        theme(
                strip.background = element_rect(fill = "orange"),
                strip.text = element_text(color = "red"),
                panel.spacing = unit(0.3, "inch") # ,
                # strip.switch.pad.grid =
        )


# ch15 scales --------------------------------------------------------------
ggplot(mpg, aes(x = displ, y = hwy)) + 
        geom_point(aes(colour = class)) +
        scale_x_continuous() + 
        scale_y_continuous() + 
        scale_colour_discrete()
ggplot(mpg, aes(x = displ, y = hwy)) + 
        geom_point(aes(colour = class)) +
        scale_x_continuous(name = "X") + 
        scale_y_continuous(name = "Y") + 
        scale_colour_brewer()

# case study
gapdata <- read_csv("./demo_data/gapminder.csv")
newgapdata <- gapdata %>% 
        group_by(continent, country) %>% 
        summarise(
                across(c(lifeExp, gdpPercap, pop), mean)
        )
newgapdata

newgapdata %>% 
        ggplot(aes(x = gdpPercap, y = lifeExp)) +
        geom_point(aes(color = continent, size = pop)) +
        scale_x_continuous()
newgapdata %>% 
        ggplot(aes(x = gdpPercap, y = lifeExp)) +
        geom_point(aes(color = continent, size = pop)) +
        scale_x_log10(breaks = c(500, 1000, 3000, 10000, 30000),
                      labels = scales::dollar) +
        scale_color_viridis_d()
newgapdata %>% 
        ggplot(aes(x = gdpPercap, y = lifeExp)) +
        geom_point(aes(color = continent, size = pop)) +
        scale_x_log10() +
        scale_color_manual(
                name = "Continents",
                values = c("Africa" = "red", "Americas" = "blue", "Asia" = "orange",
                           "Europe" = "black", "Oceania" = "gray"),
                breaks = c("Africa", "Americas", "Asia", "Europe", "Oceania"),
                labels = c("Africa", "Americas", "Asia", "Europe", "Oceania")
        ) +
        scale_size(
                name = "Population",
                breaks = c(2e8, 5e8, 7e8),
                labels = c("200M", "500M", "700M")
        )

# ch16 guides --------------------------------------------------------------
mpg %>%
        ggplot(aes(x = displ, y = hwy, color = factor(cyl))) +
        geom_point() +
        ggtitle("My Title") +
        labs(x = "x_displ", y = "y_hwy") +
        guides(color = guide_legend(ncol = 4))

# remove a legend
mpg %>%
        ggplot(aes(x = displ, y = hwy, color = class, size = cyl)) +
        geom_point()
mpg %>%
        ggplot(aes(x = displ, y = hwy, color = class, size = cyl)) +
        geom_point() +
        guides(color = guide_legend(),  # keep
               size = FALSE             # remove
        )

# example
mtcars %>%
        as_tibble() %>%
        ggplot(aes(x = wt, y = mpg, shape = factor(vs), color = hp)) +
        geom_point(size = 3) +
        colorspace::scale_color_continuous_sequential(palette = "Dark Mint") +
        scale_shape_discrete(labels = c("V-shaped", "Straight")) +
        labs(
                x = "Weight (1000 lbs)", y = "Miles per gallon",
                title = "Motor Trend Car Road Tests",
                shape = "Engine", color = "Horsepower"
        ) +
        theme(
                text = element_text(size = 18, color = "white"),
                rect = element_rect(fill = "black"),
                panel.background = element_rect(fill = "black"),
                legend.key = element_rect(fill = "black"),
                axis.text = element_text(color = "white"),
                plot.title.position = "plot",
                plot.margin = margin(10, 10, 10, 10)
        ) +
        guides(
                shape =
                        guide_legend(override.aes = list(color = "white"))
        )

# ch17 extension --------------------------------------------------------------
gapdata <- read_csv("./demo_data/gapminder.csv")

# customized labels
gapdata %>%
        ggplot(aes(x = gdpPercap, y = lifeExp, color = continent)) +
        geom_point() +
        scale_x_log10() +
        labs(
                title = "My Plot Title",
                subtitle = "My Plot subtitle",
                x = "The X Variable",
                y = "The Y Variable"
        )

# customized colors
gapdata %>%
        ggplot(aes(x = gdpPercap, y = lifeExp, color = continent)) +
        geom_point() +
        scale_x_log10() +
        scale_color_manual(
                values = c("#195744", "#008148", "#C6C013", "#EF8A17", "#EF2917")
        )

# plots combination
p1 <- gapdata %>%
        ggplot(aes(x = gdpPercap, y = lifeExp)) +
        geom_point(aes(color = lifeExp > mean(lifeExp))) +
        scale_x_log10() +
        theme(legend.position = "none") +
        scale_color_manual(values = c("orange", "pink")) +
        labs(
                title = "My Plot Title",
                x = "The X Variable",
                y = "The Y Variable"
        )
p2 <- gapdata %>%
        ggplot(aes(x = gdpPercap, y = lifeExp, color = continent)) +
        geom_point() +
        scale_x_log10() +
        scale_color_manual(
                values = c("#195744", "#008148", "#C6C013", "#EF8A17", "#EF2917")
        ) +
        theme(legend.position = "none") +
        labs(
                title = "My Plot Title",
                x = "The X Variable",
                y = "The Y Variable"
        )
p1 + p2 +
        plot_annotation(
                tag_levels = "A",
                title = "The surprising truth about mtcars",
                subtitle = "These 2 plots will reveal yet-untold secrets about our beloved data-set",
                caption = "Disclaimer: None of these plots are insightful"
        )
ggsave("demo_plot.pdf", plot = p1, width = 8, height = 6)

# highlight certain category
gapdata %>%
        ggplot(
                aes(x = year, y = lifeExp, color = continent, group = country)
        ) +
        geom_line() +
        gghighlight(
                country == "China", # which is passed to dplyr::filter().
                label_key = country,
                use_group_by = FALSE
        )

gapdata %>%
        filter(continent == "Asia") %>%
        ggplot(aes(year, lifeExp, color = country, group = country)) +
        geom_line(size = 1.2, alpha = .9, color = "#E58C23") +
        theme_minimal(base_size = 14) +
        theme(
                legend.position = "none",
                panel.grid.major.x = element_blank(),
                panel.grid.minor = element_blank()
        ) +
        gghighlight(
                country %in% c("China", "India", "Japan", "Korea, Rep."),
                use_group_by = FALSE,
                use_direct_label = FALSE,
                unhighlighted_params = list(color = "grey90")
        ) +
        facet_wrap(vars(country))

# use function to generate plot
tibble(x = seq(from = -3, to = 3, by = .01)) %>%
        mutate(y = dnorm(x, mean = 0, sd = 1)) %>% # dnorm gives the density
        ggplot(aes(x = x, y = y)) +
        geom_line(color = "grey33")
ggplot(data = data.frame(x = c(-3, 3)), aes(x = x)) +
        stat_function(fun = dnorm, geom = "line", color = "steelblue")

# map
nyc_squirrels <- read_csv("./demo_data/nyc_squirrels.csv")
central_park <- sf::read_sf("./demo_data/central_park")
ggplot() +
        geom_sf(data = central_park)
nyc_squirrels %>%
        drop_na(primary_fur_color) %>%
        ggplot() +
        geom_sf(data = central_park, color = "grey85") +
        geom_point(
                aes(x = long, y = lat, color = primary_fur_color),
                size = .8
        )
nyc_squirrels %>%
        drop_na(primary_fur_color) %>%
        ggplot() +
        geom_sf(data = central_park, color = "grey85") +
        geom_point(
                aes(x = long, y = lat, color = primary_fur_color),
                size = .8
        ) +
        gghighlight(
                label_key = primary_fur_color,
                use_direct_label = FALSE
        ) +
        facet_wrap(vars(primary_fur_color)) +
        cowplot::theme_map(16) +
        theme(legend.position = "none")

# Latex formula
ggplot(mpg, aes(x = displ, y = hwy)) +
        geom_point() +
        annotate("text",
                 x = 4, y = 40,
                 label = TeX("$\\alpha^2 + \\theta^2 = \\omega^2$"),
                 size = 9
        ) +
        labs(
                title = TeX("The ratio of 1 and 2 is $\\,\\, \\frac{1}{2}$"),
                x = TeX("$\\alpha$"),
                y = TeX("$\\alpha^2$")
        )






# ch18 layer --------------------------------------------------------------
# traditional method
simple_data <- tibble(group = factor(rep(c("A", "B"), each = 15)),
                      subject = 1:30,
                      score = c(rnorm(15, 40, 20), rnorm(15, 60, 10)))
simple_data
simple_data_bar <- simple_data %>%
        group_by(group) %>% 
        summarize(
                mean_score = mean(score),
                .groups = 'drop'
        )

simple_data_errorbar <- simple_data %>% 
        group_by(group) %>% 
        summarize(
                mean_score = mean(score),
                se = sqrt(var(score)/length(score)),
                .groups = 'drop'
        ) %>% 
        mutate(
                lower = mean_score - se,
                upper = mean_score + se
        )

ggplot() +
        geom_col(
                aes(x = group, y = mean_score),
                data = simple_data_bar
        ) +
        geom_errorbar(
                aes(x = group, y = mean_score, ymin = lower, ymax = upper),
                data = simple_data_errorbar
        )

# method with stat_summary()
simple_data %>% 
        ggplot(aes(x = group, y = score)) +
        stat_summary(geom = "bar") +
        stat_summary(geom = "errorbar")

# another example
set.seed(2020)
height_df <- tibble(group = factor(rep(c("A", "B"), each = 30)),
                    height = c(rnorm(30, 170, 10), rnorm(30, 120, 10)))
height_df %>% 
        ggplot(aes(x = group, y = height)) +
        geom_point()
# mean_se is to calculate the point range, see ?mean_se().
height_df %>% 
        ggplot(aes(x = group, y = height)) +
        stat_summary(geom = "pointrange", fun.data = mean_se)
height_df %>% 
        ggplot(aes(x = group, y = height)) +
        stat_summary(geom = "errorbar", fun.data = ~mean_se(., mult = 1.96)) # Increase `mult` value for bigger interval!
mean_se(height_df$height)
pointrange_plot <- height_df %>% 
        ggplot(aes(x = group, y = height)) +
        stat_summary()
layer_data(pointrange_plot, 1) # pull the data behind graph



#--------------------------------------------------#
########## chapter 19 tidyverse summary ############
#--------------------------------------------------#
# readr and tibble
penguins <- read_csv("./demo_data/penguins.csv") 
penguins
glimpse(penguins)

# ggplot2
ggplot(data = penguins, aes(x = sex, y = body_mass_g)) +
        geom_boxplot(aes(fill = species))

# dplyr
penguins %>% 
        group_by(sex, species) %>%
        summarize(count = n())
penguins %>% 
        group_by(species) %>%
        mutate(count_species = n()) %>%
        ungroup() %>%
        group_by(species, sex, count_species) %>%
        summarize(count = n()) %>%
        mutate(prop = count/count_species*100)

# forcats
penguins_new <-
        penguins %>%
        mutate(year_factor = factor(year, levels = unique(year)))
penguins_new
class(penguins_new$year_factor)
levels(penguins_new$year_factor)

# stringr
penguins %>%
        select(species, island) %>%
        mutate(ISLAND = str_to_upper(island)) %>%
        mutate(species_island = str_c(species, ISLAND, sep = "_"))

# tidyr
untidy_penguins <-
        penguins %>%
        pivot_wider(names_from = sex,
                    values_from = body_mass_g)
untidy_penguins
# number of rows will increase due to NAs
untidy_penguins %>%
        pivot_longer(cols = male:`NA`, 
                     names_to = "sex",
                     values_to = "body_mass_g")

# purrr
penguins %>% map(~sum(is.na(.)))

#--------------------------------------------------#
########## chapter 20 tidyverse tips ############
#--------------------------------------------------#
# count
df <- tibble(
        name = c("Alice", "Alice", "Bob", "Bob", "Carol", "Carol"),
        type = c("english", "math", "english", "math", "english", "math"),
        score = c(60.2, 90.5, 92.2, 98.8, 82.5, 74.6)
)
df %>% count(name)
df %>% 
        group_by(name) %>% 
        summarise(n = n())
df %>% add_count(name) # add the count column
df %>% count(range = 10 * (score %/% 10))

c("a", "c", "d", "k") %>% first()
c("a", "c", "d", "k") %>% last()
c("a", "c", "d", "k") %>% nth(3)

df %>% top_n(2, score)

# not include
`%nin%` <- purrr::negate(`%in%`)
3:10 %nin% c(1:5)

# replace na
dt <- tribble(
        ~x, ~y,
        1, NA,
        2, NA,
        NA, -3,
        NA, -4,
        5, -5
)
dt %>% mutate(x = replace_na(x, 0))
dt %>% mutate(
        z = coalesce(x, 0)
        # z = coalesce(x, y)
)

# list-column
gapminder <- read_csv("./demo_data/gapminder.csv")
gapminder %>%
        group_nest(continent) %>%
        mutate(test = map(data, ~ t.test(.x$gdpPercap))) %>%
        mutate(tidied = map(test, broom::tidy)) %>%
        unnest(tidied)

gapminder %>%
        distinct(continent, country) %>%
        count(continent) %>%
        ggplot(aes(x = fct_reorder(continent, n), y = n)) +
        geom_col() +
        coord_flip()

gapminder %>%
        distinct(continent, country) %>%
        count(continent) %>% 
        mutate(coll = if_else(continent == "Asia", "red", "gray")) %>% 
        ggplot(aes(x = fct_reorder(continent, n), y = n)) +
        geom_text(aes(label = n), hjust = -0.25) +
        geom_col(width = 0.8, aes(fill = coll) ) +
        coord_flip() +
        annotate("text", x = 3.8, y = 45, label = "this is important\ncase", 
                 color = "#D55E00", size = 5) +
        annotate(
                geom = "curve", x = 4.1, y = 48, xend = 4.1, yend = 35, 
                curvature = .3, arrow = arrow(length = unit(2, "mm"))
        ) +
        theme_classic() +
        scale_fill_manual(values = c("#b3b3b3a0", "#D55E00")) +
        theme(legend.position = "none",
              axis.text = element_text(size = 11)
        ) +
        labs(title = "Number of Countries in Each Continent", x = "", y = "")

# fct_lump()
tb <- tibble::tribble(
        ~disease,  ~n,
        "鼻塞", 112,
        "流涕", 130,
        "发热",  89,
        "腹泻",   5,
        "呕吐",  12,
        "咳嗽", 102,
        "咽痛",  98,
        "乏力",  15,
        "腹痛",   2,
        "妄想",   3,
        "幻听",   6,
        "失眠",   1,
        "贫血",   8,
        "多动",   2,
        "胸痛",   4,
        "胸闷",   5
)

p1 <- tb %>% 
        uncount(n) %>% 
        ggplot(aes(x = disease, fill = disease)) +
        geom_bar() +
        coord_flip() +
        theme(legend.position = "none")
p2 <- tb %>% 
        uncount(n) %>% 
        mutate(
                disease = forcats::fct_lump(disease, 5),
                disease = forcats::fct_reorder(disease, .x = disease, .fun = length)
        ) %>% 
        ggplot(aes(x = disease, fill = disease)) +
        geom_bar() +
        coord_flip() +
        theme(legend.position = "none")
p1 + p2


# fct_reorder2
dat_wide <- tibble(
        x = 1:3,
        top = c(4.5, 4, 5),
        middle = c(4, 4.75, 6),
        bottom = c(3.5, 3.75, 7)
)
dat <- dat_wide %>% 
        pivot_longer(
                cols = c(top, middle, bottom),
                names_to = "region",
                values_to = "awfulness") %>% 
        mutate(region_sane = fct_reorder2(region, x, awfulness))
dat %>% 
        ggplot(aes(x, awfulness, colour = region_sane)) +
        geom_line() + theme(legend.justification = c(1, 0.85))

# unite, separate, extract
dfc <- tibble(x = c("1-12week", "1-10wk", "5-12w", "01-05weeks"))
dfc
dfc %>% tidyr::extract(
        col = x,
        into = c("start", "end", "letter"), 
        regex = "(\\d+)-(\\d+)([a-z]+)",
        remove = FALSE
)

# crossing
tidyr::crossing(trials = 1:10, m = 1:5) %>%
        group_by(trials) %>%
        mutate(
                guess = sample.int(5, n(), replace = TRUE),
                result = m == guess
        ) %>%
        summarise(score = sum(result) / n())

# central limit theory
sim <- tribble(
        ~f, ~params,
        "rbinom", list(size = 1, prob = 0.5, n = 10)
)
rep_sim <- sim %>%
        crossing(rep = 1:1e5) %>%
        mutate(sim = invoke_map(f, params)) %>%
        unnest(sim) %>%
        group_by(rep) %>%
        summarise(mean_sim = mean(sim))
rep_sim %>% 
        ggplot(aes(x = mean_sim)) +
        geom_histogram(binwidth = 0.05,  fill = "skyblue") +
        theme_classic()

#--------------------------------------------------#
########## chapter 21 advanced tidyverse ###########
#--------------------------------------------------#
# mutate_if
iris <- iris %>% as_tibble() 
df_iris <- iris %>% head(5)
df_iris %>% mutate_if(is.double, as.integer) # change the numeric type for multiple columns

# select_if
df <- tibble::tibble(
        x = letters[1:3],
        y = c(1:3),
        z = c(0, 0, 0)
)
to_want <- function(x) is.numeric(x) && sum(x) > 3
df %>% select_if(to_want)

# filter_if
mtcars %>% filter_if(~ all(floor(.) == .), all_vars(. != 0))

# group_by
# Group by variables selected with a predicate:
iris %>% group_by_if(is.factor)
# group_split will return list
iris %>%
        dplyr::group_split(Species) %>%
        purrr::map(~ broom::tidy(lm(Petal.Length ~ Sepal.Length, data = .x)))
## `group_map()` return a list of tibble(返回元素均为df的一个列表list(df1,df2,df3))
iris %>%
        dplyr::group_by(Species) %>%
        dplyr::group_map(~ broom::tidy(lm(Petal.Length ~ Sepal.Length, data = .x)))
# group_modify will return data frame, ## The result of .f should be a data frame(.f 必须返回数据框)
iris %>%
        dplyr::group_by(Species) %>%
        dplyr::group_modify(~ broom::tidy(lm(Petal.Length ~ Sepal.Length, data = .x)))

# batch graphs
nobel_winners <- read_csv("./demo_data/nobel_winners.csv") %>% 
        mutate(prize_age = prize_year - lubridate::year(birth_date))
nobel_winners %>%
        dplyr::group_split(category) %>%
        purrr::map(
                ~ ggplot(data = .x, aes(x = prize_age)) +
                        geom_density() +
                        ggtitle(.x$category)
        )
nobel_winners %>%
        dplyr::group_by(category) %>%
        dplyr::group_walk(
                ~ ggsave(
                        paste0(.y, ".png"),
                        ggplot(data = .x, aes(x = prize_age)) +
                                geom_density() +
                                ggtitle(.y),
                        device = "png",
                        path = "./batch_graph"
                )
        ) %>%
        invisible()

# check and replace NAs
airquality %>%
        as_tibble() %>% 
        purrr::map_df(~ sum(is.na(.)))
airquality %>%
        as_tibble() %>% 
        mutate_if(is.numeric, replace_na, replace = 0) %>% 
        purrr::map_df(~ sum(is.na(.)))

# standardization
func <- function(x) (x - mean(x)) / (sd(x))
func <- function(x) (x - min(x)) / (max(x) - min(x))
mtcars %>%
        as_tibble() %>%
        mutate_at(vars(mpg, disp), ~func(.))
# change derived column names
funs <- list(
        centered = mean, # Function object
        scaled = ~ (. - mean(.)) / sd(.) # Purrr-style lambda
)
mtcars %>%
        as_tibble() %>% 
        mutate_at(vars(mpg, disp), funs)

# across
iris %>%
        group_by(Species) %>%
        summarise(
                across(starts_with("Petal"), list(mean = mean), .names = "{col}_{fn}")
        )




#--------------------------------------------------#
########## chapter 22 colwise and rowwise ############
#--------------------------------------------------#
# summarize
df <- tibble(
        grp = rep(c("a", "b"), each = 5),
        x = c(rnorm(5, -0.25, 1), rnorm(5, 0, 1.5)),
        y = c(rnorm(5, 0.25, 1), rnorm(5, 0, 0.5))
)
df
df %>%
        group_by(grp) %>%
        summarise(
                x = quantile(x, c(0.25, 0.5, 0.75)),
                q = c(0.25, 0.5, 0.75)
        )

# select
vars <- c("x", "y", "z")
df %>% select(all_of(vars)) # will return error if any var does not exist in the data frame
df %>% select(any_of(vars))

# relocate
df %>% relocate(grp, .after = y)
df %>% relocate(grp, .after = last_col())
df %>% relocate(x, .before = 1)

# across
iris <- iris %>% as_tibble()
iris
iris %>%
        group_by(Species) %>%
        summarise(
                across(where(is.numeric), mean)
        )
iris %>%
        group_by(Species) %>%
        summarise(
                across(starts_with("Sepal"), ~ (.x - mean(.x)) / sd(.x))
        )
iris %>%
        group_by(Species) %>%
        summarise(
                across(starts_with("Petal"), list(min = min, max = max))
        )

# current group or current variable
df <- tibble(
        g = sample(rep(letters[1:3], 1:3)),
        x = runif(6),
        y = runif(6)
)
df
a <- df %>%
        group_by(g) %>%
        summarise(
                #group = list(cur_group()),
                data = list(cur_data())
        )

# rowwise operation
df <- tibble(id = letters[1:6], w = 10:15, x = 20:25, y = 30:35, z = 40:45)
df

# calculate the average of w,x,y,z.
df %>%
        rowwise(id) %>%
        mutate(
                avg = mean(c_across(w:z))
        )

# operation on list column
tb <- tibble(
        id = c(1:4),
        x = list(1, 2:3, 4:6, 5:8)
)
tb
tb %>%
        group_by(id) %>% 
        mutate(l = length(x))
tb %>%
        group_by(id) %>% 
        mutate(l = purrr::map_int(x, length))
tb %>%
        rowwise(id) %>%
        mutate(l = length(x))

# rowwise modeling
mtcars %>%
        nest_by(cyl) %>%
        mutate(model = list(lm(mpg ~ wt, data = data))) %>%
        summarise(broom::tidy(model))





#--------------------------------------------------#
########## chapter 23 NAs ############
#--------------------------------------------------#
# NA as a logical variable
TRUE & NA
FALSE & NA
TRUE | NA
FALSE | NA

# count NAs
penguins <- read_csv("demo_data/penguins.csv")
sum_of_na <- function(x){
        sum(is.na(x))
}
penguins %>% summarise(
        across(everything(), ~sum(is.na(.x)))
)

d <- tibble(x = c(1, 3, 6, NA, 8, NA))
d
d %>% mutate(
        is_even = case_when(
                x %% 2 == 0 ~ "even",
                x %% 2 == 1 ~ "not even",
                TRUE ~ NA_character_  # wrong if use NA
        )
)

# quiz: find which row has missing value
penguins %>% 
        rowid_to_column() %>% 
        mutate(
                across(everything(), ~as.character(.x))
        ) %>% 
        rowwise(rowid) %>% 
        mutate(NAs = sum(is.na(c_across(everything())))) %>% 
        filter(NAs > 0)



#--------------------------------------------------#
########## chapter 24 dot ############
#--------------------------------------------------#
# place holder
mtcars %>%
        select(cyl, disp, hp) %>%
        head(2)
mtcars %>%
        select(., cyl, disp, hp) %>%
        head(2)

# lambda function
mtcars %>%
        select_at(vars(contains("ar")), ~ toupper(.)) %>%
        head(3)

# dplyr with map
df <- tibble(
        mean = c(1, 2),
        sd = c(2, 4)
)
df
# first . refers df, send . refers df$mean
set.seed(2020)
df %>%
        dplyr::mutate(., rand = map(mean, ~ rnorm(5, .))) %>%
        tidyr::unnest_wider(rand)
df %>%
        dplyr::mutate(rand = map2(mean, sd, ~ rnorm(5, .x, .y))) %>%
        tidyr::unnest_wider(rand)

# put it together
read_csv("./demo_data/wages.csv") %>% # . indicates current work directory
        dplyr::mutate(letter = str_extract(race, "(?<=h)(.)")) %>% # . indicates any character
        dplyr::select(., -letter) %>% # . is a place holder, refers the df generated in the above step
        dplyr::mutate_at(vars(race), ~ as.factor(.)) %>% # lambda function
        dplyr::mutate_at(vars(sex), ~ if_else(. == "male", 1, 0)) %>% # lambda function
        dplyr::filter_if(~ is.numeric(.), all_vars(. != 0)) %>% # lambda function
        split(.$sex) %>% # placeholder
        purrr::map(~ lm(earn ~ ., data = .)) %>% # first . refers all other variables except earn, second . is a placeholder
        purrr::map_dfr(., ~ broom::tidy(.), .id = "sex") # first . is a placeholder, second . refers lambda function, third . is a argument prefix







#--------------------------------------------------------#
########## chapter 25 non standard evaluation ############
#--------------------------------------------------------#
# write a function
grouped_mean <- function(data, group_var, summary_var) {
        group_var <- enquo(group_var)
        summary_var <- enquo(summary_var)
        
        data %>%
                group_by(!!group_var) %>%
                summarise(mean = mean(!!summary_var))
}
grouped_mean(mtcars, cyl, mpg)




#--------------------------------------------------------#
########## chapter 26 simulation and sampling ############
#--------------------------------------------------------#
# random number generation
rnorm(5, mean = 0 , sd = 1)

# normal distribution
tibble(
        x = rnorm(n = 100, mean = 0, sd = 1)
) %>%
        ggplot(aes(x = x)) +
        geom_density() +
        stat_function(
                fun = dnorm,
                args = list(mean = 0, sd = 1),
                color = "red"
        )

# linear relationship
beta_0 <- 4
beta_1 <- 3.2
epsilon <- rnorm(n = 1000, mean = 0, sd = 1)
sim_normal <- tibble(
        x_vals = seq(from = 0, to = 5, length.out = 1000),
        y_vals = beta_0 + beta_1 * x_vals + epsilon,
)
sim_normal %>% head()
sim_normal %>%
        ggplot(aes(x = x_vals, y = y_vals)) +
        geom_point()

# multi-variable normal distribution
a <- 3.5
b <- -1
sigma_a <- 1
sigma_b <- 0.5
rho <- -0.7
mu <- c(a, b)
cov_ab <- sigma_a * sigma_b * rho
sigma <- matrix(c(
        sigma_a^2, cov_ab,
        cov_ab, sigma_b^2), ncol = 2)
d <- MASS::mvrnorm(1000, mu = mu, Sigma = sigma) %>%
        data.frame() %>%
        set_names("group_a", "group_b")
head(d)
d %>%
        ggplot(aes(x = group_a, y = group_b)) +
        geom_point() +
        stat_ellipse(type = "norm", level = 0.95)

# monte carlo simulation, estimate pi
set.seed(2020)
n <- 100000
points <- tibble("x" = runif(n, min = -1, max = 1), "y" = runif(n, min = -1, max = 1))
#points <- tibble("x" = runif(n, min = 0, max = 1), "y" = runif(n, min = 0, max = 1))
points <- points %>%
        mutate(position = map2_dbl(.x = x, .y = y, ~ .x**2 + .y**2)) %>% 
        mutate(inside = map2_dbl(.x = x, .y = y, ~ if_else(.x**2 + .y**2 < 1, 1, 0))) %>%
        rowid_to_column("N") %>% 
        mutate(estimate = 4 * cumsum(inside) / N)
points %>%
        filter(N >= 100) %>% 
        ggplot() +
        geom_line(aes(y = estimate, x = N), colour = "#82518c") +
        geom_hline(yintercept = pi)

# population and sample
true.mean <- 175.7
true.sd <- 15.19
pop.distn <-
        tibble(
                height = seq(100, 250, 0.5),
                density = dnorm(height, mean = true.mean, sd = true.sd)
        )
sample.a <-
        tibble(height = rnorm(n = 30, mean = true.mean, sd = true.sd))
sample.a %>%
        ggplot(aes(x = height)) +
        geom_histogram(aes(y = stat(density)),
                       fill = "steelblue",
                       alpha = 0.75,
                       bins = 10
        ) +
        geom_line(
                data = pop.distn,
                aes(x = height, y = density),
                alpha = 0.25, size = 1.5
        ) +
        geom_vline(xintercept = true.mean, linetype = "dashed", color = "red") +
        geom_vline(xintercept = mean(sample.a$height), linetype = "solid")

# multiple sampling
rnorm.stats <- function(n, mu, sigma) {
        the.sample <- rnorm(n, mu, sigma)
        tibble(
                sample.size = n,
                sample.mean = mean(the.sample),
                sample.sd = sd(the.sample)
        )
}
rnorm.stats(30, true.mean, true.sd)
df.samples.of.30 <-
        purrr::rerun(2500, rnorm.stats(30, true.mean, true.sd)) %>%
        dplyr::bind_rows()
df.samples.of.30 %>%
        ggplot(aes(x = sample.mean, y = stat(density))) +
        geom_histogram(bins = 25, fill = "firebrick", alpha = 0.5) +
        geom_vline(xintercept = true.mean, linetype = "dashed", color = "red") +
        labs(
                title = "Distribution of mean heights for 2500 samples of size 30"
        )
df.samples.of.30 %>%
        ggplot(aes(x = sample.mean, y = stat(density))) +
        geom_histogram(bins = 50, fill = "firebrick", alpha = 0.5) +
        geom_histogram(
                data = sample.a,
                aes(x = height, y = stat(density)),
                bins = 11, fill = "steelblue", alpha = 0.25
        ) +
        geom_vline(xintercept = true.mean, linetype = "dashed", color = "red") +
        geom_line(data = pop.distn, aes(x = height, y = density), alpha = 0.25, size = 1.5) +
        xlim(125, 225)




#--------------------------------------------------#
########## chapter 28 linear regression ############
#--------------------------------------------------#
wages <- read_csv("./demo_data/wages.csv")

# check NAs
wages %>%
        summarise_all(
                ~ sum(is.na(.))
        )

# simple statistics
wages %>% count(sex)
wages %>% count(race)
wages %>% count(sex, race)

wages %>%
        ggplot(aes(x = earn, color = sex)) +
        geom_density()

wages %>%
        group_by(sex, race) %>% 
        summarise(median_earn = median(earn)) %>% 
        ggplot(aes(x = sex, y = median_earn, fill = sex)) +
        geom_bar(stat = "identity") +
        facet_wrap("race")

# the relationship between height and earn
mod1 <- lm(
        formula = earn ~ height,
        data = wages
)
print(mod1)
names(mod1)
summary(mod1)
wages %>%
        modelr::add_predictions(mod1) %>%
        modelr::add_residuals(mod1)
wages %>% 
        ggplot(aes(x = height, y = earn, color = sex)) +
        geom_point(alpha = 0.5) +
        geom_smooth(method = "lm", formula = y~x, se = FALSE)

# multivariate linear regression
mod2 <- lm(earn ~ height + ed, data = wages)
mod2

# which variable is more important
# method 1
fit <- wages %>%
        mutate_at(vars(earn, height, ed, age), scale) %>%
        lm(earn ~ 1 + height + ed + age, data = .)
summary(fit)

# method 2
fit <- wages %>%
        lm(earn ~ 1 + height + ed + age, data = .)
caret::varImp(fit)

# categorical variable
wages %>% 
        group_by(race) %>% 
        summarise(earn_mean = mean(earn), earn_median = median(earn))
wages %>%
        ggplot(aes(x = race, y = earn, fill = race)) +
        geom_boxplot(position = position_dodge()) +
        scale_y_continuous(limits = c(0, 20000))
mod3 <- lm(earn ~ race, data = wages)
summary(mod3)
broom::tidy(mod3)

# factor
wages_fct <- wages %>%
        mutate(race = factor(race, levels = c("hispanic", "white", "black", "other"))) %>%
        select(earn, race)
mod4 <- lm(earn ~ race, data = wages_fct)
summary(mod4)
broom::tidy(mod4)

# one continuous and one categorical variable
mod5 <- lm(earn ~ height + sex, data = wages)
summary(mod5)
p1 <- wages %>%
        ggplot(aes(x = height, y = earn, color = sex)) +
        geom_point(alpha = 0.1) +
        geom_line(aes(y = predict(mod5))) +
        scale_y_continuous(limits = c(0, 100000))
p1

# cross impact
mod6 <- lm(earn ~ height + sex + height:sex, data = wages)
summary(mod6)
p2 <- wages %>%
        ggplot(aes(x = height, y = earn, color = sex)) +
        geom_point(alpha = 0.1) +
        geom_line(aes(y = predict(mod6))) +
        scale_y_continuous(limits = c(0, 100000))
p2

wages %>%
        ggplot(aes(x = height, y = earn, color = sex)) +
        #geom_point(alpha = 0.2) +
        geom_smooth(method = "lm", se = FALSE)

combined <- p1 + p2 & theme(legend.position = "bottom")
combined + plot_layout(guides = "collect")

# dummy variable
wages %>%
        group_by(sex) %>%
        group_modify(
                ~ broom::tidy(lm(earn ~ height, data = .))
        )




#--------------------------------------------------#
########## chapter 29 organize model output ############
#--------------------------------------------------#
gapminder <- read_csv("./demo_data/gapminder.csv")
gapminder

model_colors <- colorspace::qualitative_hcl(4, palette = "dark 2")
# model_colors <- c("darkorange", "purple", "cyan4")

ggplot(
        data = gapminder,
        mapping = aes(x = log(gdpPercap), y = lifeExp)
) +
        geom_point(alpha = 0.2) +
        geom_smooth(
                method = "lm",
                aes(color = "OLS", fill = "OLS") # one
        ) +
        geom_smooth(
                method = "lm", formula = y ~ splines::bs(x, df = 3),
                aes(color = "Cubic Spline", fill = "Cubic Spline") # two
        ) +
        geom_smooth(
                method = "loess",
                aes(color = "LOESS", fill = "LOESS") # three
        ) +
        scale_color_manual(name = "Models", values = model_colors) +
        scale_fill_manual(name = "Models", values = model_colors) +
        theme(legend.position = "top")

out <- lm(
        formula = lifeExp ~ gdpPercap + pop + continent,
        data = gapminder
)
summary(out)

# use broom package
tidy(out, conf.int = TRUE)
glance(out)
augment(out)

# examples
penguins <- read_csv("./demo_data/penguins.csv") %>% drop_na()
fit_ols <- function(df) {
        lm(body_mass_g ~ bill_depth_mm + bill_length_mm, data = df)
}
out_tidy <- penguins %>%
        group_nest(species) %>%
        mutate(model = purrr::map(data, fit_ols)) %>%
        mutate(tidy = purrr::map(model, ~ broom::tidy(.))) %>%
        tidyr::unnest(tidy) %>%
        dplyr::filter(!term %in% "(Intercept)")
out_tidy
out_tidy %>%
        ggplot(aes(
                x = species, y = estimate,
                ymin = estimate - 2 * std.error,
                ymax = estimate + 2 * std.error,
                color = term
        )) +
        geom_pointrange(position = position_dodge(width = 0.25)) +
        theme(legend.position = "top") +
        labs(x = NULL, y = "Estimate", color = "Coefficient")

# exercise
df <- tibble(
        x = runif(30, 2, 10),
        y = -2*x + rnorm(30, 0, 5)
)
df
ggplot(df, aes(x = x, y = y)) +
        geom_point()
mod <- df %>% 
        group_nest() %>% 
        mutate(model = purrr::map(data, ~ lm(y ~ x, data = .))) %>% 
        mutate(augment = purrr::map(model, ~ broom::augment(.))) %>%
        tidyr::unnest(augment) %>% 
        mutate(ymin = pmin(y, .fitted), ymax = pmax(y, .fitted))
ggplot(mod, aes(x = x, y = y)) +
        geom_segment(aes(xend = x, yend = .fitted), color = "grey") +
        geom_point(aes(size = abs(.resid), color = abs(.resid))) +
        geom_line(aes(x = x, y = .fitted), size = 1.2) + 
        scale_color_gradient(low = "orange", high = "orange4") +
        theme_minimal() +
        theme(legend.position = "none")







#--------------------------------------------------#
########## chapter 30 tidy statistics ##############
#--------------------------------------------------#
wages <- read_csv("./demo_data/wages.csv")
wages %>% 
        head() %>% 
        knitr::kable()

# one-way anova
t.test(earn ~ sex, data = wages)
lm(earn ~ sex, data = wages) %>% 
        summary()
aov(earn ~ sex, data = wages) %>% 
        summary()




#--------------------------------------------------------------------#
########## chapter 31 statistical test and linear model ##############
#--------------------------------------------------------------------#
edata <- read_csv("./demo_data/Experim.csv")  %>%
        mutate(group = if_else(group == "maths skills", 1, 2),
               dep_slope = depress1 - depress3) %>% 
        mutate(
                across(c(sex, id, group), as.factor)
        )
edata

## t-test
model_1_t <- t.test(edata$depress1, mu = 0)
model_1_t
# Run equivalent linear model
model_1_lm <- lm(depress1 ~ 1, data = edata)
summary(model_1_lm)
# comparison
# tidy() gets model outputs we usually use to report our results
model_1_t_tidy <- tidy(model_1_t) %>% mutate(model = "t.test(y)")
model_1_lm_tidy <- tidy(model_1_lm) %>% mutate(model = "lm(y ~ 1)")
results <- bind_rows(model_1_t_tidy, model_1_lm_tidy) %>%
        select(model, estimate, statistic, p.value)
results

# paired t-test
# run paired t-test testing depression from g1 against g2
model_2_t <- t.test(edata$depress1, edata$depress3, paired = TRUE)
model_2_t_tidy <- tidy(model_2_t) %>% mutate(model = "t.test(x, y, paired = TRUE")
model_2_t
# run linear model
model_2_lm <- lm(depress1 - depress3 ~ 1, data = edata)
model_2_lm_tidy <- tidy(model_2_lm) %>% mutate(model = "lm(y-x ~ 1)")
summary(model_2_lm)


## correlation test
ggplot(edata, aes(x = depress1, y = depress3)) + geom_point()
# Run correlation test
model_3_cor <- cor.test(edata$depress3, edata$depress1, method = "pearson")
model_3_cor_tidy <- tidy(model_3_cor) %>% mutate(model = "cor.test(x, y)")
model_3_cor
# Run equivalent linear model
model_3_lm <- lm(depress3 ~ depress1, data = edata)
model_3_lm_tidy <- tidy(model_3_lm) %>% mutate(model = "lm(y ~ x)")
summary(model_3_lm)
# we combine the two model outputs, rowwise
results <- bind_rows(model_3_cor_tidy, model_3_lm_tidy) %>%
        select(model, term, estimate, statistic, p.value)
results


## one way anova
# Run one-way anova
model_4_anova <- aov(depress1 ~ group, data = edata)
model_4_anova_tidy <- tidy(model_4_anova) %>% mutate(model = "aov(y ~ factor(x))")
summary(model_4_anova)
# Run equivalent linear model
model_4_lm <- lm(depress1 ~ group, data = edata)
model_4_lm_tidy <- tidy(model_4_lm) %>% mutate(model = "lm(y ~ factor(x))")
summary(model_4_lm)
# we combine the two model outputs, rowwise
results <- bind_rows(model_4_anova_tidy, model_4_lm_tidy) %>%
        select(model, term, estimate, statistic, p.value)
results


## one way ancova
# Run one-way anova
model_5_ancova <- aov(dep_slope ~ group + confid1, data = edata)
model_5_ancova_tidy <- tidy(model_5_ancova) %>% mutate(model = "aov(y ~ x + z)")
summary(model_5_ancova)
# Run equivalent linear model
model_5_lm <- lm(dep_slope ~ group + confid1, data = edata)
model_5_lm_tidy <- tidy(model_5_lm) %>% mutate(model = "lm(y ~ x + z)")
summary(model_5_lm)
# we combine the two model outputs, rowwise
results <- bind_rows(model_5_ancova_tidy, model_5_lm_tidy) %>%
        select(model, term, estimate, statistic, p.value)
results


## two way anova
# Run anova
model_6_anova <- aov(dep_slope ~ group * sex, data = edata)
model_6_anova_tidy <- tidy(model_6_anova) %>% mutate(model = "aov(y ~ x * z)")
summary(model_6_anova)
# Run equivalent linear model
model_6_lm <- lm(dep_slope ~ group * sex, data = edata)
model_6_lm_tidy <- tidy(model_6_lm) %>% mutate(model = "lm(y ~ x * z)")
summary(model_6_lm)
# we combine the two model outputs, rowwise
results <- bind_rows(model_6_anova_tidy, model_6_lm_tidy) %>%
        select(model, term, estimate, statistic, p.value)
results




#----------------------------------------------------#
########## chapter 32 statistic inference ############
#----------------------------------------------------#
d <- ggplot2movies::movies
d

## what kind of movie has higher rating, action or romance?
movies_genre_sample <- d %>%
        select(title, year, rating, Action, Romance) %>%
        filter(!(Action == 1 & Romance == 1)) %>% # 既是爱情片又是动作片的，删去
        mutate(genre = case_when(
                Action == 1 ~ "Action",
                Romance == 1 ~ "Romance",
                TRUE ~ "Neither"
        )) %>%
        filter(genre != "Neither") %>%
        select(-Action, -Romance) %>%
        group_by(genre) %>%
        slice_sample(n = 34) %>% # 每种题材的电影只选取了34个
        ungroup()
movies_genre_sample %>%
        ggplot(aes(x = genre, y = rating)) +
        geom_boxplot() +
        geom_jitter()
summary_ratings <- movies_genre_sample %>%
        group_by(genre) %>%
        summarize(
                mean = mean(rating),
                std_dev = sd(rating),
                n = n()
        )
summary_ratings

# traditional t test
t_test_eq <- t.test(rating ~ genre,
                    data = movies_genre_sample,
                    var.equal = TRUE
) %>%
        broom::tidy()
t_test_eq

# simulation based
obs_diff <- movies_genre_sample %>%
        specify(formula = rating ~ genre) %>%
        calculate(
                stat = "diff in means",
                order = c("Romance", "Action")
        )
obs_diff
null_dist <- movies_genre_sample %>%
        specify(formula = rating ~ genre) %>%
        hypothesize(null = "independence") %>% # H0 is there is no difference in terms of rating between romance and action movie
        generate(reps = 5000, type = "permute") %>% # randomly assign ratings across all the movies
        calculate(
                stat = "diff in means",
                order = c("Romance", "Action")
        )
head(null_dist)
null_dist %>%
        visualize()
null_dist %>%
        visualize() +
        shade_p_value(obs_stat = obs_diff, direction = "both")
pvalue <- null_dist %>%
        get_pvalue(obs_stat = obs_diff, direction = "two_sided")
pvalue


## point estimate, null hypothesis is that the mean number of hours worked per week in our population is 40
data(gss)
# make the hours less than 40 on purpose
a <- gss %>% 
        mutate(hours = rnorm(500, 39.5, 1))
point_estimate <- a %>%
        specify(response = hours) %>%
        calculate(stat = "mean")
null_dist <- a %>%
        specify(response = hours) %>%
        hypothesize(null = "point", mu = 40) %>%
        generate(reps = 1000, type = "bootstrap") %>% 
        calculate(stat = "mean")
null_dist %>%
        visualize() +
        shade_p_value(obs_stat = point_estimate, direction = "two-sided")

# note regarding to bootstrap generate https://github.com/tidymodels/infer/blob/master/R/generate.R
# mean shift operation is specified in the source code. 
# "If so, shift the variable chosen to have a mean corresponding to that specified in `hypothesize"




#--------------------------------------------------#
########## chapter 33 multi-linear model ###########
#--------------------------------------------------#
# salary of different department in an university
set.seed(2021)
create_data <- function() {
        df <- tibble(
                ids = 1:100,
                department = rep(c("sociology", "biology", "english", "informatics", "statistics"), 20),
                bases = rep(c(40000, 50000, 60000, 70000, 80000), 20) * runif(100, .9, 1.1),
                experience = floor(runif(100, 0, 10)),
                raises = rep(c(2000, 500, 500, 1700, 500), 20) * runif(100, .9, 1.1)
        )
        
        
        df <- df %>% mutate(
                salary = bases + experience * raises
        )
        df
}
df <- create_data()

# simple linear model
# Model without respect to grouping
m1 <- lm(salary ~ experience, data = df)
summary(m1)
df %>%
        add_predictions(m1) %>%
        ggplot(aes(x = experience, y = salary)) +
        geom_point() +
        geom_line(aes(x = experience, y = pred)) +
        labs(x = "Experience", y = "Predicted Salary") +
        ggtitle("linear model Salary Prediction") +
        scale_colour_discrete("Department")

# Model with varying intercept
# similar as lm(salary ~ experience + department, data = df)
m2 <- lmer(salary ~ experience + (1 | department), data = df)
m2
#broom.mixed::tidy(m2, effects = "fixed")
#broom.mixed::tidy(m2, effects = "ran_vals")
df %>%
        add_predictions(m2) %>%
        ggplot(aes(
                x = experience, y = salary, group = department,
                colour = department
        )) +
        geom_point() +
        geom_line(aes(x = experience, y = pred)) +
        labs(x = "Experience", y = "Predicted Salary") +
        ggtitle("Varying Intercept Salary Prediction") +
        scale_colour_discrete("Department")

# Model with varying slope
# similar as lm(salary ~ experience + experience:department, data = df) or lm(salary ~ experience:department, data = df)
m3 <- lmer(salary ~ experience + (0 + experience | department), data = df)
m3
# broom.mixed::tidy(m3, effects = "fixed")
# broom.mixed::tidy(m3, effects = "ran_vals")
df %>%
        add_predictions(m3) %>%
        ggplot(aes(
                x = experience, y = salary, group = department,
                colour = department
        )) +
        geom_point() +
        geom_line(aes(x = experience, y = pred)) +
        labs(x = "Experience", y = "Predicted Salary") +
        ggtitle("Varying slope Salary Prediction") +
        scale_colour_discrete("Department")

# Model with varying slope and intercept
# similar as lm(salary ~ experience + department + experience:department, data = df)
m4 <- lmer(salary ~ experience + (1 + experience | department), data = df)
m4
# broom.mixed::tidy(m4, effects = "fixed")
# broom.mixed::tidy(m4, effects = "ran_vals")
df %>%
        add_predictions(m4) %>%
        ggplot(aes(
                x = experience, y = salary, group = department,
                colour = department
        )) +
        geom_point() +
        geom_line(aes(x = experience, y = pred)) +
        labs(x = "Experience", y = "Predicted Salary") +
        ggtitle("Varying Intercept and Slopes Salary Prediction") +
        scale_colour_discrete("Department")

# information sharing
complete_pooling <-
        broom::tidy(m1) %>%
        dplyr::select(term, estimate) %>%
        tidyr::pivot_wider(
                names_from = term,
                values_from = estimate
        ) %>%
        dplyr::rename(Intercept = `(Intercept)`, slope = experience) %>%
        dplyr::mutate(pooled = "complete_pool") %>%
        dplyr::select(pooled, Intercept, slope)
partial_pooling <-
        coef(m4)$department %>%
        tibble::rownames_to_column() %>%
        dplyr::rename(level = rowname, Intercept = `(Intercept)`, slope = experience) %>%
        dplyr::mutate(pooled = "partial_pool") %>%
        dplyr::select(pooled, level, Intercept, slope)
no_pool <- df %>%
        dplyr::group_by(department) %>%
        dplyr::group_modify(
                ~ broom::tidy(lm(salary ~ 1 + experience, data = .))
        )
un_pooling <- no_pool %>%
        dplyr::select(department, term, estimate) %>%
        tidyr::pivot_wider(
                names_from = term,
                values_from = estimate
        ) %>%
        dplyr::rename(Intercept = `(Intercept)`, slope = experience) %>%
        dplyr::mutate(pooled = "no_pool") %>%
        dplyr::select(pooled, level = department, Intercept, slope)
complete_pooling
partial_pooling
un_pooling

# viz
un_pooling %>%
        dplyr::bind_rows(partial_pooling) %>%
        ggplot(aes(x = Intercept, y = slope)) +
        purrr::map(
                c(seq(from = 0.1, to = 0.9, by = 0.1)),
                .f = function(level) {
                        stat_ellipse(
                                geom = "polygon", type = "norm",
                                size = 0, alpha = 1 / 10, fill = "gray10",
                                level = level
                        )
                }
        ) +
        geom_point(aes(group = pooled, color = pooled)) +
        geom_line(aes(group = level), size = 1 / 4) +
        # geom_point(data = complete_pooling, size = 4, color = "red") +
        geom_text_repel(
                data = . %>% filter(pooled == "no_pool"),
                aes(label = level)
        ) +
        scale_color_manual(
                name = "Information Pool",
                values = c(
                        "no_pool" = "black",
                        "partial_pool" = "red" # ,
                        # "complete_pool" = "#A65141"
                )
                # labels = c(
                #         "no_pool" = "No",
                #         "partial_pool" = "部分共享" # ,
                #         # "complete_pool" = "完全共享"
                # )
        ) #+




#--------------------------------------------------#
########## chapter 34 generalized linear model ############
#--------------------------------------------------#
# case study, the relationship between pollution level and fish count
df <- read_rds("./demo_data/fish.rds")
df
df %>%
        ggplot(aes(x = pollution_level, y = number_of_fish)) +
        geom_point() +
        geom_smooth(method = lm) +
        labs(
                title = "Number of fish counted under different pollution level",
                x = "Pollution level",
                y = "Number of fish counted"
        )

## simple linear regression
m0 <- lm(number_of_fish ~ pollution_level, data = df)
summary(m0)

## poisson regression
# poisson distribution
df %>%
        ggplot(aes(x = number_of_fish)) +
        geom_histogram() +
        labs(
                title = "number of fishes (Poisson distribution)"
        )
generate_pois <- function(lambda_value) {
        tibble(
                lambda = as.character(lambda_value),
                x = seq(1, 10),
                d = dpois(x = x, lambda = lambda_value)
        )
}
tb <- seq(0.1, 1.8, by = 0.2) %>% map_dfr(generate_pois)
tb %>%
        ggplot(aes(x = x, y = d, color = lambda)) +
        geom_point() +
        geom_line() +
        scale_x_continuous(breaks = seq(1, 10, 1)) +
        theme_bw()

m <- glm(number_of_fish ~ pollution_level,
         family = poisson(link = "log"),
         data = df
)
summary(m)

# fitted value
intercept <- coef(m)[1]
beta <- coef(m)[2]
df %>%
        dplyr::mutate(theory_pred = fitted(m)) %>%
        dplyr::mutate(
                myguess_pred = exp(intercept + beta * pollution_level)
        )
df %>%
        dplyr::mutate(theory_pred = fitted(m)) %>%
        ggplot(aes(x = pollution_level, y = theory_pred)) +
        geom_point()

# prediction
pred <- predict(m, type = "response", se = TRUE) %>% as_tibble()
df_pred <- df %>%
        dplyr::mutate(
                fit = pred$fit,
                se_fit = pred$se.fit
        )
real_df <-
        tibble(
                x = seq(0, 1, length.out = 100),
                y = 4 * exp(-3.2 * x)
        )
df_pred %>%
        ggplot(aes(x = pollution_level, y = number_of_fish)) +
        geom_point() +
        geom_pointrange(aes(
                y = fit,
                ymax = fit + se_fit,
                ymin = fit - se_fit
        ), color = "red") +
        # geom_point(aes(y = fit + se_fit), color = "red") +
        # geom_point(aes(y = fit - se_fit), color = "red") +
        geom_line(data = real_df, aes(x = x, y = y), color = "black") +
        labs(
                title = "Number of fish counted under different pollution level",
                x = "Pollution level",
                y = "Number of fish counted"
        )

# model evaluation
performance::model_performance(m)

## other thoughts
# two equivalent models
set.seed(2021)
d <- tibble(
        x = 1:100,
        y = 4 + 2 * x + rnorm(100)
)
ggplot(d, aes(x, y)) + geom_line()
lm(y ~ x, data = d)
glm(y ~ x,
    family = gaussian(link = "identity"),
    data = d
)

# log link and log transforming
df1 <- df %>% 
        mutate(number_of_fish = ifelse(number_of_fish == 0, 0.0001, number_of_fish))
m1 <- glm(number_of_fish ~ pollution_level,
    family = gaussian(link = "log"),
    data = df1
)
m2 <- glm(log(number_of_fish) ~ pollution_level,
    family = gaussian(link = "identity"),
    data = df1
)
m3 <- glm(number_of_fish ~ pollution_level,
          family = poisson(link = "log"),
          data = df
)

pred1 <- predict(m1, type = "response", se = TRUE) %>% as_tibble()
pred2 <- predict(m2, type = "response", se = TRUE) %>% as_tibble()
pred3 <- predict(m3, type = "response", se = TRUE) %>% as_tibble()

df_pred <- df %>% 
        mutate(pred1 = pred1$fit, pred2 = exp(pred2$fit), pred3 = pred3$fit)
        
df_pred %>% 
        ggplot(aes(x = pollution_level, y = number_of_fish)) +
        geom_point() +
        geom_line(aes(y = pred1, color = "log link")) +
        geom_line(aes(y = pred2, color = "log transforming")) +
        geom_line(aes(y = pred3, color = "possion"))




#----------------------------------------------------#
########## chapter 35 logistic regression ############
#----------------------------------------------------#
gredata <- read_csv("https://raw.githubusercontent.com/perlatex/R_for_Data_Science/master/demo_data/gredata.csv")
gredata

# logistic regression
model_logit <- glm(admit ~ gre,
                   data = gredata,
                   family = binomial(link = "logit")
)
summary(model_logit)

# the log-odds scale
logit_log_odds <- broom::augment_columns(
        model_logit,
        data = gredata,
        type.predict = c("link")
) %>%
        rename(log_odds = .fitted) 
logit_log_odds %>% 
        ggplot(aes(x = gre, y = log_odds)) +
        geom_path(color = "#771C6D", size = 2) +
        labs(title = "Log odds (β)", 
             subtitle = "This is linear!",
             x = NULL,
             y = TeX("$log \\frac{p}{1 - p}$")) +
        theme_minimal() +
        theme(
                plot.title = element_text(face = "bold"),
                axis.title.y = element_text(angle = 90)
        )

# the log scale
logit_odds <- broom::augment_columns(
        model_logit,
        data = gredata,
        type.predict = c("link")
) %>%
        rename(log_odds = .fitted) %>%
        mutate(odds_ratio = exp(log_odds))
logit_odds %>% 
        ggplot(aes(x = gre, y = odds_ratio)) +
        geom_line(color = "#FB9E07", size = 2) +
        labs(title = "Odds (exp(β))", 
             subtitle = "This is curvy, but it's a mathy transformation of a linear value",
             x = NULL,
             y = TeX("$\\frac{p}{1 - p}$")) +
        theme_minimal() +
        theme(
                plot.title = element_text(face = "bold"),
                axis.title.y = element_text(angle = 90)
        )

# the probability scale
logit_probs <- broom::augment_columns(
        model_logit,
        data = gredata,
        type.predict = c("response")
) %>% 
        rename(pred_prob = .fitted)
logit_probs %>% 
        ggplot(aes(x = gre, y = pred_prob)) +
        #geom_point(aes(x = gre, y = admit)) +
        geom_line(color = "#CF4446", size = 2) +
        labs(title = "Predicted probabilities", 
             sutitle = "Plug values of X into ",
             x = "X (value of explanatory variable)",
             y = TeX("\\hat{P(Y)} ")) +
        theme_minimal() +
        theme(plot.title = element_text(face = "bold"))

## predict and fitted value
gredata %>%
        mutate(
                pred = predict(model_logit),
                fitted = fitted(model_logit),
                pred2 = exp(pred) / (1 + exp(pred)),
                pred3 = predict(model_logit, type = "response")
        )

newdata <- tibble(
        gre = c(550, 660, 700, 780)
)
newdata %>% 
        mutate(
                pred_log_odds = predict(model_logit, newdata = newdata, type = "link") # return log_odds
        ) %>% 
        mutate(
                pred_prob = predict(model_logit, newdata = newdata, type = "response"), # return probability
        )




#--------------------------------------------------#
########## chapter 36 ordinal logistic regression ############
#--------------------------------------------------#
tb <- readr::read_rds("./demo_data/cfps.rds")
head(tb)

# assign edu 5,6,7,8 to 5
df <- tb %>%
        dplyr::mutate(
                across(
                        starts_with("edu"),
                        ~ if_else(. %in% c(5, 6, 7, 8), 5, .)
                )
        )
df %>% count(edu)
df %>% count(edu_f)
df %>% count(edu_m)

## father match mother?
df %>%
        dplyr::summarise(
                eq_n = sum(edu_m == edu_f),
                n = n()
        ) %>%
        dplyr::mutate(prop = eq_n / n)

## who has more impact, father or mother?
library(MASS)
df1 <- df %>%
        dplyr::mutate(
                across(c(edu, sex, urban), as.factor),
                across(edu, ~ fct_inseq(., ordered = TRUE))
        )

mod_mass <- polr(edu ~ edu_f + edu_m + sex + num_siblings + urban,
                 data = df1,
                 method = c("logistic")
)
summary(mod_mass)




#--------------------------------------------------#
########## chapter 37 machine learning ############
#--------------------------------------------------#
penguins <- read_csv("./demo_data/penguins.csv") %>%
        janitor::clean_names() %>% 
        drop_na()
penguins %>%
        ggplot(aes(x = bill_length_mm, y = bill_depth_mm, 
                   color = species, shape = species)
        ) +
        geom_point()

split <- penguins %>% 
        mutate(species = as_factor(species)) %>% 
        mutate(species = fct_lump(species, 1)) %>% 
        initial_split()
split
training_data <- training(split)
training_data
testing_data <- testing(split)
testing_data

# modeling
model_logistic <- parsnip::logistic_reg() %>% 
        set_engine("glm") %>% 
        set_mode("classification") %>% 
        fit(species ~ bill_length_mm + bill_depth_mm, data = training_data)
bind_cols(
        predict(model_logistic, new_data = testing_data, type = "class"),
        predict(model_logistic, new_data = testing_data, type = "prob"),
        testing_data
)
predict(model_logistic, new_data = testing_data) %>% 
        bind_cols(testing_data) %>% 
        count(.pred_class, species)

# work flow
split <- penguins %>% 
        rsample::initial_split(prop = 3/4)

training_data <- rsample::training(split)
testing_data  <- rsample::testing(split)




#--------------------------------------------------#
########## chapter 38 Bayes ############
#--------------------------------------------------#
#--------------------------------------------------#
########## chapter 40 case study: nobel prize ############
#--------------------------------------------------#
df <- read_csv("./demo_data/nobel_winners.csv")
df

# check NA
df %>%
        summarise(
                across(everything(), ~ sum(is.na(.)))
        )

# why missing gender？
df %>% count(laureate_type)

# how many prizes within each category
df %>% janitor::tabyl(category)
df %>%
        count(category) %>%
        ggplot(aes(x = fct_reorder(category, n, .desc = TRUE), y = n, fill = category)) +
        geom_col() +
        geom_text(aes(label = n), vjust = -0.25) +
        labs(title = "How many prizes were awarded within each subject", x = "category", y = "count") +
        theme(legend.position = "none")

# library(gganimate)
# df %>%
#         count(category) %>%
#         mutate(category = fct_reorder(category, n)) %>%
#         ggplot(aes(x = category, y = n)) +
#         geom_text(aes(label = n), vjust = -0.25) +
#         geom_col(fill = c("#003f5c", "#444e86", "#955196", "#dd5182", "#ff6e54", "#ffa600")) +
#         labs(title = "How many prizes were awarded within each subject", x = "category", y = "count") +
#         theme(legend.position = "none") +
#         transition_states(category) +
#         shadow_mark(past = TRUE)

# prevent duplicates, charles kuen kao will appear twice due to multiple addresses
df %>%
        dplyr::filter(birth_country == "China") %>%
        dplyr::select(full_name, prize_year, category)
nobel_winners <- df %>%
        mutate_if(is.character, tolower) %>%
        distinct_at(vars(full_name, prize_year, category), .keep_all = TRUE) %>%
        mutate(
                decade = 10 * (prize_year %/% 10),
                prize_age = prize_year - year(birth_date)
        )
nobel_winners %>%
        dplyr::filter(birth_country == "china") %>%
        dplyr::select(full_name, prize_year, category)

# who received Nobel prices more than once?
nobel_winners %>% count(full_name, sort = T)

# how old are those people when they were awarded?
nobel_winners %>%
        mutate(category = fct_reorder(category, prize_age, median, na.rm = TRUE)) %>%
        ggplot(aes(category, prize_age)) +
        geom_point() +
        geom_boxplot() +
        coord_flip()

# gender distribution
nobel_winners %>%
        dplyr::filter(laureate_type == "individual") %>%
        count(category, gender) %>%
        group_by(category) %>%
        mutate(prop = n / sum(n)) -> df
df %>% 
        ggplot(aes(x = category, y = prop, fill = gender)) +
        geom_bar(stat = "identity", position = "dodge") +
        geom_text(aes(label = round(prop, 2)), position = position_dodge(0.9), vjust = -0.25)

nobel_winners %>%
        count(decade,
              category,
              gender = coalesce(gender, laureate_type)
        ) %>%
        group_by(decade, category) %>%
        mutate(percent = n / sum(n)) %>%
        ggplot(aes(decade, n, fill = gender)) +
        geom_col() +
        facet_wrap(~category) +
        theme(legend.position = "bottom")
        labs(
                x = "Decade",
                y = "# of nobel prize winners",
                fill = "Gender",
                title = "Nobel Prize gender distribution over time"
        )

nobel_winners %>%
        select(category, birth_date) %>%
        mutate(year = floor(year(birth_date) / 10) * 10) %>%
        count(category, year) %>%
        dplyr::filter(!is.na(year)) %>%
        ggplot(aes(x = year, y = n)) +
        geom_col() +
        scale_x_continuous(breaks = seq(1810, 1990, 20)) +
        ggrepel::geom_text_repel(aes(label = n)) +
        facet_wrap(vars(category)) +
        #guides(x = guide_axis(n.dodge = 2)) +
        guides(x = guide_axis(check.overlap = TRUE))

# youngest winner
nobel_winners %>%
        dplyr::filter(
                rank(prize_year - year(birth_date)) == 1
        )
        
# which country has the most winners
nobel_winners_clean <- nobel_winners %>%
        mutate_at(
                vars(birth_country, death_country),
                ~ ifelse(str_detect(., "\\("), str_extract(., "(?<=\\().*?(?=\\))"), .)
        ) %>%
        mutate_at(
                vars(birth_country, death_country),
                ~ case_when(
                        . == "scotland" ~ "united kingdom",
                        . == "northern ireland" ~ "united kingdom",
                        str_detect(., "czech") ~ "czechia",
                        str_detect(., "germany") ~ "germany",
                        TRUE ~ .
                )
        ) %>%
        select(full_name, prize_year, category, birth_date, birth_country, gender, organization_name, organization_country, death_country)
topCountries <- nobel_winners_clean %>%
        count(birth_country, sort = TRUE) %>%
        na.omit() %>%
        top_n(8)
df4 <- nobel_winners_clean %>%
        filter(birth_country %in% topCountries$birth_country) %>%
        group_by(birth_country, category, prize_year) %>%
        summarise(prizes = n()) %>%
        mutate(cumPrizes = cumsum(prizes))
df4 %>%
        mutate(prize_year = as.integer(prize_year)) %>%
        ggplot(aes(x = birth_country, y = category, color = birth_country)) +
        geom_point(aes(size = cumPrizes), alpha = 0.6) +
        # geom_text(aes(label = cumPrizes)) +
        scale_size_continuous(range = c(2, 30)) +
        gganimate::transition_reveal(prize_year) +
        labs(
                title = "Top 10 Countries",
                subtitle = "Year: {frame_along}",
                y = "Category"
        ) +
        theme_minimal() +
        theme(
                plot.title = element_text(size = 22),
                axis.title = element_blank()
        ) +
        scale_color_brewer(palette = "RdYlBu") +
        theme(legend.position = "none") +
        theme(plot.margin = margin(5.5, 5.5, 5.5, 5.5))





