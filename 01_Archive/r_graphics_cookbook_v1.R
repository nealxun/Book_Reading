# objective: notes of R graphics cookbook

# housekeeping
rm(list = ls())

# load necessary packages
library(tidyverse) # data wranglings
library(gcookbook)
library(patchwork)
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




#------------------------------------------------------------------------------#
########## chapter 3 bar graphs ###########
#------------------------------------------------------------------------------#
# basic bar graph
ggplot(pg_mean, aes(x = group, y = weight)) +
        geom_col(fill = "lightblue", colour = "black")

# x variable is continuous
ggplot(BOD, aes(x = Time, y = demand)) +
        geom_col()
# x variable is discrete
ggplot(BOD, aes(x = factor(Time), y = demand)) +
        geom_col()

# grouping bars together
ggplot(cabbage_exp, aes(x = Date, y = Weight, fill = Cultivar)) +
        geom_col(position = "dodge", colour = "black") +
        scale_fill_brewer(palette = "Pastel1")

ce_missing <- cabbage_exp %>% 
        head(nrow(cabbage_exp) - 1)
ggplot(ce_missing, aes(x = Date, y = Weight, fill = Cultivar)) +
        geom_col(position = "dodge", colour = "black") +
        scale_fill_brewer(palette = "Pastel1")
ce_na <- cabbage_exp %>% 
        mutate(Weight = ifelse(Cultivar == "c52" & Date == "d21", NA, Weight))
ggplot(ce_na, aes(x = Date, y = Weight, fill = Cultivar)) +
        geom_col(position = "dodge", colour = "black") +
        scale_fill_brewer(palette = "Pastel1")

# bar graph of counts
ggplot(diamonds, aes(x = cut)) +
        geom_bar(stat = "count")

# bar graph colors
upc <- uspopchange %>%
        arrange(desc(Change)) %>%
        slice(1:10)
ggplot(upc, aes(x = reorder(Abb, Change), y = Change, fill = Region)) +
        geom_col(color = "black") +
        #scale_fill_brewer(palette = "Set1") + 
        scale_fill_manual(values = c("#669933", "#FFCC66")) +
        labs(x = "State")

# color negative and positive bars differently
climate_sub <- climate %>%
        filter(Source == "Berkeley" & Year >= 1900) %>%
        mutate(pos = Anomaly10y >= 0)
ggplot(climate_sub, aes(x = Year, y = Anomaly10y, fill = pos)) +
        #geom_col()
        geom_col(position = "identity", color = "black", size = 0.25) +
        scale_fill_manual(values = c("#CCEEFF", "#FFDDDD"), guide = "none") +
        labs(title = "It seems that the world is getting warmer")

# bar width
ggplot(pg_mean, aes(x = group, y = weight)) +
        geom_col()
ggplot(pg_mean, aes(x = group, y = weight)) +
        geom_col(width = 0.5)
ggplot(pg_mean, aes(x = group, y = weight)) +
        geom_col(width = 1)

# add space between bars
p1 <- ggplot(cabbage_exp, aes(x = Date, y = Weight, fill = Cultivar)) +
        geom_col(width = 0.5, position = position_dodge(0.5))
p2 <- ggplot(cabbage_exp, aes(x = Date, y = Weight, fill = Cultivar)) +
        geom_col(width = 0.5, position = position_dodge(0.8))
p3 <- ggplot(cabbage_exp, aes(x = Date, y = Weight, fill = Cultivar)) +
        geom_col(width = 0.5, position = position_dodge(0.2))
p1 / p2 / p3

# stacked bar graph
ggplot(cabbage_exp, aes(x = Date, y = Weight, fill = Cultivar)) +
        geom_col(position = position_stack(reverse = TRUE), color = "black") + # reverse the stacking order
        guides(fill = guide_legend(reverse = TRUE)) + # reverse the order of legend
        scale_fill_brewer(palette = "Pastel1")

# proportional stacked bar graph
ggplot(cabbage_exp, aes(x = Date, y = Weight, fill = Cultivar)) +
        geom_col(colour = "black", position = "fill") +
        scale_y_continuous(labels = scales::percent) +
        scale_fill_brewer(palette = "Pastel1")

# adding labels
# Below the top
ggplot(cabbage_exp, aes(x = interaction(Date, Cultivar), y = Weight)) +
        geom_col() +
        geom_text(aes(label = Weight), vjust = 1.5, colour = "white")

# Above the top
ggplot(cabbage_exp, aes(x = interaction(Date, Cultivar), y = Weight)) +
        geom_col() +
        geom_text(aes(label = Weight), vjust = -0.2)

# Adjust y limits to be a little higher
ggplot(cabbage_exp, aes(x = interaction(Date, Cultivar), y = Weight)) +
        geom_col() +
        geom_text(aes(label = Weight), vjust = -0.2) +
        ylim(0, max(cabbage_exp$Weight) * 1.2)

# for grouped bar graph
ggplot(cabbage_exp, aes(x = Date, y = Weight, fill = Cultivar)) +
        geom_col(position = "dodge") +
        geom_text(
                aes(label = Weight),
                colour = "white", size = 3,
                vjust = 1.5, position = position_dodge(.9)
        )

# for stacked bar graph
ce <- cabbage_exp %>%
        arrange(Date, rev(Cultivar)) %>% 
        group_by(Date) %>%
        # Calculate y position, placing it in the middle
        mutate(label_y = cumsum(Weight) - 0.5 * Weight)
ggplot(ce, aes(x = Date, y = Weight, fill = Cultivar)) +
        geom_col(colour = "black") +
        geom_text(aes(y = label_y, label = paste(format(Weight, nsmall = 2), "kg")), size = 4) +
        scale_fill_brewer(palette = "Pastel1")

# Cleveland dot plot
# batting average is calculated by dividing hits by at bats
tophit <- tophitters2001[1:25, ] # Take the top 25 from the tophitters data set
ggplot(tophit, aes(x = avg, y = reorder(name, avg))) + # default is bottom up, ascending order
        geom_segment(aes(yend = name), xend = 0, colour = "grey50") +
        geom_point(aes(color = lg), size = 3) +  # Use a larger dot
        scale_colour_brewer(palette = "Set1", limits = c("NL", "AL"), guide = "none") +
        facet_grid(lg ~ ., scales = "free_y", space = "free_y") +
        labs(title = "Batting averages of MLB hitters in 2001", 
             x = "Batting Average", y = "Player") +
        theme_bw() +
        theme(
                panel.grid.major.y = element_blank()
        )




#------------------------------------------------------------------------------#
########## chapter 4 line graphs ###########
#------------------------------------------------------------------------------#
# basic line graph
# continuous x variable
ggplot(BOD, aes(x = Time, y = demand)) +
        geom_line() +
        expand_limits(y = 0)

# discrete x variable, no entry for x = 6
BOD1 <- BOD %>% 
        # Make a copy of the data
        mutate(Time = factor(Time))
ggplot(BOD1, aes(x = Time, y = demand, group = 1)) +
        geom_line() +
        expand_limits(y = 0)

# adding points
ggplot(BOD, aes(x = Time, y = demand)) +
        geom_line() +
        geom_point() +
        expand_limits(y = 0)

ggplot(worldpop, aes(x = Year, y = Population)) +
        geom_line() +
        geom_point()
# Same with a log y-axis
ggplot(worldpop, aes(x = Year, y = Population)) +
        geom_line() +
        geom_point() +
        scale_y_log10() # per 1 unit increase in y axis is 10 times more than the previous value

# multiple lines
ggplot(tg, aes(x = dose, y = length, colour = supp)) +
        geom_line() +
        geom_point()
tg %>% 
        #filter(supp == "OJ") %>% 
ggplot(aes(x = factor(dose), y = length, colour = supp, group = supp)) +
        geom_line() +
        geom_point(aes(fill = supp), size = 4, shape = 21, alpha = 0.5)

# dodging to avoid overlapping points
ggplot(tg, aes(x = factor(dose), y = length, colour = supp, group = supp)) +
        geom_line(position = position_dodge(0.2)) +           # Dodge lines by 0.2
        geom_point(position = position_dodge(0.2), size = 4) +  # Dodge points by 0.2
        scale_color_brewer(palette = "Set2")

# changing line appearance
# If both lines have the same properties, you need to specify a variable to use for grouping
ggplot(tg, aes(x = dose, y = length)) +
        geom_line(colour = "darkgreen", size = 1.5, linetype = "dashed")
ggplot(tg, aes(x = dose, y = length, group = supp)) +
        geom_line(colour = "darkgreen", size = 1.5, linetype = "dashed")

# changing point appearance
ggplot(BOD, aes(x = Time, y = demand)) +
        geom_line() +
        geom_point(size = 4, shape = 21, fill = "white")

# shaded area
sunspotyear <- data.frame(
        Year     = as.numeric(time(sunspot.year)),
        Sunspots = as.numeric(sunspot.year)
)
ggplot(sunspotyear, aes(x = Year, y = Sunspots)) +
        geom_area(colour = "black", fill = "blue", alpha = .2)
ggplot(sunspotyear, aes(x = Year, y = Sunspots)) +
        geom_area(fill = "blue", alpha = .2) +
        geom_line()

# stacked area graph
ggplot(uspopage, aes(x = Year, y = Thousands, fill = AgeGroup)) +
        geom_area(colour = "black", size = .2, alpha = .4) +
        scale_fill_brewer(palette = "Blues")

# stacked area graph
ggplot(uspopage, aes(x = Year, y = Thousands, fill = AgeGroup)) +
        geom_area(position = "fill", colour = "black", size = .2, alpha = .4) +
        scale_fill_brewer(palette = "Blues") +
        scale_y_continuous(labels = scales::percent)

# confidence region
climate_mod <- climate %>%
        filter(Source == "Berkeley") %>%
        select(Year, Anomaly10y, Unc10y)
ggplot(climate_mod, aes(x = Year, y = Anomaly10y)) +
        geom_ribbon(aes(ymin = Anomaly10y - Unc10y, ymax = Anomaly10y + Unc10y), alpha = 0.2) +
        geom_line()




#------------------------------------------------------------------------------#
########## chapter 5 scatter plots ###########
#------------------------------------------------------------------------------#

