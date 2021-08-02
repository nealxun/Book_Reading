# objective: efficient r programming book reading

# preparation
rm(list = ls())
# obtain the current source file work directory
wd <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(wd)

# load necessary packages
library(tidyverse)
library(reshape)
library(lubridate)
library(openxlsx)



#------------------------------------------------------#
########## chapter 7 efficient optimisation ############
#------------------------------------------------------#
# prerequisites
library(microbenchmark)
library(ggplot2movies)
library(profvis)
library(Rcpp)

# a simple c++ function
cppFunction('
  double add_cpp(double x, double y) {
    double value = x + y;
    return value;
  }
')
add_cpp(1, 2)

# vectors and loops
# r function
mean_r = function(x) {
        m = 0
        n = length(x)
        for(i in 1:n)
                m = m + x[i] / n
        m
}
# c++ function
sourceCpp("mean_cpp.cpp")
# test
x = rnorm(1e4)
z = microbenchmark(
        mean(x), mean_r(x), mean_cpp(x),
        times = 1000
)
z



#------------------------------------------------------#
########## chapter 8 efficient hardware ############
#------------------------------------------------------#
# prerequisites
library(benchmarkme)
library(pryr)

# RAM
head(USArrests)
d <- dist(USArrests)
pryr::object_size(USArrests)
pryr::object_size(d)
# see how much RAM is left
benchmarkme::get_ram()

# see wheter your system is 32-bit or 64-bit (will return 8)
.Machine$sizeof.pointer

# CPU performance testing
res <- benchmark_std()
plot(res)
upload_results(res)




#------------------------------------------------------#
########## chapter 9 efficient collaboration############
#------------------------------------------------------#
library(tidyverse)
library(lubridate)

# code style (press cmd + shift + A)
if(!exists("x")){
  x=c(3,5)
  y=x[2]}




#------------------------------------------------------#
########## chapter 10 efficient learning   ############
#------------------------------------------------------#
library(swirl)

# use R's internal help
??optim
help.search(pattern = "optimisation|optimization", fields = c("title", "concept"), package = "stats")
apropos("optim")

# vignette
browseVignettes(package = "benchmarkme") # open result in a browser
vignette(package = "benchmarkme")
browseVignettes() # see all the vignettes
# see source code of the vignette
v = vignette(package = "benchmarkme")
edit(v)
# directory of the vignette files
# /Library/Frameworks/R.framework/Versions/3.6/Resources/library/dplyr/doc

# help on functions
?optim
example(optim)

# read source code
getFunction("NCOL")
View(optim)

# interactive learning
swirl()

# create a reproducible example
set.seed(1)
example_df = data.frame(x = rnorm(4), y = rnorm(4), z = sample(LETTERS, 4))
dput(example_df)






