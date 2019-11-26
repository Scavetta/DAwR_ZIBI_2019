# Intro to R
# Rick Scavetta
# 25.11.2019
# DAwR workshop for ZIBI

# clear workspace
rm(list = ls())

# Load packages
# First, install (but only once!)
library(tidyverse)

# R syntax
n <- log2(8)
n

# The PlantGrowth dataset ----
# A built-in dataset
PlantGrowth

# Explore our data
summary(PlantGrowth)
str(PlantGrowth)
glimpse(PlantGrowth) # dplyr package (part of tidyverse)

# Descriptive statistics ----
# global mean
mean(PlantGrowth$weight)

# group-wise means
# Use shift + ctrl + m to get %>%, aka the "pipe operator"
# Say "... and then ..."
PlantGrowth %>% 
  group_by(group) %>%
  summarise(avg = mean(weight),
            stdev = sd(weight),
            n = n())

# Data Viz ----
# Use ggplot2, part of the tidyverse
# 3 essential layers
# 1 - Data
# 2 - Aesthetics, MAPPING a variable onto a scale/axis (x, y, color, size)
# 3 - Geometry, how the plot actually looks

# A "dotplot"
ggplot(PlantGrowth, aes(group, weight, color = group)) +
  geom_jitter(width = 0.2, alpha = 0.6)

# Boxplot
ggplot(PlantGrowth, aes(x = group, y = weight)) +
  geom_boxplot()

# Q-Q plot to see normality
# for all data together
ggplot(PlantGrowth, aes(sample = weight)) +
  stat_qq() +
  stat_qq_line(color = "red")

# group-wise:
ggplot(PlantGrowth, aes(sample = weight)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  facet_grid(. ~ group)

# Extra example with QQ plots
sam <- 1000
myData <- data.frame(type = rep(c("Positive", 
                                  "Negative", 
                                  "Norm"), 
                                each = sam),
                     x = c(rnorm(sam)^2,
                           log(rnorm(sam)),
                           rnorm(sam)))

ggplot(myData, aes(x = x)) +
  geom_histogram(aes(y = ..density..)) +
  facet_wrap(type ~ ., scales = "free_x", ncol = 1)

ggplot(myData, aes(sample = x)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  facet_wrap(type ~ ., ncol = 1, scale = "free_y")


# Inferential statistics ----
# First, define a linear model:
Plant_lm <- lm(weight ~ group, data = PlantGrowth)

# print the lm to the screen:
Plant_lm
# the 2nd and 3rd coefficients are 
# the differences in the means

# t-tests: Typically, use t.test() but here it's
# actually less convenient
# So here, use:
summary(Plant_lm)
# p-values are in "Pr(>|t|)"

# ANOVA
anova(Plant_lm)

## Alternatives ----
# All pair-wise t-test (aka Tukey's HSD Post-hoc test)
# Basically like the lm() above
Plant_aov <- aov(weight ~ group, data = PlantGrowth)
class(Plant_aov)
# ANOVA: Exactly as above
summary(Plant_aov)

# But now you can do the post-hoc test:
TukeyHSD(Plant_aov)

# So two different ways of doing an ANOVA:
# anova(lm(y ~ x))
# summary(aov(y ~ x))

# How to do just a single two-sample t-test:
sleep 
# y = extra (extra hours of sleep) 
# x = group (the drug given)

t.test(extra ~ group, data = sleep)
# p-value = 0.07939

ggplot(sleep, aes(x = group, y = extra)) +
  geom_jitter(width = 0.2, alpha = 0.6)

# Element 2: Functions ----
# Anything that happens is because of a function
# i.e. Verbs

# Arithmetic operators
34 + 6

# what really happened:
`+`(34, 6)

# BEDMAS - Order of operations
# Brakets, Exponents, Div, Mult, Add, Sub

2 + 3/4 # 2.75
(2 + 3)/4 # 1.25

# Make some simple objects
n <- 34
p <- 6

# Use just like numbers:
n + p

# Generic form of functions
# fun_name(fun_agr1 = ..., fun_arg2 = ...)

# Some features of functions:
# 1 - Call args by name or positional matching
log2(8) # short form, positional matching
log2(x = 8) # short form, named args
log(x = 8, base = 2) # long form, named args
log(base = 2, x = 8) # long form, named args
log(8, 2) # long form, positional matching
log(8, base = 2) # mix, very typical
log(2, x = 8) # Confusing! don't get fancy

# 2 - Call args by partial names
log(8, b = 2) # partial name matching

# 3 - Funs can have 0 to many uncountable args
# 4 - Args may be named or unnamed
ls() # 0 args

# e.g. very common functions - Combine c()
xx <- c(3, 8, 9, 23)
myNames <- c("healthy", "tissue", "quantity")

# seq() - Creates a seq of numbers
foo1 <- seq(from = 1, to = 100, by = 7)
foo1
# same as:
seq(1, 100, 7)

foo2 <- seq(1, n, p)
foo2  

# The colon operator, :, short cur for:
seq(1, 10, 1)
1:10

# Two fundamental classes of math functions:
# 1 - Transformation - same number of output as input
# e.g. log, sqrt, z-score, normalization, arithmetic operators
# 2 - Aggregrations - one value as output
# e.g. mean, sd, n, median, var

foo2 + 100 # Transformation
foo2 + foo2 # Transformation
sum(foo2) + foo2 # Aggregration, followed by Transformation
1:3 + foo2 # Transformation

###### FUNDAMENTAL CONCEPT!!! #######
###### VECTOR RECYCLING #############

1:4 + foo2

# 3 Different types of messages:
# 1 - information
# 2 - "Warning message": Maybe something went wrong
# 3 - "Error message": full stop, something IS wrong

# Calculate a linear transformation of xx
# where slope = 1.12, and y-intercept = -0.4
# e.g. mx + b (m is slope, b is y-intercept)
m <- 1.12
b <- -0.4
m * xx + b

# Element 3: Objects ----
# Anything that exists is an object
# e.g. Nouns

# The most data storage objects

# Vectors: 1-dimensional, homogenous data type

# e.g.
foo1 # 15 elements
foo2 # 6 elements
length(foo2) # how many elements
myNames # 3 elements

# 4 most common used-defined
# Atomic vector types
# Logical: TRUE/FALSE, T/F, 1/0 aka binary, Boolean
# Integers: Whole numbers (-5, -3, 6, 0, 8)
# Double: Real numbers (with decimals) aka float
# Character: aka strings (anything)

# Numeric: Generic for either integer or double

# type of data:
typeof(foo2)
typeof(myNames)


foo3 <- c("Liver", "Brain", "Testes", "Muscle",
          "Intestine", "Heart")
typeof(foo3)

foo4 <- c(TRUE, FALSE, FALSE, TRUE, TRUE, FALSE)
typeof(foo4)

# Hierarchy and Coercion
test <- c(1:10, "bob")
test # type is character

mean(test) # can't do math
# solutions:
# 1) Don't contaminate your numbers
# 2) Coercion with an as.*() function
test <- as.numeric(test)

# now we can do math
typeof(test)
mean(test, na.rm = TRUE) # don't forget to remove NAs in function

# Lists: 1-dimensional, heterogenous data types
# e.g.
typeof(Plant_lm)
length(Plant_lm) # 13 elements

# Plant_lm # a short-cut for...
# print(Plant_lm)

# attributes (meta-data)
attributes(Plant_lm)
# 2 attributes, names and class
# 13 named elements

# Access attributes with the typical accessor functions
names(Plant_lm)

# Any named element can be accessed with $ notation
Plant_lm$coefficients # a 3-element long vector
Plant_lm$residuals # a 30-element long vector
Plant_lm$model # the original data

# The class attribute
class(Plant_lm) # Tells R functions how to handle this object

# e.g.
class(Plant_aov) # aov
summary(Plant_aov) # ANOVA

class(Plant_lm) # lm
summary(Plant_lm) # t-tests

class(PlantGrowth) # data.frame
summary(PlantGrowth) # summarize each column

# Data frames: 2-dimensional, heterogenous data types
# R's version of an excel table
# rows = observations
# columns = variables
# Special class of type list
# where each element is a vector of the SAME length

# e.g.
PlantGrowth

names(PlantGrowth)
PlantGrowth$weight # numeric vector in a data frame

class(PlantGrowth)
typeof(PlantGrowth)

# typical functions:
dim(PlantGrowth) # 30 rows & 2 columns
nrow(PlantGrowth) # 30
ncol(PlantGrowth) # 2


