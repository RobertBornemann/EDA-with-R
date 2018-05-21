data("diamonds")
install.packages('scales')
install.packages('memisc')
install.packages('lattice')
install.packages('MASS')
install.packages('car')
install.packages('reshape')
install.packages('plyr')
install.packages('RColorBrewer', dependencies = TRUE)

library(scales)
library(memisc)
library(lattice)
library(MASS)
library(car)
library(reshape)
library(plyr)
library(ggplot2)
library(gridExtra)
library(RColorBrewer)

# Histogram of diamond prices, faceting by diamond color
# and using cut to color the histogram bars

ggplot(data = diamonds, aes(log(price))) +
  geom_histogram(aes(color = cut)) +
  facet_wrap( ~ color)

# Scatterplot of diamond price vs. table, coloring the points by the cut of
# the diamond.

ggplot(data = diamonds, aes(table, price)) +
  geom_point(aes(color = cut, binwidth = 5))

by(diamonds$table, diamonds$cut, range)

# Scatterplot of the price/carat ratio of diamonds

ggplot(data = diamonds, aes(cut, price/carat)) +
  geom_point(aes(color = color), binwidth=5) +
  facet_wrap(~ clarity)

# Scatterplot of price vs carat weight limiting the x-axis and y-axis to omit the top 1% of values.

ggplot(diamonds, aes(carat, price)) +
  geom_point(aes(color = carat)) +
  xlim(0, quantile(diamonds$carat, .99)) +
  ylim(0, quantile(diamonds$price, .99))

# plotting all the variables available in the dataset to do exploratory data analysis 

ggpairs(diamond_samp,
        lower = list(continuous = wrap("points", shape = I('.'))),
        upper = list(combo = wrap("box", outlier.shape = I('.'))))

# facet histograms of the price variable to compare the effect of log10 on price

plot1 = ggplot(data = diamonds, aes(price)) +
  geom_histogram(binwidth = 500) +
  ggtitle("Price of Diamonds")

plot2 = ggplot(data = diamonds, aes(price)) +
  geom_histogram(bins = 50) +
  scale_x_log10() +
  ggtitle("Log10 Price of Diamonds")

grid.arrange(plot1, plot2)

# after transforming the price variable to log10 to create a less skewed price distribution
# I will also transform carat to volume to bring the variable on a better scale

cuberoot_trans = function() trans_new('cuberoot',
                                      transform = function(x) x^(1/3),
                                      inverse = function(x) x^3)

# Scatterplotting the transformed variables and comparing geom_point position arguments for suitability

testing_position1 = ggplot(diamonds, aes(carat, price)) +
  geom_point(alpha = .5, size = 1, position = 'jitter') +
  scale_x_continuous(trans = cuberoot_trans(), limits = c(0.2,3)) +
  scale_y_continuous(trans = log10_trans(), limits = c(350, 15000))

testing_position2 = ggplot(diamonds, aes(carat, price)) +
  geom_point(alpha = .5, size = 1, position = 'dodge') +
  scale_x_continuous(trans = cuberoot_trans(), limits = c(0.2,3)) +
  scale_y_continuous(trans = log10_trans(), limits = c(350, 15000))

grid.arrange(testing_position1, testing_position2)

# plotting Price (log10) by Cube-Root of Carat and coloring by Clarity
# to highlight how it serves as a good predictor for price

ggplot(aes(x = carat, y = price), data = diamonds) + 
  geom_point(alpha = 0.5, size = 1, position = 'jitter', aes(color = clarity)) +
  scale_color_brewer(type = 'div',
                     guide = guide_legend(title = 'Diamond Clarity', reverse = T,
                                          override.aes = list(alpha = 1, size = 2))) +  
  scale_x_continuous(trans = cuberoot_trans(), limits = c(0.2, 3),
                     breaks = c(0.2, 0.5, 1, 2, 3)) + 
  scale_y_continuous(trans = log10_trans(), limits = c(350, 15000),
                     breaks = c(350, 1000, 5000, 10000, 15000)) +
  ggtitle('Price (log10) by Cube-Root of Carat and Clarity')

# plotting Price (log10) by Cube-Root of Carat and coloring by Cut
# however, Cut doesn't seem to be a great predictor for the diamond price by itself

ggplot(aes(x = carat, y = price), data = diamonds) + 
  geom_point(alpha = 0.5, size = 1, position = 'jitter', aes(color = cut)) +
  scale_color_brewer(type = 'div',
                     guide = guide_legend(title = 'Diamond Cut', reverse = T,
                                          override.aes = list(alpha = 1, size = 2))) +  
  scale_x_continuous(trans = cuberoot_trans(), limits = c(0.2, 3),
                     breaks = c(0.2, 0.5, 1, 2, 3)) + 
  scale_y_continuous(trans = log10_trans(), limits = c(350, 15000),
                     breaks = c(350, 1000, 5000, 10000, 15000)) +
  ggtitle('Price (log10) by Cube-Root of Carat and Cut')

# plotting Price (log10) by Cube-Root of Carat and coloring by Color
# Color is indeed also a strong predictor for the diamond price

ggplot(aes(x = carat, y = price), data = diamonds) + 
  geom_point(alpha = 0.5, size = 1, position = 'jitter', aes(color = color)) +
  scale_color_brewer(type = 'div',
                     guide = guide_legend(title = 'Diamond Color', reverse = FALSE,
                                          override.aes = list(alpha = 1, size = 2))) +  
  scale_x_continuous(trans = cuberoot_trans(), limits = c(0.2, 3),
                     breaks = c(0.2, 0.5, 1, 2, 3)) + 
  scale_y_continuous(trans = log10_trans(), limits = c(350, 15000),
                     breaks = c(350, 1000, 5000, 10000, 15000)) +
  ggtitle('Price (log10) by Cube-Root of Carat and Color')

# finally building a linear model assuming that price increases exponentially with carat due to the rarity of the object
# i am using log10 of price and the cubic root of carat to build the linear model
# applying a forward selection approach adding all the other predictors to the model

model1 = lm(data = diamonds, I(log10(price)) ~ I(carat^(1/3)))
model2 = update(model1, ~ . + carat)
model3 = update(model2, ~ . + clarity)
model4 = update(model3, ~ . + color)
model5 = update(model4, ~ . + cut)

mtable(model1, model2, model3, model4, model5)

# conclusion: the model explains pretty much all the variance in price for diamonds assuming a perfect world
# in the real world, other external influential factors and sources for variance would most definitely have an effect
# on the quality of the models predictions

# extra note: the ggplot diamonds dataset is and easy and effective way to get started with ggplot


