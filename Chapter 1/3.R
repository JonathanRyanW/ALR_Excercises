# (Data file: Mitchell)
# The data give average soil temperature in degrees C at 20 cm depth in Mitchell,
# Nebraska for 17 years beginning January 1976

# 1.3.1 Summarize the information in the graph about the dependence of 
# soil temperature on month number.

library(alr4)
mitchell <- Mitchell

names(mitchell)
head(mitchell)

#Creating a line plot of Temp and Month
library(ggplot2)
ggplot(mitchell, aes(x = Month, y = Temp)) +
  geom_line() +
  theme_bw()

"The data is highly seasonal!"

#Plotting for only the first 12 months
library(dplyr)
ggplot(filter(mitchell, Month <= 12), aes(x = Month, y = Temp)) +
  geom_line() +
  theme_bw()

"We can see that the average temperature rises steadily form January to June.
The temperature peaked at June and began to decrease all the way until December.
It is very cold in Mitchell, Nebraska. Between October and Feb, the temperature
is below zero degrees celcius."

#Creating a scatter plot of Temp and Month
ggplot(mitchell, aes(x = Month, y = Temp)) +
  geom_point() +
  theme_bw()

"OMG nothing can be seen here. Except the fact that if a linear regression line
is attempted to fit this data, it will almost certainly be a horizontal line at
Temp = around 10."

#Creating a linear regression model to fit the data
mitchell_linear <- lm(Temp ~ Month, data = mitchell)
summary(mitchell_linear)

#Plotting the linear regression line into the scatter plot of Temp and Month
ggplot(mitchell, aes(x = Month, y = Temp)) +
  geom_point() +
  geom_abline(intercept = 9.27211, slope = 0.01025, color = "red") +
  theme_bw()

"As expected, a very similar line to a horizontal line at Temp = 10. Can the
slightly positive slope of the line be evidence for global warming at Mitchell,
Nebraska? The avg temperature does increase after all."

# 1.3.2 The data used to draw Figure 1.12 are in the file Mitchell. Redraw 
# the graph, but this time make the length of the horizontal axis at 
# least 4 times the length of the vertical axis. Repeat Problem 1.3.1.

ggplot(mitchell, aes(x = Month, y = Temp)) +
  geom_line() +
  theme_bw()

"I saved the file as a png named Average Temparature of Mitchell, Nebraska.png
with a width of 1200 and height of 313. the file can be seen in
./output/Average Temparature of Mitchell, Nebraska.png"
