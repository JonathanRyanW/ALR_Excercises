# Old Faithful (Data file: oldfaith)
#The data file gives information about eruptions of Old Faithful Geyser during
#October 1980.

# Variables:
# Duration in seconds of the current eruption
# Interval, the time in minutes to the next eruption

# Apart from missing data for the period from midnight to 6 a.m., this is a
# complete record of eruptions for that month

# Old Faithful Geyser is an important tourist attraction, with up to several 
# thousand people watching it erupt on pleasant summer days. The park 
# service uses data like these to obtain a prediction equation for the time 
# to the next eruption.

# Draw the relevant summary graph for predicting interval from duration 
# and summarize your results

#Reading the dataset
library(alr4)

names(oldfaith)
head(oldfaith)
dim(oldfaith)

#Creating scatter plot of Interval and Duration
library(ggplot2)
ggplot(oldfaith, aes(x = Duration, y = Interval)) +
  geom_point() +
  theme_bw()

"From this scatter plot we can see that a linear model will be able to fit the
data quiet well. It is very clear that as Duration increases, Interval also
increases.

Some thing to note here is that it seems that most of the eruptions are either
very long eruptions or very short eruptions. It is rare to see a moderately
long eruption. As can be seen from the apparent emptyness in the centre of the
scatter plot."

#Building the linear regression model to predict Interval based on Duration
oldfaith_linear <- lm(Interval ~ Duration, data = oldfaith)
summary(oldfaith_linear)

"Both intercept and Duration does affect Interval as shown by the virtually zero
p-value for both of them. Our model's Adjusted R-squared is 0.8022 which is very
good. These findings gave evidence that Duration can be used pretty accurately
to predict Interval.

However it is important to note that the coefficient for Duration is close to 0,
that is 0.1768. It means that we are very sure that Duration does affect Interval
However the effect might not be that big. The increase of interval from 1
increase in Duration is only 0.1768.

The fact that the coef is positive means that the higher the duration the longer
it takes to wait for the next eruption. Makes perfect sense."

#Plotting the regression line into the scatter plot
ggplot(oldfaith, aes(x = Duration, y = Interval)) +
  geom_point() +
  geom_abline(intercept = 33.987808, slope = 0.176863,
              color = "red", size = 1.5) +
  theme_bw()