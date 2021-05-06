#Smallmouth bass data (Data file: wblake) Compute the means and the 
#variances for each of the eight subpopulations in the smallmouth bass data. 
#Draw a graph of average length versus Age and compare with Figure 1.5. 
#Draw a graph of the standard deviations versus age. If the variance function is
#constant, then the plot of standard deviation versus Age should be 
#a null plot. Summarize the information.

#Reading the dataset
library(alr4)
wblake <- wblake

names(wblake)
head(wblake)

#Computing the mean and var of every 8 subpopulations of smallmouth bass
tapply(wblake$Length, wblake$Age, mean)
tapply(wblake$Length, wblake$Age, var)

"Here we can see all the mean and var values of length for every age."

#Creating a scatter plot between Age and Length
plot(wblake$Age, wblake$Length)

"The plot looks exactly the same as the one in the book."

#Creating scatter plot between age and standard deviations of length
age_sd <- data.frame(Age = c(1:8), Var = tapply(wblake$Length, wblake$Age, var))

library(dplyr)
age_sd <- mutate(age_sd, SD = age_sd["Var"]^0.5)

library(ggplot2)
ggplot(data = age_sd, aes(x = Age, y = SD)) +
  geom_line() +
  theme_bw()

"No, the standard deviations are not constant. The upward trend is recognizeable
therefore the constant variance assumption of the linear model is not fulfilled.

The linear regression assumes that for every Age the variance of length is the
same. It is clearly not, it is increasing, a line with a positive slope, not a
horizontal line."