# Water runoff in the Sierras (Data file: water)

# Can Southern California’s water supply in future years be predicted from past
# data? One factor affecting water availability is stream runoff.

# If runoff could be predicted, engineers, planners, and policy makers could
# do their jobs more efficiently.

# The data file contains 43 years’ worth of precipitation measurements taken 
# at six sites in the Sierra Nevada mountains (labeled APMAM, APSAB, 
# APSLAKE, OPBPC, OPRC, and OPSLAKE) and stream runoff volume at a                                             
# site near Bishop, California, labeled BSAAM.

# Draw the scatterplot matrix for these data and summarize the information
# available from these plots

#Reading the dataset
library(alr4)

head(water)
names(water)
dim(water)

sapply(water, class)

#Creating a scatter plot matrix
plot(water[,2:8])

"Looking at these plots it seems that there are 2 distinct groups of variables
that are intercorrelated.

The first group:
APMAM
APSAB
APSLAKE

The second group:
OPBPC
OPRC
OPSLAKE
BSAAM

Each of the member of the group have very strong correlation with each other. We
can see it in a correlation plot here."

#Creating a correlation plot
library(corrplot)

correlations <- cor(water[,2:8])
corrplot(correlations, method = "color")

"This plots summarized what i just said after looking at the scatter plots.
There are actually 2 groups that is interrelated."

#Building a linear model with all variables as a predictor
water_linear <- lm(BSAAM ~ APMAM+APSAB+APSLAKE+OPBPC+OPRC+OPSLAKE,
                   data = water)
summary(water_linear)                   

"We can see that the first 3 variable is not influencing BSAAM. This is expected
as we can see the scatter plot of BSAAM and the 3 variables are not linearish.

Our model has Adjusted R-squared:  0.9123. A very good model indeed.

What is surprising is that of the 3 last predictor, only 2 of them are actually
affecting BSAAM, that is, only OPRC and OPSLAKE are affecting BSAAM. Even though
OPBPC has such a high correlation with BSAAM."

cor(water$OPBPC, water$BSAAM) #0.8857478
cor(water[,5:8])

"The correlation is a very stong one, 0.8857. This suggest that OPBPC must have
been credited for the influence or other variable. Maybe OPBPC is also affected
by OPRC and OPSLAKE and as such the high OPBPC and BSAAM is the combined result
of high OPRC and OPSLAKE, which means OPBPC does not affect BSAAM, rather it is
influenced alongside BSAAM!"

#Creating a linear regression model with only OPRC and OPSLAKE as predictor
water_linear_selected <- lm(BSAAM ~ OPRC+OPSLAKE, data = water)
summary(water_linear_selected)
"Our model has an Adjusted R-squared of 0.8967. This model is prefeable as it is
less complex than the previous one while still maintaining a very good predictive
value."

#Creating the residual plots for both model
plot(water_linear$residuals)
plot(water_linear_selected$residuals)

"Both residual plots seems to be non systematic. I can't see a clear trend in
the residuals"

#Creating the histogram for both residuals
hist(water_linear$residuals)
hist(water_linear_selected$residuals)

"The residual of the first model (with all variables as predictor) does not seem
to be normal. It is not symetrical. It looks like it has a positive skew. This
is not a normal distribution.

The histogram for second model on the other hand looks more similar to normal.
Altough looking at the flat peak, i have a little doubt that the curtosis for 
the residuals is near 3 (the curtosis for normally distributed variables)."

#Creating a qqplot of the residuals
residuals <- data.frame(water_linear$residuals,
                        water_linear_selected$residuals)
residuals[,1] <- (residuals[,1] - mean(residuals[,1])) / sd(residuals[,1])
residuals[,2] <- (residuals[,2] - mean(residuals[,2])) / sd(residuals[,2])

ggplot(residuals, mapping = aes(sample = water_linear.residuals)) +
  stat_qq() +
  stat_qq_line() +
  theme_bw()

ggplot(residuals, mapping = aes(sample = water_linear_selected.residuals)) +
  stat_qq() +
  stat_qq_line() +
  theme_bw()

"Looking at both of them, they look like they systematically deviates from the
line. In the first plot the dots looks like they are above the line for low and
high quantiles but below the line for close to 0.5 quantiles (the middle of the
line).

While on the second plot the dots seems to be above the line all the way from
the lowest quantile to the highest quantile. This is a sign that the residuals
are not normally distributed."

#Perfroming Shapiro_Wilk test of normality to both residuals
shapiro.test(water_linear$residuals)
shapiro.test(water_linear_selected$residuals)

mean(water_linear_selected$residuals) #2.210118e-15
"The p-value for the first model is 0.4327, which means that it is in fact not
a normally distributed variable. The second model has p-value = 0.911 which
also mean that it is not normally distributed. This is expected as i can see
that there are so many observations with residuals between -10000 and 0 even
(the 3 long bars side by side) even thoudh the mean of the residuals is 0.

Therefore both of our model's residual is not normally distributed. We need
another model other than linear regression."