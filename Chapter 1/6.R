#Professor ratings (Data file: Rateprof)
# In the website and online forum RateMyProfessors.com, students rate and comment
# on their instructors. Launched in 1999, the site includes millions of ratings on
# thousands of instructors. The data file includes the summaries of the ratings of 
# 364 instructors at a large campus in the Midwest (Bleske-Rechek and 
# Fritsch, 2011).
# 
# Each instructor included in the data had at least 10 ratings over a several year
# period. Students provided ratings of 1–5 on various measures:
# quality
# helpfulness
# clarity
# easiness of instructor’s courses
# rater's nterest in the subject matter covered in the instructor’s courses.

# The data file provides the averages of these five ratings, and these are shown 
# in the scatterplot matrix in Figure 1.13.
# Provide a brief description of the relationships between the five ratings.

library(alr4)
rateprof <- Rateprof

head(rateprof)
dim(rateprof)
names(rateprof)

any(is.na(rateprof))
any(is.null(rateprof))

"There are 17 variables. The last 10 are the summary of the 5 measurements. That
is the mean and sd of the measurements. The first 5 are information about the
instructor.

There are 366 instructor rated in the website. There is no NA nor NULL value in
the dataset.

There are a total of 366 * 17 = 6222 values we want to analyze."

#Creating scatter plots of the mean of measurements
plot(rateprof[,8:12])

"We can see that quality, helpfulness and clarity is very-very closely related.

Those 3 variables however has a weaker connection with easiness and raterInterest.
However, even though the relation is weaker we can still see that they still have
a positive correlation with easiness and raterInterest.

It's interesting that people who are more interested in a subject tend to rate
their instructor higher than those who are less interested in that subject. The
causality can be both ways. It could be that because they like the subject, they
percieved the instructor to give higher quality information, be more helpful and
providing better clarity. It could also be that because they like the subject
the subject feels a little bit easier, and as such they report it to be easy.

The causality can be reversed though. It could be that because of high quality
information, amazing clarity, and instructors' helpfulness the students become
more immersed into the subject and as such like it more and percieve it as
easier."

#Creating a correlation plot of the measurements
library(corrplot)

corrplot(cor(rateprof[,8:12]), method = "color")

"The result is the same with our previous narative, the correlation between the
first 3 variables is very high. Even though it is weaker, the first 3 measurement
are still positively correlated with easiness and raterInterest.

We can see here that easiness and raterinterest have very weak correlation. This
can also be seen in the previous plot, the scatter plot between easiness and
raterInterest is very rectangle like, giving evidence that they dont influence
each other."

#How many rated instructor are males or females?
table(rateprof$gender)
table(rateprof$gender) / 366

"There are 207 (56.55%) male rated instructor and 159 (43.44%) female
counterparts. It might be interesting to know the gender of the students who
rate these instructors. There might be some tendencies for a gender to rate
the opposite or the same gender. But we can't know that as our dataset does not
contain the gender of the rater."

#Creating histogram for every measurement
library(ggplot2)

for (i in c(8:12)) {
  name <- paste("Histogram ", names(rateprof)[i], ".png", sep = "")
  png(filename = name)
  
  print(
    ggplot(rateprof, aes(rateprof[,i])) +
      geom_histogram(bins = 15, color = "black", fill = "white") +
      ylab("Number of Instructors") +
      xlab("Rating of Measurement") +
      ggtitle(paste("Histogram of ", names(rateprof)[i], sep = "")) +
      theme_bw()
  )
  
  dev.off()
}

rm(i, name)

"Looking at those histograms, it seems that the ratings are not normally
distributed except for easiness and raterInterest. We shall do a Shapiro-Wilk
test to determine whether any of them are in fact normally distributed"

#Performing Shapiro-Wilk test for normality on the mean of measurements
measurement <- names(rateprof[,8:12])
shapiro_p_value <- c()

for (i in c(8:12)) {
  shapiro_p_value <- append(shapiro_p_value,
                            shapiro.test(rateprof[,i])$p.value)
}

shapiro_normal <- shapiro_p_value > 0.05

rate_prof_shapiro_test <- data.frame(cbind(measurement,
                                           shapiro_p_value,
                                           shapiro_normal))

rm(i, measurement, shapiro_p_value, shapiro_normal)

"The resulting p-values shows that none of the variables are normally distributed
except for raterInterest. I am surprised that easiness is not normal despite
looking like it."

#Are there any difference between the ratings of female and male instructors?
Measurement <- names(rateprof[,8:12])
result <- c()
for (i in c(8:12)){
  result <- append(result,tapply(rateprof[,i], rateprof$gender, mean))
}

result <- matrix(result, nrow = 5, byrow = TRUE)

gender_mean <- cbind(Measurement, result)
gender_mean <- data.frame(gender_mean)

library(dplyr)
gender_mean <- rename(gender_mean, Female = V2, Male = V3)

rm(i, Measurement, result)

"We can see that there are no huge difference in the ratings for female and
male instructors. However male instructors are rated higher on all measurement
except for easiness. Interesting, maybe male instructors are more ruthless when
they give assignments and as such students percieve their courses to be harder."

"Now we are going to do the same thing but with sd instead of mean"

Measurement <- names(rateprof[,8:12])
result <- c()
for (i in c(13:17)){
  result <- append(result,tapply(rateprof[,i], rateprof$gender, mean))
}

result <- matrix(result, nrow = 5, byrow = TRUE)

gender_sd <- cbind(Measurement, result)
gender_sd <- data.frame(gender_sd)

library(dplyr)
gender_sd <- rename(gender_sd, Female = V2, Male = V3)

rm(i, Measurement, result)

"There is no considerable difference in variance in any of the measurement.
However the rating for the male instructor are more varied than the ratings
for their female counterpart for every measurement except easiness. But the
difference is so small."

#Are there differences in the number of rate given by students towards the
#female or male instructors?

tapply(rateprof$numRaters, rateprof$gender, sum)
tapply(rateprof$numRaters, rateprof$gender, mean)
tapply(rateprof$numRaters, rateprof$gender, sd)

"Okay so male instructors are getting more ratings than their female counterpart.
This number has to be adjusted to the number of male and female instructors
because we already know that there are in fact more male instructors than females.

The mean does not differ much, female instructors get 27 ratings on average
while their male counterpart gets 29 ratings. The number of ratings for male
instructors varied more than the number of ratings for female instructors."

#Creating a correlation plot between all the variables
corrplot(cor(rateprof[,c(2:4, 8:12)]), method = "color")

"We already did this with the mean of measurement variables, this time we will
do it for every variable excluding the Standard Deviation variables.

Ok so the more years an instructor has been teaching the more ratings he/she
gets. This makes perfect sense as as time goes by more and more students are
taught and as such more and more opportunities for them to be rated by their
students.

We also find that the longer that instructor has been teaching, the more courses
he/she teaches in. This also makes perfect sense as the more experienced the
instructor, the greater his/her responsibility to the faculty and as such he/she
will have to teach in more courses.

We also find that the more courses an instructor teach, the more rates he/she
gets. This also makes sense as the more courses an instructor teaches, the more
exposure he/she gets with the students and as such the more likely he/she is to
get rated by students.

The interesting thing is that these experience-like variables are negatively
correlated with their ratings. This is surprising because i expect that an 
experienced instructor will be able to hone their teaching skills and able to
provide better information and clarity to the students.

It's interesting that the more years an instructor has taught, the more students
percieved their courses to be hard. Maybe because they are old, they are smart,
they give harder tests and assignments."

"I am not going to continue the EDA as i have many other things i have to do.
I have already done what the book wants me to do and more. This dataset is
interesting but i simply can't spend too much time here. This is it from me."