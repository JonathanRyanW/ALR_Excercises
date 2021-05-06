"Problem:
United Nations (Data file: UN11) The data in the file UN11 contains 
several variables, including ppgdp, the gross national product per person 
in U.S. dollars, and fertility, the birth rate per 1000 females, both from 
the year 2009. The data are for 199 localities, mostly UN member countries, 
but also other areas such as Hong Kong that are not independent countries.
The data were collected from United Nations (2011). We will study 
the dependence of fertility on ppgdp."

#The solution

#1.1.1 Identify the predictor and the response.

"The predictor is of course the ppgdp and the response is fertility."

#1.1.2 Draw the scatterplot of fertility on the vertical axis versus 
#ppgdp on the horizontal axis and summarize the information in this 
#graph. Does a straight-line mean function seem to be plausible for 
#a summary of this graph?

library(alr4)
un <- UN11
plot(un$ppgdp, un$fertility)

"No. There is no way a straight line can fit these data well. It looks like
the relationship is negative exponential."

#1.1.3 Draw the scatterplot of log(fertility) versus log(ppgdp) using 
#natural logarithms. Does the simple linear regression model seem 
#plausible for a summary of this graph? If you use a different base of 
#logarithms, the shape of the graph won’t change, but the values on 
#the axes will change"

plot(log(un$ppgdp), log(un$fertility))

"Now simple linear regression looks plausible to use. However looking at the
plot i think a curve might fit the data better than a line."