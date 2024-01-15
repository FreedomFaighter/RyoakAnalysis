#Ryoak analysis
library(ggplot2)
require(gdata)
require(stats)
require(fitdistrplus)
source('secant.r')
industrialsales <- read.xls("spreadsheet.xlsx")
industrialsales.trim <- industrialsales[complete.cases(industrialsales[,"Price.Per.SF"]),]
industrialsales.trim <- industrialsales.trim[industrialsales.trim$Price.Per.SF >= 10,]
k = function(x)
{
  sum(industrialsales.trim$Price.Per.SF^x * log(industrialsales.trim$Price.Per.Land.SF))/sum(industrialsales.trim$Price.Per.SF)-1/length(industrialsales.trim$Price.Per.SF)*sum(log(industrialsales.trim$Price.Per.SF))-1/x
}

firstMoment = sum(industrialsales.trim$Price.Per.SF)/length(industrialsales.trim$Price.Per.SF)
secondMoment = sum(industrialsales.trim$Price.Per.SF^2) / length(industrialsales.trim$Price.Per.SF)
mu=log(firstMoment^2/sqrt(secondMoment))
sd2 = log(secondMoment/firstMoment^2)

fit.industrial <- fitdist(industrialsales.trim$Price.Per.SF, "weibull")
domain <- c(1:max(industrialsales.trim$Price.Per.SF))
range <- dweibull(domain, shape=fit.industrial$estimate[1],scale=fit.industrial$estimate[2])
df <- data.frame(x = domain, y = range)

dfTest <- df
dfTest$y = lapply(dfTest$x, k)

alpha=.2
c=qweibull(c(alpha/2,1-alpha/2),shape = k1, scale = lambdaHat)
df2 = data.frame(x=round(c[1]):round(c[2]),y=df$y[round(c[1]):round(c[2])])
ggplot(industrialsales.trim, aes(Price.Per.SF, ..density..)) +
  geom_histogram(binwidth = 15, colour="green", center = 5) +
  geom_line(data=df, mapping=aes(x,y), colour="red") +
  geom_area(data=df2, mapping=aes(x,y),alpha=0.2,colour="blue") +
  ggtitle(toString(c("Probability of Price Per Square Feet in a Sample of ", toString(length(industrialsales.trim$Price.Per.SF)), " Industrial Assessments"))) +
  labs(x="Price Per Square Feet", y="Probability")

