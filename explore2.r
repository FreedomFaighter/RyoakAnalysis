#Ryoak analysis
library(ggplot2)
require(gdata)
industrialsales <- read.xls("spreadsheet.xlsx")
industrialsales.trim <- industrialsales[complete.cases(industrialsales[,"Price.Per.SF"]),]

firstMoment = sum(industrialsales.trim$Price.Per.SF)/length(industrialsales.trim$Price.Per.SF)
secondMoment = sum(industrialsales.trim$Price.Per.SF^2) / length(industrialsales.trim$Price.Per.SF)
mu=log(firstMoment^2/sqrt(secondMoment))
sd2 = log(secondMoment/firstMoment^2)
domain <- c(1:max(industrialsales.trim$Price.Per.SF))
range <- dlnorm(domain, meanlog=mu, sdlog=sqrt(sd2))
df <- data.frame(x = domain, y = range)
alpha=.2
c=qlnorm(c(alpha/2,1-alpha/2),meanlog=mu,sdlog=sqrt(sd2))
df2 = data.frame(x=round(c[1]):round(c[2]),y=df[round(c[1]):round(c[2]),"y"])
ggplot(industrialsales.trim, aes(Price.Per.SF, ..density..)) +
  geom_histogram(binwidth = 15, colour="green", center = 7) +
  geom_line(data=df, mapping=aes(x,y), colour="red") +
  geom_area(data=df2, mapping=aes(x,y),alpha=0.2,colour="blue") +
  ggtitle(toString(c("Probability of Price Per Square Feet in a Sample of ", toString(length(industrialsales.trim$Price.Per.SF)), " Industrial Assessments"))) +
  labs(x="Price Per Square Feet", y="Probability")

