#Ryoak analysis
library(ggplot2)
salescomps <- read.csv("salescomps.csv")
salescomps.trim <- salescomps[complete.cases(salescomps[,"Price.Per.SF"]),]
firstMoment = sum(salescomps.trim$Price.Per.SF)/length(salescomps.trim$Price.Per.SF)
secondMoment = sum(salescomps.trim$Price.Per.SF^2) / length(salescomps.trim$Price.Per.SF)
mu=log(firstMoment^2/sqrt(secondMoment))
sd2 = var(salescomps.trim$Price.Per.SF)
domain <- c(1:max(salescomps.trim$Price.Per.SF))
range <- dlnorm(domain, meanlog=mu, sdlog=sqrt(sd2))
df <- data.frame(x = domain, y = range)
alpha=.2
c=qlnorm(c(alpha/2,1-alpha/2),meanlog=mu,sdlog=sqrt(sd2))
df2 = data.frame(x=round(c[1]):round(c[2]),y=df[round(c[1]):round(c[2]),"y"])
ggplot(salescomps.trim, aes(Price.Per.SF, ..density..)) +
  geom_histogram(binwidth = 20, colour="green") +
  geom_line(data=df, mapping=aes(x,y), colour="red") +
  geom_area(data=df2, mapping=aes(x,y),alpha=0.2,colour="blue") +
  ggtitle("Probability of Price Per Square Feet in a Sample of Twenty-Five Industrial Assessments") +
  labs(x="Price Per Square Feet", y="Probability")

