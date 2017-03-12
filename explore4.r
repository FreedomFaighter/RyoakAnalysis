require(ggplot2)
require(psych)
require(scales)
require(MASS)
industrialsales <- read.xls("spreadsheet.xlsx")
industrialsales.trim <- industrialsales[complete.cases(industrialsales[,"Price.Per.SF"]),]
industrialsales.trim <- industrialsales.trim[complete.cases(industrialsales.trim[,"Percent.Office"]),]
industrialsales.trim <- industrialsales.trim[complete.cases(industrialsales.trim[,"Condition"]),]

industrialsales.trim$recip.Price.Per.SF <- 1/industrialsales.trim$Price.Per.SF
ggplot(industrialsales.trim, aes(Percent.Office,Price.Per.SF)) +
  geom_point() + 
  stat_smooth(method = "lm") + 
  facet_wrap(~Condition)


industrialsales.average <- industrialsales.trim[industrialsales.trim$Condition=="Average",]

industrialsales.average$OfficeSF <- 

industrialsales.good <- industrialsales.trim[industrialsales.trim$Condition=="Good",]
lm1 <- lm(Price.Per.SF ~ Percent.Office, data = industrialsales.good)
bc <- boxcox(Price.Per.SF ~ Percent.Office, data=industrialsales.good, lambda = seq(-2,2,.01), plotit = FALSE)
which.max(bc$y)
bc$x[215]
industrialsales.good$Price.Per.SF0.05 = industrialsales.good$Price.Per.SF^bc$x[which.max(bc$y)]

ggplot(industrialsales.good, aes(Percent.Office, Price.Per.SF0.05)) +
  geom_point() + 
  stat_smooth(method = "lm")
#industrial.trim finding a lm on the facet condition
industrialsales.trim$Office.SF = industrialsales.trim$Net.Rentable.Area*industrialsales.trim$Percent.Office
industrialsales.trim$Warehouse.SF = industrialsales.trim$Net.Rentable.Area*(1-industrialsales.trim$Percent.Office)

ggplot(industrialsales.trim, aes(Office.SF, Price.Per.SF)) +
  geom_point() +
  stat_smooth(method = "lm") +
  facet_wrap(~Condition)

ggplot(industrialsales.trim, aes(Warehouse.SF, Price.Per.SF)) +
  geom_point() +
  stat_smooth(method = "lm" ) +
  facet_wrap(~Condition)

ggplot(industrialsales.trim, aes(Office.SF, Warehouse.SF)) +
  geom_point() +
  stat_smooth(method = "lm") +
  facet_wrap(~Condition)

industrialsales.good$Office.SF = industrialsales.good$Net.Rentable.Area*industrialsales.good$Percent.Office
industrialsales.good$WarehouseSF = industrialsales.good$Net.Rentable.Area*(1-industrialsales.good$Percent.Office)
industrialsales.good.abovezero = industrialsales.good[industrialsales.good$Office.SF > 0,]

plot(industrialsales.good.abovezero$Office.SF, industrialsales.good.abovezero$WarehouseSF)

plot(industrialsales.good.abovezero$Office.SF, industrialsales.good.abovezero$Improvement.Assessment)
plot(industrialsales.good.abovezero$WarehouseSF, industrialsales.good.abovezero$Improvement.Assessment)
boxplot(industrialsales.good.abovezero$Office.SF)
boxplot(industrialsales.good.abovezero$WarehouseSF)
industrialsales.good.abovezero <- industrialsales.good.abovezero[industrialsales.good.abovezero$Office.SF < 15000,]
industrialsales.good.abovezero <- industrialsales.good.abovezero[industrialsales.good.abovezero$WarehouseSF < 30000,]
lm2 <- lm(Improvement.Assessment ~ Office.SF + WarehouseSF, data=industrialsales.good.abovezero)
summary(lm2)
bc1 <- boxcox(Improvement.Assessment ~ Office.SF + WarehouseSF, data=industrialsales.good.abovezero, lambda = seq(-2,2,0.001), plotit=TRUE)
max(bc1$y)
bc1$x[which.max(bc1$y)]
industrialsales.good.abovezero$Improvement.Assessment.Transform <- industrialsales.good.abovezero$Improvement.Assessment^bc1$x[which.max(bc1$y)]

lm3 <- lm(Improvement.Assessment.Transform ~ Office.SF + WarehouseSF, data=industrialsales.good.abovezero)

summary(lm3)

plot(industrialsales.good.abovezero$Office.SF, industrialsales.good.abovezero$Improvement.Assessment.Transform)
plot(log(industrialsales.good.abovezero$WarehouseSF)^2, industrialsales.good.abovezero$Improvement.Assessment.Transform)
industrialsales.good.abovezero$log.WarehouseSF2 = log(industrialsales.good.abovezero$WarehouseSF)^2
lm3 <- lm(Improvement.Assessment.Transform ~ 0 + Office.SF + log.WarehouseSF2, data=industrialsales.good.abovezero)
summary(lm3)

