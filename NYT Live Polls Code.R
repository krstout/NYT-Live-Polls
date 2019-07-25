rm(list = ls())

setwd("C:\\Users\\Kevin\\Dropbox")

d <- read.csv("NYT Live Polls.csv")

d$margin <- d$Democrat-d$Republican

d$DFavorable <- d$Dem.Fav-d$Dem.Unfav
d$RFavorable <- d$Rep.Fav-d$Rep.Unfav

d$GenderGap <- d$Female+d$Male

model <- lm(d$margin ~ Female, data = d)
summary(model)

#model2 <- lm(GenderGap ~ Days.Before.Election, data = d)
#summary(model2)

#plot(d$Days.Before.Election, d$GenderGap)

#model3 <- lm(margin ~ Days.Before.Election, data = d)
#summary(model3)

#plot(d$Days.Before.Election, d$margin)

d$miss <- abs(d$margin - d$Actual.D.Margin)
summary(d$miss)

three.week <- d[d$Days.Before.Election < 22,]
summary(three.week$miss)

model2 <- lm(miss ~ Days.Before.Election, data = three.week)
summary(model2)

three.week$response.rate <- (three.week$Responses/three.week$Calls)*100
summary(three.week$response.rate)

model3 <- lm(miss ~ Days.Before.Election + response.rate, data = three.week)
summary(model3) # Higher response, lower error

model4 <- lm(miss ~ Days.Before.Election + Int.Cell, data = three.week)
summary(model4) # More cell interviews, lower error

model5 <- lm(miss ~ Days.Before.Election + Int.Landline, data = three.week)
summary(model4) # More landline interviews, higher error

