setwd("C:/Users/tosur/OneDrive/Desktop/POL683/midterm-data")
install.packages("tidyverse") 
library(dplyr) # in tidyverse for merging
install.packages("tidyr")
library(tidyr) # for spread function

####Download data sets#### 
####** poll average data set####
poll.p <- read.csv("presidential_poll_averages_2020.csv")
states.p <- unique(poll.p['state'])
#dim(states.p)
#View(states.p)
#typeof(states.p) #is a list
#class(states.p) #is a dataframe?

####** economic index data set####
econ.p <- read.csv("economic_index.csv")
econ.wide <- econ.p %>% spread(category, current_zscore) 
View(econ.wide)
econ.wide
colname(econ.wide)

econ <- econ.wide %>% select(modeldate, "stock market", spending, manufacturing, jobs, inflation, income, combined) 
# For some reason "stock market" has to be in quote for the command to run
View(econ)  

####Build a new data set with econ index averaged for the past month starting on the 10th day####
## that is T-10 to T-40

####** build vectors with average of past T-10 to T-40 day value####
##stock market
stock.avg.1m <- matrix(nrow=1)
for(i in 1:146) {
  j <- (i+10)*7
  k <- (i+40)*7
  m <- mean(econ$`stock market`[j:k], na.rm = TRUE)
  stock.avg.1m <- rbind(stock.avg.1m, m)
}  
stock.avg.1m <- stock.avg.1m[2:137]

##spending
spending.avg.1m <- matrix(nrow=1)
for(i in 1:146) {
  j <- (i+10)*7
  k <- (i+40)*7
  m <- mean(econ$`spending`[j:k], na.rm = TRUE)
  spending.avg.1m <- rbind(spending.avg.1m, m)
}  
spending.avg.1m <- spending.avg.1m[2:137]

##manufacturing
manufacturing.avg.1m <- matrix(nrow=1)
for(i in 1:146) {
  j <- (i+10)*7
  k <- (i+40)*7
  m <- mean(econ$`manufacturing`[j:k], na.rm = TRUE)
  manufacturing.avg.1m <- rbind(manufacturing.avg.1m, m)
}  
manufacturing.avg.1m <- manufacturing.avg.1m[2:137]

##jobs
jobs.avg.1m <- matrix(nrow=1)
for(i in 1:146) {
  j <- (i+10)*7
  k <- (i+40)*7
  m <- mean(econ$`jobs`[j:k], na.rm = TRUE)
  jobs.avg.1m <- rbind(jobs.avg.1m, m)
}  
jobs.avg.1m <- jobs.avg.1m[2:137]

##inflation
inflation.avg.1m <- matrix(nrow=1)
for(i in 1:146) {
  j <- (i+10)*7
  k <- (i+40)*7
  m <- mean(econ$`inflation`[j:k], na.rm = TRUE)
  inflation.avg.1m <- rbind(inflation.avg.1m, m)
}  
inflation.avg.1m <- inflation.avg.1m[2:137]

##income
income.avg.1m <- matrix(nrow=1)
for(i in 1:146) {
  j <- (i+10)*7
  k <- (i+40)*7
  m <- mean(econ$`income`[j:k], na.rm = TRUE)
  income.avg.1m <- rbind(income.avg.1m, m)
}  
income.avg.1m <- income.avg.1m[2:137]

##combined
combined.avg.1m <- matrix(nrow=1)
for(i in 1:146) {
  j <- (i+10)*7
  k <- (i+40)*7
  m <- mean(econ$`combined`[j:k], na.rm = TRUE)
  combined.avg.1m <- rbind(combined.avg.1m, m)
}  
combined.avg.1m <- combined.avg.1m[2:137]
View(combined.avg.1m)
####** put vectors into a new data frame####
date <- unique(econ$'modeldate')
#View(date)
econ.avg.1m <- data.frame("modeldate" = date[1:136], "stock market" = stock.avg.1m, "spending" = spending.avg.1m, "manufacturing" = manufacturing.avg.1m, "jobs" = jobs.avg.1m, "inflation" = inflation.avg.1m, "income" = income.avg.1m, "combined" = combined.avg.1m)
View(econ.avg.1m)

####Regress to get the B's for the forecast model####
#Let's try to do this for one state (actually, let's do national) and see what happens
#Biden
poll.National.Biden <- poll.p %>% filter(state == "National", candidate_name == "Joseph R. Biden Jr.")
poll.National.Biden.c <- merge(poll.National.Biden[1:136,],econ.avg.1m, by.x = "modeldate", by.y = "modeldate")
#View(poll.National.Biden.c)

lm1 <- lm( pct_estimate ~ stock.market + spending + manufacturing + jobs + inflation + income , poll.National.Biden.c) 
summary(lm1)

poll.WI.Biden <- poll.p %>% filter(state == "Wisconsin", candidate_name == "Joseph R. Biden Jr.")
poll.WI.Biden.c <- merge(poll.WI.Biden[1:136,],econ.avg.1m, by.x = "modeldate", by.y = "modeldate")
lm3 <- lm( pct_estimate ~ stock.market + spending + manufacturing + jobs + inflation + income , poll.WI.Biden.c) 
summary(lm3)

#Trump
poll.National.Trump <- poll.p %>% filter(state == "National", candidate_name == "Donald Trump")
poll.National.Trump.c <- merge(poll.National.Trump[1:136,],econ.avg.1m, by.x = "modeldate", by.y = "modeldate")

lm2 <- lm( pct_estimate ~ stock.market + spending + manufacturing + jobs + inflation + income , poll.National.Trump.c) 
summary(lm2)

####Plug in economic factors for the last 30 days#### 
##lagged economic conditions
lagged.econ <- econ.avg.1m[1,2:7] # extracting the average value of each economic index from T-10 to T-40 from election day

##Biden
beta0.lm1 <- summary(lm1)$coefficients[1,1] # extracting beta_0
coeff.lm1 <- summary(lm1)$coefficients[2:7,1] # extracting other beta's in a form of vector

biden.pop <- (coeff.lm1 %*% t(lagged.econ)) + beta0.lm1
biden.pop

##Trump
beta0.lm2 <- summary(lm2)$coefficients[1,1] # extracting beta_0
coeff.lm2 <- summary(lm2)$coefficients[2:7,1] # extracting other beta's in a form of vector

trump.pop <- (coeff.lm2 %*% t(lagged.econ)) + beta0.lm2
trump.pop