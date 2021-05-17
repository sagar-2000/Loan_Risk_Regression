#for(x in loans$status){
#if (x == "Charged Off" || x == "Default"){
#loans$default <- 1
#} else if (x == "Fully Paid") {
#loans$default <- 0
#} else {
# loans$default <- 2
#}
#}

# Creating a new dataset with the dependant variable "def" 
setwd("~/Downloads")
loan50k <- read.csv("loans.csv")

library(RSQLite)
library(sqldf)

loan <- sqldf("SELECT *,
              CASE
              WHEN status = 'Fully Paid' THEN 0
              WHEN status = 'Default' THEN 1
              WHEN status = 'Charged Off' THEN 1
              END as def
              from loan50k
              ")
loan_main <- sqldf(" SELECT def, amount, term, rate, grade, length, home, income, verified, reason, state, debtIncRat, delinq2yr, inq6mth, openAcc, pubRec, revolRatio totalAcc, totalBal, totalRevLim, accOpen24, bcOpen, bcRatio, totalLim, totalRevBal, totalBclim, totalIlLim
                  from loan
                  WHERE status = 'Fully Paid' or status = 'Default' or status = 'Charged Off'
                   ")

#Splitting loan_main randomly in train, test and eval
#split(loan_main, sample(c(rep("train", 17328), rep("test", 8664),rep("eval", 8663))))

library(caTools)
split = sample.split(loan_main, SplitRatio = 0.5)
train = subset(loan_main, split == TRUE)
nottrain = subset(loan_main, split == FALSE)

split2 = sample.split(nottrain, SplitRatio = 0.5)
test = subset(nottrain, split == TRUE)
eval = subset(nottrain, split == FALSE)


#modeling
library(rms)

lrm(def~grade, data=test)


boxplot(rnorm(1000))
with(train, table(verified, grade))


mod5<- lrm(def ~ grade + verified + rate , data=train)
mod5

















