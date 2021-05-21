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
loan1 <- sqldf("SELECT *,
              CASE
              WHEN verified = 'Verified' THEN 'Verified'
              WHEN verified = 'Source Verified' THEN 'Verified'
              WHEN verified = 'Not Verified' THEN 'Not Verified'
              END as IncVeri
              from loan
              ")

loan_main <- sqldf(" SELECT def, amount, term, rate, grade, length, home, income, IncVeri, reason, state, debtIncRat, delinq2yr, inq6mth, openAcc, pubRec, revolRatio totalAcc, totalBal, totalRevLim, accOpen24, bcOpen, bcRatio, totalLim, totalRevBal, totalBclim, totalIlLim
                  from loan1
                  WHERE status = 'Fully Paid' or status = 'Default' or status = 'Charged Off'
                   ")

#Splitting loan_main randomly in train, test and eval
#split(loan_main, sample(c(rep("train", 17328), rep("test", 8664),rep("eval", 8663))))

library(caTools)

set.seed(123)
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
with(train, table(IncVeri, grade))

income
mod5<- lrm(def ~ grade + IncVeri + rate + AmIncRate, data=train)
mod5

mod6<- lrm(def ~ reason, data=train)
mod6

mod6<- lrm(def ~ reason, data=train)
mod6

#mod6<- lrm(amount ~ reason, data=train)
mod8<- glm(def ~ grade + IncVeri + rate + amount, data = train, family = binomial() )
mod8

mod9<- lrm(def ~ AmIncRate, data=train)
mod9

train$AmIncRate<- train$amount/train$income
AIC(mod5)
timeLine <- c(-20 , +20)
plot(train$amount , train$reason)

hist(train$reason)

plot( table( train$reason['house'], train$amount), xlab="amount",ylab="reason")

barplot(train$amount,train$reason )

x<- train$reason["house"].Length()

xtabs(~ train$def + train$reason)

xtabs(~ train$grade + train$def)

mod10<- glm(def ~ ., data = train, family = binomial() )
summary(mod10)

library(gmodels)


data <-CrossTable(train$home, train$def, prop.r = TRUE, prop.c = FALSE, prop.t = FALSE, prop.chisq = FALSE)

data_relationship <- data$prop.row[,2]  

position <- data_relationship / 2
text(x = barplot(data_relationship),labels=names(data_relationship), y = position)


new_train <- train

new_train$reason <- NULL
new_train$state <- NULL
new_train$income_sq <- NULL

mod_full<- glm(def ~ ., data = new_train, family = binomial() )
summary(mod_full)

mod_selected<- glm(def ~ term + grade + rate + home + debtIncRat + delinq2yr + inq6mth + accOpen24 + totalIlLim, data = train, family = binomial() )
summary(mod_selected)

library(caret)
train.control <- trainControl(method = "cv", number = 5)
# Train the model
modelll <- train(def ~ term + grade + rate + home + debtIncRat + delinq2yr + inq6mth + accOpen24 + totalIlLim, data = train, method = "lm",
               trControl = train.control)
# Summarize the results
print(model)
