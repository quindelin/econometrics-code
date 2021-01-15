#read.table("bwght.txt", na.strings = ".", header = F)

c1.2 <- read.table("bwght.txt", na.strings = ".", header = F)
#read the description file to find column details
attach(c1.2)
faminc <- V1
cigs <- V10
#1

#i
length(cigs)
length(cigs[cigs>0])

#ii
mean(cigs)
#this is not a good measure of average, as only 15% of pregnant women smoke in the first place

#iii
mean(cigs[cigs>0])
#13.67 cigarettes per day is a better average amount of cigarettes smoked by pregnant women
#because it includes the women who actually smoke
fatheredu <- V5

#iv
#average of those who had educational entries, NA entries removed
mean(fatheredu, na.rm = T)
#there are fewer entries because we removed all entries where there was no data

#v
#family income
faminc1 <- faminc*1000
mean(faminc1, na.rm = T)
sd(faminc1, na.rm = T)
detach(c1.2)

#2
q2 <- read.table("jtrain2.txt", header = F, na.string = ".")
attach(q2)
#i
mean(q2$V1 == "1")

#ii
trained_rows <- which(q2$V1 == "1")
trained_earn <- q2[trained_rows,]
mean(trained_earn$V11)*1000
untrained_rows <- which(q2$V1 == "0")
untrained_earn <- q2[untrained_rows,]
mean(untrained_earn$V11)*1000
#the difference is about $1800

#iii
mean(q2[trained_rows,]$V14 == "1")
mean(q2[untrained_rows,]$V14 == "0")
#there is about a 40% difference in the unemployment rates between having recieved
#training and those who did not. It is much better to be trained if you want to be employed

#iv
#the job training program seems to increase earnings and increase employment chances, though
#these are not mutually exclusive. It would be good to do tests on these variables to see
#if they would be statistically significant in supporting the job training program
detach(q2)

#5
q5 <- read.table("ceosal2.txt", header = F, na.string = ".")
attach(q5)
#i
salary <- V1
comten <- V5
mean(salary)
mean(comten)

#ii
ceoten <- V6
length(ceoten[ceoten==0])
max(ceoten)

#iii
lsalary <- V10
model = lm(lsalary~ceoten)
summary(model)
#given one more year as CEO, your logged salary should increase by
#.009724, in real dollars that should be about a $9000 increase
#the model is log(salary) = 6.5055 + .009724(ceoten)
detach(q5)

#6
q6 <- read.table("wage2.txt", header = F, na.string = ".")
attach(q6)

#i
wage <- V1
salary <- V1 * 12
IQ <- V3
mean(salary)
mean(IQ)
sd(IQ)

#ii
model <- lm(wage~IQ)
summary(model)
8.3031*15
#IQ only explains 9.55% of the variation in wage

#iii
log_wage <- log(wage)
model2 <- lm(log_wage~IQ)
summary(model2)
#If the model has one-point increases in IQ mean the same
#percentage increase in wage, then a 15 point increase in IQ
#would mean that wages increase by 15 percent.
detach(q6)


#7
q7 <- read.table("meap93.txt", header = F, na.string ".")
attach(q7)
#i
math_pass <- V9
spending <- V4
model <- lm(math_pass~spending)
summary(model)
squared_spending <- spending^2
quadratic_model <- lm(math_pass~(spending+squared_spending))
summary(quadratic_model)
#I think a diminishing quadratic model would better suit this data,
#and the R-squared from a quadratic improves in both normal and adjusted
#methods of calculating R-squared

#ii

#iii
expend_log <- log(spending)
log_model <- lm(math_pass~expend_log)
summary(log_model)
#equation is 
#math10 = .43965 + .12769(log(expend)) + u
#sample size
length(math_pass)
#R-squared is .00937

#iv

#v
#per-pupil spending will not go practically to the levels necessary
#to get math10 to go above 100. 100 is natural limit of education

#8
#i
x <- runif(500, 0, 10) #uniform dist, 500 obs, 0-10
mean(x)
sd(x)

#ii
u <- rnorm(500, 0, 6) #normal dist, mean 0, standard error 6
mean(u)
#the sample average is close to 0, but not 0
#this is because it is a random sample of the distribution
#not the population distribution
sd(u)
#similarly, this is close to 6 but not six

#iii
y <- 1 + (2*x) + u
test_model <- lm(y~x)
summary(test_model)
#the coefficient is 2.02, so close to 2
#the intercept is .94058, close to 1

#iv
sum(residuals(test_model))
#this is very close to 0
x * sum(residuals(test_model))
#this table will add to 0

#v
sum(u)
#this is a positive number
sum(x*u)
#this is also a positive number
#there are errors in the data; the model is underestimating
#y overall

#vi
a <- runif(500,0,10)
b <- rnorm(500,0,6)
c <- 1 + (2*a) + b
second_model <- lm(c~a)
summary(second_model)
#the coefficients are slightly different,
#within the amount alllowed randomly by white noise

