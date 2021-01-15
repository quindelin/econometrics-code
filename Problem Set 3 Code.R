install.packages("multcomp")
library(foreign)
library(multcomp)

voting_data <- read.table("vote1.txt", na.strings = ".", header = F)
attach(voting_data)
voteA<-V4
length(voteA)
lexpendA<-V8
lexpendB<-V9
prtystrA<-V7
model<-lm(voteA~lexpendA+lexpendB+prtystrA)
summary(model)
ltotexpend<-lexpendA+lexpendB
model2<-lm(voteA~lexpendA+ltotexpend+prtystrA)
summary(model2)
detach(voting_data)
wage_data <- read.table("wage2.txt", na.strings = ".", header = F)
attach(wage_data)
lwage <- V17
length(lwage)
educ <- V5
exper <- V6
tenure <- V7
model3<-lm(lwage~educ+exper+tenure)
summary(model3)
vcov(model3)
comb <- exper - tenure
cov(exper,tenure)
detach(wage_data)
fin_data <- read.table("401ksubs.txt", na.strings = ".", header = F)
attach(fin_data)
single_data <- which(fin_data$V6 == 1)
what <- fin_data[single_data,]
View(what)
length(single_data)
inc <- what$V2
age <- what$V5
netffa <- what$V7
model4<-lm(netffa~inc+age)
summary(model4)
model5<-lm(netffa~inc)
summary(model5)
