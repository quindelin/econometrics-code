q4<-read.table("wage2.txt",na.strings=".",header = F)
attach(q4)
iq<-V3
educ<-V5
lwage<-V17
model<-lm(lwage~educ+iq)
summary(model)
View(q4)

