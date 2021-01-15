loanapp <- read.table("loanapp.txt")
attach(loanapp)
white <- V59
approve <- V50
hrat <- V26
obrat <- V27
loanprc <- V57
unem <- V39
male <- V48
married <- V9
dep <- V10
sch <- V45
cosign <- V35
chist <- V55
pubrec <- V25
mortlat1 <- V53
mortlat2 <- V54
vr <- V44
model <- lm(approve~white)
summary(model)
model2 <- lm(approve~white+hrat+obrat+loanprc+unem+male+married+dep+sch+cosign+chist+pubrec+mortlat1+mortlat2+I(white*obrat))
summary(model2)
model3 <- glm(approve ~ white, family = binomial(link = "logit"))
summary(model3)
model4 <- glm(approve ~ white+hrat+obrat+loanprc+unem+male+married+dep+sch+cosign+chist+pubrec+mortlat1+mortlat2+vr, family = binomial(link = "probit"))
summary(model4)
detach(loanapp)
ceo <- read.table("ceosal1.txt")
attach(ceo)
lsalary <- V11
lsales <- V12
roe <- V4
finance <- V8
consprod <- V9
utility <- V10
model5 <- lm(lsalary ~ lsales + roe + finance + consprod + utility)
summary(model5)



