research <- read.table("rdchem.txt")
attach(research)
sales <- V2
sales_sq <- V6
rdintens <- V4
salesbil <- sales/1000
salesbil_sq <- sales_sq/(1000^2)
model1 <- lm(rdintens ~ sales + sales_sq)
model2 <- lm(rdintens ~ sales)
model3 <- lm(rdintens ~ salesbil + salesbil_sq)
anova(model2,model1)
summary(model1)
summary(model3)
max(sales)

detach(research)
wage2 <- read.table("wage2.txt")
attach(wage2)
lwage <- V17
educ <- V5
exper <- V6
tenure <- V7
married <- V9
black <- V10
south <- V11
urban <- V12

model5 <-lm(lwage ~ educ + exper + tenure + married + black + south + urban)
summary(model5)
model6 <- lm(lwage ~ I(educ*black) + exper + tenure + married + black + south + urban)
summary(model6)

nonblack <- 1 - black
single <- 1 - married
length(black)

lm(lwage ~ educ + exper + tenure + south + urban + I(married * black) + I(married*nonblack) + I(single*black) + I(single*nonblack)) %>% summary()
