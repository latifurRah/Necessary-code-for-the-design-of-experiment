#data import from excel file

library(readxl)
CRD_pro_2 <- read_excel("E:/R/CRD_pro_2.xlsx")
View(CRD_pro_2)
model=lm(CRD_pro_2$yield~CRD_pro_2$treat)
model

ano=anova(model)
ano
#here the null hypothesis is not rejected.
plot(model,which = 2,main = "CRD_PROBLEM-2",col="red")

