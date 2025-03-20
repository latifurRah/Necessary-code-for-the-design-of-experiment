library(readxl)
CRD_prob_3 <- read_excel("CRD_prob_3.xlsx")
View(CRD_prob_3)
model=lm(CRD_prob_3$Yield~CRD_prob_3$Treat)
model
ano=anova(model)
ano
library(agricolae)
#===========lsd test

out <- LSD.test(model,"Treat")
out
#stargraph
# Variation range: max and min
plot(out)
#endgraph
# Old version LSD.test()
df<-df.residual(model)
MSerror<-deviance(model)/df
out <- with(CRD_prob_3,LSD.test(Yield,Treat,df,MSerror))
#stargraph
# Variation interquartil range: Q75 and Q25
plot(out,variation="IQR")
#endgraph
out<-LSD.test(model,"Treat",p.adj="hommel",console=TRUE)
plot(out,variation="SD") # variation standard deviation
