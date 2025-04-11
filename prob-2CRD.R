setwd("E:\\R\\3203-Design of experiment")

library(readxl)
df=read_excel("CRD_pro_2.xlsx")
df=read_excel("CRD_pro_2.xlsx")
df
View(df)
head(df)
df$treat=as.factor(df$treat)
# df$treat=as.factor(df$treat)
# df$treat
# checking the structure 
str(df)

#=============analysis of variance===============\\
r=5 
k=6
grand_total=sum(df$yield)
grand_total
grand_mean=grand_total/(r*k)
grand_mean

treatment_total=aggregate(yield~treat,df,sum)
treatment_total
####---------sum of squares----------------

total_ss=sum(df$yield^2)-(grand_total^2/(r*k))
total_ss

treatment_ss=sum(treatment_total$yield^2)/r-(grand_total^2/(r*k))
treatment_ss

error_ss=total_ss-treatment_ss
error_ss
##------------mean sum of squares-------------

treatment_ms=treatment_ss/(k-1)
treatment_ms

error_ms=error_ss/((r*k)-k)
error_ms

F_cal=round(treatment_ms/error_ms,2)
F_cal

F_tab=round(qf(0.05,(k-1),(r*k)-k,lower.tail = FALSE),2)
F_tab

#-------------anova table------------------------
anova=data.frame(source_of_variation=c("Treatment","Error"),
                 DF=c(k-1,(r*k)-k),
                 ss=c(treatment_ss,error_ss),
                 ms=c(treatment_ms,error_ms),
                 F_cal=c(F_cal,"  "),
                 F_tab=c(F_tab,"  ")
                 
                 
                 
                 
                  )
anova


########------------mean comparison--------------#############
#------null hypothesis
#Ho:mu A=mu E
treatment_mean=aggregate(yield~treat,df,mean)
treatment_mean

y_bar_A=treatment_mean$yield[1]
y_bar_E=treatment_mean$yield[5]

s=sqrt(error_ms)

T_cal=(y_bar_A-y_bar_E)/(s*sqrt(2/r))
T_cal
#---------t_tablulated value-------------

T_tab=qt(0.05/2,(r*k)-k,lower.tail=FALSE)
T_tab
t_tab1=qt(0.975,(r*k)-k,lower.tail = TRUE)
t_tab1

#---null hypothesis is accepted or there is no enough information to reject the 
#--- null hypothesis

#---------------confidence interval for A_E------------------------#
dif=y_bar_A-y_bar_E
lower_limit=round(dif-(T_tab*s*sqrt(2/r)),2)
lower_limit

upper_limit=round(dif+(T_tab*s*sqrt(2/r)),2)
upper_limit
#------------CI--------------------
print("lower_limit<=mu(A)-mu(E)<=upper_limit")



