setwd("E:\\R\\3203-Design of experiment")
library("readxl")

df=read_excel("CRD_prob_3.xlsx")
df
head(df)
str(df)
df$Treat=as.factor(df$Treat)
str(df)
r=6
k=5


grand_total=sum(df$Yield)
grand_total
treatment_total=aggregate(Yield~Treat,df,sum)
treatment_total
total_ss=sum(df$Yield^2)-(grand_total^2)/(r*k)
total_ss
treatment_ss=sum(treatment_total$Yield^2)/r-(grand_total^2)/(r*k)
treatment_ss
error_ss=total_ss-treatment_ss
error_ss

treatment_ms=round(treatment_ss/(k-1),2)
treatment_ms
error_ms=round(error_ss/((r*k)-k),2)
error_ms
F_cal=round((treatment_ms/error_ms),2)
F_cal

F_tab=round(qf(.05,k-1,r*k-1,lower.tail = FALSE),2)
F_tab

anova=data.frame(s_v=c("treatment","Error"),
                 df=c(k-1,(r*k)-k),
                 ss=c(treatment_ss,error_ss),
                 ms=c(treatment_ms,error_ms),
                 F_cal=c(F_cal,"  "),
                 F_tab=c(F_tab,"  ")
                 )
anova
# multiple comparison lsd
std_error=sqrt(error_ms)
T_cal=qt(0.05/2,25,lower.tail = FALSE)
T_cal
LSD=T_cal*std_error*sqrt(2/r)
LSD
treatment_mean=aggregate(Yield~Treat,df,mean)
treatment_mean
y_bar_A=treatment_mean$Yield[1]
y_bar_B=treatment_mean$Yield[2]
y_bar_C=treatment_mean$Yield[3]
y_bar_D=treatment_mean$Yield[4]
y_bar_E=treatment_mean$Yield[5]
#The difference of averages
d1=abs(y_bar_A-y_bar_B)
d2=abs(y_bar_A-y_bar_C)
d3=abs(y_bar_A-y_bar_D)

d4=abs(y_bar_A-y_bar_E)
d5=abs(y_bar_B-y_bar_C)
d6=abs(y_bar_B-y_bar_D)
d7=abs(y_bar_B-y_bar_E)
d8=abs(y_bar_C-y_bar_D)
d9=abs(y_bar_C-y_bar_E)
d10=abs(y_bar_D-y_bar_E)

lsd_set=round(c(d1,d2,d3,d4,d5,d6,d7,d8,d9,d10),2)
lsd_set
for(x in lsd_set){
  if(x>LSD){
    print(paste("Indicates sigificant"))
    print(x)
    
  }
  else{
    print(paste("Insignificant"))
    print(x)
  }
    
}
#"Comment: significant difference which here exceeds LSD or CD=83.7705








