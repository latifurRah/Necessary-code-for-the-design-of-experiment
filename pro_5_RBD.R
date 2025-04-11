setwd("E:\\R\\3203-Design of experiment")
library(readxl)
df=read_excel("RBD-prob-5.xlsx")
df
head(df)
df$rep=as.factor(df$rep)
df$treat=as.factor((df$treat))
str(df)
grand_total=sum(df$yiled)
grand_total

block_total=aggregate(df$yiled~df$rep,df,sum)
block_total

treatment_total=aggregate(df$yiled~df$treat,df,sum)
treatment_total
r=5
k=5
#-----------sum of squares------------------
total_ss=sum(df$yiled^2)-((grand_total^2)/(r*k))
total_ss

block_ss=(sum(block_total$`df$yiled`^2))/k-((grand_total^2)/(r*k))
block_ss

treatment_ss=(sum(treatment_total$`df$yiled`^2)/r)-((grand_total^2)/(r*k))
treatment_ss

error_ss=total_ss-block_ss-treatment_ss
error_ss
#----------------mean sum of squares--------------------------
block_ms=block_ss/(r-1)
block_ms

treatment_ms=treatment_ss/(k-1)
treatment_ms

error_ms=error_ss/((r-1)*(k-1))
error_ms

F_cal=treatment_ms/error_ms
F_cal
#--------F_tabulated value------------
F_tab=round(qf(0.05,k-1,((r-1)*(k-1)),lower.tail=FALSE))
F_tab
anov=data.frame(
  s_v=c("Block","treatment","error"),
  DF=c(r-1,k-1,(r-1)*(k-1)),
  SS=c(block_ss,treatment_ss,error_ss),
  MS=c(block_ms,treatment_ms,error_ms),
  F_cal=c("  ",F_cal,"  "),
  F_tab=c("  ",F_tab,"  ")
)
anov
#null hypothesis is rejected

treatment_mean=aggregate(df$yiled~df$treat,df,mean)
treatment_mean

#----------confidence interval 90% level at significance-------
#-------CI of A AND D---------------
y_bar_A=treatment_mean$`df$yiled`[1]
y_bar_D=treatment_mean$`df$yiled`[4]
dif=y_bar_A-y_bar_D

#------------lower limit----------------
sd_error=sqrt(error_ms)*sqrt(2/r)


T_cal=qt(.95,((r-1)*(k-1)))
lower_limit=dif-T_cal*sd_error
print(lower_limit)
#-------------Upper limit--------------------

T_cal1=qt(0.05,16,lower.tail = FALSE)
Upper_limit=dif+T_cal1*sd_error
print(Upper_limit)

#--------Duncan method for multiple comparison----
#standard error of a treatment mean
s=sqrt(error_ms)
y_bar=(s)/sqrt(r)
y_bar

#----pairwise comparison-------------
x_bar_A=treatment_mean$`df$yiled`[1]
x_bar_B=treatment_mean$`df$yiled`[2]
x_bar_C=treatment_mean$`df$yiled`[3]
x_bar_D=treatment_mean$`df$yiled`[4]
x_bar_E=treatment_mean$`df$yiled`[5]

#========standard error of a treatment mean =  s_y_bar =============
s_y_bar=s/sqrt(r)
s_y_bar

#---------SSR(significant studentized range)-----------







