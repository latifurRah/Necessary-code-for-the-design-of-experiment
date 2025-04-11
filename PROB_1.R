setwd("E:\\R\\lab_exam(3203)")
library(readxl)
df=read_excel("PROBLEM_1.xlsx")
df
head(df)
df$Treat=as.factor(df$Treat)

str(df)

grand_total=sum(df$Yield)
grand_total

block_total=aggregate(df$Yield~df$Block,df,sum)
block_total

treatment_total=aggregate(df$Yield~df$Treat,df,sum)
treatment_total

block_variety=aggregate(df$Yield~df$Block+df$Treat,df,sum)
block_variety

#----------sum of squares-----------------

r=6
k=4
n=2*(r*k)
total_ss=sum(df$Yield^2)-((grand_total^2)/(n))
total_ss

block_ss=sum((block_total$`df$Yield`^2)/k)-((grand_total^2)/(n))
block_ss

treatment_ss=sum((treatment_total$`df$Yield`^2)/r)-((grand_total^2)/(n))
treatment_ss

block_variety_ss=sum((block_variety$`df$Yield`^2)/(r*k))-((grand_total^2)/(n))
block_variety_ss

interact_ss=block_variety_ss-block_ss-treatment_ss
interact

error_ss=total_ss-block_ss-treatment_ss
error_ss


#---------mean sum of squares----------------
b_ms=block_ss/(r-1)
b_ms

e_df=(n-1)-((r-1)*(k-1))-(15)
e_df
t_ms=treatment_ss/(k-1)

e_ms=error_ss/e_df

F_cal=t_ms/e_ms
F_cal

F_tab=qf(0.05,k-1,e_df,lower.tail = FALSE)
F_tab


#------------C and D mean compare

treatment_mean=aggregate(df$Yield~df$Treat,df,mean)
treatment_mean

meanv3=sum(treatment_mean$`df$Yield`[13:18])
meanv3

meanv4=sum(treatment_mean$`df$Yield`[19:24])
meanv4
dif=meanv3-meanv4
s=sqrt(abs(e_ms))
t_cal=dif/(s*sqrt(2/r))
t_cal

t_tab=qt(.025,e_df,lower.tail = FALSE)
