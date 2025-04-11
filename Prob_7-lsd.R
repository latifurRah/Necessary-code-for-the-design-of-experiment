setwd("E:\\R\\3203-Design of experiment")
library(readxl)
df=read_excel("Prob_7_LSD.xlsx")
df
head(df)
# df$Rows=as.factor(df$Rows)
# df$Column=as.factor(df$Column)

df$Treat=as.factor(df$Treat)
str(df)

grand_total=sum(df$Yield)
grand_total

row_total=aggregate(df$Yield~df$Rows,df,sum)
row_total

column_total=aggregate(df$Yield~df$Column,df,sum)
column_total

treatment_total=aggregate(df$Yield~df$Treat,df,sum)
treatment_total

#----In latin square design rows and columns are equal
r=5

#------------sum of squares--------------------
total_ss=sum(df$Yield^2)-(grand_total^2)/(r*r)
total_ss

row_ss=sum(row_total$`df$Yield`^2)/r-(grand_total^2)/(r*r)
row_ss

column_ss=sum(column_total$`df$Yield`^2)/r-(grand_total^2)/(r*r)
column_ss

treatment_ss=sum(treatment_total$`df$Yield`^2)/r-(grand_total^2)/(r*r)
treatment_ss

error_ss=total_ss-row_ss-column_ss-treatment_ss
error_ss

########--------mean sum squares-------------------######
row_ms=row_ss/(r-1)
row_ms

col_ms=column_ss/(r-1)
col_ms

treat_ms=treatment_ss/(r-1)
treat_ms
error_df=((r*r)-1)-((r-1)+(r-1)+(r-1))

error_ms=error_ss/error_df
error_ms
#---------------F_calculated values--------------
F_row=row_ms/error_ms
F_row

F_treat=treat_ms/error_ms
F_treat

#-----------F-critical value-------------
F_tab_treat=qf(0.05,r-1,error_df,lower.tail = FALSE)
F_tab_treat

#--------------p value-------------
p_value=pf(65.47,r-1,error_df,lower.tail = FALSE)

#-----------------anova table-------------
anova=data.frame(s_v=c("Rows","Columns","Treatments","Error"),
                 DF=c(r-1,r-1,r-1,error_df),
                 SS=c(row_ss,column_ss,treatment_ss,error_ss),
                 MS=c(row_ms,col_ms,treat_ms,error_ms),
                 F_cal=c(F_row,"  ",F_treat,"  "),
                 F_tab=c("  ","  ",F_tab_treat,"  "),
                 p_valu=c("  ","  ",p_value,"  "))
anova
#-------comment:Calculated of f is grater than the critical value.Hence observed
#--value of F is significant and so null hypothesis is rejected.We therefore 
# conclude that the fertilizers are not equally effective.Since B has highest
# yield among all the treatments under comparison ,treatment B is preferred to
# others

##################-----mean comparison of A and D treatment-------###########

#----null hypothesis:Ho:mu(A)=mu(D)
#---alternative hypothesis :is not equal

treatment_mean=aggregate(df$Yield~df$Treat,df,mean)
treatment_mean 

y_bar_A=treatment_mean$`df$Yield`[1]
y_bar_D=treatment_mean$`df$Yield`[4]

dif=(y_bar_A-y_bar_D)
dif
s=sqrt(error_ms)
s
std_error=s*sqrt(2/r)
std_error

T_statistic=dif/std_error
T_statistic

#--------------t_tabulated value--------------------
T_tab=qt(0.05/2,error_df,lower.tail = FALSE)
T_tab

#Comment: calculated value of t is greater than the  critical value.so the null
# hypothesis is rejected at 5% level of significance.thus we conclude that treat
#ments A and D are not equally effective .

#

#---------------LSD-----------TEST------------

LSD=T_tab*s*sqrt(2/r)
LSD
#-------------TREATMENT MEAN--------------------
# treatment_mean

A=y_bar_A=treatment_mean$`df$Yield`[1]
B=y_bar_B=treatment_mean$`df$Yield`[2]
C=y_bar_C=treatment_mean$`df$Yield`[3]
D=y_bar_D=treatment_mean$`df$Yield`[4]
E=y_bar_E=treatment_mean$`df$Yield`[5]

result=sort(c(A,B,C,D,E))
result

####################--------dif---------------#####################
d12=A-B
d13=A-C
d14=A-D
d15=A-E
d23=B-C
d24=B-D
d25=B-E
d34=C-D
d35=C-E
d45=D-E

dif=c(d12,d13,d14,d15,d23,d24,d25,d34,d35,d45)

for(i in dif){
  if(i>LSD){
    print("significant")
    print(i)
  }
  else
    {
      print(paste("insignificant"))
      print(i)
    }
}


