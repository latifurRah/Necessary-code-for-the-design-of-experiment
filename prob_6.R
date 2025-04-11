setwd("E:\\R\\3203-Design of experiment")
library(readxl)
df=read_excel("RBD_Prob-6.xlsx")
df
# View(df)
head(df)
# df$rep=as.factor(df$rep)
df$variety=as.factor((df$variety))
str(df)
grand_total=sum(df$yiled)
grand_total

block_total=aggregate(df$yiled~df$rep,df,sum)
block_total

variety_total=aggregate(df$yiled~df$variety,df,sum)
variety_total

#---------sum of squares --------------
r=4
k=3
n=3*(r*k)
total_ss=sum(df$yiled^2)-(grand_total^2)/(n)
total_ss


# variet=data.frame(
#   #block wise variety yield total
#   
# )
vari_yield=aggregate(df$yiled~df$rep+df$variety,df,sum)
vari_yield


#------total ss of block * variety table---------

total_ss_block_variety=(sum(vari_yield$`df$yiled`^2))/k-(grand_total^2)/(n)
total_ss_block_variety

block_ss=sum(block_total$`df$yiled`^2)/(k*k)-(grand_total^2)/(n)
block_ss

variety_ss=sum(variety_total$`df$yiled`^2)/(r*k)-(grand_total^2)/(n)
variety_ss

#block variety interaction ss

interact_ss=total_ss_block_variety-block_ss-variety_ss
interact_ss

error_ss=total_ss-block_ss-variety_ss-interact_ss
error_ss  
#############-------------mean sum squares---------------########################
block_ms=block_ss/(r-1)
block_ms
variety_ms=variety_ss/(k-1)
variety_ms

interaction_ms=interact_ss/((r-1)*(k-1))
interaction_ms

error_df=(n-1)-((r-1)+(k-1)+((r-1)*(k-1)))
error_df

error_ms=error_ss/error_df
error_ms
#---------F_cal_____---
F_cal_1=round((variety_ms/error_ms),2)
F_cal_1

F_interact=round((interaction_ms/error_ms),2)
F_interact

#-------------F_tabulated values------------------
F_tab_vari=round(qf(0.05,k-1,error_df,lower.tail = FALSE),2)
F_tab_vari

F_tab_interact=round(qf(0.05,(r-1)*(k-1),error_df,lower.tail = FALSE),2)
F_tab_interact
#------ anova table-----------------
anov=data.frame(
                s_v=c("block","variety","interaction","error"),
                DF=c(r-1,k-1,(r-1)*(k-1),error_df),
                SS=c(block_ss,variety_ss,interact_ss,error_ss),
                MS=c(block_ms,variety_ms,interaction_ms,error_ms),
                F_cal=c("  ",F_cal_1,F_interact,"  "),
                F_tab=c("  ",F_tab_vari,F_tab_interact,"  ")
                
                )

anov

#--comment:null hypothesis is rejected .all varieties are not equal.
#=========for interaction===============
#----comment:null hypothesis is accepted that's means interaction between block and
#----variety is insignificant

#==-----------(2)----------------
#error_ms
std_error=sqrt(error_ms)*sqrt(2/(r*3))
std_error
#null hypothesis-Ho:mu(1)=mu(3)
treatment_mean=aggregate(df$yiled~df$rep+df$variety,df,mean)
treatment_mean

meanv1=sum(head(treatment_mean$`df$yiled`,3))
meanv1

meanv2=sum(head(treatment_mean$`df$yiled`[4:6]))
meanv2

meanv3=sum(head(treatment_mean$`df$yiled`[7:9]))
meanv3

# y_bar_v1=treatment_mean$`df$yiled`[1]
# y_bar_v1
# 
# y_bar_v2=treatment_mean$`df$yiled`[3]
# y_bar_v2

t_statistic=abs((meanv1-meanv3)/std_error)
t_statistic

#------t_tabulated value--------------
t_tab=qt(0.05/2,error_df,lower.tail = FALSE)
t_tab

#-------Comment:The calculated value is grater than the tabluated value
#---------then null hypothesis is rejected at 5% level of significance.


