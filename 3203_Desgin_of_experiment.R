---
title: "Design of Experiment(3203)"
author: "Latifur"
date: "2023-11-15"
output: html_document
---
                                problem-2

```{r}
setwd("E:\\R\\3203-Design of experiment")
library(readxl)
df=read_excel("CRD_pro_2.xlsx")
df
head(df)
df$treat=as.factor(df$treat)
df$treat
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
total_ss=sum(df$yield^2)-(grand_total^2/(r*k))
total_ss
treatment_ss=sum(treatment_total$yield^2)/5-(grand_total^2/(r*k))
treatment_ss
error_ss=total_ss-treatment_ss
error_ss
treatment_ms=treatment_ss/(k-1)
treatment_ms
error_ms=error_ss/((r*k)-k)
error_ms

F_cal=round(treatment_ms/error_ms,2)
F_cal

F_tab=round(qf(0.05,(k-1),(r*k)-k,lower.tail = FALSE),2)
F_tab

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

std_error=sqrt(error_ms)
T_cal=(y_bar_A-y_bar_E)/(std_error*sqrt(2/r))
T_cal
T_tab=qt(0.05/2,(r*k)-k,lower.tail=FALSE)
T_tab

#---null hypothesis is accepted or there is no enough information to reject the 
#--- null hypothesis

```

                              Problem-3
```{r}
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
abs(y_bar_A-y_bar_B)
abs(y_bar_A-y_bar_C)
abs(y_bar_A-y_bar_D)

abs(y_bar_A-y_bar_E)
abs(y_bar_B-y_bar_C)
abs(y_bar_B-y_bar_D)
abs(y_bar_B-y_bar_E)
abs(y_bar_C-y_bar_D)
abs(y_bar_C-y_bar_E)
abs(y_bar_D-y_bar_E)

#"Comment: significant difference which here exceeds LSD or CD=83.7705








                                         
```


                                    Problem-4
```{r}
setwd("E:\\R\\3203-Design of experiment")
library(readxl)
df=read_excel("RBD_problem_4.xlsx")
df
head(df)
df$rep=as.factor(df$rep)
df$treat=as.factor(df$treat)
str(df)


grand_total=sum(df$yield)
grand_total
block_total=aggregate(yield~rep,df,sum)
block_total


treatment_total=aggregate(df$yield~df$treat,df,sum)
treatment_total
r=4
k=5
grand_mean=(grand_total/(r*k))
grand_mean
#--------sum squares ----------------------

total_ss=sum(df$yield^2)-(grand_total^2)/(r*k)#---total sum square#
total_ss

block_ss=sum(block_total$yield^2)/(k)-(grand_total^2)/(r*k)
block_ss

treatment_ss=sum(treatment_total$`df$yield`^2)/r-(grand_total^2)/(r*k)
treatment_ss

error_ss=total_ss-block_ss-treatment_ss
error_ss
#===========mean sum squares-------------
block_ms=block_ss/(r-1)
block_ms

treatment_ms=treatment_ss/(k-1)
treatment_ms

error_ms=error_ss/((r-1)*(k-1))
error_ms

#------F_calculated value---------------
F_cal=treatment_ms/error_ms
F_cal

#-------------F_tabluted value--------------------########
F_tab=qf(0.05,k-1,((r-1)*(k-1)),lower.tail = FALSE)
F_tab

#------------anova table------------------------
anova_table=data.frame(
                 s_v=c("Blocks","Treatment","Error"),
                    DF=c(r-1,k-1,((r*k)-(k-1))),
                   SS=c(block_ss,treatment_ss,error_ss),
                 MS=c(block_ms,treatment_ms,error_ms),
                  F_cal=c("  ",F_cal,"  "),
                 F_tab=c("  ",F_tab,"  ")
                      
                 
                 )
anova_table
#comment:Null hypothesis accepted here calculated value is less than the
# tabulated value
########################-----------------------#######################
###### question(2):relative efficiency of RBD TO CRD
#----------------FORMULA--------------
#--------------RE=Ecr/Erb---=(r-1)B+r(k-1)Erb/(rk-1)Erb--------

treatment_df=k-1
error_df=((k-1)*(r-1))
error_df
total_df=r*k-1

RE=block_ss+((treatment_df+error_df)*error_ms)/((total_df)*error_ms)
RE

#---------------3(a)---------------------------
#------------null:Ho:mu(A)=mu(c)
treatment_mean=aggregate(df$yield~df$treat,df,mean)
treatment_mean

y_bar_A=treatment_mean$`df$yield`[1]
y_bar_A
y_bar_C=treatment_mean$`df$yield`[3]
std_error=sqrt(error_ms)

T_cal=(y_bar_A-y_bar_C)/(std_error*sqrt(2/r))
T_cal

#T_tabulated
T_tab=qt(0.05/2,error_df,lower.tail = FALSE)
T_tab
#comment: There is no enough information to reject the null hypothesis.

#----comparison between B and E-----------
##--Ho:mu(B)=mu(E)
##--H1: is not equal
y_bar_B=treatment_mean$`df$yield`[2]
y_bar_E=treatment_mean$`df$yield`[5]

# f statistic
F_cal2=(y_bar_B-y_bar_E)/(std_error*(sqrt(2/r)))
F_cal2
T_tab=qt(0.05/2,error_df,lower.tail = FALSE)
T_tab

#---comment: null hypothesis is accepted.

#confidence interval for B and E
#lower limit
TT=(y_bar_B-y_bar_E)
lower_ste=(std_error*(sqrt(2/r)))*qt(0.975,12)
lower_ste
lower_lim=TT-lower_ste
lower_lim

#upper limit
upper_ste=(std_error*(sqrt(2/r)))*qt(0.25,12,lower.tail = FALSE)
upper_ste
upper_limit=TT+upper_ste
upper_limit





```
                               Problem-5
```{r}
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
std=sqrt(error_ms)
y_bar=(std)/sqrt(r)
y_bar

#----pairwise comparison-------------
x_bar_A=treatment_mean$`df$yiled`[1]
x_bar_B=treatment_mean$`df$yiled`[2]
x_bar_C=treatment_mean$`df$yiled`[3]
x_bar_D=treatment_mean$`df$yiled`[4]
x_bar_E=treatment_mean$`df$yiled`[5]

#========difference =============








```
                                PROBLEM-6
```{r}
setwd("E:\\R\\3203-Design of experiment")
library(readxl)
df=read_excel("RBD_Prob-6.xlsx")
df
View(df)
head(df)
df$rep=as.factor(df$rep)
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

error_ss=total_ss-total_ss_block_variety
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
F_cal_1=variety_ms/error_ms
F_cal_1

F_interact=interaction_ms/error_ms

#-------------F_tabulated values------------------
F_tab_vari=qf(0.05,k-1,error_df,lower.tail = FALSE)
F_tab_vari

F_tab_interact=qf(0.05,(r-1)*(k-1),error_df,lower.tail = FALSE)
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
treatment_mean=aggregate(df$yiled~df$variety,df,mean)
treatment_mean

y_bar_v1=treatment_mean$`df$yiled`[1]
y_bar_v1

y_bar_v2=treatment_mean$`df$yiled`[3]
y_bar_v2

t_statistic=(y_bar_v1-y_bar_v2)/std_error
t_statistic

#------t_tabulated value--------------

t_tab=qt(0.05/2,error_df,lower.tail = FALSE)
t_tab

#-------Comment:The calculated value is grater than the tabluated value
#---------then null hypothesis is rejected at 5% level of significance.



```

                             PROBLEM-7
```{r}
setwd("E:\\R\\3203-Design of experiment")
library(readxl)
df=read_excel("Prob_7_LSD.xlsx")
df
head(df)
df$Rows=as.factor(df$Rows)
df$Column=as.factor(df$Column)
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











```

