setwd("E:\\R\\3203-Design of experiment")
library(readxl)
df=read_excel("RBD_problem_4.xlsx")
df
View(df)
head(df)
df$block=as.factor(df$block)
# df$treat=as.factor(df$treat)
df$treat=as.factor(df$treat)
str(df)


grand_total=sum(df$yield)
grand_total
block_total=aggregate(yield~block,df,sum)
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
                  F_cal=c("__",F_cal,"__"),
                 F_tab=c("__",F_tab,"__")
                      
                 
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

RE=(block_ss+((treatment_df+error_df)*error_ms))/(total_df*error_ms)
RE

#comment: so block factor has been effective slightly in reducing error 
# variation 


#---------------3(a)---------------------------
#------------null:Ho:mu(A)=mu(c)
treatment_mean=aggregate(df$yield~df$treat,df,mean)
treatment_mean

y_bar_A=treatment_mean$`df$yield`[1]
y_bar_A
y_bar_C=treatment_mean$`df$yield`[3]
s=sqrt(error_ms)

T_cal=(y_bar_A-y_bar_C)/(s*sqrt(2/r))
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
F_cal2=(y_bar_B-y_bar_E)/(s*(sqrt(2/r)))
F_cal2
T_tab=qt(0.05/2,error_df,lower.tail = FALSE)
T_tab

#---comment: null hypothesis is accepted.

##########--------------4(D)------------------###############
#confidence interval for B and E
#lower limit
dif=(y_bar_B-y_bar_E)
lower_ste=(s*(sqrt(2/r)))*qt(0.975,12)
lower_ste
lower_lim=dif-lower_ste
lower_lim

#upper limit
upper_ste=(s*(sqrt(2/r)))*qt(0.25,12,lower.tail = FALSE)
upper_ste
upper_limit=dif+upper_ste
upper_limit




