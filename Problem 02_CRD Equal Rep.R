setwd("E:\\Courses\\STAT-3102 Design of Experiment\\Lab")   #Setting working directory
df <- read.csv("Data_CRD.csv") # Read/open data file
head(df)  #Checking first 6 rows
names(df) #Checking the variable names

df$Treatment <- as.factor(df$Treatment)  ## Define as Factor
str(df)  # Checking the data structure


######################## Analysis of Variance (ANOVA) ################################
r=5 # No. of block
k=6 #No. of Treatments

Grand_total <- sum(df$Yield)   #Grand Total
Grand_total

Grand_mean <- Grand_total/(r*k) #Grand mean
Grand_mean

Treatment_total <- aggregate(Yield ~ Treatment, df, sum)  #Treatment total
Treatment_total

Total_SS <- sum(df$Yield^2)-(Grand_total^2/(r*k)) #Total sum of squares
Total_SS

Treatment_SS <- sum(Treatment_total$Yield^2)/5-(Grand_total^2/(r*k)) #Treatment sum of squares
Treatment_SS

Error_SS <- Total_SS-Treatment_SS #Error sum of squares
Error_SS

Treatment_MS <- Treatment_SS/(k-1) #Treatment mean sum of squares
Treatment_MS

Error_MS <- Error_SS/(r*k-k) #Error mean sum of squares
Error_MS

F_Cal <- Treatment_MS/Error_MS  #Calculated F Value
F_Cal <- round(F_Cal, 3)
F_Cal

F_tab <- qf(p=0.05, df1=k-1, df2=(r*k-k), lower.tail = FALSE) #F Critical Value
F_tab <- round(F_tab, 3)
F_tab
#################### Output in a tabular form  ####################
ANOVA <- data.frame(SV=c("Treatment", "Error"),
                    DF=c((k-1),(r*k-k)),
                    SS=c(Treatment_SS, Error_SS),
                    MS=c(Treatment_MS, Error_MS),
                    F_Cal=c(F_Cal, '--'), F_Tab=c(F_tab,'--')
                    )
ANOVA

############### Comparison treatment means of A & E #############################
#Ho: mu(A)=mu(E)
Treatment_mean <- aggregate(Yield ~ Treatment, df, mean)
Treatment_mean
y_bar_A <- Treatment_mean$Yield[1]
y_bar_E <- Treatment_mean$Yield[5]
se <- sqrt(Error_MS)
se
t_cal <- (y_bar_A-y_bar_E)/(se*sqrt(2/r))
t_cal

t_tab <- qt(p=0.05/2, df=(r*k-k), lower.tail = FALSE)
t_tab
