setwd("G:/FDA PROJECT")    #Working Directry
lab_data=read.csv("loans_data_Apply_Linear_ Regression.csv",stringsAsFactors = FALSE)      #loading the data of loans_data_Apply_Linear_ Regression
View(lab_data)       #To View The data
dim(lab_data)             #To Know the Dimensions of the data
colnames(lab_data)           #To Know about the Column Names of the data
#########################
########################
#######################
# Create the function.to get the mode
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
#####################
###################
#################

#Pre-Processing Tecquiniques


#for id
which(lab_data$ID==".") #to find the row numbers which are having "." s   0 rows
which(is.na(lab_data$ID)) #to find the row numbers which are having NA   1 row
#id can not be pridicted so the row is removed
lab_data=lab_data[-which(is.na(lab_data$ID)),]
colSums(is.na(lab_data)) #To check the NA values whether they are present or not in all coloums

#To know categorical or numeric using table() all columns
#By applying Mode or mean We can fill the missing value
class(lab_data$Loan.Length)     #to find data type of the column
lab_data$Amount.Requested=as.numeric(lab_data$Amount.Requested)

####################### Finding the OutLiers ###################
boxplot(lab_data$Amount.Requested) #to find outliers




#for Amount Required

table(lab_data$Amount.Requested)
class(lab_data$Amount.Requested)    #character class due to there is a one character in it called '.'
which(lab_data$Amount.Requested==".") #to find the row numbers which are having "." s   4 rows
which(is.na(lab_data$Amount.Requested)) #to find the row numbers which are having NA   1 row

lab_data$Amount.Requested[lab_data$Amount.Requested=="."] <-NA   #remove or replace '.' with 'NA'
which(is.na(lab_data$Amount.Requested)) #to find the row numbers which are having NA   5 rows

class(lab_data$Amount.Requested)    #now the class of this column is numeric
lab_data$Amount.Requested=as.numeric(lab_data$Amount.Requested)   #changing data to numeric
class(lab_data$Amount.Requested)    #character class due to there is a one character in it called '.'
#checking the skew ness of the to fill the misssing data to get more accuracy
skewness(lab_data$Amount.Requested,na.rm = TRUE)           #here skew is positive so try to reduce it with going function mean
#skew of the coloum is 0.91161
#replacing null values with mean of the data 
lab_data$Amount.Requested[which(is.na(lab_data$Amount.Requested))]=mean(lab_data$Amount.Requested,na.rm = TRUE)

####################### Finding the OutLiers ###################
#library(outliers)
boxplot(lab_data$Amount.Requested) #to find outliers 
#outliers are above 31,000 are outliers
lab_data=lab_data[-(which(lab_data$Amount.Requested>=31000)),]

######################################################
###############################################
#temparary=lab_data          #just for calculation created duplicated
############################################
#####################################################



#Amount funded by investors
table(lab_data$Amount.Funded.By.Investors)
class(lab_data$Amount.Funded.By.Investors)    #character class due to there is a one character in it called '.'
which(lab_data$Amount.Funded.By.Investors==".") #to find the row numbers which are having "." s   4 rows
which(is.na(lab_data$Amount.Funded.By.Investors)) #to find the row numbers which are having NA   1 row

lab_data$Amount.Funded.By.Investors[lab_data$Amount.Funded.By.Investors=="."] <-NA     #. to na value
which(is.na(lab_data$Amount.Funded.By.Investors)) #to find the row numbers which are having NA   5 rows

#now this column is character class so change to numeric
lab_data$Amount.Funded.By.Investors=as.numeric(lab_data$Amount.Funded.By.Investors)
class(lab_data$Amount.Funded.By.Investors)     #now numeric
#NA values filling with mean
lab_data$Amount.Funded.By.Investors[which(is.na(lab_data$Amount.Funded.By.Investors))]=mean(lab_data$Amount.Funded.By.Investors,na.rm = TRUE)
which(is.na(lab_data$Amount.Funded.By.Investors)) #to find the row numbers which are having NA   0 rows


####################### Finding the OutLiers ###################
boxplot(lab_data$Amount.Funded.By.Investors) #to find outliers 
#no outliers




#Intrest rate
table(lab_data$Interest.Rate)
class(lab_data$Interest.Rate)   #character class
lab_data$Interest.Rate <- as.numeric(gsub("[%]", "", lab_data$Interest.Rate))
lab_data$Debt.To.Income.Ratio <- as.numeric(gsub("[%]", "", lab_data$Debt.To.Income.Ratio))
which(lab_data$Interest.Rate==".") #to find the row numbers which are having "." s   0 rows
which(is.na(lab_data$Interest.Rate)) #to find the row numbers which are having NA   0 row
#no  NA values

####################### Finding the OutLiers ###################
boxplot(lab_data$Interest.Rate) #to find outliers 
#outliers are above 24 are outliers are removed
lab_data=lab_data[-(which(lab_data$Interest.Rate>=24)),]




#Loan Length
table(lab_data$Loan.Length)
which(lab_data$Loan.Length==".") #to find the row numbers which are having "." s   1 rows
which(is.na(lab_data$Loan.Length)) #to find the row numbers which are having NA   1 row
lab_data$Loan.Length[lab_data$Loan.Length=="."] <-NA  #filling dot value to NA value
class(lab_data$Loan.Length)   #character class it is actually factor so after fill in data it is choanged to factor
factor(lab_data$Loan.Length)
#NA values filling with mean
lab_data$Loan.Length[which(is.na(lab_data$Loan.Length))]=getmode(lab_data$Loan.Length)
which(is.na(lab_data$Loan.Length)) #to find the row numbers which are having NA   finnaly removed the missing data 0 rows
#changing to factors
lab_data$Loan.Length <- as.factor(lab_data$Loan.Length)
class(lab_data$Loan.Length)   #factor

####################### Finding the OutLiers ###################
#it is factor data type class so no need to apply outliers




#loan purpose
table(lab_data$Loan.Purpose)
class(lab_data$Loan.Purpose)   #character class
factor(lab_data$Loan.Purpose)    #it is also factor class
which(lab_data$Loan.Purpose==".") #to find the row numbers which are having "." s   0 rows
which(is.na(lab_data$Loan.Purpose)) #to find the row numbers which are having NA   1 row
#missing data is filled with the mode because it is categorical data
lab_data$Loan.Purpose[which(is.na(lab_data$Loan.Purpose))]=getmode(lab_data$Loan.Purpose)
which(is.na(lab_data$Loan.Purpose)) #to find the row numbers which are having NA   0 row
#changing to factors of data
lab_data$Loan.Purpose=as.factor(lab_data$Loan.Purpose)
class(lab_data$Loan.Purpose)   #now it is factor class

####################### Finding the OutLiers ###################
#factor data type column



#debt to income ratio
table(lab_data$Debt.To.Income.Ratio)  
class(lab_data$Debt.To.Income.Ratio)   #numeric
which(lab_data$Debt.To.Income.Ratio==".") #to find the row numbers which are having "." s   0 rows
which(is.na(lab_data$Debt.To.Income.Ratio)) #to find the row numbers which are having NA   1 row
#filling the data with mean as it is numeric data 
lab_data$Debt.To.Income.Ratio[which(is.na(lab_data$Debt.To.Income.Ratio))]=mean(lab_data$Debt.To.Income.Ratio,na.rm = TRUE)

####################### Finding the OutLiers ###################
boxplot(lab_data$Debt.To.Income.Ratio) #to find outliers 
#No outliers





#state 
table(lab_data$State)
which(lab_data$State==".") #to find the row numbers which are having "." s   1 rows
which(is.na(lab_data$State)) #to find the row numbers which are having NA   1 row
lab_data$State[lab_data$State=="."] <-NA
#data is filled with the mode
lab_data$State[which(is.na(lab_data$State))]=getmode(lab_data$State)     #getmode method is written in top
#changing to class data type to factor
lab_data$State=as.factor(lab_data$State)
class(lab_data$State)  #class data type is factor

####################### Finding the OutLiers ###################
#factor type





#home owner ship
table(lab_data$Home.Ownership)
which(lab_data$Home.Ownership==".") #to find the row numbers which are having "." s   0 rows
which(is.na(lab_data$Home.Ownership)) #to find the row numbers which are having NA   0 row

#replacing missing value with mode 
lab_data$Home.Ownership[which(is.na(lab_data$Home.Ownership))]=getmode(lab_data$Home.Ownership)   #here there is one colum so we can replace with the mode as it is factor class
#changing to factor class
lab_data$Home.Ownership=as.factor(lab_data$Monthly.Income)

#lab_data$Home.Ownership=as.factor(lab_data$Home.Ownership)
class(lab_data$Home.Ownership)

####################### Finding the OutLiers ###################
#factor type




#Montly income
table(lab_data$Monthly.Income)
which(lab_data$Monthly.Income==".") #to find the row numbers which are having "." s   0 rows
which(is.na(lab_data$Monthly.Income)) #to find the row numbers which are having NA   3 row
#replace the missing data with Mean
lab_data$Monthly.Income[which(is.na(lab_data$Monthly.Income))]=mean(lab_data$Monthly.Income,na.rm = TRUE)
#here it is in Numeric class only because it is not having any character like '.'
class(lab_data$Monthly.Income)     #numeric

####################### Finding the OutLiers ###################
boxplot(lab_data$Monthly.Income) #to find outliers 
#outliers are above 20K are outliers are should be removed
lab_data=lab_data[-(which(lab_data$Monthly.Income>=20000)),]
boxplot(lab_data$Monthly.Income) #to find outliers 
#still outliers are above 10,500 are outliers are should be removed
lab_data=lab_data[-(which(lab_data$Monthly.Income>=10500)),]
 
###Outliers are removed




#fico range
table(lab_data$FICO.Range)
which(lab_data$FICO.Range==".") #to find the row numbers which are having "." s   0 rows
which(is.na(lab_data$FICO.Range)) #to find the row numbers which are having NA   3 row
#no missing data in it 

####################### Finding the OutLiers ###################
#ranges




#open credit lines
table(lab_data$Open.CREDIT.Lines)
class(lab_data$Open.CREDIT.Lines)   #character class and actually categorical
which(lab_data$Open.CREDIT.Lines==".") #to find the row numbers which are having "." s   5 rows
which(is.na(lab_data$Open.CREDIT.Lines)) #to find the row numbers which are having NA   4 row
lab_data$Open.CREDIT.Lines[lab_data$Open.CREDIT.Lines=="."] <-NA    #"." are changed to NA
##fill the missing data with mode because it is a categorical data
#here we can apply skew of data
skewness(lab_data$Open.CREDIT.Lines,na.rm = TRUE)           #here skew is positive so try to reduce it with going function mean  0.8893
lab_data$Open.CREDIT.Lines[which(is.na(lab_data$Open.CREDIT.Lines))]=getmode(lab_data$Open.CREDIT.Lines)   #here there is one colum so we can replace with the mode as it is factor class
lab_data$Open.CREDIT.Lines=as.numeric(lab_data$Open.CREDIT.Lines)
class(lab_data$Open.CREDIT.Lines)     #now numeric
#here we can apply skew of data
skewness(lab_data$Open.CREDIT.Lines,na.rm = TRUE)           #here skew is positive so try to reduce it with going function mean  0.8893

####################### Finding the OutLiers ###################
boxplot(lab_data$Open.CREDIT.Lines) #to find outliers 
#outliers are above 22 are outliers are should be removed
lab_data=lab_data[-(which(lab_data$Open.CREDIT.Lines>=22)),]


                                                                                                                             #temparary1=lab_data   lab_data=temparary1  (Just for testing purpose)
#####Recoding the data 
# 2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17  18  19  20  21 
#23  57  99 136 213 195 236 203 157 162 116 128 108  74  52  44  37  22  18  15
table(new.data)
#checking with the employee lenght so that as per they are in the company those time they may be enquired so...
tb2<-table(lab_data$Employment.Length,lab_data$Inquiries.in.the.Last.6.Months) 
chisq.test(tb2)# before Changing p vaalue =0.7054
#####################################
#####main prediction#####  just 2 and 3are combined and last 2 are combined we got less pvalue among that remain like same
#####################################
new.data=recode(lab_data$Open.CREDIT.Lines,"2=2;3=2;4=3;5=4;6=5;7=6;8=7;9=8;10=9;11=10;12=11;13=12;14=13;15=14;16=15;17=16;18=17;19=17;20=18;21=18")
tb2<-table(lab_data$Employment.Length,new.data) #removed the  categorial data to number 
chisq.test(tb2)# After Changing p vaalue =0.1583
#####main prediction#####
#####################################
new.data=recode(lab_data$Inquiries.in.the.Last.6.Months,"2=2;3=2;4=3;5=4;6=5;7=6;8=7;9=8;10=9;11=10;12=11;13=12;14=13;15=14;16=15;17=16;18=16;19=17;20=17;21=17")
tb2<-table(lab_data$Employment.Length,new.data) #removed the7, 8,9 categorial data to number 7
chisq.test(tb2)# before Changing p vaalue =0.7822


new.data=recode(lab_data$Inquiries.in.the.Last.6.Months,"2=2;3=2;4=2;5=3;6=4;7=5;8=6;9=7;10=8;11=9;12=10;13=11;14=12;15=13;16=14;17=15;18=16;19=18;20=19;21=19")
tb2<-table(lab_data$Employment.Length,new.data) #removed the 6,7,8,9 categorial data to number 7
chisq.test(tb2)# before Changing p vaalue =0.7813


#By among those 5 sets pvalue=0.7054 is less compared to the remaing pvalues so now replacing with orginal data
lab_data$Inquiries.in.the.Last.6.Months=new.data




#Revolving credit balance
table(lab_data$Revolving.CREDIT.Balance)
which(lab_data$Revolving.CREDIT.Balance==".") #to find the row numbers which are having "." s   2 rows
which(is.na(lab_data$Revolving.CREDIT.Balance)) #to find the row numbers which are having NA   3 row
lab_data$Revolving.CREDIT.Balance[lab_data$Revolving.CREDIT.Balance=="."] <-NA
class(lab_data$Revolving.CREDIT.Balance)   #character class
#converting the data to numeric to apply mean to the column
lab_data$Revolving.CREDIT.Balance=as.numeric(lab_data$Revolving.CREDIT.Balance)
#filling data with mean
lab_data$Revolving.CREDIT.Balance[which(is.na(lab_data$Revolving.CREDIT.Balance))]=mean(lab_data$Revolving.CREDIT.Balance,na.rm = TRUE)   #here there is one colum so we can replace with the mode as it is factor class
class(lab_data$Revolving.CREDIT.Balance)     #now numeric

####################### Finding the OutLiers ###################

boxplot(lab_data$Revolving.CREDIT.Balance) #to find outliers 
#outliers are above 30,000 are outliers are should be removed
lab_data=lab_data[-(which(lab_data$Revolving.CREDIT.Balance>=30000)),]




#inquires in the last 6 months
table(lab_data$Inquiries.in.the.Last.6.Months)
class(lab_data$Inquiries.in.the.Last.6.Months)     #here the class is numeric
which(lab_data$Inquiries.in.the.Last.6.Months==".") #to find the row numbers which are having "." s   0 rows
which(is.na(lab_data$Inquiries.in.the.Last.6.Months)) #to find the row numbers which are having NA   3 row

factor(lab_data$Inquiries.in.the.Last.6.Months)     #to check for factors for it
lab_data$Inquiries.in.the.Last.6.Months=as.factor(lab_data$Inquiries.in.the.Last.6.Months)   #converting into factors 
class(lab_data$Inquiries.in.the.Last.6.Months)   #noe the class is factor

####################### Finding the OutLiers ###################
#factor data

#####Recoding the data 
#   0    1    2    3    4    5    6    7    8    9 
#1058  541  278  140   44   12    7    6    2    5 
#in the above data just to reduce the categories i merged the 7,8,9
new.data=recode(lab_data$Inquiries.in.the.Last.6.Months,"0=0;1=1;2=2;3=3;4=4;5=5;6=6;7=7;8=7;9=7")
table(new.data)
#checking with the employee lenght so that as per they are in the company those time they may be enquired so...
tb2<-table(lab_data$Employment.Length,lab_data$Inquiries.in.the.Last.6.Months) 
chisq.test(tb2)# before Changing p vaalue =0.8284

tb2<-table(lab_data$Employment.Length,new.data) #removed the 8,9 categorial data to number 7
chisq.test(tb2)# After Changing p vaalue =0.7414

new.data=recode(lab_data$Inquiries.in.the.Last.6.Months,"0=0;1=1;2=2;3=3;4=4;5=5;6=6;7=6;8=6;9=6")
tb2<-table(lab_data$Employment.Length,new.data) #removed the7, 8,9 categorial data to number 7
chisq.test(tb2)# before Changing p vaalue =0.7689

#####################################
#####Main prediction######
new.data=recode(lab_data$Inquiries.in.the.Last.6.Months,"0=0;1=1;2=2;3=3;4=4;5=5;6=5;7=5;8=5;9=5")
tb2<-table(lab_data$Employment.Length,new.data) #removed the 6,7,8,9 categorial data to number 7
chisq.test(tb2)# before Changing p vaalue =0.7054
#####main prediction#####
#####################################

new.data=recode(lab_data$Inquiries.in.the.Last.6.Months,"0=0;1=1;2=2;3=3;4=4;5=4;6=4;7=4;8=4;9=4")
tb2<-table(lab_data$Employment.Length,new.data) #removed the 5,6,7, 8,9 categorial data to number 7
chisq.test(tb2)# before Changing p vaalue =0.9114

#By among those 5 sets pvalue=0.7054 is less compared to the remaing pvalues so now replacing with orginal data
lab_data$Inquiries.in.the.Last.6.Months=new.data




#employment length
table(lab_data$Employment.Length)
which(lab_data$Employment.Length==".") #to find the row numbers which are having "." s   2 rows
which(is.na(lab_data$Employment.Length)) #to find the row numbers which are having NA   1 row
lab_data$Inquiries.in.the.Last.6.Months[lab_data$Inquiries.in.the.Last.6.Months=="."] <-NA
lab_data$Inquiries.in.the.Last.6.Months[lab_data$Inquiries.in.the.Last.6.Months=="n/a"] <-NA
##fill the missing data with mode because it is a categorical data
lab_data$Employment.Length[which(is.na(lab_data$Employment.Length))]=getmode(lab_data$Employment.Length)   #here there is one colum so we can replace with the mode as it is factor class

class(lab_data$Employment.Length)   #character class
lab_data$Employment.Length=as.factor(lab_data$Employment.Length)   #converting into factors 
class(lab_data$Employment.Length)   #now the class is factor

####################### Finding the OutLiers ###################
#factor


############################################
#######################################
##############################
#For predicting the lab_data$Intrest.Rate
#creating a data model using regression
#library("caTools")
sample = sample.split(lab_data,SplitRatio = 0.80) # splits the data in the ratio of 80% for train and remaing 20% for test. After splitting marks these rows as logical TRUE and the the remaining are marked as logical FALSE
train =subset(lab_data,sample ==TRUE) # creates a training dataset named train1 with rows which are marked as TRUE
test=subset(lab_data, sample==FALSE)
View(train)
View(test)
dim(train)   # to know the dimension of the matrix

#applying Multiple regression to the model of the data 
##################################################################
##1)with out removing any column 
train_regression = lm(formula = Interest.Rate ~. ,data =lab_data)    ####Multiple R-squared:  0.8166,	Adjusted R-squared:  0.8058
summary(train_regression)                          ####Multiple R-squared:  0.8166,	Adjusted R-squared:  0.8058
##################################################################
##2)with out removing some  columns
train_regression = lm(formula = Interest.Rate ~ Amount.Requested+Loan.Length+State+Home.Ownership+Inquiries.in.the.Last.6.Months+Employment.Length+Revolving.CREDIT.Balance ,data =lab_data)    ####Multiple R-squared:  0.8166,	Adjusted R-squared:  0.8058
summary(train_regression)                          ####Multiple R-squared:  0.2565,	Adjusted R-squared:  0.2338 

##2)with out removing some  columns
train_regression = lm(formula = Interest.Rate ~ Amount.Requested+Loan.Length+State+Home.Ownership+Inquiries.in.the.Last.6.Months+Revolving.CREDIT.Balance ,data =lab_data)    ####Multiple R-squared:  0.8166,	Adjusted R-squared:  0.8058
summary(train_regression)                          ####Multiple R-squared:  0.2523,	Adjusted R-squared:  0.2336 


#finally for more R2 that model will predict more accuracy in Multiple Regresion 
#from 1 st set of model we got the around 80% of R2
#remaing 2 regression models just we got around 20% to 30% of R2.
#so 1 st one is better for pridtions
############################################################################################################################ 
######################################################################################################
#################################################################################
###############################################################
#the larger the R2, the better the regression model fits your observations.
train_regression = lm(formula = Interest.Rate ~. ,data =lab_data)    ####Multiple R-squared:  0.8166,	Adjusted R-squared:  0.8058
summary(train_regression)                          ####Multiple R-squared:  0.8166,	Adjusted R-squared:  0.8058

                      #########!!!!Multiple R-squared:  0.8166,	Adjusted R-squared:  0.8058!!!!#################
                               ###############################################################
                      #################################################################################
            ######################################################################################################
############################################################################################################################ 
#testing the data
y_pred = predict(train_regression, data = test)


#Explanation About the data and process done in above
#i have found the missing values and by checking class of the colums there are some colums resulted as string so.because in those colums 
#these is a character called "."(dot) so it is showing the class as Character.
#first i have replaced dot with the NA value and finnally filled the NA values of all colum with appropriate method by finding the skew
#there are some data which is categorical and for categorical Mode is best way to fill the missing values as there are less number of missing values
#After filling all missing values in data except in the ID column because ID colum is unique and can't fill that column so just i have removed that coloumn
#next preprocess done with the Outliers for numerical data and removed all data of outliers
####IMP##### here there are categorical and factor class colums so for some coloums just i have applied the transformation to reduce
#the categories ofcolumn and to improve the accuracy **whether suitable or not is checked by ****chi-square test**** is also done ***
#finnaly i have did more 3 types of availability for each column and replace with less p-value with among those all the and fixed as the process as final
###########here preprocessing is completed#####
####splitting of data is done.As there is less data just 2000 rows soi have took the ration od 80:20  for train-80% and test-20%
#***********8finally predicted the Linear regression for INTREST.RATE colums(class label)*************
#*********************Got R square as 0.8166 so 81% has occured *************
#*******the larger the R2, the better the regression model fits your observations.**********

#############################################################Completed###################################################



#################Librarries used in it
library(car)
library(moments)
#install.packages("outliers")
library(outliers)
library("caTools")