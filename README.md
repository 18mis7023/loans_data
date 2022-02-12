# loans_data
loans_data_Apply_Linear_ Regression     And PreProcessing Technique
***This data analysis had did using the R programming Language***


## Explanation About the data and process done in above
#### i have found the missing values and by checking class of the colums there are some colums resulted as string so.because in those colums these is a character called "."(dot) so it is showing the class as Character. First i have replaced dot with the NA value and finnally filled the NA values of all colum with appropriate method by finding the skew there are some data which is categorical and for categorical Mode is best way to fill the missing values as there are less number of missing values After filling all missing values in data except in the ID column because ID colum is unique and can't fill that column so just i have removed that coloumn next preprocess done with the Outliers for numerical data and removed all data of outliers
### ***IMP*** here there are categorical and factor class colums so for some coloums just i have applied the transformation to reduce
#### The categories ofcolumn and to improve the accuracy **whether suitable or not is checked by ****chi-square test**** is also done ***
#### Finnaly i have did more 3 types of availability for each column and replace with less p-value with among those all the and fixed as the process as final
### **Here preprocessing is completed**
### splitting of data is done.As there is less data just 2000 rows soi have took the ration od 80:20  for train-80% and test-20%
## ***finally predicted the Linear regression for INTREST.RATE colums(class label)***
## *********************Got R square as 0.8166 so 81% has occured *************
## *******the larger the R2, the better the regression model fits your observations.*******
