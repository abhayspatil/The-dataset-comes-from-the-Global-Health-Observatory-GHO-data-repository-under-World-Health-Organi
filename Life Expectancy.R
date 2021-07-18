# Loading required Packages
library(naniar)
library(data.table)
library(caTools)
library(caret)
library(ggplot2)
library(dplyr)
library(Amelia)
library(moments)
library(car)

#-------------------------------------------1-----------------------------------------------------------------------

#1)	Understand the variables 

df = read.csv('Life Expectancy Data.csv')

# Names of variables
names(df)

# dim of data
dim(df)

#checking data type of each data frame column
str(df)
# Convert some features in factor
df$Status = as.factor(df$Status)

Continuous_cols = c("Life_Expectancy","Adult_Mortality","Infant_Deaths","Alcohol","Percentage_Expenditure",
              "Hepatitis_B","Measles","BMI","Under.five_Deaths","Polio","Total_Expenditure","Diphtheria","HIV.AIDS","GDP",
              "Per_Capita_GDP","Population","Thinness_1.19_Years","Thinness_5.9_Years","Income_Composition_of_Resources","Schooling")
CorrData=cor(df[, Continuous_cols], use = "complete.obs")
CorrData
CorrData['Life_Expectancy',]
abs(CorrData['Life_Expectancy',])>0.5






#---------------------------------------------2-------------------------------------------------------------------

# 2)	Create hypothesis and validate.
# H0= features are independent of Life expectancy which mean null hypothesis says that dependent variable is not having 
       # any relationship with independent variables in data set.
# H1 = Dependent variable is having impacted by independent variables.

# Continuous Vs Categorical relationship strength: ANOVA
# Analysis of Variance(ANOVA)

# H0: Variables are NOT related
# P-Value<0.05--> Variables are related(H0 is rejected)
# P-Value>=0.05--> Variables are NOT related (H0 is accepted)

Anov_Cols = c("Country" ,"Year","Status","Adult_Mortality","Infant_Deaths","Alcohol","Percentage_Expenditure",
              "Hepatitis_B","Measles","BMI","Under.five_Deaths","Polio","Total_Expenditure","Diphtheria","HIV.AIDS","GDP",
              "Per_Capita_GDP","Population","Thinness_1.19_Years","Thinness_5.9_Years","Income_Composition_of_Resources","Schooling" )
for (av_Col in Anov_Cols) 
{
  print(paste("Anova test for",av_Col))
 
  Sumry = summary(aov(df$Life_Expectancy~df[,c(av_Col)]))
  print(Sumry)
  
}

# p-value for Population" is 0.209 which mean that we failed to reject null hypothesis.
# in rest of case p-values is less than 0.05 as we accept alternate hypothesis (H1)


#------------------------------------------3------------------------------------------------------------------------
#.3)	Identify the statistical model to use 


#-------------------------------------------4----------------------------------------------------------------------
# data pre-processing

# check missing values
colSums(is.na(df))

# we have missing values in 

miss_var_summary(df)



# in the given scenario we have Hepatitis_B feature missing 18.8% of missing data. so we drop this feature.


df = subset(df,select = -c(Hepatitis_B))
dim(df)
colnames(df)


# imputation of missing values:
df$Total_Expenditure[is.na(df$Total_Expenditure)] = median(df$Total_Expenditure,na.rm = T)
df$Alcohol[is.na(df$Alcohol)] = median(df$Alcohol,na.rm = T)
df$Income_Composition_of_Resources[is.na(df$Income_Composition_of_Resources)] = median(df$Income_Composition_of_Resources,na.rm = T)
df$Schooling[is.na(df$Schooling)] = median(df$Schooling,na.rm = T)
df$GDP[is.na(df$GDP)] = median(df$GDP,na.rm = T)
df$Per_Capita_GDP[is.na(df$Per_Capita_GDP)] = median(df$Per_Capita_GDP,na.rm = T)
df$BMI[is.na(df$BMI)] = median(df$BMI,na.rm = T)
df$Thinness_1.19_Years[is.na(df$Thinness_1.19_Years)] = median(df$Thinness_1.19_Years,na.rm = T)
df$Thinness_5.9_Years[is.na(df$Thinness_5.9_Years)] = median(df$Thinness_5.9_Years,na.rm = T)
df$Polio[is.na(df$Polio)] = median(df$Polio,na.rm = T)
df$Diphtheria[is.na(df$Diphtheria)] = median(df$Diphtheria,na.rm = T)
df$Life_Expectancy[is.na(df$Life_Expectancy)] = median(df$Life_Expectancy, na.rm = T)
df$Adult_Mortality[is.na(df$Adult_Mortality)] = median(df$Adult_Mortality,na.rm = T)
df$Population[is.na(df$Population)] = median(df$Population,na.rm = T)

miss_var_summary(df)


# detection of outliers and treatment with IQR

# Life_Expectancy

quantile(df$Life_Expectancy)
upper_lim = quantile(df$Life_Expectancy,0.75) + (1.5*(IQR(df$Life_Expectancy)))
upper_lim
lower_lim = quantile(df$Life_Expectancy,0.25) - (1.5*(IQR(df$Life_Expectancy)))
lower_lim
boxplot(df$Life_Expectancy)
sum(df$Life_Expectancy < 44.6)

df$Life_Expectancy=ifelse(df$Life_Expectancy < 44.6, 44.6,df$Life_Expectancy)

boxplot(df$Life_Expectancy)
# Adult_Mortality

quantile(df$Adult_Mortality)
histogram(df$Adult_Mortality)
boxplot(df$Adult_Mortality)
upper_lim = quantile(df$Adult_Mortality,0.75) + (1.5*(IQR(df$Adult_Mortality)))
upper_lim
lower_lim = quantile(df$Adult_Mortality,0.25) - (1.5*(IQR(df$Adult_Mortality)))
lower_lim
df$Adult_Mortality=ifelse(df$Adult_Mortality > 456.50, 456.50,df$Adult_Mortality)
boxplot(df$Adult_Mortality)


#Infant_Deaths
quantile(df$Infant_Deaths)
histogram(df$Infant_Deaths)
boxplot(df$Infant_Deaths)
upper_lim = quantile(df$Infant_Deaths,0.75) + (1.5*(IQR(df$Infant_Deaths)))
upper_lim
lower_lim = quantile(df$Infant_Deaths,0.25) - (1.5*(IQR(df$Infant_Deaths)))
lower_lim
df$Infant_Deaths=ifelse(df$Infant_Deaths > 55, 55,df$Infant_Deaths)
boxplot(df$Infant_Deaths)


# Alcohol
quantile(df$Alcohol)
histogram(df$Alcohol)
boxplot(df$Alcohol)
upper_lim = quantile(df$Alcohol,0.75) + (1.5*(IQR(df$Alcohol)))
upper_lim
lower_lim = quantile(df$Alcohol,0.25) - (1.5*(IQR(df$Alcohol)))
lower_lim
df$Alcohol=ifelse(df$Alcohol > 16.83625, 16.83625,df$Alcohol)
boxplot(df$Alcohol)

#Percentage_Expenditure
quantile(df$Percentage_Expenditure)
histogram(df$Percentage_Expenditure)
boxplot(df$Percentage_Expenditure)
upper_lim = quantile(df$Percentage_Expenditure,0.75) + (1.5*(IQR(df$Percentage_Expenditure)))
upper_lim
lower_lim = quantile(df$Percentage_Expenditure,0.25) - (1.5*(IQR(df$Percentage_Expenditure)))
lower_lim
df$Percentage_Expenditure=ifelse(df$Percentage_Expenditure > 1096.807, 1096.807,df$Percentage_Expenditure)
boxplot(df$Percentage_Expenditure)

#Measles
quantile(df$Measles)
histogram(df$Measles)
boxplot(df$Measles)
upper_lim = quantile(df$Measles,0.75) + (1.5*(IQR(df$Measles)))
upper_lim
lower_lim = quantile(df$Measles,0.25) - (1.5*(IQR(df$Measles)))
lower_lim
df$Measles=ifelse(df$Measles > 900.625, 900.625,df$Measles)
boxplot(df$Measles)

#Under-five_Deaths
quantile(df$Under.five_Deaths)
histogram(df$Under.five_Deaths)
boxplot(df$Under.five_Deaths)
upper_lim = quantile(df$Under.five_Deaths,0.75) + (1.5*(IQR(df$Under.five_Deaths)))
upper_lim
lower_lim = quantile(df$Under.five_Deaths,0.25) - (1.5*(IQR(df$Under.five_Deaths)))
lower_lim
df$Under.five_Deaths=ifelse(df$Under.five_Deaths > 70, 70,df$Under.five_Deaths)
boxplot(df$Under.five_Deaths)

#Polio
quantile(df$Polio)
histogram(df$Polio)
boxplot(df$Polio)
upper_lim = quantile(df$Polio,0.75) + (1.5*(IQR(df$Polio)))
upper_lim
lower_lim = quantile(df$Polio,0.25) - (1.5*(IQR(df$Polio)))
lower_lim
df$Polio=ifelse(df$Polio < 49.50, 49.50,df$Polio)
boxplot(df$Polio)


#Total_Expenditure
quantile(df$Total_Expenditure)
histogram(df$Total_Expenditure)
boxplot(df$Total_Expenditure)
upper_lim = quantile(df$Total_Expenditure,0.75) + (1.5*(IQR(df$Total_Expenditure)))
upper_lim
lower_lim = quantile(df$Total_Expenditure,0.25) - (1.5*(IQR(df$Total_Expenditure)))
lower_lim
df$Total_Expenditure=ifelse(df$Total_Expenditure > 11.77, 11.77,df$Total_Expenditure)
boxplot(df$Total_Expenditure)


#Diphtheria
quantile(df$Diphtheria)
histogram(df$Diphtheria)
boxplot(df$Diphtheria)
upper_lim = quantile(df$Diphtheria,0.75) + (1.5*(IQR(df$Diphtheria)))
upper_lim
lower_lim = quantile(df$Diphtheria,0.25) - (1.5*(IQR(df$Diphtheria)))
lower_lim
df$Diphtheria=ifelse(df$Diphtheria < 49.50, 49.50,df$Diphtheria)
boxplot(df$Diphtheria)

                                                    
#HIV/AIDS
quantile(df$HIV.AIDS)
histogram(df$HIV.AIDS)
boxplot(df$HIV.AIDS)
upper_lim = quantile(df$HIV.AIDS,0.75) + (1.5*(IQR(df$HIV.AIDS)))
upper_lim
lower_lim = quantile(df$HIV.AIDS,0.25) - (1.5*(IQR(df$HIV.AIDS)))
lower_lim
df$HIV.AIDS=ifelse(df$HIV.AIDS > 1.85, 1.85,df$HIV.AIDS)
boxplot(df$HIV.AIDS)

#GDP
quantile(df$GDP)
histogram(df$GDP)
boxplot(df$GDP)
upper_lim = quantile(df$GDP,0.75) + (1.5*(IQR(df$GDP)))
upper_lim
lower_lim = quantile(df$GDP,0.25) - (1.5*(IQR(df$GDP)))
lower_lim
df$GDP=ifelse(df$GDP > 331407152250, 331407152250,df$GDP)
boxplot(df$GDP)

#Per_Capita_GDP
quantile(df$Per_Capita_GDP)
histogram(df$Per_Capita_GDP)
boxplot(df$Per_Capita_GDP)
upper_lim = quantile(df$Per_Capita_GDP,0.75) + (1.5*(IQR(df$Per_Capita_GDP)))
upper_lim
lower_lim = quantile(df$Per_Capita_GDP,0.25) - (1.5*(IQR(df$Per_Capita_GDP)))
lower_lim
df$Per_Capita_GDP=ifelse(df$Per_Capita_GDP > 29279.34, 29279.34,df$Per_Capita_GDP)
boxplot(df$Per_Capita_GDP)

#Population
quantile(df$Population)
histogram(df$Population)
boxplot(df$Population)
upper_lim = quantile(df$Population,0.75) + (1.5*(IQR(df$Population)))
upper_lim
lower_lim = quantile(df$Population,0.25) - (1.5*(IQR(df$Population)))
lower_lim
df$Population=ifelse(df$Population > 58151949, 58151949,df$Population)
boxplot(df$Population)

#Thinness_1-19_Years
quantile(df$Thinness_1.19_Years)
histogram(df$Thinness_1.19_Years)
boxplot(df$Thinness_1.19_Years)
upper_lim = quantile(df$Thinness_1.19_Years,0.75) + (1.5*(IQR(df$Thinness_1.19_Years)))
upper_lim
lower_lim = quantile(df$Thinness_1.19_Years,0.25) - (1.5*(IQR(df$Thinness_1.19_Years)))
lower_lim
df$Thinness_1.19_Years=ifelse(df$Thinness_1.19_Years > 15.35, 15.35,df$Thinness_1.19_Years)
boxplot(df$Thinness_1.19_Years)

#Thinness_5-9_Years
quantile(df$Thinness_5.9_Years)
histogram(df$Thinness_5.9_Years)
boxplot(df$Thinness_5.9_Years)
upper_lim = quantile(df$Thinness_5.9_Years,0.75) + (1.5*(IQR(df$Thinness_5.9_Years)))
upper_lim
lower_lim = quantile(df$Thinness_5.9_Years,0.25) - (1.5*(IQR(df$Thinness_5.9_Years)))
lower_lim
df$Thinness_5.9_Years=ifelse(df$Thinness_5.9_Years > 15.60, 15.60,df$Thinness_5.9_Years)
boxplot(df$Thinness_5.9_Years)

#Income_Composition_of_Resources
quantile(df$Income_Composition_of_Resources)
histogram(df$Income_Composition_of_Resources)
boxplot(df$Income_Composition_of_Resources)
upper_lim = quantile(df$Income_Composition_of_Resources,0.75) + (1.5*(IQR(df$Income_Composition_of_Resources)))
upper_lim
lower_lim = quantile(df$Income_Composition_of_Resources,0.25) - (1.5*(IQR(df$Income_Composition_of_Resources)))
lower_lim
df$Income_Composition_of_Resources=ifelse(df$Income_Composition_of_Resources < 0.102625, 0.102625,df$Income_Composition_of_Resources)
boxplot(df$Income_Composition_of_Resources)


#Schooling
quantile(df$Schooling)
histogram(df$Schooling)
boxplot(df$Schooling)
upper_lim = quantile(df$Schooling,0.75) + (1.5*(IQR(df$Schooling)))
upper_lim
lower_lim = quantile(df$Schooling,0.25) - (1.5*(IQR(df$Schooling)))
lower_lim
df$Schooling=ifelse(df$Schooling < 4.60, 4.60,df$Schooling)
df$Schooling=ifelse(df$Schooling > 19.80, 19.80,df$Schooling)
boxplot(df$Schooling)

#Univariate Analysis:
############## Exploring Multiple Continuous features ################


######continuous columns#####
library(RColorBrewer)
continuous_columns = c( "GDP","Per_Capita_GDP","Population","Thinness_1.19_Years","BMI","Polio",
                       "Thinness_5.9_Years","Income_Composition_of_Resources", "Schooling" )
par(mfrow=c(3,3))


for (i in continuous_columns){
  hist(df[,c(i)], xlab = i, main=paste('Histogram of:',i), col = brewer.pal(8,"Paired"))
}


conti_columns = c("Life_Expectancy","Adult_Mortality","Infant_Deaths","Alcohol","Percentage_Expenditure","Measles",
                        "Total_Expenditure","Diphtheria","HIV_AIDS","Under_five_Deaths","Year")


par(mfrow=c(3,3))


for (i in conti_columns){
  hist(df[,c(i)], xlab = i, main=paste('Histogram of:',i),col=brewer.pal(8,"Paired"))
}


######### Continuous Vs Categorical Visual Analysis : BOXPLOT #########

categorical_cols=c("Status")
categorical_cols

par(mfrow=c(1,1))

for (box_cols in categorical_cols) {
  boxplot(Life_Expectancy~(df[ , c(box_cols)]), data = df,
          main = paste('Box Plot of:', box_cols), col = brewer.pal(8,"Paired"))
}

# developing country has less life_expentancy compare to developed.



###########################################################################################################################################
# Linear Regression Model
set.seed(300)
split = sample.split(df, SplitRatio = 0.7)


train = subset(df, split == TRUE)
test = subset(df, split == FALSE)

str(train)
str(test)

# Builed Linear Regression Model without Country.

lm1 = lm(Life_Expectancy ~ .-Country, data = train)
summary(lm1)

# build a model using step-wise regression with backward step!

Backward_Step_Model = step(lm1, direction = "backward")

# based on AIC we rebuild model and see R square and p-values

lm2 = lm(Life_Expectancy ~ Year + Status + Adult_Mortality + Infant_Deaths + 
           Alcohol + Percentage_Expenditure + Measles + Under.five_Deaths + 
           Polio + Diphtheria + HIV.AIDS + GDP + Per_Capita_GDP + Population + 
           Thinness_5.9_Years + Income_Composition_of_Resources + Schooling, data = train)
summary(lm2)
# re run model without Alcohol

lm3 = lm(Life_Expectancy ~ Year + Status + Adult_Mortality + Infant_Deaths + 
          Percentage_Expenditure + Measles + Under.five_Deaths + 
          Polio + Diphtheria + HIV.AIDS + GDP + Per_Capita_GDP + Population + 
          Thinness_5.9_Years + Income_Composition_of_Resources + Schooling, data = train)
summary(lm3)

# removing Alcohol our R squre is reduced by 0.0001

VIF=sort(vif(lm3))
data.frame(VIF)

# we have almost all the features are below threshold level of 5 VIF. however there are two features Under.five_Deaths
 # and Infant_Deaths having VIF 124.99 and 127.13  respectively. we have to remove one by one anc check VIF 
# value. 

lm4 = lm(Life_Expectancy ~ Year + Status + Adult_Mortality + Under.five_Deaths+
           Percentage_Expenditure + Measles + Polio + Diphtheria + HIV.AIDS + GDP + Per_Capita_GDP + Population + 
           Thinness_5.9_Years + Income_Composition_of_Resources + Schooling, data = train)

summary(lm4)

lm5 = lm(Life_Expectancy ~ Year + Status + Adult_Mortality + Under.five_Deaths+
           Percentage_Expenditure + Measles  + Diphtheria + HIV.AIDS + GDP + Per_Capita_GDP + Population + 
           Thinness_5.9_Years + Income_Composition_of_Resources + Schooling, data = train)

summary(lm5)


# after r
VIF=sort(vif(lm4))
data.frame(VIF)

# Now we dont have any VIF value more than 5 threshold level.

# , Homoscedasticity test


par(mfrow = c(2, 2))
plot(lm4$fitted.values, lm4$residuals)
abline(h=0, col = "red")

plot(lm3$fitted.values, lm3$residuals)
abline(h=0, col = "red")


plot(lm2$fitted.values, lm2$residuals)
abline(h=0, col = "red")

plot(lm1$fitted.values, lm1$residuals)
abline(h=0, col = "red")

# The residuals from all models are spread adequately equal along with the ranges of predictors.


# Checking Accuracy of model on Testing data
test$Pred_LM=predict(lm4, test)
head(test)


# Calculating the Absolute Percentage Error for each prediction
test$LM_APE= 100 *(abs((test$Life_Expectancy-test$Pred_LM)/test$Life_Expectancy))
head(test)

MeanAPE=mean(test$LM_APE)
MedianAPE=median(test$LM_APE)

print(paste('### Mean Accuracy of Linear Regression Model is: ', 100 - MeanAPE))
print(paste('### Median Accuracy of Linear Regression Model is: ', 100 - MedianAPE))

# MAPE
mean(abs((test$Life_Expectancy-test$Pred_LM)/test$Life_Expectancy)) * 100


#The MAPE for this model turns out to be 3.96%. That is, the average absolute difference between the forecasted value 
# and the actual value is 3.96%.


# calculate MAPE from labrary
#load MLmetrics package
library(MLmetrics)

#calculate MAPE
MAPE(test$Pred_LM, test$Life_Expectancy)









