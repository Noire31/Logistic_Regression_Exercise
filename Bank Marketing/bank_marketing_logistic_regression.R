# Import the library 
library(caTools)
library(car)
library (plyr)

# Load the data
df = read.csv('bank-additional-full.csv', sep = ';')

# Count the categorical data
count(df$job)
count(df$marital)
count(df$education)
count(df$default)
count(df$housing)
count(df$loan)
count(df$contact)
count(df$month)
count(df$day_of_week)
count(df$poutcome)
count(df$y)

# Map the data
df$job = recode(df$job,"'admin.' =1; 'blue-collar'=2; 'entrepreneur'=3; 
                'housemaid'=4; 'management'=5; 'retired'=6; 'self-employed'=7;
                'services'=8; 'student'=9; 'technician'=10; 'unemployed'=11;
                'unknown'=12 ")

df$marital = recode(df$marital,"'divorced'=1; 'married'=2; 'single'=3; 'unknown'=4")
df$education = recode(df$education, "'basic.4y'=1; 'basic.6y'=2; 'basic.9y'=3;
                      'high.school'=4; 'illiterate'=5; 'professional.course'=6;
                      'university.degree'=7; 'unknown'=8 ")
df$default = recode(df$default,"'no'=1; 'unknown'=2; 'yes'=3; ")
df$housing = recode(df$housing,"'no'=1; 'unknown'=2; 'yes'=3; ")
df$loan = recode(df$loan,"'no'=1; 'unknown'=2; 'yes'=3; ")
df$contact = recode(df$contact,"'cellular'=1; 'telephone'=2 ")
df$month = recode(df$month,"'apr'=1; 'aug'=2; 'dec'=3; 'jul'=4; 'jun'=5; 'mar'=6;
                  'may'=7; 'nov'=8; 'oct'=9; 'sep'=10; ")
df$day_of_week = recode(df$day_of_week, "'fri'=1; 'mon'=2; 'thu'=3; 'tue'=4; 'wed'=5 ")
df$poutcome = recode(df$poutcome, "'failure'=1; 'nonexistent'=2; 'success'=3 ")
df$y = recode(df$y, "'no'=0; 'yes'=1 ")

# Feature Scaling 
df[,1:20] = scale(df[,1:20])

# Train test split
set.seed(123)

split = sample.split(df$y, SplitRatio = 0.7)
training_set = subset(df,split == TRUE)
test_set = subset(df, split==FALSE)

# Logistic Regression
classifier = glm(formula = y~ ., family = binomial, data=training_set)
summary(classifier)

# Predict new data
y_pred = predict(classifier, type='response', newdata = test_set[-21])

pred = ifelse(y_pred>= 0.5,1,0)

result = cbind(test_set,pred)
