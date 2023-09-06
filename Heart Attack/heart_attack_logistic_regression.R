# Import the library 
library(caTools)
library(car)
library (plyr)

# Load the data
df = read.csv('heart.csv')

# Feature Scaling 
df[,1:13] = scale(df[,1:13])

# Train test split
set.seed(123)

split = sample.split(df$output, SplitRatio = 0.7)
training_set = subset(df,split == TRUE)
test_set = subset(df, split==FALSE)

# Logistic Regression
classifier = glm(formula = output~ ., family = binomial, data=training_set)
summary(classifier)

# Predict new data
output_pred = predict(classifier, type='response', newdata = test_set[-14])

pred = ifelse(output_pred>= 0.5,1,0)

result = cbind(test_set,pred)
