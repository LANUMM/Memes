#Load packages
library(caret)

#STEP 1
df <- read.csv('Titanic_updated.csv')

#c)

training <- createDataPartition(df, p = 0.75)
