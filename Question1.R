#Load packages
library(caret)

#STEP 1
#a)
df <- read.csv('Titanic_updated.csv', stringsAsFactors = TRUE)
df[ ,c(1,2,7,8)] <- lapply(df[,c(1,2,7,8)], factor)
str(df)

#b)
lapply(lapply(df,is.na), sum)
df$Age[is.na(df$Age)] <- median(df$Age, na.rm = TRUE)

#STEP 2

#c)
training <- createDataPartition(df, p = 0.75)