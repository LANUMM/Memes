library(cluster)
library(factoextra)
library(magrittr)
library(Rtsne)
library(dplyr)
library(caret)

#Preprocessing
df <- read.csv('Titanic_updated.csv', stringsAsFactors = TRUE)
df[ ,c(1,2,7,8)] <- lapply(df[,c(1,2,7,8)], factor)
lapply(lapply(df,is.na), sum)
df$Age[is.na(df$Age)] <- median(df$Age, na.rm = TRUE)

df[,c("Pclass")]=as.numeric(as.character(df[,c("Pclass")]))
levels(df[,c("Embarked")])[1]="U"

#a)
ggplot(df, aes(fill=Survived, y=Survived, x=Sex))+ geom_bar(position = "stack", stat = "identity")
plot(Survived ~ Sex, data = df)
paste("Female survival rate: ",  round(length(df$Survived[df$Sex=="female" & df$Survived=="1"]) / length(df$Survived[df$Sex=="female"]) * 100, 2), "%")
paste("Male survival rate: ",  round(length(df$Survived[df$Sex=="male" & df$Survived=="1"]) / length(df$Survived[df$Sex=="male"]) * 100, 2), "%")

#As shown by the stacked bar plot and by the printed figures, the Male survival rate was around 19% while the female survival rate was around 74%. 
# As a result there does appear to be a significant association between these two columns. 

#b)
dmy <- dummyVars(~ Pclass+Sex+SibSp+Parch+Fare+Embarked, data=df)
dfOH <- data.frame(predict(dmy, newdata=df), "Survived" = df$Survived)
head(dfOH)

#c)
# https://jootse84.github.io/notes/jaccard-index-calculation-in-R 
library('philentropy')
distance(cbind(dfOH$Sex.female, dfOH$Sex.male), method="jaccard")
