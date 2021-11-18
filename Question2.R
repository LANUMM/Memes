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
dmy <- dummyVars(~ Pclass+Sex+Age+SibSp+Parch+Fare+Embarked, data=df)
dfOH <- data.frame(predict(dmy, newdata=df), "Survived" = df$Survived)
head(dfOH)

#c)
#Sex
get_dist(dfOH[ ,c(4,5)], method="binary")
#Embarked
get_dist(dfOH[ ,c(10,11,12)], method="binary")

#d)
lapply(dfOH[,c(6,7,8,9)], get_dist, method="euclidean")
lapply(dfOH[,c(6,7,8,9)], get_dist, method="kendall")

#e)
#1.
#One drawback of the K-medoids algorithm is that you are required to estimate the number of clusters ahead of time. 

#2.
##compute the weighted sum of three distance matricies, weights are equally weighted (each origional column occupied 1/8 weight)
my.d = 0.75*d.interval.kd + 0.125*d.sex + 0.125*d.eb
##recombine the numeric and categorical data
my_data = cbind.data.frame(interval.data,nominal.onehot)
fviz_nbclust(x=my_data, FUNcluster=cluster::pam, method="wss", diss=my.d)

#3.
# http://www.sthda.com/english/articles/29-cluster-validation-essentials/96-determiningthe-optimal-number-of-clusters-3-must-know-methods/ 
#Run the clustering algorithm for varying number of clusters, plot that against the within-cluster sum of square (wss) for that cluster, and the number of clusters with the bendy bit is the one you want. 

#f)
myPam <- pam(dfOH,2)
preds <- myPam$clustering -1
real <- dfOH$Survived
sum(preds == real)/ length(preds)
#The accuracy was 68.12% 