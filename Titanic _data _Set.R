df.train <- read.csv('titanic_train.csv')
df.test <-  read.csv('titanic_test.csv')
head(df.train)
library(ggplot2)
install.packages('Amelia')
library(Amelia)
missmap(df.train,main = 'Main',col= c('yellow','black'),legend = FALSE)
ggplot(df.train,aes(Survived))+geom_bar()
ggplot(df.train,aes(Pclass))+geom_bar()
ggplot(df.train,aes(Sex))+geom_bar()
ggplot(df.train,aes(Age))+geom_histogram(color='blue',bins=20)
ggplot(df.train,aes(SibSp))+geom_histogram()
ggplot(df.train,aes(Fare))+geom_histogram()
#mising Age value
ggplot(df.train,aes(Pclass,Age))+geom_boxplot(aes(group=Pclass,fill=factor(Pclass)))+scale_y_continuous(breaks = seq(min(0),max(80),by=3))

impute_age <- function(age,class){
  
  
  out <- age
  
  for(i in 1:length(age)){
    if(is.na(age[i])){
      if(class[i]==1){
        out[i] <- 37
      }else if (class[i]==2){
        out[i] <- 29
      }else if (class[i]==3){
        out[i] <- 24
      }
      
    }else {
      out[i] <- age[i]
    }
  }
  return(out)
  
}



fixed.ages <- impute_age(df.train$Age,df.train$Pclass)
df.train$Age <- fixed.ages
df.train$Age
missmap(df.train,main = 'Main',col= c('yellow','black'),legend = FALSE)
str(df.train)
library(dplyr)

df.train <-select(df.train,-Name,-Cabin,-Ticket,-PassengerId)
df.train
head(df.train)
str(df.train)
df.train$Survived <- factor(df.train$Survived)
df.train$Pclass <- factor(df.train$Pclass)
df.train$Parch <- factor(df.train$Parch)
df.train$SibSp <- factor(df.train$SibSp)
ggplot(df.train,aes(SibSp))+geom_bar()
str(df.train)
log.model <- glm(Survived ~.,family = binomial(link = 'logit'),data = df.train)
summary(log.model)