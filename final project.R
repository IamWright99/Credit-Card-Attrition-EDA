library(tidyverse)
install.packages('cluster')
library(cluster)
install.packages('factoextra')
library(factoextra)
install.packages('dendextend')
library(dendextend)
install.packages('ggplot2')
library(ggplot2)
install.packages('car')
library(car)
install.packages('caret')
library(caret)
install.packages('Mass')
library(MASS)
install.packages("leaps")
library(leaps)

CC <- read.csv("creditcard.csv")
summary(CC)
view(CC)
head(CC)

CC <- na.omit(CC)

CC$Attrition_Flag <- ifelse(CC$Attrition_Flag =="Attrited Customer","1","0")
CC$Attrition_Flag <- as.factor(CC$Attrition_Flag)
colnames(CC)[2] <- 'label'

nonattritionCC <- CC %>%
  filter(label == '0')
view(nonattritionCC)
attritionCC <- CC %>%
  filter(label == '1')
view(attritionCC)# test

nonattritionCC_income <- nonattritionCC %>%
  filter(Income_Category == '$40K - $60K')
attritionCC_income <- attritionCC %>%
  filter(Income_Category == '$40K - $60K')

nonattritionCC_graduate <- nonattritionCC %>%
  filter(Education_Level == 'Graduate')
attritionCC_graduate <- attritionCC %>%
  filter(Education_Level == 'Graduate')
#univariate
ggplot(CC, aes(x = label))+
  geom_bar()
ggplot(CC, aes(x = Gender))+
  geom_bar()
ggplot(CC, aes(x = Education_Level))+
  geom_bar()
ggplot(CC, aes(x = Marital_Status))+
  geom_bar()
ggplot(CC, aes(x = Income_Category))+
  geom_bar()
ggplot(CC, aes(x = Card_Category))+
  geom_bar()
ggplot(CC, aes(x = Dependent_count))+
  geom_bar()
hist(CC$Customer_Age, breaks = 5)
hist(CC$Credit_Limit, breaks = 10)
hist(nonattritionCC$Customer_Age, breaks = 10)
hist(attritionCC$Customer_Age, breaks = 10)
#bivariate
scatterplot(Months_on_book ~ Customer_Age, data = CC)
scatterplot(Credit_Limit ~ Months_on_book, data = CC)
scatterplot(Months_on_book ~ Customer_Age, data = nonattritionCC_income)
scatterplot(Months_on_book ~ Customer_Age, data = attritionCC_income)
scatterplot(Credit_Limit ~ Customer_Age, data = nonattritionCC_income)
scatterplot(Credit_Limit ~ Customer_Age, data = attritionCC_income)
scatterplot(Months_on_book ~ Customer_Age, data = nonattritionCC_graduate)
scatterplot(Months_on_book ~ Customer_Age, data = attritionCC_graduate)
#multivariate
ggplot(CC, aes(x= Customer_Age, fill = label)) + geom_histogram(stat="count")  + geom_bar(position= position_dodge(), alpha = .75)
p1 <- CC %>% 
  group_by(label,Months_on_book,Customer_Age) %>%
  ggplot(aes(x= Customer_Age, y= Months_on_book,fill=label)) + geom_col(position="dodge") + theme_light() + theme(legend.position="bottom")
p1
#model
df <- CC%>% select(-c(Months_on_book,Total_Trans_Amt, Total_Amt_Chng_Q4_Q1, Avg_Utilization_Ratio, Avg_Open_To_Buy))
dim(df)
trainIdx <- createDataPartition(df$label, p = .75,list=FALSE)
trainCC <- df[trainIdx,]
testCC <- df[-trainIdx,]
trainCC <- trainCC %>% select(-c('CLIENTNUM'))
testCC <- testing %>% select(-c('CLIENTNUM'))

model <- glm(label~.,data = trainCC, family = 'binomial')
model

newmodel <- lm(as.numeric(label) ~ Customer_Age + Gender + Dependent_count + Education_Level + Marital_Status + Income_Category + Card_Category+ Credit_Limit ,data = df)
summary(newmodel)
newmodel$coefficients

models <- regsubsets(label~., data = trainCC, nvmax = 11,
                     method = "backward")
summary(models)

model2 <- stepAIC(newmodel, direction = "backward", 
                  trace = FALSE)
model2$coefficients



