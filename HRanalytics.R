# HR Analytics
hrtrain=read.csv(file.choose(),na.strings = c("",'NA'))
hrtest=read.csv(file.choose(),na.strings = c("",'NA'))
head(hrtrain)
tail(hrtrain)
nrow(hrtrain)
ncol(hrtrain)
class(hrtrain)
str(hrtrain)
# Dependent Variable (y)- is_promoted (0/1)-(No/Yes)
# BinaryVariables (0/1)-(No/Yes) -awards_won, KPIs_met80
table(hrtrain$is_promoted) # No - 50140; Yes-4668
# Find Missing Values or NAs
sort(colSums(is.na(hrtrain)),decreasing = T)
sort(colSums(is.na(hrtest)),decreasing = T)
# Combine Data Since same Columns have NAs
hrtest['is_promoted']='test'
combinedf=rbind(hrtrain,hrtest)
sort(colSums(is.na(combinedf)),decreasing = T)
# Missing Values are in Categorical Variables
table(combinedf$previous_year_rating)
combinedf$previous_year_rating=ifelse(is.na(
  combinedf$previous_year_rating),which.max(table(
    combinedf$previous_year_rating)),
  combinedf$previous_year_rating)
table(combinedf$education)
combinedf$education=ifelse(is.na(combinedf$education),
                           "Bachelor's",combinedf$education)
table(combinedf$education)
colnames(combinedf)
# Convert Character Columns to Factor Columns
factorcols=c("department","region","education","gender",
             "recruitment_channel","previous_year_rating",
             "KPIs_met..80.","awards_won.")
combinedf[,factorcols]=lapply(combinedf[,factorcols],
                              factor)  
str(combinedf)
#-------------------------------------------------------------------------------

# Split Data back to train & test
hrtraindf=subset(combinedf,combinedf$is_promoted!='test')
hrtestdf=subset(combinedf,combinedf$is_promoted=='test')
hrtestdf=hrtestdf[-14]
hrtraindf$is_promoted=as.factor(hrtraindf$is_promoted)

#-------------------------------------------------------------------------------

# Build logistic Regression Model
hrlogistic=glm(is_promoted~.,data=hrtraindf[-1],
               family = 'binomial')
summary(hrlogistic)
# AIC: 21487 - Comparitive Metric with any other AIC
# Null deviance: 31922  on 54807  degrees of freedom
# Residual deviance: 21373  on 54751  degrees of freedom
hrlogisticpredict=predict(hrlogistic,type='response')
table(Actual=hrtraindf$is_promoted,
      Predicted=hrlogisticpredict>0.50)
(49808+1272)/(49808+332+3396+1272)# Accuracy=0.9319807

#-------------------------------------------------------------------------------

# Hypothesis Testing
# Test Null Average Age of is_promoted equal?
# aggregate(), identify test, Conduct Test, infer - p-value
aggregate(hrtraindf$age~hrtraindf$is_promoted,FUN=mean)
# test - 2 Sample Independent ttest
t.test(hrtraindf$age~hrtraindf$is_promoted)
# Since p-value = 1.636e-05 is less than 0.05, Reject Null
aggregate(hrtraindf$length_of_service~hrtraindf$is_promoted,
          FUN=mean)
t.test(hrtraindf$length_of_service~hrtraindf$is_promoted)
# Since p-value = 0.008263 is less than 0.05, Reject Null
aggregate(hrtraindf$length_of_service~hrtraindf$education,
          FUN=mean)
# test - Anova Single Factor
summary(aov(hrtraindf$length_of_service~hrtraindf$education))
# Since Pr(>F)=<2e-16 is less than 0.05, Reject Null
aggregate(hrtraindf$avg_training_score~hrtraindf$recruitment_channel,
          FUN=mean)
summary(aov(hrtraindf$avg_training_score~hrtraindf$recruitment_channel))
# Since Pr(>F) =6.59e-12 is less than 0.05, Reject Null
table(hrtraindf$gender,hrtraindf$awards_won.)
# test = Chi Square test of Independence
chisq.test(hrtraindf$gender,hrtraindf$awards_won.)
# Since p-value = 0.5986 is greater than 0.05, Fail to
# Reject Null
# Test Null No Association between gender & is_promoted
chisq.test(hrtraindf$gender,hrtraindf$is_promoted)
# Since p-value = 0.009765 is less than 0.05, Reject null

#-------------------------------------------------------------------------------

# Decision Tree Model - Classification
library(rpart) # Recursive Partitioning Trees
install.packages('rpart.plot')
library(rpart.plot)
hrdectree=rpart(is_promoted~.,data=hrtraindf[-1])
summary(hrdectree)
rpart.plot(hrdectree,cex=0.50)
rpart.rules(hrdectree)
rpartpredict=predict(hrdectree,type="class")
table(Actual=hrtraindf$is_promoted,
      Predicted=rpartpredict)
(49906+1194)/(49906+234+3474+1194)
library(caret)
confusionMatrix(hrtraindf$is_promoted,rpartpredict,
                positive = '1')

#-------------------------------------------------------------------------------


# Random Forest
library(randomForest)
hrRF=randomForest(is_promoted~.,data=hrtraindf[-1])
print(hrRF)
100-6.18 # Accuracy - 93.82
plot(hrRF)
varImpPlot(hrRF)

#-------------------------------------------------------------------------------


# Imbalanced Data - Huge Difference in Classes Counts
table(hrtraindf$is_promoted) # Dependent Variable
install.packages('ROSE')
library(ROSE)
hrtrain_balanced <- ovun.sample(is_promoted ~ ., 
                                data = hrtraindf, 
                                method = "over",
                                N = 50140*2, seed = 1)
nrow(hrtrain_balanced)
table(hrtrain_balanced$is_promoted)
write.csv(hrtrain_balanced,"hrbalance.csv")
hrRF2=randomForest(is_promoted~.,data=hrtrain_balanced[-1])
print(hrRF2)
100-3.36 # Accuracy - 96.64
plot(hrRF2)
varImpPlot(hrRF2)

#-------------------------------------------------------------------------------



# Gradient Boosting Machine
library(caret)
trCtrl=trainControl(method='cv',number=2)
tree=expand.grid(n.trees=c(1000),interaction.depth=c(2),shrinkage=0.1,
                 n.minobsinnode = 10)
hrgbm=train(is_promoted~.,data=hrtraindf[-1],method='gbm',trControl=trCtrl,
            verbose=T,tuneGrid=tree)
summary(trCtrl)
print(trCtrl)
