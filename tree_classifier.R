library(ISLR)
library(tree)
library(MASS)
library(caret)
library(e1071)
library(pROC)
library(readxl)

library(rpart)
library(rpart.plot)

#Data preprocess and cleaning
df_final_raw <- read.csv("D:/Ghazi/Kuliah/Semester 6/MetLit/ta_metlit/df_raw_updated.csv")
df_final_raw_deleted <- read.csv("D:/Ghazi/Kuliah/Semester 6/MetLit/TA/df_raw_updated_deleted.csv")
df_final_raw_photokat <- read_excel("D:/Ghazi/Kuliah/Semester 6/MetLit/ta_metlit/data TA.xlsx")
dfr = df_final_raw_photokat
dfr$photo_quan = NULL

dfr$gender = as.factor(dfr$gender)
dfr$job = as.factor(dfr$job)
dfr$phone_dur = as.factor(dfr$phone_dur)
dfr$app_dur = as.factor(dfr$app_dur)
dfr$h_w_inc = as.factor(dfr$h_w_inc)
dfr$education = as.factor(dfr$education)
dfr$earning = as.factor(dfr$earning)
dfr$get = as.factor(dfr$get)
dfr$photo_quan_kat = as.factor(dfr$photo_quan_kat)

dfr$photo_quan = as.integer(dfr$photo_quan)
dfr$age = as.integer(dfr$age)
dfr$height = as.integer(dfr$height)
dfr$weight = as.integer(dfr$weight)

str(dfr)

#Training dataset
set.seed(101)
train = sample(1:nrow(dfr),77)
dfr_train = dfr[train,]
dfr_test = dfr[-train,]

#Tree full var
tree1 = tree(get~.,dfr,subset = train)
summary(tree1)
plot(tree1)
text(tree1,pretty=0)
prediction = predict(tree1,dfr[train,],type='class')
with(dfr[train,],table(prediction,get))

#Tree cv
treecv = cv.tree(tree1,FUN = prune.misclass)
plot(treecv)
prune.tree = prune.misclass(tree1,best=12)
plot(prune.tree)
text(prune.tree,pretty=0)
prediction = predict(prune.tree,dfr,type='class')
with(dfr,table(prediction,get))

#Tree rpart
treer = rpart(get~. , data=dfr,method='class')
rpart.plot(treer)
predict_treer = predict(treer, dfr,type='class')
table_treer = table(dfr$get,predict_treer)
table_treer
acc = sum(diag(table_treer)) / sum(table_treer)
acc
confusionMatrix(predict_treer,dfr$get,positive = 'Ya')

#Roc auc
p1 = predict(treer,dfr,type = 'prob')
p1 = p1[,2]
r = multiclass.roc(dfr$get,p1,percent = TRUE)
roc = r[['rocs']]
r1 = roc[[1]]
plot.roc(r1,
         print.auc=TRUE,
         auc.polygon=TRUE,
         grid=c(0.1, 0.2),
         grid.col=c("green", "red"),
         max.auc.polygon=TRUE,
         auc.polygon.col="lightblue",
         print.thres=TRUE,
         main= 'ROC Curve')
