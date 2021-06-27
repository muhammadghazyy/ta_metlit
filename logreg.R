library(MASS)
library(ggplot2)
library(caret)
library(e1071)
library(pROC)
library(ResourceSelection)
library(readxl)


df_final <- read.csv("D:/Ghazi/Kuliah/Semester 6/MetLit/TA/df_final.csv")
df_final_onehot <- read.csv("D:/Ghazi/Kuliah/Semester 6/MetLit/TA/df_final_onehot.csv")


#Raw 
df_final_raw <- read.csv("D:/Ghazi/Kuliah/Semester 6/MetLit/ta_metlit/df_raw_updated.csv")
df_final_raw_deleted <- read.csv("D:/Ghazi/Kuliah/Semester 6/MetLit/TA/df_raw_updated_deleted.csv")
df_final_raw_photokat <- read_excel("D:/Ghazi/Kuliah/Semester 6/MetLit/ta_metlit/data TA.xlsx")
dfr = df_final_raw
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
xtabs(~get + gender , data=dfr)
xtabs(~get + job , data=dfr)
xtabs(~get + phone_dur , data=dfr)
xtabs(~get + app_dur , data=dfr)
xtabs(~get + h_w_inc , data=dfr)
xtabs(~get + education , data=dfr)
xtabs(~get + earning , data=dfr)


#Full model
logistic <- glm(get~.,family = binomial(link="logit"),data=dfr)

summary(logistic)

ll.null <- logistic$null.deviance/-2
ll.proposed <- logistic$deviance/-2
logmcfaddenr2 = ((logistic$null.deviance/-2) - (logistic$deviance/-2)) / (logistic$null.deviance/-2)
logmcfaddenr2
pval = 1 - pchisq(2*(ll.proposed - ll.null), df=(length(logistic$coefficients)-1))
pval

#Model kita
modelkita = glm(get~gender+age+height+weight+photo_quan+education,data = dfr,family = binomial((link='logit')))
summary(modelkita)

#Stepwise moodel
step = stepAIC(logistic,direction = 'both',trace=FALSE)
step
summary(step)
step_r2 = ((step$null.deviance/-2) - (step$deviance/-2)) / (step$null.deviance/-2)
step_r2
hoslem.test(dfr$get , fitted(step))

#Final model
finalmodel = glm(get~app_dur,family = binomial(link='logit'),data=dfr)
summary(finalmodel)
finalr2 = ((finalmodel$null.deviance/-2) - (finalmodel$deviance/-2)) / (finalmodel$null.deviance/-2)
finalr2

########## plotting model full
predicted.data <- data.frame(
  probability.of.dapet=logistic$fitted.values,
  get=dfr$get)

predicted.data <- predicted.data[order(predicted.data$probability.of.dapet, decreasing=FALSE),]

predicted.data$rank <- 1:nrow(predicted.data)

predicted.data

ggplot(data=predicted.data, aes(x=rank, y=probability.of.dapet)) +
  geom_point(aes(color=get), alpha=1, shape=10, stroke=2) +
  xlab("Index") +
  ylab("Predicted probability of getting partner")


############ plotting model satunya lagi
predicted.data.step <- data.frame(
  probability.of.dapet=step$fitted.values,
  get=dfr$get)

predicted.data.step <- predicted.data.step[
  order(predicted.data.step$probability.of.dapet, decreasing=FALSE),]
predicted.data.step$rank <- 1:nrow(predicted.data.step)

predicted.data.step

ggplot(data=predicted.data.step, aes(x=rank, y=probability.of.dapet)) +
  geom_point(aes(color=get), alpha=1, shape=10, stroke=2) +
  xlab("Index") +
  ylab("Predicted probability of getting partner")


### Roc auc logistic
r = multiclass.roc(dfr$get,fitted(step),percent = TRUE)
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


### Step paper
#1 decision tree , interpretasi
#2 full log model, perhatikan AIC
#3 stepaic full log model , aic lebih bagus
#4 goodnes of fit hoslem test , step model, model bagus
#5 interpretasi
#6 kesimpulan