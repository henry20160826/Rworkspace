library(rpart)
library(caret)
library(ROCR)
fit <- rpart(Kyphosis ~ Age + Number + Start, data = kyphosis)
#读取数据
data<-read.table("E:\\graduated life\\pattern recognition\\data.txt",header = T)
#空缺的喜好数据按0.5处理，表示对这个喜好中立

# #全部数据生成决策树
# d<-data[,c(2,4,5,7,8,9)]
# fit <- rpart(sex ~ height+weight+math+literature+sport, data = d)
# par(mfrow = c(1,2), xpd = NA) # otherwise on some devices the text is clipped
# plot(fit)
# text(fit, use.n = TRUE)
# #test
# pred<-predict(fit, type = "matrix")
# bpred<-round(pred)
# trueSex<-d[,1]
printPredictResult<-function()
{
  print(confusionMatrix(bpred,trueSex,positive = "1"))
  # test(trainSet,result)
  #画出ROC曲线
  pred <- prediction(pred,trueSex)
  perf <- performance(pred,"tpr","fpr")
  #男生女生按数量分开时auc值变化不大
  auc<-performance(pred,"auc")
  print(auc)#0.9760652#0.9755639
  plot(perf,colorize=TRUE)
}
printPredictResult()#0.9128比想象好得多

#交叉验证
#交叉验证
ktime=10
fold=createFolds(data$num,k=ktime)

pred=NULL
bpred<-NULL
trueSex<-NULL

for(i in 1:ktime)
{
  #生成决策树
  trainSet<-data[-fold[[i]],c(2,4,5,7,8,9)]
  fit <- rpart(sex ~ height+weight+math+literature+sport, data = trainSet)
  par(mfrow = c(1,2), xpd = NA) # otherwise on some devices the text is clipped
  plot(fit)
  text(fit, use.n = TRUE)
  #test
  testSet<-data[fold[[i]],c(2,4,5,7,8,9)]
  temppred<-predict(fit,testSet ,type = "matrix")
  tempbpred<-round(temppred)
  trueSex<-c(trueSex,testSet[,1])
  pred<-c(pred,temppred)
  # print(pred)
  bpred<-c(bpred,tempbpred)
  # print(bpred)
}
printPredictResult()#0.8389









