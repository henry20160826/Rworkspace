#SVM
#install.packages("e1071") 
library(e1071)
library(caret)
library(ROCR)
printPredictResult<-function()
{
  print(confusionMatrix(bpred,trueSex,positive = "1"))
  # 画出ROC曲线
  pred <- prediction(pred,trueSex)
  perf <- performance(pred,"tpr","fpr")
  #男生女生按数量分开时auc值变化不大
  auc<-performance(pred,"auc")
  print(auc)
  plot(perf,colorize=TRUE)
}
#读取数据
d<-read.table("E:\\graduated life\\pattern recognition\\data.txt",header = T)
#使用$按列名取出data.frame中的某一列，支持中文

# # #训练集为全集
# x <- d[, c (4,5,7,8,9)]
# y <- as.factor(d$sex)
# # 训练svm模型
# model <- svm(x,y, kernel = "linear", cost = 1, scale = TRUE)
# # print(model)
# pred<-predict(model,x,decision.values = TRUE)
# pred<-attr(pred,"decision.values")
# # print(pred)
# bpred<-predict(model,x,decision.values = FALSE)
# # print(bpred)
# trueSex<-y
# printPredictResult()

# #radial 0.9329
# #sigmoid 0.8523
# #polynomial 0.9329
# #linear 0.9195

#交叉验证
ktime=10
fold=createFolds(d$num,k=ktime)
pred=NULL
bpred<-NULL
trueSex<-NULL
for(i in 1:ktime)
{
  x<-d[-fold[[i]],c (4,5,7,8,9)]
  y<-as.factor(d[-fold[[i]],2])
  # 训练svm模型
  model <- svm(x,y, data = inputData, kernel = "polynomial", cost = 10, scale = TRUE)
  # print(model)
  #预测模型
  x<-d[fold[[i]],c (4,5,7,8,9)]
  y<-as.factor(d[fold[[i]],2])
  trueSex<-c(trueSex,y)
  temppred<-predict(model,x,decision.values = TRUE)
  temppred<-attr(temppred,"decision.values")
  pred<-c(pred,temppred)
  # print(pred)
  tempbpred<-as.numeric(predict(model,x,decision.values = FALSE))
  bpred<-c(bpred,tempbpred)
  # print(bpred)
}
bpred<-as.factor(bpred-1)
trueSex<-as.factor(trueSex-1)
printPredictResult()
#radial 0.8591
#linear 0.9128
#polynomial 0.9396
#sigmoid 0.8255



#交叉验证
#共149个人,分成5组进行交叉验证
#增加随机数列，排序进行分组，从而保证随机分组
# r=sample(0:300,149)
# adata<-data.frame(d,r)
# adata<-adata[order(adata[,11]),]
# m=1
# #进行5次验证
# for(i in 0:4)
# {
#   #取出训练集与测试集
#   if(i!=4)
#   {
#     testSet<-adata[(i*30+1):(i*30+30),]
#   }
#   else
#   {
#     testSet<-adata[(i*30+1):149,]
#   }
#   if(i==4)
#   {
#     train<-adata[1:(i*30),]
#   }
#   else if(i==0)
#   {
#     train<-adata[(i*30+31):149,]
#   }
#   else
#   {
#     a=1:(i*30)
#     b=(i*30+31):149
#     a<-c(a,b)
#     train<-adata[a,]
#   }
#   #训练模型
#   inputData <- data.frame(train[, c (4,5,7,8,9)], response = as.factor(train$性别))
#   # radial SVM
#   svmfit <- svm(response ~ ., data = inputData, kernel = "radial", cost = 10, scale = FALSE) # radial svm, scaling turned OFF
#   #使用testSet进行测试
#   test<-data.frame(testSet[, c (4,5,7,8,9)])
#   p=predict(svmfit,test)
#   # print(p)
#   # print(testSet$性别)
#   #取出测试集真实性别
#   idata=testSet$性别
#   #比较测试数据与真实数据
#   compareTable <- table (idata, p)  # tabulate
#   # print(compareTable)
#   #保存每次的平均错误率
#   m[i+1]=mean(idata!= p)
#   print(m[i+1])
# }
# #计算总体平均错误率
# mean(m)
# #几次运行的最终结果
# #radial
# #全部训练时6.04%
# #16.07%
# #18.73%
# #17.51%
# #14.11%
# #是否归一化对结果影响不大
# #sigmod
# #全部训练时14.77%
# #23.40%
# #23.47%
# #23.54%
# #23.47%
# #linear
# #全部训练时7.38%
# #8.69%
# #10.05%
# #8.74%
# #10.76%