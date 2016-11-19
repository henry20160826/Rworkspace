library(e1071)
library(caret)
library(ROCR)
#读取数据
original_data<-read.table("E:\\graduated life\\pattern recognition\\data.txt",header = T)
#空缺的喜好数据按0.5处理，表示对这个喜好中立
d<-original_data[,c(4,5,7,8,9,10)]
#按列归一化数据
for(i in 1:6)
{
  d[,i]<-scale(d[,i])
}
#R能直接计算协方差矩阵，也是醉了，自己写了那么多
o<-cov(d)
print(o)
k<-norm(o,"O")*norm(solve(o),"O")
print(k)
# #是否归一化对特征值影响很大,但实际分类时，差别不大
# mean_vector<-function(data)
# {
#   len<-dim(data)[2]#data列数
#   r<-vector(mode="numeric",length = len)
#   for(i in 1:len)
#   {
#     r[i]<-mean(data[,i])
#   }
#   return(r)
# }
# Ed<-mean_vector(d)#总体的均值矢量
# matrix<-0
# for(i in 1:149)
# {
#   v<-as.matrix(d[i,])-Ed
#   print(v)
#   m=t(v)%*%v
#   matrix<-matrix+m
# }
# #得到协方差矩阵
# o<-matrix/149
# print(o)
#计算特征值和特征向量
eigenResult=eigen(o)
lambda=eigenResult[[1]]
lambda_vecror=eigenResult[[2]]
sum1_lambda=lambda/sum(lambda)
plot(type = 'o',sum1_lambda,main="数据中心化标准化后的特征值图",ylim=c(0,0.35))
# print(format(sum1_lambda,digits = 2))
text(sum1_lambda,paste(format(sum1_lambda,digits = 2)),pos = 3)
#eigen()求得的特征向量以列向量的方式放在矩阵中
newd<-matrix(0,149,6)
#对原始特征进行变换
for(i in 1:6)
{
  for(j in 1:149)
  {
    v1=as.numeric(d[j,])
    v2=lambda_vecror[,i]
    newd[j,i]<-v1%*%v2
  }
}

# print(t(lambda_vecror)%*%lambda_vecror)#得到的是标准正交化的特征向量

#newd为使用特征向量变换后的新特征数据
#为newd添加性别列

newd<-data.frame(original_data[,2],newd)
d<-newd
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
ktime=10
fold=createFolds(original_data$num,k=ktime)
pred=NULL
bpred<-NULL
trueSex<-NULL
for(i in 1:ktime)
{
  lie<-c (2,3,4,5,6)
  x<-d[-fold[[i]],lie]
  y<-as.factor(d[-fold[[i]],1])
  # 训练svm模型
  model <- svm(x,y, data = inputData, kernel = "polynomial", cost = 10, scale = TRUE)
  # print(model)
  #预测模型
  x<-d[fold[[i]],lie]
  y<-as.factor(d[fold[[i]],1])
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
print(sum1_lambda)
