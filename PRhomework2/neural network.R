library(caret)
library(ROCR)
#读取数据
data<-read.table("E:\\graduated life\\pattern recognition\\data.txt",header = T)
#空缺的喜好数据按0.5处理，表示对这个喜好中立
#归一化身高体重
d=data
d[,4]<-scale(d[,4])
d[,5]<-scale(d[,5])

inputNodeNum=5
hiddenNodeNum=5
outputNodeNum=1

#初始化参数
#用随机矩阵生成初始值
setArguments<-function()
{
  # set.seed(1)
  w1 <<- runif(hiddenNodeNum^2,-0.3,0.3)
  dim(w1)<<-c(hiddenNodeNum,hiddenNodeNum)
  w2 <<- runif(hiddenNodeNum,0.-0.3,0.3)
  dim(w2)<<-c(1,hiddenNodeNum)
  # print(w2)
  #theta对分类效果影响极大，没有theta,男生女生只能按照数量比例分开
  theta1<<-runif(hiddenNodeNum,-0.3,0.3)
  theta2<<-runif(outputNodeNum,-0.3,0.3)
  # theta1=c(0,0,0,0,0)
  # theta2=0
  dim(theta1)<<-c(1,hiddenNodeNum)
}


sigmoid<-function(x)
{
  r=1+exp(-x)
  return(1/r)
}

tempPredictData=1
tempPresult<-factor(0:1)
tempTrueSex<-factor(0:1)
test<-function(testSet,result)
{
  len=dim(testSet)[1]
  tempPredictData<<-1
  tempPresult<<-1
  tempTrueSex<<-1
  for(i in 1:len)
  {
    tempPredictData[i]<<-NNpredict(testSet[i,])
    tempPresult[i]<<-round(tempPredictData[i])
    tempTrueSex[i]<<-result[i]
  }
  t=table(tempPresult,result)
  # print(confusionMatrix(pr,tureSex,positive = "1"))
  # print(t)
  # print(result)
  #返回错误数
  if(1==dim(t)[1])
  {
    if(1==dim(t)[2])
    {
      return(0)#dim(t)=1*1
    }
    return(t[1,2])#dim(t)=1*2
  }
  if(1==dim(t)[2])
  {
    return(t[2,1])#dim(t)=2*1
  }
  return(t[1,2]+t[2,1])#dim(t)=2*2
}
train<-function(trainSet,result,step=0.3,acc=0.95,time=15)
{
  len=dim(trainSet)[1]
  #将训练集最多训练100遍
  for(j in 1:time)
  {
    for(i in 1:len)
    {
      #正向计算
      datax=as.matrix(trainSet[i,])
      dim(datax)<-c(inputNodeNum,1)
      x=w1%*%datax+t(theta1)#5*1
      xo=sigmoid(x)#5*1
      y=w2%*%xo+theta2#1*1
      yo=as.numeric(sigmoid(y))#1*1
      
      #反向传播
      error=result[i]-yo#1*1
      d2=error*yo*(1-yo)#g 1*1
      # print(xo)
      # print(w2)
      d1=xo*(1-xo)*t(w2)*d2#5*1
      #更新隐层到输出层权值
      #使用<<-在函数内进行复制才能影响外面的变量
      w2<<-w2+step*d2*t(xo)#5*1
      # print(t(as.matrix(trainSet[i,])))
      # print(d1)
      # print(kronecker(d1,t(as.matrix(trainSet[i,]))))
      w1<<-w1+step*d1%*%t(datax)#5*5
      theta2<<-theta2+step*d2
      theta1<<-theta1+step*t(d1)
    }
    error=test(trainSet,result)
    if(error/len<=1-acc)
    {
      print("训练正确率达标")
      return(TRUE)
    }
  }
  if(error/len>1-acc)
  {
    print("训练正确率未达标")
    return(FALSE)
  }

  
}
NNpredict<-function(x)
{
  #正向计算
  # print(x)
  x=as.matrix(x)
  dim(x)<-c(inputNodeNum,1)
  # print(x)
  # print(w1)
  # print(t(theta1))
  x=w1%*%x+t(theta1)
  
  xo=sigmoid(x)
  y=w2%*%xo+theta2
  yo=sigmoid(y)
  return(yo)
}

predictData=NULL
presult<-NULL
trueSex<-NULL

printPredictResult<-function()
{
  print(confusionMatrix(presult,trueSex,positive = "1"))
  # test(trainSet,result)
  #画出ROC曲线
  pred <- prediction(predictData,trueSex)
  perf <- performance(pred,"tpr","fpr")
  #男生女生按数量分开时auc值变化不大
  auc<-performance(pred,"auc")
  print(auc)#0.9760652#0.9755639
  plot(perf,colorize=TRUE)
}

# # 使用全部数据进行训练测试
# trainSet=d[, c (4,5,7,8,9)]
# result=d[,2]
# trainSet<-as.matrix(trainSet)
# setArguments()
# train(trainSet,result,step=0.3,acc=1,time=10)
# #10 0.9195
# #100 0.9329
# #1000 0.9732
# #10000 0.9732
# #全部样本训练最少有4个样本被错分
# presult<-tempPresult
# trueSex<-tempTrueSex
# predictData<-tempPredictData
# printPredictResult()

#交叉验证
ktime=10
fold=createFolds(d$num,k=ktime)
for(i in 1:ktime)
{
  setArguments()
  x<-d[-fold[[i]],c (4,5,7,8,9)]
  y<-d[-fold[[i]],2]
  testX<-d[fold[[i]],c (4,5,7,8,9)]
  testY<-d[fold[[i]],2]
  train(x,y,time=15)
  test(testX,testY)
  predictData<-c(predictData,tempPredictData)
  # print(predictData)
  presult<-c(presult,tempPresult)
  trueSex<-c(trueSex,tempTrueSex)
  #得到多次交叉验证的
}
printPredictResult()
#3 0.7651
#5 0.8389
#8 0.906
#10 0.9195 
#13 0.9128
#15 0.9128
#20 0.9128
#30 0.906
#40 0.9195
#50 0.8993
#100 0.906
#200 0.8926


