library(xlsx);
library(caret)
library(e1071)
#数据预处理
sampledata=read.xlsx("E:\\graduated life\\pattern recognition\\SampleData.xls",sheetName="Sheet1",header = T,encoding = "UTF-8");
folds<-createFolds(sampledata$编号,k=10)
sampledata<-sampledata[,c(2,4,5,7,8,9)];#抽取性别身高体重等数据
sampledata[,2]=(sampledata[,2]-171)/7.23#身高归一化
sampledata[,3]=(sampledata[,3]-62.17)/11.4#体重归一化


sigmoid<-function(x){#simoid函数
  1/(1+exp(-x))
}

predictions<-vector()
TrueY<-vector()
for(time in 1:10){#10倍交叉验证
  trainX<-sampledata[-folds[[time]],c(2,3,4,5,6)]
  trainY<-sampledata[-folds[[time]],1]
  testX<-sampledata[folds[[time]],c(2,3,4,5,6)]
  testY<-sampledata[folds[[time]],1]
  
  #初始化参数
  samp_num=length(trainX[,1])
  input_num=length(trainX[1,])
  output_num=1
  hidden_num=5
  omega1=matrix(0.1*runif(input_num*hidden_num,0,1),input_num,hidden_num)#输入层权值
  omega2=matrix(0.1*runif(hidden_num*output_num,0,1),hidden_num,output_num)#隐层权值
  theta1=matrix(0.1*runif(hidden_num,0,1),1,hidden_num)#隐层偏置量
  theat2=matrix(0.1*runif(output_num,0,1),output_num,1)#输出层偏置量
  eta=0.3 #学习率
  
  #训练
  for(j in 1:12){
    # num=0#Ctrl+Shift+C
    for(i in 1:samp_num){
      result=trainY[i]
      #前向传播
      hidden_value=as.matrix(trainX[i,],nrow=1)%*%omega1+theta1
      hidden_output=apply(hidden_value,2,sigmoid)#1*5
      output_value=hidden_output%*%omega2+theat2
      out=apply(output_value,2,sigmoid)
      
      #逆向过程
      error=result-out
      delta2=error*out*(1-out)#g 1*1
      delta1=hidden_output*(1-hidden_output)*t(omega2)*delta2#e 5*1
      #更新隐层到输出层权值
      omega2=omega2+eta*delta2*as.matrix(hidden_output)#5*1
      omega1=omega1+eta*t(as.matrix(trainX[i,],nrow=1))%*%delta1#5*5
      theat2=theat2+eta*delta2
      theta1=theta1+eta*delta1
      #print(error)
      # if(abs(error)>0.5){
      #   num=num+1
      #   print(c(i,error))
      # }
    }
    # print(num)
  }
  
  #test
  for(i in (1:length(testX[,1]))){
    hidden_value=as.matrix(testX[i,],nrow=1)%*%omega1+theta1
    hidden_output=apply(hidden_value,2,sigmoid)#1*5
    output_value=hidden_output%*%omega2+theat2
    out=apply(output_value,2,sigmoid)
    #result=testY[i]
    #error=result-out
    #print(error)
    # if(abs(error)>0.5){
    #   num=num+1
    #   print(c(i,error))
    # }
    predictions<-c(predictions,round(out))
  }
  TrueY<-c(TrueY,testY)
}

confusionMatrix(predictions,TrueY,positive="1")


