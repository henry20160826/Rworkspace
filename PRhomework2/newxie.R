library(xlsx);
library(caret)
library(e1071)
#数据预处理
sampledata=read.table("E:\\graduated life\\pattern recognition\\data.txt",header = T)
folds<-createFolds(sampledata$编号,k=10)
sampledata<-sampledata[,c(2,4,5,7,8,9)];#抽取性别身高体重等数据
sampledata[,2]=(sampledata[,2]-171)/7.23#身高归一化
sampledata[,3]=(sampledata[,3]-62.17)/11.4#体重归一化


sigmoid<-function(x){#simoid函数
  1/(1+exp(-x))
}

predictions<-vector()
TrueY<-vector()
for(time in 1:1){#10倍交叉验证
  trainX<-sampledata[,c(2,3,4,5,6)]
  trainY<-sampledata[,1]
  testX<-sampledata[,c(2,3,4,5,6)]
  testY<-sampledata[,1]
  
  #初始化参数
  samp_num=length(trainX[,1])
  input_num=length(trainX[1,])
  output_num=1
  hidden_num=5
  w1<-c(0.0261425670,0.0819623340,0.0970150437,0.0524093603,0.0677613818,0.0252034826,0.0077238966,0.0334101093,0.0013001170,0.0085025507,0.0093330588,0.0763552567,0.0579968153,0.0473597212,0.0735342627,0.0454154698,0.0151626913,0.0966017500,0.0594729639,0.0161690623,0.0030731637,0.0579156959,0.0735095435,0.0452782375,0.0172195707)
  dim(w1)=c(5,5)
  w2<-c(0.0242756692,0.0401885895,0.0005054474,0.0996186960,0.0352521698)
  dim(w2)=c(5,1)
  theta1=c(0.0242756692,0.0401885895,0.0005054474,0.0996186960,0.0352521698)
  dim(theta1)=c(1,5)
  theta2=0.0401885895
  # theta1=0
  # theta2=0
  eta=0.3 #学习率
  
  #训练
  for(j in 1:12){
    # num=0#Ctrl+Shift+C
    for(i in 1:149){
      result=trainY[i]
      #前向传播
      hidden_value=as.matrix(trainX[i,],nrow=1)%*%w1+theta1#1*5
      hidden_output=apply(hidden_value,2,sigmoid)#1*5
      output_value=hidden_output%*%w2+theta2#1*1
      out=apply(output_value,2,sigmoid)#1*1
      # print(out)
      
      #逆向过程
      error=result-out
      delta2=error*out*(1-out)#g 1*1
      delta1=hidden_output*(1-hidden_output)*t(w2)*delta2#e 1*5
      #更新隐层到输出层权值
      w2=w2+eta*delta2*as.matrix(hidden_output)#5*1
      w1=w1+eta*t(as.matrix(trainX[i,],nrow=1))%*%delta1#5*5
      theta2=theta2+eta*delta2
      theta1=theta1+eta*delta1
      # print(delta1)
      # if(abs(error)>0.5){
      #   num=num+1
      #   print(c(i,error))
      # }
    }
    # print(num)
  }
  
  #test
  for(i in (1:length(testX[,1]))){
    hidden_value=as.matrix(testX[i,],nrow=1)%*%w1+theta1
    hidden_output=apply(hidden_value,2,sigmoid)#1*5
    output_value=hidden_output%*%w2+theta2
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

confusionMatrix(predictions,testY,positive="1")


