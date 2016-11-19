#读取数据
original_data<-read.table("E:\\graduated life\\pattern recognition\\data.txt",header = T)
#空缺的喜好数据按0.5处理，表示对这个喜好中立
d<-original_data[,c(2,4,5,7,8,9,10)]
#归一化身高体重
# d[,2]<-scale(d[,2])
# d[,3]<-scale(d[,3])
chosenNum=6#待选择特征个数，特征编码位数
groupNum<-16#一代群体的个体个数
swapNum=8#表示交换后几位，8表示交换后三位
group<-sample(1:63,groupNum)#至少需要一个特征
boy<-d[d[,1]==1,]#114
girl<-d[d[,1]==0,]#35
#计算均值向量，mean函数不能直接处理data.frame数据
mean_vector<-function(data)
{
  len<-dim(data)[2]#data列数
  r<-vector(mode="numeric",length = len)
  for(i in 1:len)
  {
    r[i]<-mean(data[,i])
  }
  return(r)
}
m<-mean_vector(d)#总体的均值矢量
# print(m)
mboy<-mean_vector(boy)#男生类的均值矢量
mgirl<-mean_vector(girl)#女生类的均值矢量
#计算适应程度
lenOfData=dim(d)[1]#data行数
judge<-function(c)
{
  # print(c)
  m<-m[c]
  # print(data)
  mboy<-mboy[c]
  # print(mboy)
  mgirl<-mgirl[c]
  # print(mgirl)
  Swsum=0
  Sbsum=0
  #类内离差
  for(i in 1:lenOfData)
  {
    # print("error1")
    # print(i)
    # print(data[i,1])
    if(d[i,1]==1)
    {
      #男生
      x=d[i,c]-mboy
    }
    else
    {
      #女生
      x=d[i,c]-mgirl
    }
    # print(c)
    # print(x)
    x<-as.matrix(x[,-1])
    dim(x)[1]=1
    # print(x)
    Swsum=Swsum+t(x)%*%x
    # print(Swsum)
  }
  Sw=Swsum/lenOfData

  #类间离差
  mboym=mboy-m
  mgirlm=mgirl-m
  # print(mgirlm)
  # print(mgirlm[-1])
  x1<-as.matrix(mboym[-1])
  # print(x1)
  x2<-as.matrix(mgirlm[-1])
  Sb=114/149*x1%*%t(x1)+35/149*x2%*%t(x2)
  # print(Sb)
  #J1 best 63
  r=sum(diag(solve(Sw)%*%Sb))
  #J2 出现负无穷，运算终止
  # r=log(det(Sb)/det(Sw))
  #J3 best 1
  # r=sum(diag(Sb))/sum(diag(Sw))
  #J4 63
  # r=det(Sb+Sw)/det(Sw)
  # print(r)
  return(r)
}
#通过适应性程度，计算出随机的交叉次数
getCrossNum<-function(judgement)
{
  #对向量不能使用dim
  len<-length(judgement)
  # print(judgement)
  # print(len)
  #计算每个个体占总体的比重
  judgement<-judgement/sum(judgement)
  #计算每段区间的端点
  for(i in 2:len)
  {
    judgement[i]<-judgement[i]+judgement[i-1]
  }
  # print(judgement)
  random<-runif(groupNum,min = 0,max = 1)#产生随机数
  result<-vector(mode = "numeric",length = groupNum)
  for(i in 1:len)
  {
    for(j in 1:len)
    {
      if(random[i]<judgement[j])
      {
        #在此区间内
        result[j]<-result[j]+1
        break
      }
    }
  }
  return(result)
}

#变异函数
variation<-function(p)
{
  #为变异概率
  #使用<<-对group中的数据赋值，以改变外部的group数据
  random<-runif(1,0,1)
  if(random<p)
  {
    s=sample(1:groupNum,1)
    w=sample(1:chosenNum,1)
    x=intToBits(group[s])
    if(x[w]==1)
    {
      x[w]=as.raw(0)
    }
    else
    {
      x[w]=as.raw(1)
    }
    new=0
    for(i in 1:groupNum)
    {
      new=new+as.numeric(x[i])^i
    }
    group[s]<<-new
    print("variation")
  }
}

getLie<-function(num)
{
  lie<-NULL
  x<-intToBits(num)
  for(k in 1:chosenNum)
  {
    if(x[k]==1)
    {
      # print("ok")
      lie<-c(lie,k+1)
    }
  }
  return(lie)
}

max=0
maxjudgement=0

bestOne=0
bestJudgement=0
#穷举求全局最优
for(i in 1:(2^chosenNum-1))
{
  lie<-1#性别数据每次都需要取出
  x<-intToBits(i)
  # print(i)
  for(k in 1:chosenNum)
  {
    if(x[k]==1)
    {
      # print("ok")
      lie<-c(lie,k+1)
    }
  }
  # print(lie)
  #计算当前组合的适应程度
  # print(chosenNum)

  now<-judge(lie)
  if(now>bestJudgement)
  {
    bestJudgement<-now
    bestOne<-i
  }
  # print(i)
  # print(now)
}
print(bestOne)



# #GA算法
# #循环10次,产生的个体数量已经超过可能存在的特征组合数
# #在实际中不会出现这种情况
# #保留最优个体

judgement<-vector(mode = "numeric",length = groupNum)

#繁殖代数
# for(i in 1:5)
# {
#   #对数据进行适应性评判
#   for(j in 1:groupNum)
#   {
#     lie<-1#性别数据每次都需要取出
#     #得到特征的列号
#     x<-intToBits(group[j])
#     for(k in 1:chosenNum)
#     {
#       if(x[k]==1)
#       {
#         # print("ok")
#         lie<-c(lie,k+1)
#       }
#     }
#     #计算当前组合的适应程度
#     judgement[j]<-judge(lie)
#     # print(judgement[j])
#     # print(bestJudgement)
#     if(judgement[j]>bestJudgement)
#     {
#       bestJudgement<-judgement[j]
#       bestOne<-group[j]
#     }
# 
#     # print(judgement[j])
#     lie<-1
#   }
#   #随机得到每个个体的交叉匹配次数
#   crossNum<-getCrossNum(judgement)
#   # print(crossNum)
#   #进行交叉配对
#   i=1
#   newGroup<-vector(mode = "numeric",length = 8)
#   newGroupIndex=1
#
#   # print(crossNum)
#
#   while(i<=groupNum)
#   {
#     if(crossNum[i]>0)#需要配对，寻找与之配对的对象
#     {
#       # print(paste("i:",i))
#       for(j in groupNum:i)#优先于非自己匹配
#       {
#         # print(paste("j:",j))
#         if(crossNum[j]>0 && i!=j)#找到两个配对的对象，进行配对
#         {
#           #交换后三位。后三位是对8取余的结果
#           tempi=group[i]%%swapNum
#           tempj=group[j]%%swapNum
#           #产生新一代
#           newGroup[newGroupIndex]=group[i]-tempi+tempj
#           if(newGroup[newGroupIndex]==0)#结果为0时，使其为父母中的一个
#           {
#             newGroup[newGroupIndex]=group[i]
#           }
#           newGroupIndex=newGroupIndex+1
#           newGroup[newGroupIndex]=group[j]-tempj+tempi
#           if(newGroup[newGroupIndex]==0)#结果为0时，使其为父母中的一个
#           {
#             newGroup[newGroupIndex]=group[j]
#           }
#           newGroupIndex=newGroupIndex+1
#           crossNum[i]=crossNum[i]-1
#           crossNum[j]=crossNum[j]-1
#           if(crossNum[i]==0)
#           {
#             break
#           }
#         }
#         else if(crossNum[j]>0 && i==j)#已经和其他对象匹配过
#         {
#           if(crossNum[i]%%2==0)
#           {
#             #与自己配对
#             while(crossNum[i]>0)
#             {
#               newGroup[newGroupIndex]=group[i]
#               newGroupIndex=newGroupIndex+1
#               crossNum[i]=crossNum[i]-1
#             }
#             break
#           }
#           else
#           {
#             i=i-1
#             break
#           }
#         }
#       }
#     }
#     else#不需要配对
#     {
#       i=i+1
#     }
#   }
#    # print(newGroup)
#   group<-newGroup
#   variation(0.1)
#   # print(group)
# }
# print(bestOne)
lie<-getLie(bestOne)
print(lie)

# d<-data.frame(original_data[,2],d)

#SVM
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

