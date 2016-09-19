#引入一个画图库
library(plotly)

#读取数据
data<-read.table("E:\\graduated life\\pattern recognition\\data.txt",header = T)
# data

#画出男生身高的直方图
boy<-data[data[,2]==1,]
boyheight<-boy[,4]
boyht<-table(boyheight)
barplot(boyht,main="男生身高的直方图")
#画出女生身高的直方图
girl<-data[data[,2]==0,]
girlheight<-girl[,4]
girlht<-table(girlheight)
barplot(girlht,main="女生身高的直方图")

#取出男生女生体重
boyweight<-boy[,5]
girlweight<-girl[,5]
#统计男生女生人数
boyNum<-dim(boy)[1]
girlNum<-dim(girl)[1]
#计算男生女生身高概率
girlHP<-girlheight/girlNum
boyHP<-boyheight/boyNum
#画出男生体重的直方图
boywt<-table(boyweight)
barplot(boywt,main="男生体重的直方图")
#画出女生体重的直方图
girlwt<-table(girlweight)
barplot(girlwt,main="女生体重的直方图")


#正态分布的最大似然估计参数为期望和方差
#计算身高期望
boyEh<-mean(boyheight);
girlEh<-mean(girlheight);
#计算体重期望
boyEw<-mean(boyweight);
girlEw<-mean(girlweight);
#计算身高方差
#var()函数采用无偏估计，除以N-1，所以此处未使用var函数
boyDh<-mean((boyheight-boyEh)^2)
girlDh<-mean((girlheight-girlEh)^2)
#计算体重方差
boyDw<-mean((boyweight-boyEw)^2)
girlDw<-mean((girlweight-girlEw)^2)

#身高，体重联合方式计算期望方差
getMLEu<-function()
{
  
}
getMLEo2<-function()
{
  
}
boyHW<-boy[,4:5]
uboy<-colMeans(boyHW)
oboy<-(t(boyHW)-uboy)%*%t((t(boyHW)-uboy))/boyNum

girlHW<-girl[,4:5]
ugirl<-colMeans(girlHW)
ogirl<-(t(girlHW)-ugirl)%*%t((t(girlHW)-ugirl))/girlNum

#正态分布的贝叶斯估计
getBayesE<-function(N,e,Dx,u,o2)
{
  return((N*e*o2+Dx*u)/(N*o2+Dx))
}
getBayesD<- function(o2,Dx,N) 
{
  return(o2*Dx/(N*o2+Dx))
}
#设男生身高期望服从
u<-seq(160,180,length.out = 3)
o<-seq(0,6,length.out = 7)
for(i in 1:length(u))
{
  for(j in 1:length(o))
  {
    print(getBayesE(boyNum,boyEh,boyDh,u[i],o[j]^2))
    print(getBayesD(o[j]^2,boyDh,boyNum))
  }
}
u<-200
o2<-1
getBayesE(boyNum,boyEh,boyDh,u,o2)
getBayesD(o2,boyDh,boyNum)

#设女生身高期望服从N(160,10)
u<-160
o2<-10
girlBayesEh=getBayesE(girlNum,girlEh,girlDh,u,o2)
girlBayesDh=getBayesD(o2,girlDh,girlNum);

#设男生体重期望服从N(65,100)
u<-65
o2<-100
boyBayesEw=getBayesE(boyNum,boyEw,boyDw,u,o2)
boyBayesDW=getBayesD(o2,boyDw,boyNum)
#设女生体重期望服从N(50,15)
u<-50
o2<-15
girlBayesEw=getBayesE(girlNum,girlEw,girlDw,u,o2)
girlBayesDw=getBayesD(o2,girlDw,girlNum)

#原始数据散点图
#纵坐标为体重
#横坐标为身高
#点的大小为体重与身高的乘积
#男生女生的数据有混杂在一起的部分，紧靠身高和体重两个数据，有部分数据很难区分男女生
plot_ly(data, x = data[,4], y = data[,5],mode = "markers",
        color =data[,2],size=data[,4]*data[,5])


#最小错误率贝叶斯决策
num=boyNum+girlNum
pw1=boyNum/num
pw2=girlNum/num
#画出男生的身高体重分布图，x轴身高，y轴体重，z轴概率
x<-seq(150,190,length.out = 100)
y<-seq(40,100,length.out = 100)
det_oboy<-det(oboy)
det_ogirl<-det(ogirl)
solve_oboy=solve(oboy)
solve_ogirl=solve(ogirl)
f<-function(x,y)
{
  a<-sqrt(det_oboy)*2*pi
  solve_m<-solve(oboy)
  ea<-solve_m[1,1]*(x-boyEh)^2+2*solve_m[1,2]*(x-boyEh)*(y-boyEw)+solve_m[2,2]*(y-boyEw)^2
  return(exp((-1/2)*ea)/a)
}
zboy<-outer(x,y,f)
persp(x,y,zboy,phi=90)
f1<-function(x,y)
{
  a<-sqrt(det_ogirl)*2*pi
  solve_m<-solve(ogirl)
  ea<-solve_m[1,1]*(x-girlEh)^2+2*solve_m[1,2]*(x-girlEh)*(y-girlEw)+solve_m[2,2]*(y-girlEw)^2
  return(exp((-1/2)*ea)/a)
}
zgirl<-outer(x,y,f1)
persp(x,y,zgirl,phi=90)

#男生女生散点图
# s_zboy<-f(x,y)
# plot_ly(data, x = x, y = y,mode = "markers",
#         color =s_zboy,size=x*y)
# s_zgirl<-f1(x,y)
# plot_ly(data, x = x, y = y,mode = "markers",
#         color =s_zgirl,size=x*y)

#男生，女生三维概率分布图
library(plotly)
p<-plot_ly(x=x,y=y,z = zboy,type = "surface") %>%
  add_trace(x=x,y=y,z = zgirl,type = "surface")
p
l<-matrix(0,100,100)
for(i in 1:100)
{
  for(j in 1:100)
  {
    if(zboy[i,j]>zgirl[i,j])
    {
      l[i,j]<-1
    }
    else
    {
      l[i,j]<-0
    }
  }
}
plot_ly(x=x,y=y,z=l,type = "surface")

#z
#160,45
#178,70
pboy=boyNum/num
pgirl=girlNum/num

BWboy=-1/2*solve_oboy
wboy=solve_oboy*uboy
wboy0=-1/2*t(uboy)%*%solve_oboy%*%uboy-1/2*log(det_oboy)+log(pboy)

BWgirl=-1/2*solve_ogirl
wgirl=solve_ogirl*ugirl
wgirl0=-1/2*t(ugirl)%*%solve_ogirl%*%ugirl-1/2*log(det_ogirl)+log(pgirl)

BW=BWboy-BWgirl
w=wboy-wgirl
w0=wboy0-wgirl0

gboy<-function(x)
{
  -1/2*t(x-uboy)%*%solve_oboy%*%(x-uboy)-1/2*log(det_oboy)+log(pboy)
}

ggirl<-function(x)
{
  -1/2*t(x-ugirl)%*%solve_ogirl%*%(x-ugirl)-1/2*log(det_ogirl)+log(pgirl)
}

is_boy<-function(x)
{
  if(gboy(x)>ggirl(x))
  {
    print("判断为男生")
    return(1)
  }
  else
  {
    print("判断为女生")
    return(0)
  }
}

one<-c(178,70)
gboy(one)
ggirl(one)
is_boy(one)

one<-c(160,45)
gboy(one)
ggirl(one)
is_boy(one)

#尝试用解方程的方法画出决策面
# a=BW[1,1]
# b=2*BW[1,2]*y+w[1]
# c=BW[2,2]*y^2+w[2]*y+w0
# 
# x1=(-b+sqrt(b^2-4*a*c))/2*a
# x2=(-b-sqrt(b^2-4*a*c))/2*a
# 
# plot(y,x1,type="l")
# plot(y,x2,type="l")



