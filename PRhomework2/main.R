#读取数据
data<-read.table("E:\\graduated life\\pattern recognition\\data.txt",header = T)

#决策树
sum<-0
for(i in 0:7)
{
    some<-data[data[,7]==i%/%4 & data[,8]==i%%4%/%2 & data[,9]==i%%2,]
    d<-dim(some)[1]
    boy<-dim(some[some[,2]==1,])[1]
    girl<-d-boy
    sum=sum+d
    #print(boy)
    print(girl)
    #print(d)
}
#男生人数太多，在每一个分支中，男生的比例都大于女生，所以分类是无效的
#sum

#使用两层决策树通过身高体重对数据分类，相当于把身高体重空间用两条直线分开。
#显然这样的分类效果不如二维贝叶斯分类器。
#如果想让决策树的效果提升，接近贝叶斯分类器的效果，
#可以使用更多的层来判断，判决的最优取值是贝叶斯决策面的取值。
#可以使用很多的决策树层数，使得判决的线接近曲线，效果趋近于贝叶斯决策面

