# http://blog.csdn.net/sherrymi/article/details/38341185
#读取数据
original_data<-read.table("E:\\graduated life\\pattern recognition\\data.txt",header = T)
#空缺的喜好数据按0.5处理，表示对这个喜好中立
# d <- dist(original_data[,c(4,5,7,8,9,10)])
d <- dist(original_data[,c(4,5)])
str<-c("single","complete","median","mcquitty","average","centroid","ward")

for(i in 1:7)
{
  hc1 <- hclust(d,str[i])
  plot(hc1,hang = -1)
}
#hang小于0时，树将从底部画起。
#type = c("rectangle", "triangle"),默认树形图是方形的。另一个是三角形。
#horiz  TRUE 表示竖着放，FALSE表示横着放。
