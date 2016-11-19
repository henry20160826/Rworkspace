#读取数据
original_data<-read.table("E:\\graduated life\\pattern recognition\\data.txt",header = T)
#空缺的喜好数据按0.5处理，表示对这个喜好中立
noSexData<-original_data[,c(4,5)]#取出身高体重数据

# #采用男生女生的平均身高体重作为初始的类中心
# centers<-matrix(c(162.09,173.39,50.6,65.72),2,2)
# #两个极端的数值作为中心
# centers<-matrix(c(153,188,44,98),2,2)#分界线更靠近男生
# #两类选择更男性化的值
# centers<-matrix(c(180,188,80,98),2,2)#分界线更靠近男生
# #两类选择更女性化的值
# centers<-matrix(c(155,160,45,50),2,2)#和平均值结果类似
# #两类选择中性的值
# centers<-matrix(c(165,169,58,63),2,2)#和平均值结果类似

#初始值对聚类的影响
# centers<-matrix(c(162.09,173.39,153,188,180,188,155,160,165,169,50.6,65.72,44,98,80,98,45,50,58,63),10,2)
# kccenters<-NULL
# for(i in 0:4)
# {
# # print(centers[c(i*2+1,i*2+2),])
# kc <- kmeans(noSexData, centers[c(i*2+1,i*2+2),])#分类模型训练
# cluster_result=fitted(kc)#查看具体分类情况 
# original_data[,2]<-original_data[,2]+1
# table(original_data$sex, kc$cluster)#查看分类概括
# #女生的数据更紧密，各种情况下聚类都是女生的类别数量多
# #聚类结果可视化
# plot(noSexData[c(1,2)], col = kc$cluster, pch = as.integer(original_data$sex))#不同的颜色代表不同的聚类结果，不同的形状代表训练数据集的原始分类情况。
# kccenters<-rbind(kccenters,kc$centers[,c(1, 2)])
# points(kc$centers[,c(1, 2)], col = 1:5, pch = 20, cex=4)#画出类的中心点
# points(centers[c(i*2+1,i*2+2),], col = 1:5, pch = 18, cex=3)#画出类的中心点方形
# }
# print(kccenters)
# plot(centers)
# for(i in 0:4)
# {
#   points(centers[c(i*2+1,i*2+2),],col=1:2,pch=(0+i),cex=3)
#   points(kccenters[c(i*2+1,i*2+2),],col=3:4,pch=(0+i),cex=3)
# }

#聚成多类
kc <- kmeans(noSexData,5)#分类模型训练
cluster_result=fitted(kc)#查看具体分类情况 
original_data[,2]<-original_data[,2]+1
table(original_data$sex, kc$cluster)#查看分类概括
#女生的数据更紧密，各种情况下聚类都是女生的类别数量多
#聚类结果可视化
plot(noSexData[c(1,2)], col = kc$cluster, pch = as.integer(original_data$sex))#不同的颜色代表不同的聚类结果，不同的形状代表训练数据集的原始分类情况。
kccenters<-rbind(kccenters,kc$centers[,c(1, 2)])
points(kc$centers[,c(1, 2)], col = 1:5, pch = 20, cex=4)#画出类的中心点
# points(centers[c(i*2+1,i*2+2),], col = 1:5, pch = 18, cex=3)#画出类的中心点方形



