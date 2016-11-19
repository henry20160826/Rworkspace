accuracy<-c(0.8993,0.8926,0.7919,0.8859,0.8859,0.8591)
plot(type='o',accuracy,ylim=c(0,1),main="特征选取与分类准确率的关系",xlab="选取前n个特征值")
text(accuracy,paste(format(accuracy,digits = 3)),pos = 3)
# plot(type='o',accuracy,main="特征选取与分类准确率的关系",xlab="选取前n个特征值")

accuracy<-c(0.8859,0.8859,0.8993,0.9128,0.9262,0.9195)
plot(type='o',accuracy,ylim=c(0,1),main="特征选取与分类准确率的关系",xlab="选取前n个特征值")
text(accuracy,paste(format(accuracy,digits = 3)),pos = 1)