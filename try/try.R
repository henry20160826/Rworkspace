# x<-y<-seq(-4,4,length=20)
# f<-function(x,y){(exp(-0.5*x^2-0.5*y^2))/(2*pi)}
# z<-outer(x,y,f)
# z
# persp(x,y,z,theta=45,phi=25,col='lightblue')

library(plotly)
# volcano is a numeric matrix that ships with R
plot_ly(z = volcano, type = "surface")
