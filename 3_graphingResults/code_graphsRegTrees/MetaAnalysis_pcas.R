#10/11/13
#MetaAnalysis - PCAs
#########################################################
#########################################################
setwd("~/Desktop/MetaAnalysis_R/3_Graphing results")
source('leeFunctions.R')
#########################################################
#x1
xmat1<-invtraits #4 traits
xnames1<-colnames(xmat1)
xaxlab1<-'Invader trait value'
colnames(xmat1)<-c('leafcn','littercn','leafn','littern')

#x2
xmat2<-natCWMcalc #4 traits
xnames2<-colnames(xmat2)
xaxlab2<-'Native CWM trait value'
colnames(xmat2)<-c('leafcn','littercn','leafn','littern')
colnames(xmat2)<-paste('nat',colnames(xmat2),sep='_')

#x3
xmat3<-rrCWMcalc
xnames3<-colnames(xmat3)
xaxlab3<-'CWM trait response ratio'
colnames(xmat3)<-c('leafcn','littercn','leafn','littern')
colnames(xmat3)<-paste('rr',colnames(xmat3),sep='_')

#x4
xmat4<-natsoil #6 soil responses
xnames4<-colnames(xmat4)
xaxlab4<-'Native soil'
colnames(xmat4)<-c('nh','no','totn','ammonif','nitrif','minz')
colnames(xmat4)<-paste('Nat',colnames(xmat4),sep='_')

#y1
ymat1<-invsoil #6 soil responses
ynames1<-colnames(ymat1)
yaxlab1<-'Invaded soil'
colnames(ymat1)<-c('nh','no','totn','ammonif','nitrif','minz')

#y2
ymat2<-rrsoil #6 soil responses
ynames2<-colnames(ymat2)
yaxlab2<-'Soil Response Ratio'
colnames(ymat2)<-c('nh','no','totn','ammonif','nitrif','minz')
colnames(ymat2)<-paste('rr',colnames(ymat2),sep='_')

########################

#explanatory vars 
exp<-cbind(xmat1,xmat2,xmat3,xmat4,ymat1,ymat2)

traits<-cbind(xmat1,xmat2)
View(traits)

soils<-cbind(xmat4,ymat1)
View(soils)


