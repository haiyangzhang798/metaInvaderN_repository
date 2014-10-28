#10/11/13
#MetaAnalysis - regression trees
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
colnames(ymat2)

########################

#explanatory vars for invaded soil Minz
exp<-cbind(xmat1,xmat2,xmat3,xmat4,ymat1,ymat2)
colnames(exp)

#regression trees
library(rpart)
library(partykit)
library(randomForest)
set.seed(110)

#1.Do invader trait values, the change in trait values, or native soil conditions determine soilN rr?
###########
#rr_nh
colnum<-which(colnames(exp)=='rr_nh') 
data<-exp[!is.na(exp[,colnum]),]
#build tree
tree<-rpart(rr_nh~leafcn+littercn+leafn+littern+
                 rr_leafcn+rr_littercn+rr_leafn+rr_littern+
                 Nat_nh+Nat_no+Nat_totn+
                 Nat_ammonif+Nat_nitrif+Nat_minz
               , data=data)
#prune and plot
tree.nh<-PrunPlot(tree=tree, ycol=data$rr_nh, ylabel='NH4+ LogRR')


###########
#rr_no
colnum<-which(colnames(exp)=='rr_no') 
data<-exp[!is.na(exp[,colnum]),]
#build tree
tree<-rpart(rr_no~leafcn+littercn+leafn+littern+
                   rr_leafcn+rr_littercn+rr_leafn+rr_littern+
                   Nat_nh+Nat_no+Nat_totn+
                   Nat_ammonif+Nat_nitrif+Nat_minz
                 , data=data)
#prune and plot
tree.no<-PrunPlot(tree=tree, ycol=data$rr_no, ylabel='NO3- LogRR')

###########
#rr_totn
colnum<-which(colnames(exp)=='rr_totn') 
data<-exp[!is.na(exp[,colnum]),]
#build tree
tree<-rpart(rr_totn~leafcn+littercn+leafn+littern+
                   rr_leafcn+rr_littercn+rr_leafn+rr_littern+
                   Nat_nh+Nat_no+Nat_totn+
                   Nat_ammonif+Nat_nitrif+Nat_minz
                 , data=data)
#prune and plot
tree.totn<-PrunPlot(tree=tree, ycol=data$rr_totn, ylabel='Inorganic N LogRR')


###########
#rr_ammonif
colnum<-which(colnames(exp)=='rr_ammonif') 
data<-exp[!is.na(exp[,colnum]),]
#build tree
tree<-rpart(rr_ammonif~leafcn+littercn+leafn+littern+
                   rr_leafcn+rr_littercn+rr_leafn+rr_littern+
                   Nat_nh+Nat_no+Nat_totn+
                   Nat_ammonif+Nat_nitrif+Nat_minz
                 , data=exp)
#prune and plot
tree.ammonif<-PrunPlot(tree=tree, ycol=data$rr_ammonif, ylabel='Ammonification LogRR')

###########
#rr_nitrif
colnum<-which(colnames(exp)=='rr_nitrif') 
data<-exp[!is.na(exp[,colnum]),]
#build tree
tree<-rpart(rr_nitrif~leafcn+littercn+leafn+littern+
                   rr_leafcn+rr_littercn+rr_leafn+rr_littern+
                   Nat_nh+Nat_no+Nat_totn+
                   Nat_ammonif+Nat_nitrif+Nat_minz
                 , data=exp)
#prune and plot
tree.nitrif<-PrunPlot(tree=tree, ycol=data$rr_nitrif, ylabel='Nitrification LogRR')


###########
#rr_minz
colnum<-which(colnames(exp)=='rr_minz') 
data<-exp[!is.na(exp[,colnum]),]
#build tree
tree<-rpart(rr_minz~leafcn+littercn+leafn+littern+
                   rr_leafcn+rr_littercn+rr_leafn+rr_littern+
                   Nat_nh+Nat_no+Nat_totn+
                   Nat_ammonif+Nat_nitrif+Nat_minz
                 , data=exp)
#prune and plot
tree.minz<-PrunPlot(tree=tree, ycol=data$rr_minz, ylabel='Mineralization LogRR')
print(tree.minz)



