#10/10/13
#MetaAnalysis - multiple regressions
#########################################################
#########################################################
setwd("~/Desktop/MetaAnalysis_R/3_Graphing results")
source('leeFunctions.R')

#########################################################
#(P1) Do invader vals (x1), native vals (x2), the diff between invader and native vals (x3) predict the magnitude that soils 'change' due to invasion (y)?
#x1
xmat1<-invtraits #4 traits
xaxlab1<-'Invader trait value'
xnames1<-colnames(xmat1)

#x2
xmat2<-natCWMcalc #4 traits
xaxlab2<-'Native CWM trait value'
xnames2<-colnames(xmat2)

#y
ymat<-rrsoil #6 soil responses
yaxlab<-'Soil Response Ratio'
ynames<-colnames(ymat)

#combine x data columns and y data columns into xy pairs
prepxxy<-PrepXXY(xmat1, xmat2, ymat, # returns prepxy<-list(pairs=pairs,lims=lims,ns=ns)
               xnames1, xnames2, ynames)
#str(prepxxy) #24 panels bc x=4 cols y=6 cols
numpanels<-length(colnames(ymat))*length(colnames(xmat1))*length(colnames(xmat2))
ns<-prepxxy$ns
#clean xy pairs of panel data, e.g. if x val = NA, then make y val = NA
cleanpairs<-CleanXXY(prepxxy) # returns cleanpairs

#Summarize and Regress
summats<-list()
coeftabs<-list()
dmat<-list()
for (i in 1:length(cleanpairs)){
  #run function to 1) estb dataset, 2) summarize data, 3) test lm fit
  d<-cleanpairs[[i]]
  if(sum(!is.na(d[,1]))>5){
    n<-ns[[i]]
    dat<-DataSum2(d,n)
    #consolidate summary statistics
    summats[[i]]<-dat$summat
    coeftabs[[i]]<-dat$coeftab
    dmat[[i]]<-dat$data
  }
  else(print(i))
}

for (t in 1:length(coeftabs)){
  tab<-coeftabs[[t]]
  if(sum(tab[2:4,4]<0.1)>1){print(t)}
}
coeftabs[[13]]
summats[[13]]
dmat[[13]]
str(cleanpairs)
cleanpairs[[13]]

d<-cleanpairs[[13]]
x1<-d[,1]
x2<-d[,2]
y<-d[,3]

#plot panel 13
par(mfrow = c(1,1)) #number of panels
par(mar=c(4,4,0,0), oma=c(1,1,1,1)) # clustered panels
par(cex = 0.8, tcl = -0.25, mgp = c(2, 0.6, 0)) #font size, length of tick marks, margins: axis title, labels, line

xlims<-c(12,26)
ylims<-c(.2,1.8)
plot(y~x1, type='n', axes = T,
     xlim=xlims,
     ylim=ylims,
     xlab='Leaf C:N',
     ylab='Ammonification LogRR') 
box()
text(y=y,x=x1,labels=obsids, cex=.8) #add 'points'
text(y=y,x=x2,labels=obsids, cex=.8, col=2) #add 'points'



####


#########################################################
#########################################################
#(P2) Do native soil measures, invader vals and native vals (x) predict invaded soil measures (y)?
#x1
xmat1<-invtraits #4 traits
xaxlab1<-'Invader trait value'
xnames1<-colnames(xmat1)

# #x2
# xmat2<-natCWMcalc #4 traits
# xaxlab2<-'Native CWM trait value'
# xnames2<-colnames(xmat2)

#x2
xmat2<-natsoil #6 soil responses
xaxlab2<-'Native soil'
xnames2<-colnames(xmat2)

#y
ymat<-invsoil #6 soil responses
yaxlab<-'Invaded soil'
ynames<-colnames(ymat)


#combine x data columns and y data columns into xy pairs
prepxxy2<-PrepXXY2(xmat1, xmat2, ymat, # returns prepxy<-list(pairs=pairs,lims=lims,ns=ns)
                 xnames1, xnames2, ynames)
#str(prepxxy2) #24 panels bc x=4 cols y=6 cols
numpanels<-length(colnames(ymat))*length(colnames(xmat1))*length(colnames(xmat2))
ns<-prepxxy2$ns
#clean xy pairs of panel data, e.g. if x val = NA, then make y val = NA
cleanpairs<-CleanXXY(prepxxy2) # returns cleanpairs

#Summarize and Regress
summats<-list()
coeftabs<-list()
dmat<-list()
for (i in 1:length(cleanpairs)){
  #run function to 1) estb dataset, 2) summarize data, 3) test lm fit
  d<-cleanpairs[[i]]
  if(sum(!is.na(d[,1]))>5){
    n<-ns[[i]]
    dat<-DataSum2(d,n)
    #consolidate summary statistics
    summats[[i]]<-dat$summat
    coeftabs[[i]]<-dat$coeftab
    dmat[[i]]<-dat$data
  }
  else(print(i))
}

signifs<-numeric(0)
for (t in 1:length(coeftabs)){
  tab<-coeftabs[[t]]
  if(sum(tab[2:4,4]<0.1)>1){
    idmat<-cbind(paste('panel',rep(t,5)), rbind(ns[[t]],ns[[t]],ns[[t]],ns[[t]],ns[[t]]))
    colnames(idmat)[1]<-'panel'
    signif<-cbind(idmat,tab)
    row.names(signif)<-row.names(tab)
    signifs<-rbind(signifs,signif)
  }
}
View(signifs)


#########################################################
#########################################################
#(P4) Do the diff between native and invasive trait vals (x) predict invaded soil measures (y)?
#x
xmat<-rrCWMcalc
xaxlab<-'CWM trait response ratio'
xnames<-colnames(xmat)


