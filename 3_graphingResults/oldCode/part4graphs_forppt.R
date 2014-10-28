#########################################################
#########################################################
#########################################################
#(4) Do native community vals (x) predict the magnitude that soils 'change' due to invasion (y)?
#(A) soil resps 1-3
#x
xmat<-rrCWMcalc
xaxlab<-'CWM trait response ratio'
xnames<-colnames(xmat)
lim1<-c(-.5,1)
lim2<-c(-.5,1)
lim3<-c(-.5,1)
lim4<-c(-.5,1)
xlims<-cbind(lim1,lim2,lim3,lim4)

#y
ymat<-rrsoil[,c(3,6)]#3 soil responses
yaxlab<-'Soil Response Ratio'
ynames<-colnames(ymat)
lim1<-c(-2,6)
lim2<-c(-2,6)
ylims<-cbind(lim1,lim2)

#function for consolidating regression info
lmfitTab<-function(fit){
  coefs<-summary(fit)$coefficients
  coefmat<-round(coefs, digits=2)
  r2<-c(round(summary(fit)$r.squared, digits=2),NA,NA,NA)
  coeftab<-rbind(coefmat,r2)
  return(coeftab)
}

#prep paired xy datasets
datavecs<-list()
limvecs<-list()
namevecs<-list()
index<-numeric(0)
for (j in 1:ncol(ymat)){ #loop through soil responses (y)
  for (i in 1:ncol(xmat)){ #loop through traits (x)
    counter<-i+5*j
    index<-c(index,counter)
    
    #xy pair per panel
    x<-xmat[,i]
    y<-ymat[,j]
    data<-data.frame(x,y)
    colnames(data)<-c(xnames[i],ynames[j])
    datavecs[[i+5*j]]<-data
    
    #xy lims per panel
    xlim<-xlims[,i]
    ylim<-ylims[,j]
    lim<-data.frame(xlim,ylim)
    colnames(lim)<-c(xnames[i],ynames[j])
    limvecs[[i+5*j]]<-lim
    
    #xy names per panel
    xn<-xnames[i]
    yn<-ynames[j]
    n<-data.frame(xn,yn)
    namevecs[[i+5*j]]<-n
  }
}
#datavecs
#limvecs
#namevecs
#index

pairs<-list()
lims<-list()
ns<-list()
for (k in 1:length(index)){
  pair<-datavecs[index[k]] #condense list of pairs to 1:12
  pairs[k]<-pair
  lim1<-limvecs[index[k]] #condense list of lims to 1:12
  lims[k]<-lim1
  n<-namevecs[index[k]] #condense list of names to 1:12
  ns[k]<-n
}
names(pairs)<-paste('panel',seq(1,8,1))
names(lims)<-paste('panel',seq(1,8,1))
names(ns)<-paste('panel',seq(1,8,1))
#View(pairs) # xy datasets for each panel
#View(lims) # xy min and max for each panel
#View(ns) # xy axis names for each panel

#axes
xat<-list()
yat<-list()
for (i in 1:length(lims)){
  df<-lims[[i]]
  xls<-df[,1]
  yls<-df[,2]
  xat[[i]]<-seq(xls[1],xls[2], by=20)
  yat[[i]]<-seq(yls[1],yls[2], by=2)
}
#str(xat)
#str(yat)

#plotting parameters
par(mfrow = c(2, 4)) #number of panels
par(cex = 0.8) #font size
par(mar = c(3, 3, 1, 1), oma = c(0.5, 0.5, 0.5, 0.5)) #margins: inner and outer (bottom, left, top, right)
par(tcl = -0.25) #length of tick marks
par(mgp = c(2, 0.6, 0)) #margins: axis title, labels, line

#summary and plot loops, 1 iteration = 1 panel
summats<-list()
coeftabs<-list()
for (i in 1:length(pairs)){
  #estb dataset
  d<-pairs[[i]]
  x<-d[,1]
  y<-d[,2]
  data<-data.frame(x,y)
  l<-lims[[i]]
  xlims<-l[,1]
  ylims<-l[,2]
  n<-ns[[i]]
  xname<-n[1]
  yname<-n[2]
  
  #summarize
  summats[[i]]<-matrix(data= c(range(x, na.rm=T),mean(x, na.rm=T),
                               range(y, na.rm=T), mean(y, na.rm=T)),
                       nrow=2, ncol=3, byrow=T)
  #evalutate lm fit
  fit<-lm(y~x, data=data)
  coeftab<-lmfitTab(fit)
  coeftabs[[i]]<-coeftab
  
  #plot
  plot(y~x, type='n', axes = T,
       xlim=xlims, 
       ylim=ylims,
       xlab=xname$xn,
       ylab=yname$yn) 
  box()
  #text(y=y,x=x,labels=initials, cex=.6) #add 'points'
  points(y=y,x=x, type='p',pch=16,cex=.6) #add 'points'
  #mtext(letters[i], #add panel letters
   #     side = 3, line = -1.5, adj = 0.05, cex = 1, font=2) 
  abline(h=0, lty=2)
  abline(v=0, lty=2)
#   #regression lines
  if(i != 2 & i != 4){
  if(coeftab[2,4]<0.1){
    abline(fit, lty=1)
    mtext(paste('r^2 = ',coeftab[3,1]), #r2 value
          side = 3, line = -1.5, adj = 0.95, cex=.6)
    mtext(paste('slope = ',coeftab[2,1]), #p value
          side = 3, line = -2.5, adj = 0.95, cex=.6)
    mtext(paste('p = ',coeftab[2,4]), #p value
          side = 3, line = -3.5, adj = 0.95, cex=.6)
  }
  }
  if(i == 4){
    if(coeftab[2,4]<0.1){
      abline(fit, lty=1)
      mtext(paste('r^2 = ',coeftab[3,1]), #r2 value
            side = 3, line = -5.5, adj = 0.95, cex=.6)
      mtext(paste('slope = ',coeftab[2,1]), #p value
            side = 3, line = -6.5, adj = 0.95, cex=.6)
      mtext(paste('p = ',coeftab[2,4]), #p value
            side = 3, line = -7.5, adj = 0.95, cex=.6)
    }
  }
  
}

panel4<-pairs[[4]]
View(panel4)
