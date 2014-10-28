#########################################################
#########################################################
#########################################################
#(2) 
#(A) soil measurements 1-6
#x
xmat<-natsoil[,c(3,6)] #6
xaxlab<-'Native soil'
xnames<-colnames(xmat)
lim1<-c(0,60)
lim2<-c(-2.5,10)
xlims<-cbind(lim1,lim2)

#y
ymat<-invsoil[,c(3,6)]#6
yaxlab<-'Invaded soil'
ynames<-colnames(ymat)
lim1<-c(0,60)
lim2<-c(-2.5,10)
ylims<-cbind(lim1,lim2)

#function for consolidating regression info
fitsTab<-function(fit.lm,fit.glm){
  coefs<-summary(fit.glm)$coefficients
  coefmat<-round(coefs, digits=2)
  r2<-c(round(summary(fit.lm)$r.squared, digits=2),NA,NA,NA)
  coeftab<-rbind(coefmat,r2)
  return(coeftab)
}


#prep paired xy datasets
datavecs<-list()
limvecs<-list()
namevecs<-list()
index<-numeric(0)
counter<-0
for (j in 1:ncol(ymat)){ #loop through soil responses (y) and soil vals (x), measurement by measurement
    #xy pair per panel
    counter<-j
    index<-c(index,counter)
    x<-xmat[,j]
    y<-ymat[,j]
    data<-data.frame(x,y)
    colnames(data)<-c(xnames[j],ynames[j])
    datavecs[[j]]<-data
    
    #xy lims per panel
    xlim<-xlims[,j]
    ylim<-ylims[,j]
    lim<-data.frame(xlim,ylim)
    colnames(lim)<-c(xnames[j],ynames[j])
    limvecs[[j]]<-lim
    
    #xy names per panel
    xn<-xnames[j]
    yn<-ynames[j]
    n<-data.frame(xn,yn)
    namevecs[[j]]<-n
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
names(pairs)<-paste('panel',seq(1,2,1))
names(lims)<-paste('panel',seq(1,2,1))
names(ns)<-paste('panel',seq(1,2,1))
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
par(mfrow = c(2, 1)) #number of panels
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
  #evalutate lm fit and glm fit (to calc if slope differs from 1)
  fit.lm<-lm(y~x, data=data)
  fit.glm<-glm(y~x+offset(x), data=data)
  coeftab<-fitsTab(fit.lm,fit.glm)
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
  abline(a=0,b=1, lty=2)
  #   #regression lines
  abline(fit.lm, lty=1)
    mtext(paste('r^2 = ',coeftab[3,1]), #r2 value
          side = 3, line = -6.5, adj = 0.95, cex=.6)
    mtext(paste('slope = ',coeftab[2,1]), #p value
          side = 3, line = -7.5, adj = 0.95, cex=.6)
    mtext(paste('p = ',coeftab[2,4]), #p value
          side = 3, line = -8.5, adj = 0.95, cex=.6)
  
}

#########################################################
#########################################################
#########################################################
#(2) 
#(A) soil resps 1-3
#x
xmat<-natsoil[,1:3]
xaxlab<-'Native soil'
xnames<-colnames(xmat)
lim1<-c(0,60)
lim2<-c(0,60)
lim3<-c(0,60)
xlims<-cbind(lim1,lim2,lim3)

#y
ymat<-rrsoil[,1:3]#3 soil responses
yaxlab<-'Soil Response Ratio'
ynames<-colnames(ymat)
lim1<-c(-10,10)
lim2<-c(-10,10)
lim3<-c(-10,10)
ylims<-cbind(lim1,lim2,lim3)

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
counter<-0
for (j in 1:ncol(ymat)){ #loop through soil responses (y) and soil vals (x), measurement by measurement
  #xy pair per panel
  counter<-j
  index<-c(index,counter)
  x<-xmat[,j]
  y<-ymat[,j]
  data<-data.frame(x,y)
  colnames(data)<-c(xnames[j],ynames[j])
  datavecs[[j]]<-data
  
  #xy lims per panel
  xlim<-xlims[,j]
  ylim<-ylims[,j]
  lim<-data.frame(xlim,ylim)
  colnames(lim)<-c(xnames[j],ynames[j])
  limvecs[[j]]<-lim
  
  #xy names per panel
  xn<-xnames[j]
  yn<-ynames[j]
  n<-data.frame(xn,yn)
  namevecs[[j]]<-n
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
names(pairs)<-paste('panel',seq(1,3,1))
names(lims)<-paste('panel',seq(1,3,1))
names(ns)<-paste('panel',seq(1,3,1))
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
par(mfrow = c(3, 1)) #number of panels
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
  #   #regression lines
  if(coeftab[2,4]<0.09){
    abline(fit, lty=1)
    mtext(paste('r^2 = ',coeftab[3,1]), #r2 value
          side = 3, line = -1.5, adj = 0.95, cex=.6)
    mtext(paste('slope = ',coeftab[2,1]), #p value
          side = 3, line = -2.5, adj = 0.95, cex=.6)
    mtext(paste('p = ',coeftab[2,4]), #p value
          side = 3, line = -3.5, adj = 0.95, cex=.6)
  }
  #x-axes
  if (i %in% (1:12)){ 
    #axis(1, at = xat[[i]])
    #mtext(as.character(xname), side = 1, outer = F, cex = 1, adj= 0.55, line=2.2)
  }
  #y-axes
  if (i %in% (1:12)){ 
    #axis(2, at = yat[[i]])
    #mtext(as.character(yname), side = 2, outer = F, cex = 1, line=2.2)
  }
  
  
}



##########################################
#(B) soil resps 4-6
#x
xmat<-natsoil[,4:6]
xaxlab<-'Native soil'
xnames<-colnames(xmat)
lim1<-c(0,20)
lim2<-c(0,10)
lim3<-c(0,10)
xlims<-cbind(lim1,lim2,lim3)

#y
ymat<-rrsoil[,4:6]#3 soil responses
yaxlab<-'Soil Response Ratio'
ynames<-colnames(ymat)
lim1<-c(-10,10)
lim2<-c(-10,10)
lim3<-c(-10,10)
ylims<-cbind(lim1,lim2,lim3)

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
counter<-0
for (j in 1:ncol(ymat)){ #loop through soil responses (y) and soil vals (x), measurement by measurement
  #xy pair per panel
  counter<-j
  index<-c(index,counter)
  x<-xmat[,j]
  y<-ymat[,j]
  data<-data.frame(x,y)
  colnames(data)<-c(xnames[j],ynames[j])
  datavecs[[j]]<-data
  
  #xy lims per panel
  xlim<-xlims[,j]
  ylim<-ylims[,j]
  lim<-data.frame(xlim,ylim)
  colnames(lim)<-c(xnames[j],ynames[j])
  limvecs[[j]]<-lim
  
  #xy names per panel
  xn<-xnames[j]
  yn<-ynames[j]
  n<-data.frame(xn,yn)
  namevecs[[j]]<-n
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
names(pairs)<-paste('panel',seq(1,3,1))
names(lims)<-paste('panel',seq(1,3,1))
names(ns)<-paste('panel',seq(1,3,1))
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
par(mfrow = c(3, 1)) #number of panels
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
  #   #regression lines
  if(coeftab[2,4]<0.09){
    abline(fit, lty=1)
    mtext(paste('r^2 = ',coeftab[3,1]), #r2 value
          side = 3, line = -1.5, adj = 0.95, cex=.6)
    mtext(paste('slope = ',coeftab[2,1]), #p value
          side = 3, line = -2.5, adj = 0.95, cex=.6)
    mtext(paste('p = ',coeftab[2,4]), #p value
          side = 3, line = -3.5, adj = 0.95, cex=.6)
  }
  #x-axes
  if (i %in% (1:12)){ 
    #axis(1, at = xat[[i]])
    #mtext(as.character(xname), side = 1, outer = F, cex = 1, adj= 0.55, line=2.2)
  }
  #y-axes
  if (i %in% (1:12)){ 
    #axis(2, at = yat[[i]])
    #mtext(as.character(yname), side = 2, outer = F, cex = 1, line=2.2)
  }
  
  
}

