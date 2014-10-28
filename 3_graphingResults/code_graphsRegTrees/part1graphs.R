#########################################################
#########################################################
#########################################################
source('leeFunctions.R')

#########################################################
#(P1) Do invader vals (x) predict the magnitude that soils 'change' due to invasion (y)?
#(P1A) soil resps 1-3
#x
xmat<-invtraits
xaxlab<-'Invader trait value'
xnames<-colnames(xmat)
lim1<-c(10,45)
lim2<-c(20,130)
lim3<-c(0,6)
lim4<-c(0,2)
xlims<-cbind(lim1,lim2,lim3,lim4)

#y
ymat<-rrsoil[,1:3]#3 soil responses
yaxlab<-'Soil Response Ratio'
ynames<-colnames(ymat)
lim1<-c(-8,8)
lim2<-c(-3,5)
lim3<-c(-3,6)
ylims<-cbind(lim1,lim2,lim3)

####
#combine x data columns and y data columns into xy pairs
prepxy<-PrepXY(xmat, ymat, # returns prepxy<-list(pairs=pairs,lims=lims,ns=ns)
       xnames, ynames, 
       xlims, ylims)
lims<-prepxy$lims
ns<-prepxy$ns
#clean xy pairs of panel data, e.g. if x val = NA, then make y val = NA
cleanpairs<-CleanXY(prepxy) # returns cleanpairs

#plotting parameters
par(mfrow = c(3, 4)) #number of panels
par(mar=c(0,0,0,0), oma=c(6,6,1,1)) # clustered panels
par(cex = 0.8, tcl = -0.25, mgp = c(2, 0.6, 0)) #font size, length of tick marks, margins: axis title, labels, line

#summarize and plot xy data for each panel, 1 iteration = 1 panel
summats<-list()
coeftabs<-list()
for (i in 1:length(cleanpairs)){
  #run function to 1) estb dataset, 2) summarize data, 3) test lm fit
  d<-cleanpairs[[i]]
  l<-lims[[i]]
  n<-ns[[i]]
  dat<-DataSum(d,l,n)
  #consolidate summary statistics
  summats[[i]]<-dat$summat
  coeftabs[[i]]<-dat$coeftab
  #plot
  PlotDat(dat)
}
mtext('Invader trait values',side=1,line=4,outer=T) # x label
mtext('Invader impacts',side=2,line=4,outer=T) # y label

#reformat lists into tables
summats.df<-ListToDF(summats)
coeftabs.df<-ListToDF(coeftabs)
####

#write tables
write.csv(summats.df,file='P1Asum.csv')
write.csv(coeftabs.df,file='P1Acoef.csv')

##########################################
#(P1B) soil resps 4-6
#x
xmat<-invtraits
xaxlab<-'Invader trait value'
xnames<-colnames(xmat)
#lim1<-c(10,130)
#lim2<-c(20,80)
#lim3<-c(0,6)
#lim4<-c(0,2.5)
#xlims<-cbind(lim1,lim2,lim3,lim4)


#y
ymat<-rrsoil[,4:6] #3 soil responses
yaxlab<-'Soil Response Ratio'
ynames<-colnames(ymat)
lim1<-c(-1,4)
lim2<-c(-1,8)
lim3<-c(-1,3)
ylims<-cbind(lim1,lim2,lim3)

####
#combine x data columns and y data columns into xy pairs
prepxy<-PrepXY(xmat, ymat, # returns prepxy<-list(pairs=pairs,lims=lims,ns=ns)
               xnames, ynames, 
               xlims, ylims)
lims<-prepxy$lims
ns<-prepxy$ns
#clean xy pairs of panel data, e.g. if x val = NA, then make y val = NA
cleanpairs<-CleanXY(prepxy) # returns cleanpairs

#plotting parameters
par(mfrow = c(3, 4)) #number of panels
par(mar=c(0,0,0,0), oma=c(6,6,1,1)) # clustered panels
par(cex = 0.8, tcl = -0.25, mgp = c(2, 0.6, 0)) #font size, length of tick marks, margins: axis title, labels, line

#summarize and plot xy data for each panel, 1 iteration = 1 panel
summats<-list()
coeftabs<-list()

for (i in 1:length(cleanpairs)){
  #run function to 1) estb dataset, 2) summarize data, 3) test lm fit
  d<-cleanpairs[[i]]
  l<-lims[[i]]
  n<-ns[[i]]
  dat<-DataSum(d,l,n)
  #consolidate summary statistics
  summats[[i]]<-dat$summat
  coeftabs[[i]]<-dat$coeftab
  #plot
  PlotDat(dat)
}
mtext('Invader trait values',side=1,line=4,outer=T) # x label
mtext('Invader impacts',side=2,line=4,outer=T) # y label

#reformat lists into tables
summats.df<-ListToDF(summats)
coeftabs.df<-ListToDF(coeftabs)
####

#write tables
write.csv(summats.df,file='P1Bsum.csv')
write.csv(coeftabs.df,file='P1Bcoef.csv')

#########################################
