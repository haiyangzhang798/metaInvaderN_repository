#########################################################
#########################################################
#########################################################
source('leeFunctions.R')

#########################################################
#(P2) Do native soil measures (x) predict invaded soil measures (y)?
#(P2A) soil resps 1-3
#x
xmat<-natsoil[,(1:6)] #6
xaxlab<-'Native soil'
xnames<-colnames(xmat)
lim1<-c(-2,80)
lim2<-c(-2,35)
lim3<-c(-2,90)
lim4<-c(-2,30)
lim5<-c(-2,13)
lim6<-c(-3,10)
xlims<-cbind(lim1,lim2,lim3,lim4,lim5,lim6)

#y
ymat<-invsoil[,(1:6)] #6
yaxlab<-'Invaded soil'
ynames<-colnames(ymat)
ylims<-xlims

####
#combine x data columns and y data columns into xy pairs
prepxy2<-PrepXY2(xmat, ymat, # returns prepxy2<-list(pairs=pairs,lims=lims,ns=ns)
               xnames, ynames, 
               xlims, ylims)
lims<-prepxy2$lims
ns<-prepxy2$ns
#clean xy pairs of panel data, e.g. if x val = NA, then make y val = NA
cleanpairs<-CleanXY(prepxy2) # returns cleanpairs

#plotting parameters
par(mfcol = c(3, 2)) #number of panels, 3rows and 2col
par(mar=c(3.5,3.5,.5,.5), oma=c(2,2,0,0)) # clustered panels
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
  PlotDat2(dat)
}
mtext('Native soil',side=1,line=0.5,outer=T) # x label
mtext('Invaded soil',side=2,line=0.5,outer=T) # y label

#reformat lists into tables
summats.df<-ListToDF(summats)
coeftabs.df<-ListToDF(coeftabs)
####

#write tables
write.csv(summats.df,file='P2sum.csv')
write.csv(coeftabs.df,file='P2coef.csv')


