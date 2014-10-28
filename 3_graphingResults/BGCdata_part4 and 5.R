#BCG data - parts 4 and 5, preliminary analyses
#########################################################
#(1) Make exploratory plots, loop though trait types: (part 4)
  #calcCWM nat vs calcCWM inv
  #calcCWM nat vs invader_traits
  #soilresps vs invader_traits
  #soilvals vs calcCWM nat
  #soilvals vs invader_traits
  #... also consider how variation in species trait vals within a community contributes to soilvals
#(2) Answer the following questions (part 5)
	#(a) Do invader vals predict the magnitude that soils 'change' due to invasion?
	#(b) Do native CWM vals predict the magnitude that soils 'change' due to invasion?
	#(c) Incorp: invader vals, native CWM vals, invader abundance --> magnitude and direction of soil changes

#need these txt files:
#'natCWMcalc_ex60.txt' -- calculated native CWM vals
#'invadersONLY.txt' -- invader vals
#'resps_soil.txt' -- soil responses

#NOTES on excluded data:
#exclude these obsIDs: 67 and 116
  #obsID 67 - was a greenhouse experiment, before/after comparison so no other spp

#this code will produce these txt files:


#########################################################
#part4
setwd("~/Desktop/MetaAnalysis_R/3_Graphing results")
library(reshape)
library(doBy)

#invader trait values
inv.traits<-read.table('invadersONLY.txt',header=TRUE, sep='\t')

#invaded area community-weighted mean trait values -- reported
invCWMreported<-read.table('invCWMreported.txt',header=TRUE, sep='\t')
#native area community-weighted mean trait values -- reported
natCWMreported<-read.table('natCWMreported.txt',header=TRUE, sep='\t')

#invaded area community-weighted mean trait values -- over 60%
invCWMcalc.ex60<-read.table('invCWMcalc_ex60.txt',sep='\t')
#native area community-weighted mean trait values -- over 60%
natCWMcalc.ex60<-read.table('natCWMcalc_ex60.txt',header=TRUE, sep='\t')

#soil responses - log response ratio (greater than 1 means higher values in invaded areas)
resps.soil<-read.table('resps_soil.txt',header=TRUE, sep='\t')
#native area soil responses
nat.soil<-read.table('nat_soil.txt',header=TRUE, sep='\t')

#measurement id table
traitskey<-read.table("data_traitids_unitconv.txt",header=TRUE,sep="\t")

#########################################################
#########################################################

#(2) Make exploratory plots, loop though trait types:
  #(a) calcCWM nat vs calcCWM inv
  #(b) calcCWM nat vs invader_traits
  #(c) soilresps vs invader_traits
  #(d) soilvals vs calcCWM nat
  #(e) soilvals vs invader_traits
#... also consider how variation in species trait vals within a community contributes to soilvals

#set up pretty trait labels for inv.traits
inv.labs<-colnames(inv.traits)[9:24]
inv.labs1<-as.character(traitskey[3:18,'MeasName'])
#set up pretty trait labels for CWMcalc
CWMcalc.labs<-colnames(invCWMcalc.ex60)[-1]
CWMcalc.labs1<-as.character(traitskey[3:18,'MeasName'])
#set up pretty trait labels for CWMreported
CWMrep.labs<-colnames(invCWMreported)[-1]
CWMrep.labs1<-as.character(traitskey[19:30,'MeasName'])
#set up pretty trait labels for nat.soil and resps.soil
soil.labs<-colnames(nat.soil)[-1]
soil.labs1<-as.character(traitskey[31:42,'MeasName'])


##########################
#(a) calcCWM nat (x) vs calcCWM inv (y)

#Calc meta data
storey<-numeric(0); storex<-numeric(0)
ymat<-invCWMcalc.ex60[,-1]
xmat<-natCWMcalc.ex60[,-1]
dim(ymat); dim(xmat)
#for y
ncols<-dim(ymat)[2] # number of cols
for (i in 1:ncols){ #loop through each col and summarize data
  ny<-sum(!is.na(ymat[,i]), na.rm=T)
  rangey<-range(ymat[,i], na.rm=T)
  rowy<-c(i,colnames(ymat)[i],ny,rangey[1],rangey[2])
  storey<-rbind(storey,rowy)
}
#for x
ncols<-dim(xmat)[2] # number of cols
for (i in 1:ncols){ #loop through each col and summarize data
  nx<-sum(!is.na(xmat[,i]), na.rm=T)
  rangex<-range(xmat[,i], na.rm=T)
  rowx<-c(i,colnames(xmat)[i],nx,rangex[1],rangex[2])
  storex<-rbind(storex,rowx)
}
colnames(storey)<-c('colnum','traitname','n','min','max'); colnames(storex)<-c('colnum','traitname','n','min','max')
metay<-data.frame(storey); metax<-data.frame(storex)
View(metay)
View(metax)
#for y
tcols<-dim(metay)[2]
for (k in 1:tcols){ #loop through each col of 'meta' and covert to the correct datatypes
  if(!k==2){metay[,k]<-as.numeric(as.character(metay[,k]))}
}
metay$longname<-CWMcalc.labs1
#for x
tcols<-dim(metax)[2]
for (k in 1:tcols){ #loop through each col of 'meta' and covert to the correct datatypes
  if(!k==2){metax[,k]<-as.numeric(as.character(metax[,k]))}
}
metax$longname<-CWMcalc.labs1

#subset trait types with >2 points
keepy<-which(metay$n>2); keepx<-which(metax$n>2)
keepy
keepx %in% keepy; keepy %in% keepx #use keepx

#subset desired traits
keeptraits<-keepy


#Plot
par(mfrow = c(2, 3)) #number of panels
par(cex = 0.8) #font size
par(mar = c(4, 4, 0, 1), oma = c(0.5, 0.5, 0.5, 0.5)) #size of inner, outer margins 
par(tcl = -0.25) #length of tick marks
par(mgp = c(2, 0.6, 0))
ncols<-dim(ymat)[2] # number of cols
for (i in 1:ncols){
  if (i %in% keeptraits){
    plot(ymat[,i]~xmat[,i], type='n',
         xlim=c(metax[i,'min']-1,metax[i,'max']+1), 
         ylim=c(metay[i,'min']-1,metay[i,'max']+1), 
         xlab='Native CWM',
         ylab='Invaded CWM'
         ) 
    abline(0,1, col='gray')
    #text(x=xmat[,i],y=ymat[,i],labels=inv.traits$genus, cex=.6)
    text(x=xmat[,i],y=ymat[,i],labels=inv.traits$obsID, cex=.6)
    mtext(metay[metay$colnum==i,'longname'], cex=.8, side=4)
  }
}


##########################
#(b) calcCWM nat (x) vs invader_traits (y)
#########

#Calc meta data
storey<-numeric(0); storex<-numeric(0)
ymat<-inv.traits[,9:24]
xmat<-natCWMcalc.ex60[,-1]
dim(ymat); dim(xmat)
#for y
ncols<-dim(ymat)[2] # number of cols
for (i in 1:ncols){ #loop through each col and summarize data
  ny<-sum(!is.na(ymat[,i]), na.rm=T)
  rangey<-range(ymat[,i], na.rm=T)
  rowy<-c(i,colnames(ymat)[i],ny,rangey[1],rangey[2])
  storey<-rbind(storey,rowy)
}
#for x
ncols<-dim(xmat)[2] # number of cols
for (i in 1:ncols){ #loop through each col and summarize data
  nx<-sum(!is.na(xmat[,i]), na.rm=T)
  rangex<-range(xmat[,i], na.rm=T)
  rowx<-c(i,colnames(xmat)[i],nx,rangex[1],rangex[2])
  storex<-rbind(storex,rowx)
}
colnames(storey)<-c('colnum','traitname','n','min','max'); colnames(storex)<-c('colnum','traitname','n','min','max')
metay<-data.frame(storey); metax<-data.frame(storex)
#for y
tcols<-dim(metay)[2]
for (k in 1:tcols){ #loop through each col of 'meta' and covert to the correct datatypes
  if(!k==2){metay[,k]<-as.numeric(as.character(metay[,k]))}
}
metay$longname<-inv.labs1

#for x
tcols<-dim(metax)[2]
for (k in 1:tcols){ #loop through each col of 'meta' and covert to the correct datatypes
  if(!k==2){metax[,k]<-as.numeric(as.character(metax[,k]))}
}
metax$longname<-CWMcalc.labs1

#subset trait types with >2 points
keepy<-which(metay$n>2); keepx<-which(metax$n>2)
keepy
keepx %in% keepy; keepy %in% keepx #use keepx

#subset desired traits
keeptraits<-keepy

#species initials colors
initlist<-unique(inv.traits$initials)
length(initlist)
initcols<-seq(1,length(initlist))
colids<-data.frame(initlist,initcols)
colornums<-numeric(0)
for (i in 1:dim(inv.traits)[1]){
  rowid<-which(colids$initlist==inv.traits[i,'initials'])
  colornum<-colids[rowid,'initcols']
  colornums<-c(colornums, colornum)
}
inv.traits$color<-colornums

#Plot
par(mfcol = c(2, 2)) #number of panels
par(cex = 0.8) #font size
par(mar = c(4, 4, 0, 1), oma = c(0.5, 0.5, 0.5, 0.5)) #size of inner, outer margins 
par(tcl = -0.25) #length of tick marks
par(mgp = c(2, 0.6, 0))
ncols<-dim(ymat)[2] # number of cols
for (i in 1:length(keeptraits)){
    plot(ymat[,keeptraits[i]]~xmat[,keeptraits[i]], type='n',
         xlim=c(metax[keeptraits[i],'min']-5,metax[keeptraits[i],'max']+5), 
         ylim=c(metay[keeptraits[i],'min']-5,metay[keeptraits[i],'max']+5), 
         xlab='Native CWM',
         ylab='Invader Mean'
    ) 
    abline(0,1, col='gray')
    #abline(lm(ymat[,keeptraits[i]]~xmat[,keeptraits[i]]), lty=2)
    text(x=xmat[,keeptraits[i]],y=ymat[,keeptraits[i]],labels=inv.traits$initials, cex=.8)
    #text(x=xmat[,keeptraits[i]],y=ymat[,keeptraits[i]],labels=inv.traits$obsID, cex=.6)
    mtext(metay[metay$colnum==keeptraits[i],'longname'], side=4, cex=.8)
}


##########################
#(c) soilresps (y) vs invader_traits (x)
#########

#Calc meta data
storey<-numeric(0); storex<-numeric(0)
ymat<-resps.soil[,-1]
xmat<-inv.traits[,9:24]
dim(ymat); dim(xmat)
#for y
ycols<-dim(ymat)[2] # number of cols
for (i in 1:ycols){ #loop through each col and summarize data
  ny<-sum(!is.na(ymat[,i]), na.rm=T)
  rangey<-range(ymat[,i], na.rm=T)
  rowy<-c(i,colnames(ymat)[i],ny,rangey[1],rangey[2])
  storey<-rbind(storey,rowy)
}
#for x
xcols<-dim(xmat)[2] # number of cols
for (i in 1:xcols){ #loop through each col and summarize data
  nx<-sum(!is.na(xmat[,i]), na.rm=T)
  rangex<-range(xmat[,i], na.rm=T)
  rowx<-c(i,colnames(xmat)[i],nx,rangex[1],rangex[2])
  storex<-rbind(storex,rowx)
}
colnames(storey)<-c('colnum','traitname','n','min','max'); colnames(storex)<-c('colnum','traitname','n','min','max')
metay<-data.frame(storey); metax<-data.frame(storex)
#for y
tcols<-dim(metay)[2]
for (k in 1:tcols){ #loop through each col of 'meta' and covert to the correct datatypes
  if(!k==2){metay[,k]<-as.numeric(as.character(metay[,k]))}
}
metay$longname<-soil.labs1
#for x
tcols<-dim(metax)[2]
for (k in 1:tcols){ #loop through each col of 'meta' and covert to the correct datatypes
  if(!k==2){metax[,k]<-as.numeric(as.character(metax[,k]))}
}
metax$longname<-inv.labs1

#subset y categories with >2 points
keepy<-which(metay$n>2)
keepy
keepx<-which(metax$n>2)
keepx

#subset desired traits
#keeptraits
keepsoils<-keepy
keeptraits<-keepx


#Plot
par(mfrow = c(2, 2)) #number of panels
par(cex = 0.8) #font size
par(mar = c(4,4, 0, 2), oma = c(0.5, 0.5, 0.5, 0.5)) #size of inner, outer margins 
par(tcl = -0.25) #length of tick marks
par(mgp = c(2, 0.6, 0))
rows<-numeric(0)
for (j in 1:length(keepsoils)){
    for (i in 1:length(keeptraits)){
      
        d<-cbind(xmat[,i],ymat[,j])
        occup<-rowSums(!is.na(d), na.rm=T)
        n<-length(which(occup==2))
        
        if(n>2){
          
          xname<-metax[metax$colnum==i,'longname']
          yname<-metay[metay$colnum==j,'longname']
          ixnum<-i
          jynum<-j
          row<-c(ixnum, xname, jynum, yname, n)
          rows<-rbind(rows,row)
          
          plot(ymat[,keepsoils[j]]~xmat[,keeptraits[i]], type='n',
               xlim=c(metax[keeptraits[i],'min']-1,metax[keeptraits[i],'max']+1), 
               ylim=c(metay[keepsoils[j],'min']-1,metay[keepsoils[j],'max']+1),
               xlab='Invader Mean',
               ylab='Soil Response') 
          abline(0,1)
          #abline(lm(ymat[,keepsoils[j]]~xmat[,keeptraits[i]]), lty=2)
          #text(x=xmat[,i],y=ymat[,j],labels=inv.traits$genus, cex=.6, offset=0.5, pos=4)
          text(x=xmat[,keeptraits[i]],y=ymat[,keepsoils[j]],labels=inv.traits$initials, cex=.6)
          mtext(paste(metay[metay$colnum==keepsoils[j],'longname'], 
                      metax[metax$colnum==keeptraits[i],'longname'], sep=' vs. \n'), 
                side=4, cex=.8, line=1)
        }
} 
}
colnames(rows)<-c('ixnum','xname', 'jynum', 'yname', 'n')



#########################################################
#########################################################
#########################################################
#########################################################
#part5
library(gplots)
# 
# natcwmcalc<-natCWMcalc.ex60[,-1] #just trait data
# obsIDs<-natCWMcalc.ex60[,1] #pull out obsID vector, length=116
# 
# natcwmrep<-natCWMreported[,-1] #just trait data, but...
# colnames(natcwmrep) #... need to reorganize columns so that they match with calculated CWM trait vals
# colnames(natcwmcalc)
# natcwmrep1<-natcwmrep[,c(3,9,2,8)]
# 
# invtraits<-read.table("invtraits.txt",header=TRUE,sep="\t") # from BGCdata_part4 code
# invvals<-invtraits[,7:32] #just trait data 
# initials<-invtraits$initials #pull out vector of invader initials; 'mm' = multiple invasive species
# invabund<-invtraits[,33:34] #just invader percent abundance in native and invaded areas
# 
# traitskey<-read.table('traitskey1.txt',header=TRUE,sep='\t') # from BGCdata_part4 code
# soilresps<-read.table("soilresps.txt",header=TRUE,sep="\t") # from BGCdata_part3 code
# colnames(soilresps)[1]<-'obsID'
# resps.soil<-soilresps[!soilresps$obsID=='67' & !soilresps$obsID=='116' ,] #exclude 2 studies w/o natives
# resps.soil[resps.soil$obsID=='13','nminz_logRR']<-NA # replace cell with NA
# resps.soil[resps.soil$obsID=='114','nh_logRR']<-NA # replace cell with NA
# resps.soil[resps.soil$obsID=='115','nh_logRR']<-NA # replace cell with NA
# resps.soil[resps.soil$obsID=='13','ammon_logRR']<-NA # replace cell with NA
# resps.soil[resps.soil$obsID=='13','nitrif_logRR']<-NA # replace cell with NA
# dim(resps.soil)
# soil<-resps.soil[,-1] #just soil response data 

#########################################################
#identify desired traits
keeptraits<-c(19,21,25,23)
ktnames<-colnames(invvals)[keeptraits]
keepsoils<-c(3,5,6)
ksnames<-colnames(soil)[keepsoils]

#simplify datasets, accordingly
identifiers<-data.frame(obsIDs, initials, invabund)
natcwmcalc1<-natcwmcalc[keeptraits]
natcwmrep1<-natcwmrep1
invvals1<-invvals[keeptraits]
soil1<-soil[keepsoils]

#########################################################
#########################################################
#########################################################
#(1) Do invader vals (x) predict the magnitude that soils 'change' due to invasion (y)?

#x
xmat<-invvals1 #4 traits
xaxlab<-'Invader Mean'
xnames<-colnames(xmat)
lim1<-c(0,60)
lim2<-c(0,170)
lim3<-c(0,60)
lim4<-c(0,20)
xlims<-cbind(lim1,lim2,lim3,lim4)

#y
ymat<-soil1 #3 soil responses
yaxlab<-'Soil Response'
ynames<-colnames(ymat)
lim1<-c(-5,5)
lim2<-c(-3,3)
lim3<-c(-2,2)
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
names(pairs)<-paste('panel',seq(1,12,1))
names(lims)<-paste('panel',seq(1,12,1))
names(ns)<-paste('panel',seq(1,12,1))

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
par(mfrow = c(2, 2)) #number of panels
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
  plot(y~x, type='n', axes = FALSE,
       xlim=xlims, 
       ylim=ylims,
       xlab=xname,
       ylab=yname) 
  box()
  text(y=y,x=x,labels=initials, cex=.6) #add 'points'
  mtext(letters[i], #add panel letters
        side = 3, line = -1.5, adj = 0.05, cex = 1, font=2) 
  #regression lines
  if(coeftab[2,4]<0.1){
    abline(fit, lty=2)
    mtext(paste('r^2 = ',coeftab[3,1]), #r2 value
          side = 3, line = -1.5, adj = 0.05) 
  }
  #x-axes
  if (i %in% c(2, 4)){ 
    axis(1, at = xat[[i]])
    mtext("Plant type", side = 1, outer = F, cex = 1, adj= 0.55, line=2.2)
  }
  #y-axes
  if (i %in% c(2, 4)){ 
    axis(2, at = yat[[i]])
    mtext(respnames[i], side = 2, outer = F, cex = 1, line=2.2)
  }
  
  
}




#########################################################
#########################################################
#########################################################
#(2) Do (calculated) native CWM vals (x) predict the magnitude that soils 'change' due to invasion (y)?
####
#compare x1 and x2
matches<-list()
pairs<-list()
for (i in 1:ncol(natcwmcalc1)){
  fill1<-which(!is.na(natcwmcalc1[,i]))
  fill2<-which(!is.na(natcwmrep1[,i]))
  matches[[i]]<-fill1[fill1 %in% fill2]
  pair<-cbind(matches[[i]],natcwmcalc1[matches[[i]],i],natcwmrep1[matches[[i]],i])
  colnames(pair)<-c('obsID','cwmcalc','cwmrep')
  pairs[[i]]<-pair
}
names(pairs)<-colnames(invvals1)
names(matches)<-colnames(invvals1)
pairs

#x
xmat<-natcwmcalc1 #4 traits
xaxlab<-'Native CW-Mean'
xnames<-colnames(xmat)
lim1<-c(0,60)
lim2<-c(0,100)
lim3<-c(0,35)
lim4<-c(0,10)
xlims<-cbind(lim1,lim2,lim3,lim4)

#x2
xmat2<-natcwmrep1 #4 traits
xaxlab2<-'Reported Native CW-Mean'
xnames2<-colnames(xmat)

#y
ymat<-soil1 #3 soil responses
yaxlab<-'Soil Response'
ynames<-colnames(ymat)
lim1<-c(-5,5)
lim2<-c(-5,5)
lim3<-c(-5,5)
ylims<-cbind(lim1,lim2,lim3)

#plotting parameters
par(mfrow = c(2, 4)) #number of panels
par(cex = 0.8) #font size
par(mar = c(3, 3, 1, 1), oma = c(0.5, 0.5, 0.5, 0.5)) #margins: inner and outer (bottom, left, top, right)
par(tcl = -0.25) #length of tick marks
par(mgp = c(2, 0.6, 0)) #margins: axis title, labels, line

#plot
for (j in 1:ncol(ymat)){ #loop through soil responses (y)
  for (i in 1:ncol(xmat)){ #loop through traits (x)
    
    fit<-lm(ymat[,j]~xmat[,i])
    coefs<-summary(fit)$coefficients
    coeftab<-round(coefs[,c(1,4)], digits=2)
    r2.row<-c('',round(summary(fit)$r.squared, digits=2))
    coeftab<-rbind(coeftab,rep('',2),r2.row)
    rownames(coeftab)<-c('Intercept','Slope','','R.squared')
    
    #textplot(coeftab, halign='right',cex = 0.8, cmar = 0.25, rmar = 0.5) 
    
    plot(ymat[,j]~xmat[,i], type='n',
         xlim=xlims[,i], 
         ylim=ylims[,j],
         xlab=paste(xaxlab,xnames[i], sep=' = '),
         ylab=paste(yaxlab,ynames[j], sep=' = ')) 
    abline(fit, lty=2)
    text(y=ymat[,j],x=xmat[,i],labels=obsIDs, cex=.6)
    text(y=ymat[,j],x=xmat2[,i],labels=obsIDs, cex=.6, col=2)
  } 
}

#########################################################

