#BCG data - part3
#Read-in bgc data collected from meta-analysis papers (soil, community)
#Read-in trait data complied for each meta-analysis subobs
  #(1) Set up the soil response variables, i.e. calc log response ratios from obsID_soil -> soilresps
  #(2) Make a table like obsID_community using bgcTABLE -> obsID_community_fromtraits
  #(3) Update obsID_community with obsID_community_fromtraits -> communityresps

#need these txt files:
  #'obsID_soil_frompapers.txt'
  #'bgcTABLE.txt'
  #'TRYnBGC_traitskey.txt'

# this code will produce these txt files:
  #SOIL
    #'soilresps.txt'
    #'soilresps_raw.txt'
  #SPECIES TRAITS and calc'd REL. ABUND
    #'traits_calcabund.txt'
  #Calc'd Community-Weighed Mean (CWM) trait vals
    #'CWMtraits_calc.txt'

setwd("~/Desktop/MetaAnalysis_R/2_BGC data processing")

#########################################################
obsID_soil1<-read.table("obsID_soil_frompapers.txt",header=TRUE,sep="\t") # raw
  #View(obsID_soil)
  obsID_soil<-obsID_soil1[1:118,]
traits<-read.table("bgcTABLE.txt",header=TRUE,sep="\t") # from BGCdata_part2.R
#traitskey<-read.table("TRYnBGC_traitskey.txt",header=TRUE,sep="\t") #raw
traitskey<-read.table("data_traitids_unitconv.txt",header=TRUE,sep="\t") #raw

#########################################################
# #########################################################
# #(1) Set up the soil response variables, i.e. calc log response ratios from obsID_soil -> soilresps
# #subset the response variables that will be calculated from obsID_soil
# colnames(obsID_soil)
# temp<-colnames(obsID_soil)[c(2,4,6,8,10,12,14,16,18,20,22,24)] #raw response data
# temp2<-unlist(strsplit(temp,"_"))
# respvars1<-temp2[temp2 != 'I']
# respvars2<-paste(respvars1,'logRR',sep='_') #make the list of response names to be calculated
# respdata<-list(
#   nh=obsID_soil[,2:3],
#   no=obsID_soil[,4:5],
#   toti=obsID_soil[,6:7],
#   ammon=obsID_soil[,8:9],
#   nitrif=obsID_soil[,10:11],
#   nminz=obsID_soil[,12:13],
#   soilN=obsID_soil[,14:15],
#   SOM=obsID_soil[,16:17],
#   soilcn=obsID_soil[,18:19],
#   microbialcn=obsID_soil[,20:21],
#   soilmoi=obsID_soil[,22:23],
#   pH=obsID_soil[,24:25]
# )
# write.table(respdata, file='soilresps_raw.txt', sep='\t',row.names=T, col.names=T)
# ########################################################
# #Prep respdata for taking the log response ratio
# #SUMMARY: (1) replace 0's with 0.0001; (2) replace negative values with orginal value + minvalue + 1; (3) replace log(1)'s in the denominator with log(1.0001).
# #Look for 0s
# out1<-list()
# out2<-list()
# for (i in 1:length(respdata)){
#   temp<-respdata[[i]] #rename the list item
#   l1<-temp[,1] == 0
#   l2<-temp[,2] == 0
#   t1<-which(l1 == T)
#   t2<-which(l2 == T)
#   out1[[i]]<-t1
#   out2[[i]]<-t2
# }
# names(out1)<-respvars1; names(out2)<-respvars1
# out1
# out2
# #deal with 0s
# no.new<-respdata$no
# no.new[out1$no,1]<-rep(0.0001,2)
# no.new[out2$no,2]<-rep(0.0001,3)
# respdata$no<-no.new
# nitrif.new<-respdata$nitrif
# nitrif.new[out1$nitrif,1]<-rep(0.0001,2)
# nitrif.new[out2$nitrif,2]<-rep(0.0001,2)
# respdata$nitrif<-nitrif.new
# nh.new<-respdata$nh
# nh.new[out2$nh,2]<-rep(0.0001,1)
# respdata$nh<-nh.new
# 
# #Look for negative vals
# out1<-list()
# out2<-list()
# for (i in 1:length(respdata)){
#   temp<-respdata[[i]] #rename the list item
#   l1<-temp[,1] < 0
#   l2<-temp[,2] < 0
#   t1<-which(l1 == T)
#   t2<-which(l2 == T)
#   out1[[i]]<-t1
#   out2[[i]]<-t2
# }
# names(out1)<-respvars1; names(out2)<-respvars1
# out1
# out2
# #Deal with negative vals... ammon, nitrif, nminz
# ammon.new<-respdata$ammon
# nitrif.new<-respdata$nitrif
# nminz.new<-respdata$nminz
# ammon.min<-min(ammon.new, na.rm=T)
# nitrif.min<-min(nitrif.new, na.rm=T)
# nminz.min<-min(nminz.new, na.rm=T)
# ammon.new1<-ammon.new+(-1*ammon.min)+1
# nitrif.new1<-nitrif.new+(-1*nitrif.min)+1
# nminz.new1<-nminz.new+(-1*nminz.min)+1
# respdata$ammon<-ammon.new1
# respdata$nitrif<-nitrif.new1
# respdata$nminz<-nminz.new1
# 
# #########################################################
# #Calculate the log response ratio = log(treatment-control)
# out<-list()
# for (i in 1:length(respdata)){
#   temp<-respdata[[i]] #rename the list item
#   logI<-log(temp[,1])
#   logC<-log(temp[,2])
#   logRR<-logI/logC
#   out[[i]]<-cbind(logI,logC,logRR)
# }
# names(out)<-respvars1
# resp.list<-out
# sum(is.infinite(unlist(resp.list)))
# #Did the calculation work? .. No
# #Identify where logRR = Inf
# out<-list()
# for (i in 1:length(resp.list)){
#   temp<-resp.list[[i]]
#   infs<-is.infinite(temp[,3])
#   logRR.infs<-which(infs == T)
#   out[[i]]<-logRR.infs
# }
# names(out)<-respvars1
# out
# #Deal with inf logRR vals... nh, ammon, nitrif, nminz
# #need to replace cell in respdata with 1.0001 b/c log of 1 is 0 and x/0 = Inf
# temp<-respdata$nh
# temp[out$nh,2]<-1.0001
# respdata$nh<-temp
# temp<-respdata$ammon
# temp[out$ammon,2]<-1.0001
# respdata$ammon<-temp
# temp<-respdata$nitrif
# temp[out$nitrif,2]<-1.0001
# respdata$nitrif<-temp
# 
# write.table(respdata, file='soilresps_transformed.txt', sep='\t', row.names=T,col.names=T)
# #View(respdata)
#########################################################

#read back in
respdata<-read.table(file='soilresps_transformed.txt',header=TRUE,sep="\t")

#names for soilresps
temp<-colnames(obsID_soil)[c(2,4,6,8,10,12,14,16,18,20,22,24)] #raw response data
temp2<-unlist(strsplit(temp,"_"))
respvars1<-temp2[temp2 != 'I']

#Re-run the logRR calculation using the transformed soilresps
out<-list()
invs<-seq(1,23,2)
nats<-seq(2,24,2)
n<-length(invs)
for (i in 1:n){
  tempI<-respdata[[invs[i]]] #rename the list item
  tempN<-respdata[[nats[i]]] #rename the list item
  logI<-log(tempI)
  logC<-log(tempN)
  logRR<-logI/logC
  out[[i]]<-logRR
}
names(out)<-respvars1
resp.list<-out #resp.list has relevant response vars, logRRs
View(resp.list)

mat<-numeric(0)
for(i in (1:length(resp.list))){
  temp<-resp.list[[i]]
  mat<-cbind(mat,temp)
}
View(mat)
colnames(mat)<-paste(respvars1,'logRR', sep='_')
obsIDlist<-seq(1,118,1)
obsID_resps<-cbind(obsIDlist, mat)

write.table(obsID_resps, file='soilresps.txt', sep='\t', row.names=T,col.names=T)

#########################################################
#########################################################
#(2) Make a table like obsID_community using bgcTABLE -> obsID_community_fromtraits
#First, assign a percent to each species in invaded and native communities
STUDY<-unique(traits$obsID)
store1<-numeric(0)
store2<-numeric(0)
for (i in 1:length(STUDY)){
  studydata<-traits[traits$obsID==STUDY[i],]    #subset rows of traits that are in one obsID (STUDY)
  
  nI<-dim(studydata[studydata$status=='I',])[1] #count the number of invasive spp
  nN<-dim(studydata[studydata$status=='N',])[1] #count the number of native spp
  ntot<-nI+nN                                   #count the total number of species
  row1<-c(STUDY[i], nI, nN, ntot)               #store the species-type counts
  store1<-rbind(store1,row1)
  
  percIinfo<-sum(!is.na(studydata$spperc_inv))  #for how many species is there info on its abundance in invaded areas?
  percNinfo<-sum(!is.na(studydata$spperc_nat))  #for how many species is there info on its abundance in native areas?
  row2<-c(percIinfo,percNinfo)                  #store the number of species with info about invaded area abundance, native area abundance
  store2<-rbind(store2,row2)
} 
colnames(store1)<-c('obsID','nI','nN','ntot')
colnames(store2)<-c('percIinfo','percNinfo')
percinfo<-cbind(store1,store2)
percinfo<-data.frame(percinfo)
#why are there 0 native species for obsID 67 and 116?
#obsID 67 - was a greenhouse experiment, before/after comparison so no other spp
#obsID 116 - was a field experiment, cleared vs invaded comparison so no other spp
#exclude these obsIDs from 'traits'
traits1<-traits[!traits$obsID=='67' & !traits$obsID=='116' ,]
traits<-traits1
#exclude these obsIDs from 'percinfo'
percinfo1<-percinfo[!percinfo$obsID=='67' & !percinfo$obsID=='116' ,]
percinfo<-percinfo1
        #make empty columns for the 'obsNsubID v traits' table to fill the estimated relative abundance of each species (rows) in invaded and native areas
        #sppercI<-rep(0, dim(traits)[1]) 
        #sppercN<-rep(0, dim(traits)[1])

#in general terms: want to use spperc info if it exists, if not, divide the community evenly by the number of species

#indices
STUDY<-unique(traits$obsID)
length(STUDY)
totalnumspp<-percinfo$ntot
numinvspp<-percinfo$nI
numnatspp<-percinfo$nN
#initialize storage and counters
Istoredrows<-numeric(0)
Nstoredrows<-numeric(0)
results<-numeric(0)
i<-0
k<-0
p<-0
bindedcolnames<-c(colnames(traits)[1:7],'invAperc','natAperc')
for (i in 1:length(STUDY)){
  
  studydata<-traits[traits$obsID==STUDY[i],] #subset rows of traits that are in one obsID (STUDY)
    #for invasive species...
    Istudydata<-studydata[studydata$status=='I',]
    for (k in 1:dim(Istudydata)[1]){
      Irow<-Istudydata[k,]
      IrowID<-Irow[1:7]
      #spperc_nat
      if(is.na(Irow$spperc_nat)==T){Inat.row<-0}
      if(!is.na(Irow$spperc_nat)==T){Inat.row<-Irow$spperc_nat}
      #spperc_inv
      if(is.na(Irow$spperc_inv)==T){Iinv.row<-(1/totalnumspp[i])*100}
      if(!is.na(Irow$spperc_inv)==T){Iinv.row<-Irow$spperc_inv}
      Istoredrow<-cbind(IrowID,Iinv.row,Inat.row)
      colnames(Istoredrow)<-bindedcolnames
      Istoredrows<-rbind(Istoredrows,Istoredrow)
    }
    
    #for native speices...
    Nstudydata<-studydata[studydata$status=='N',]
    for (p in 1:dim(Nstudydata)[1]){
      Nrow<-Nstudydata[p,]
      NrowID<-Nrow[1:7]
      #spperc_nat
      if(is.na(Nrow$spperc_nat)==T){Nnat.row<-(1/numnatspp[i])*100}
      if(!is.na(Nrow$spperc_nat)==T){Nnat.row<-Nrow$spperc_nat}
      #spperc_inv
      if(is.na(Nrow$spperc_inv)==T){Ninv.row<-(1/totalnumspp[i])*100}
      if(!is.na(Nrow$spperc_inv)==T){Ninv.row<-Nrow$spperc_inv}
      Nstoredrow<-cbind(NrowID,Ninv.row,Nnat.row)
      colnames(Nstoredrow)<-bindedcolnames
      Nstoredrows<-rbind(Nstoredrows,Nstoredrow)
    }
}
results<-rbind(Istoredrows,Nstoredrows)
obsNsubID_abund<-results
#reorder by obsNsubID so that it matches the rows in 'traits'
library(doBy)
temp<-orderBy(~obsNsubID, obsNsubID_abund)
obsNsubID_abund<-temp
#add two columns to 'traits'
traits$invAperc<-obsNsubID_abund$invAperc
traits$natAperc<-obsNsubID_abund$natAperc

write.table(traits, file='traits_calcabund.txt', sep='\t', row.names=T,col.names=T)

#################################
#################################
#################################

#(3) Use 'traits' (species abund per area & species trait value) to calc community weighted mean (CWM) trait values for each obsID's invaded and native areas. Then, use this calc value if there isn't already an existing value.
# trait CWM = SUM[(spp abund i.e. 50% of community /100)*(spp trait val)]
#View(traits)

traitdf<-data.frame(traits[,c(1:2,12:29)]) #trait matrix by obsNsubID
colnames(traitdf)
numtraits<-dim(traitdf)[2]-4
STUDY<-unique(traitdf$obsID)
length(STUDY)
TRAIT<-seq(1,numtraits,1)
length(TRAIT)
results<-numeric(0)

for (i in 1:length(STUDY)){ #loop through each obsID, for that obsID..

  for (t in 1:length(TRAIT)){ #loop through each trait, for that trait...
      
    studydata<-traitdf[traitdf$obsID==STUDY[i],] #subset rows of traits that are in one obsID (STUDY)
    invAbund<-studydata$invAperc/100 #do some renaming of species vectors
    natAbund<-studydata$natAperc/100
    traitvals<-studydata[,TRAIT[t]+2] #subset trait column
    traitname<-colnames(studydata)[TRAIT[t]+2]
        
    #calc CWM for invaded area; CWM=SUM[(spp abund i.e. 50% of community /100)*(spp trait val)]
    tempI<-invAbund*traitvals
    numspp<-length(studydata$obsID)
    numinfospp<-sum(!is.na(studydata[,TRAIT[t]+2]))
    invCWM <-sum(tempI, na.rm=T)
    if(sum(is.na(tempI))/length(tempI)==1){invCWM<-NA}
    
    #calc CWM for native area; CWM=SUM[(spp abund i.e. 50% of community /100)*(spp trait val)]
    tempN<-natAbund*traitvals
    natCWM <-sum(tempN, na.rm=T)
    if(sum(is.na(tempN))/length(tempN)==1){natCWM<-NA}
    
    #store CWM for invaded area and CWM for native area
    store<-c(STUDY[i],TRAIT[t], traitname, invCWM, natCWM, numspp, numinfospp)
    results<-rbind(results,store)
  }
}
colnames(results)<-c('obsID','traitcolnum','traitname','invCWM','natCWM','numspp','numinfospp')
CWMtraits<-data.frame(results)
CWMtraits$invCWM<-as.numeric(as.character(CWMtraits$invCWM))
CWMtraits$natCWM<-as.numeric(as.character(CWMtraits$natCWM))
CWMtraits$numspp<-as.numeric(as.character(CWMtraits$numspp))
CWMtraits$numinfospp<-as.numeric(as.character(CWMtraits$numinfospp))
CWMtraits$info <- round((CWMtraits$numinfospp / CWMtraits$numspp)*100) # 'info' = the percent of spp in the community with trait data
#View(CWMtraits)
write.table(CWMtraits, file='CWMtraits_calc.txt', sep='\t', row.names=T,col.names=T)

#########################################################
#########################################################
#########################################################
#########################################################

#BCG data - part4
#########################################################
#(1) Reformat the following datasets: 
  #calcCWM_traits for nat and inv areas
  #reportedCWM_traits for nat and inv areas
  #invader_traits
  #soilvals for nat areas
  #soilresps difference between nat and inv areas

#need these txt files:
  #'soilresps_raw.txt' --> soilvals
  #'soilresps.txt' --> soilresps
  #'traits_calcabund.txt' --> invader_traits
  #'CWMtraits_calc.txt' --> calcCWM nat and inv
  #'obsID_community_frompapers.txt' --> reportedCWM nat and inv

#this code will produce these txt files:
  #'obsID_community_frompapers_mggUNITS.txt' #same as 'obsID_community_frompapers.txt' but with sppercN and splitterpercN in units of mg/g instead of %
  #'invCWMreported.txt'
  #'natCWMreported.txt'
  #'traitskey1.txt'
  #'invCWMcalc_ex60.txt'
  #'natCWMcalc_ex60.txt'
  #'invtraits.txt'
  #'respssoil.txt'

setwd("~/Desktop/MetaAnalysis_R/2_BGC data processing")
library(reshape)
library(doBy)

#########################################################
soilresps_raw<-read.table("soilresps_raw.txt",header=TRUE,sep="\t") # from BGCdata_part3 code
soilresps<-read.table("soilresps.txt",header=TRUE,sep="\t")  # from BGCdata_part3 code
traits<-read.table("traits_calcabund.txt",header=TRUE,sep="\t") # from BGCdata_part3 code
CWMtraits<-read.table("CWMtraits_calc.txt",header=TRUE,sep="\t") # from BGCdata_part3 code
obsID_community1<-read.table("obsID_community_frompapers.txt",header=TRUE,sep="\t") # raw
obsID_community<-obsID_community1[1:118,1:25]
#traitskey<-read.table("TRYnBGC_traitskey.txt",header=TRUE,sep="\t") # raw
traitskey<-read.table("data_traitids_unitconv.txt",header=TRUE,sep="\t") # raw

#########################################################
#########################################################

#(1)Reformat the following datasets: 
  #(a) calcCWM_traits for nat and inv areas
  #(b) reportedCWM_traits for nat and inv areas
  #(c) invader_traits
  #(d) soilvals for nat areas
  #(e) soilresps difference between nat and inv areas

#NOTE
#why are there 0 native species for obsID 67 and 116?
#obsID 67 - was a greenhouse experiment, before/after comparison so no other spp
#obsID 116 - was a field experiment, cleared vs invaded comparison so no other spp
#exclude these obsIDs

##########################
#(a) calcCWM_traits for nat and inv areas
#########
#reshape the data
CWMtraits1<-data.frame(obsID=CWMtraits$obsID,traitname=CWMtraits$traitname,invCWM=CWMtraits$invCWM,natCWM=CWMtraits$natCWM, info=CWMtraits$info)
melted<-melt.data.frame(CWMtraits1, id.vars=c('obsID','traitname'))
melted$status<-rep(NA,dim(melted)[1]) #add a column to 'melted' for status, fill with NAs
melted[melted$variable=='invCWM','status']<-'inv'
melted[melted$variable=='natCWM','status']<-'nat'
#organize percinfo by trait types and obsIDs
# 'info' = the percent of spp in the community with trait data
info1<-subset(melted, variable=='info')  
info.M<-data.frame(obsID=info1$obsID,traitname=info1$traitname,value=info1$value)
info.wide<-reshape(info.M, v.names='value',idvar=c('obsID'), timevar='traitname', direction='wide')
#organize trait values by trait types and obsIDs
CWMtraits.M<-data.frame(obsID=melted$obsID,status=melted$status,traitname=melted$traitname,value=melted$value)
CWMtraits.wide<-reshape(CWMtraits.M, v.names='value',idvar=c('obsID','status'), timevar='traitname', direction='wide')
#isolate inv rows from nat rows and re-attached 'obsID' column
invtemp<-CWMtraits.wide[1:(dim(CWMtraits.wide)[1]/2),-(1:2)]
nattemp<-CWMtraits.wide[((dim(CWMtraits.wide)[1]/2)+1):dim(CWMtraits.wide)[1],-(1:2)]
obsID<-CWMtraits.wide[1:(dim(CWMtraits.wide)[1]/2),1]
invCWMcalc<-cbind(obsID,invtemp)
natCWMcalc<-cbind(obsID,nattemp)
#rename trait columns with MeasID nums
colnames(invCWMcalc)<-colnames(traits)[c(2,12:27)]
colnames(natCWMcalc)<-colnames(traits)[c(2,12:27)]
#remove trait columns that do not have matching 'community' categories, see traitskey
w<-which(!is.na(traitskey[3:18,'MeasMatch'])) #identify cells in the species scale that have a MeasMatch on the community scale
invCWMcalc1<-invCWMcalc[,c(1,w+1)]
natCWMcalc1<-natCWMcalc[,c(1,w+1)]

write.table(invCWMcalc1, file='invCWMcalc.txt',sep='\t')
write.table(natCWMcalc1, file='natCWMcalc.txt',sep='\t')

##########################
#(b) reported CWM_traits for nat and inv areas
#########
#exclude 2 studies w/o natives
obsID_community1<-obsID_community[!obsID_community$obsID=='67' & !obsID_community$obsID=='116' ,] 
#rename columns using MeasID nums
colnames(obsID_community1)<-c(colnames(traits)[2],paste('Meas',rep(19:30, each = 2),rep(c('inv','nat'),11),sep='_'))
#reshape the data
melted<-melt.data.frame(obsID_community1, id.vars='obsID')
temp<-matrix(unlist(strsplit(as.character(melted$variable), '_',fixed=T)), 
             ncol=3, byrow=T) #list of traits and inv/nat
melted$traitname<-temp[,2] #add traitNAME and status cols to 'melted'
melted$status<-temp[,3]
melted1<-melted[,-2] #remove 'variable' column
#reshape so that traits are columns
obsID_comm.M<-data.frame(obsID=melted1$obsID,status=melted1$status,traitname=melted1$traitname,value=melted1$value)
obsID_comm.wide<-reshape(obsID_comm.M, v.names='value',idvar=c('obsID','status'), timevar='traitname', direction='wide')
#separate rows by status (inv/nat) and then remove status column
invtemp<-obsID_comm.wide[1:(dim(obsID_comm.wide)[1]/2),]
nattemp<-obsID_comm.wide[((dim(obsID_comm.wide)[1]/2)+1):dim(obsID_comm.wide)[1],]
invCWMreported<-invtemp[,-2]
natCWMreported<-nattemp[,-2]
#rename trait columns with MeasID nums
colnames(invCWMreported)<-c(colnames(traits)[2],paste('Meas',rep(19:30, each = 1),sep='_'))
colnames(natCWMreported)<-c(colnames(traits)[2],paste('Meas',rep(19:30, each = 1),sep='_'))

write.table(invCWMreported, file='invCWMreported.txt',sep='\t')
write.table(natCWMreported, file='natCWMreported.txt',sep='\t')

##########################
#(c) invader_traits 
#NOTE: this is NOT the same as invaded area CWM trait values.  Goal is to get an average 'invader' value for each invaded area - this value does not incorporate the trait values of natives that happen to be in invaded areas too
#########
str(traits)
traits$GenusSpecies<-as.character(traits$GenusSpecies)
traits$Genus<-as.character(traits$Genus)
traits$Species<-as.character(traits$Species)

STUDY<-unique(traits$obsID)
store<-character(0)
i<-0
for (i in 1:length(STUDY)){
  studydata<-traits[traits$obsID==STUDY[i],] #subset rows of traits that are in one obsID (STUDY)
  Istudydata<-studydata[studydata$status=='I',]
  tmp<-apply(Istudydata[,-(1:7)], 2, mean, na.rm=T)
  tmp2<-(as.matrix(tmp))
  names<-Istudydata[1,2:5]
  row<-cbind(names,t(tmp2))
  if(dim(Istudydata)[1]>1){row[1,2:4]<-'multiple'}
  store<-rbind(store,row)
}
NaNreplace<-function(x){x[is.nan(x)] <- NA; x}
store[] <- lapply(store, NaNreplace) 
inv.traits<-store

#make a column for abbrev names to plot
inv.traits$initials<-paste(substr(inv.traits$Genus, 1, 1),substr(inv.traits$Species, 1, 1), sep='')

write.table(inv.traits, file='invadersONLY.txt',sep='\t')

##########################
#(d) soilvals for nat areas
#########
#separate native area soil data from invaded area soil data
index<-rep(1:2,times=dim(soilresps_raw)[2]/2)
natsoil<-soilresps_raw[,which(index==2)]
invsoil<-soilresps_raw[,which(index==1)]
#re-attached obsID labels
obsID<-soilresps$obsID 
natsoil1<-cbind(obsID,natsoil)
invsoil1<-cbind(obsID,invsoil)
#exclude 2 obsIDs
nat.soil<-natsoil1[!natsoil1$obsID=='67' & !natsoil1$obsID=='116' ,] #exclude 2 studies w/o natives
inv.soil<-invsoil1[!invsoil1$obsID=='67' & !invsoil1$obsID=='116' ,] #exclude 2 studies w/o natives
#rename columns
temp<-matrix(unlist(strsplit(colnames(nat.soil)[-1],'.',fixed=T)), 
             ncol=2, byrow=T)
colnames(nat.soil)<-c('obsID',temp[,2])
temp<-matrix(unlist(strsplit(colnames(inv.soil)[-1],'.',fixed=T)), 
             ncol=2, byrow=T)
colnames(inv.soil)<-c('obsID',temp[,2])

write.table(nat.soil, file='nat_soil.txt',sep='\t')
write.table(inv.soil, file='inv_soil.txt',sep='\t')

##########################
#(e) soilresps difference between nat and inv areas
#########
#rename obsIDlist -> 'obsID'
colnames(soilresps)[1]<-'obsID'
#exclude 2 studies w/o natives
resps.soil<-soilresps[!soilresps$obsID=='67' & !soilresps$obsID=='116' ,] 

#determine if there are outliers
#nminz
hist(resps.soil$nminz_logRR)
range(resps.soil$nminz_logRR, na.rm=T)
#nh
hist(resps.soil$nh_logRR)
range(resps.soil$nh_logRR, na.rm=T) # outliers
resps.soil[which(resps.soil$nh_logRR==max(resps.soil$nh_logRR, na.rm=T)),'obsID'] #obsID=115
resps.soil[resps.soil$obsID=='115','nh_logRR']<-NA # replace cell with NA
#ammon
hist(resps.soil$ammon_logRR) 
range(resps.soil$ammon_logRR, na.rm=T) 
#nitrif
hist(resps.soil$nitrif_logRR)
range(resps.soil$nitrif_logRR, na.rm=T) 

write.table(resps.soil, file='resps_soil.txt',sep='\t')

##########################
#Quality Control the CWM trait values by excluding CWM data with less that 60% of species per community contribution
#exclude invCWMcalc and natCWMcalc with less than 60% community info
info.wide.ex60<-info.wide
#function that replaces a cell with NA if it has a value less than 60
FilterInfo60<-function(x){x[x<60] <- NA; x}
#apply this function across info.wide.ex60... this table holds the percent contribution of species within communities to the CWM trait value
info.wide.ex60[,-1] <- lapply(info.wide.ex60[,-1], FilterInfo60) 

#apply info.wide.ex60 data template to invCWMcalc and natCWMcalc tables to exclude the appropriate cells
invCWMcalc.ex60<-invCWMcalc; natCWMcalc.ex60<-natCWMcalc #initialize new tables
numcols<-dim(invCWMcalc.ex60)[2] # count the number of columns... this number includes obsID column, but its okay
for (i in 1:numcols){ #loop through each column of info.wide.ex60
  rownums<-which(is.na(info.wide.ex60[,i])) #select vals in template column that are NA
  invCWMcalc.ex60[rownums,i]<- NA #replace vals in the corresponding finaldata column with NA
  natCWMcalc.ex60[rownums,i]<- NA
}

write.table(info.wide.ex60, file='info_wide_ex60.txt',sep='\t')
write.table(invCWMcalc.ex60, file='invCWMcalc_ex60.txt',sep='\t')
write.table(natCWMcalc.ex60, file='natCWMcalc_ex60.txt',sep='\t')

