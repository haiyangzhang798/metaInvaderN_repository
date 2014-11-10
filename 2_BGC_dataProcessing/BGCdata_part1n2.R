#BGC data - part1
#Identify common traits between BGC and TRY datasets
#Construct a new spp x trait datatable, fill with BGC trait data from reference papers 1st, then fill with species-specific TRY data 2nd, then fill with genus specific TRY data 3rd

#will need these txt files: 
#'obsNsubID_species_frompapers.txt' #rows are unique combinations of obsID and subID - columns hold the species and its associated trait data values
#'TRY_dataidsBYsppAVGs.txt' #rows are unique genus/species from the TRY dataset, columns hold the average trait data values
#'TRY_dataidsBYgenusAVGs.txt' #rows are unique genus from the TRY dataset, columns hold the average trait data values
#'TRYnBGC_traitskey.txt' #key that links TRY and BGC trait categories

#this code will produce these txt files:
#'bgcTABLE_bgctraits.txt' #same as 'obsNsubID_species_frompapers_mggUNITS.txt, but with labelled and organized columns
#'bgcTABLE_trytraitsGENUSSPP.txt'
#'bgcTABLE_trytraitsGENUS.txt'

#setwd("~/Desktop/BGC_R")
setwd("~/Desktop/MetaAnalysis_R/2_BGC data processing")

#########################################################
#data_sp<-read.table("obsNsubID_species_frompapers.txt",header=TRUE,sep="\t") #raw
    #View(data_sp)
#data_species<-data_sp[1:576,]
#   #View(data_species)
# #data_traitids<-read.table("TRYnBGC_traitskey.txt",header=TRUE,sep="\t") #raw
# key<-read.table("measurementsKey.txt",header=TRUE,sep="\t") #raw
#   #View(key); View(key[1:42,1:14])
#   data_traitids<-key[1:42,1:14]
  #View(data_traitids)
# GSdata_try<-read.table("TRY_dataidsBYsppAVGs.txt",header=TRUE,sep="\t") #from TRYpart2 code
#   #View(GSdata_try)
# Gdata_try<-read.table("TRY_dataidsBYgenusAVGs.txt",header=TRUE,sep="\t") #from TRYpart2 code
#   #View(Gdata_try)

#########################################################
# View(data_traitids) #need to convert TRY data from mg/g to % for TRY N conc values
# tmp<-subset(data_traitids,data_traitids$TRY_Unit == 'mg/g')
# tmp2<-tmp$TRY_DataID
# selectcols<-which(colnames(GSdata_try) %in% tmp2) #find out which columns in GSdata_try and Gdata_try have data that need to be converted
# 
# #dataset<-GSdata_try
# dataset<-Gdata_try
# 
# matrixcols<-length(selectcols)
# matrixrows<-dim(dataset)[1]
# newdataset<-dataset
# for (i in 1:matrixrows){
#   cellvals<-dataset[i,selectcols]
#   newcellvals<-(cellvals/1000)*100 # convert mg/g to % 
#   newdataset[i,selectcols]<-newcellvals
# }
# View(cbind(dataset[1:100,selectcols[1]],newdataset[1:100,selectcols[1]]))
# 
# #GSdata_try_unitconv<-newdataset
# Gdata_try_unitconv<-newdataset
# 
# write.table(GSdata_try_unitconv, file='GSdata_try_unitconv.txt', sep='\t')
# write.table(Gdata_try_unitconv, file='Gdata_try_unitconv.txt', sep='\t')
# 
# #update measurementsKey
# tmp<-which(data_traitids$TRY_Unit == 'mg/g')
# data_traitids$TRY_Unit <- factor(data_traitids$TRY_Unit, levels = c(levels(data_traitids$TRY_Unit), "%"))
# data_traitids[tmp,'TRY_Unit']<-rep('%',length(tmp))
#   #View(data_traitids)
# write.table(data_traitids, file='data_traitids_unitconv.txt', sep='\t')
# 
# GSdata_try<-read.table('GSdata_try_unitconv.txt',header=TRUE,sep="\t")
#   #View(GSdata_try)
# Gdata_try<-read.table('Gdata_try_unitconv.txt',header=TRUE,sep="\t")
#   #View(Gdata_try)

#########################################################
# #select TRY columns that are in the data_traitids table, rename and re-order with MeasID
# #For GS dataset
#dataset<-GSdata_try
# #For G dataset
#dataset<-Gdata_try

# dataidlist<-data_traitids[!is.na(data_traitids$TRY_DataID),'TRY_DataID'] #make a list of the dataIDs that are listed in the measurementKey table
# n<-length(dataidlist) #number of dataIDs (cols) in new table
# nc<-n*dim(dataset)[1] #number of cells in new table
# store<-rep(NA,n) #initialize a storage vector to hold the GSdata_try column number that matches each dataID in the dataidlist
# for (i in 1:n){store[i]<-which(colnames(dataset)==dataidlist[i])}
# tempdata<-matrix(data=rep(NA,nc),nrow=dim(dataset)[1],ncol=n) # initialize a matrix to hold GSdata_try columns that match each dataID in the dataidlist
# for (i in 1:length(store)){tempdata[,i]<-dataset[,store[i]]}
# stored<-numeric(0) #initialize a storage vector to hold the row number of each dataID in dataidlist in the measurementKey table
# for (i in 1:length(dataidlist)){
#   store<-which(data_traitids$TRY_DataID==dataidlist[i])  
#   stored<-c(stored,store)
# }
# 
# #re-attach species/genus id column as column#1 and name the rest of the columns that hold trait data with the MeasID num
# #For GS dataset
# GenusSpecies<-GSdata_try$GenusSpecies #id vector for new GSdata_try table
# GSdata_try_renamed<-data.frame(GenusSpecies,tempdata) 
# colnames(GSdata_try_renamed)[-1]<-paste('MeasID',data_traitids[stored,'MeasID'],sep='_') #name the columns of tempdata so that they reflect the MeasID
# #For G dataset
# Genus<-Gdata_try$Genus #id vector for new Gdata_try table
# Gdata_try_renamed<-data.frame(Genus,tempdata) 
# colnames(Gdata_try_renamed)[-1]<-paste('MeasID',data_traitids[stored,'MeasID'],sep='_') #name the columns of tempdata so that they reflect the MeasID
# 
# #add 4 columns for 'missing' MeasIDs
# # #For GS dataset
# #dataset<-GSdata_try_renamed
# # #For G dataset
# dataset<-Gdata_try_renamed
# 
# list<-seq(3,18,1)
# list[which(!list %in% stored)] #which MeasID nums are in 'list' but not in 'stored'
# blankvec<-rep(NA,dim(dataset)[1])
# #For GS dataset
# GSdata_try_renamed2<-data.frame(GenusSpecies, GSdata_try_renamed[,2:4],blankvec,GSdata_try_renamed[,5],blankvec,blankvec,blankvec,GSdata_try_renamed[,6:13])
# colnames(GSdata_try_renamed2)[-1]<-paste('MeasID',list, sep='_') 
#   #View(GSdata_try_renamed2)
# #For G dataset
# Gdata_try_renamed2<-data.frame(Genus, Gdata_try_renamed[,2:4],blankvec,Gdata_try_renamed[,5],blankvec,blankvec,blankvec,Gdata_try_renamed[,6:13])
# colnames(Gdata_try_renamed2)[-1]<-paste('MeasID',list, sep='_') 
# View(Gdata_try_renamed2)
# 
# #Write the renamed datasets
# write.table(GSdata_try_renamed2, file='GSdata_try_renamed.txt', sep='\t')
# write.table(Gdata_try_renamed2, file='Gdata_try_renamed.txt', sep='\t')

# #Read the renamed datasets back in
# GSdata_try<-read.table('GSdata_try_renamed.txt',header=TRUE,sep="\t")
# #View(GSdata_try)
# Gdata_try<-read.table('Gdata_try_renamed.txt',header=TRUE,sep="\t")
# #View(Gdata_try)
# 
# 
# #################################
# #rename data_species columns with MeasID
# list<-seq(3,18,1)
# colnames(data_species)[12:27]<-paste('MeasID',list, sep='_') 
# View(data_species)
# # 
# #To summarize the changes made in all datasets....
# GS.try.data<-GSdata_try
# write.table(GS.try.data, file='TRY_sppBYtraits.txt', sep='\t')
# G.try.data<-Gdata_try
# write.table(G.try.data, file='TRY_genusBYtraits.txt', sep='\t')
#bgcTABLE.bgctraits<-data_species
#write.table(bgcTABLE.bgctraits, file='bgcTABLE_bgctraits.txt', sep='\t')

# GS.try<-read.table('TRY_sppBYtraits.txt',header=TRUE,sep="\t")
# G.try<-read.table('TRY_genusBYtraits.txt',header=TRUE,sep="\t")
# bgcTABLE.bgctraits<-read.table('bgcTABLE_bgctraits.txt',header=TRUE,sep="\t")
# data_traitids<-read.table('data_traitids_unitconv.txt',header=TRUE,sep="\t")


#########################################################
#########################################################
# #Isolate data from id cols in each table
# GS.try.data<-GS.try[,-1]
# GS.try.dataID<-GS.try[,1]
# G.try.data<-G.try[,-1]
# G.try.dataID<-G.try[,1]
# bgcTABLE.bgctraits.data<-bgcTABLE.bgctraits[,12:27]
# bgcTABLE.bgctraitsID<-bgcTABLE.bgctraits[,1:11]
# 
# #For the bgcTABLE species list, fill with exact TRY data genus/species matches
# #loop through list of try.data species and subset matching bgcTABLE rows.  Fill rows with data from try.data for that genus/species.
# 
# SpeciesNameList<-as.character(bgcTABLE.bgctraitsID$GenusSpecies)
# length(SpeciesNameList) #576
# 
# newdata<-numeric(0) #initialize a table with the same dims as bgcTABLE to hold GS.try.data values
# emptyrow<-rep(NA,dim(bgcTABLE.bgctraits)[2]) #make an empty row that can be inserted into the newdata table (same dims as bgcTABLE) when no GS.try.data matches that obsNsubID
# 
# for (i in (1:length(SpeciesNameList))){
#   row<-GS.try[GS.try$GenusSpecies==SpeciesNameList[i],] #subset rows of GS.try where the species name matches the current species name
#   if(dim(row)[1]==1){newdata<-rbind(newdata,row)} #if there is only one matching row in GS.try, then add that row to 'newdata'
#   if(dim(row)[1]==0){newdata<-rbind(newdata,emptyrow)} #if there is no match, then add an empty row
# }
# #View(newdata) #note, inserting an empty row puts an 'NA' into the 1st species name column too
# newdata<-newdata[,-1] #fix that problem
# bgcTABLE.trytraitsGENUSSPP<-cbind(bgcTABLE.bgctraitsID,newdata) #new data_species based ONLY on TRY data
# #View(bgcTABLE.trytraitsGENUSSPP)
#write.table(bgcTABLE.trytraitsGENUSSPP, file='bgcTABLE_trytraitsGENUSSPP.txt', sep='\t')

#########################################################
#########################################################
#For the bgcTABLE species list, fill with only TRY data with genus matches
#loop through list of try.data genusus and subset matching bgcTABLE rows.  Fill rows with data from try.data for that genus.

# GenusNameList<-as.character(bgcTABLE.bgctraits$Genus)
# newdata<-numeric(0)
# emptyrow<-rep(NA,dim(bgcTABLE.bgctraits)[2])
# for (i in (1:length(GenusNameList))){
#   row<-G.try[G.try$Genus==GenusNameList[i],]
#   if(dim(row)[1]==1){newdata<-rbind(newdata,row)}
#   if(dim(row)[1]==0){newdata<-rbind(newdata,emptyrow)}
#   if(dim(row)[1]>1){newdata<-rbind(newdata,emptyrow)} #there are NAs in GenusNameList that 'match' all rows
# }
# #View(newdata) #note, inserting an empty row puts an 'NA' into the 1st species name column too
# newdata<-newdata[,-1] #fix that problem
# bgcTABLE.trytraitsGENUS<-cbind(bgcTABLE.bgctraitsID,newdata) #new data_species based ONLY on TRY data genuses
# #View(bgcTABLE.trytraitsGENUS)
#write.table(bgcTABLE.trytraitsGENUS, file='bgcTABLE_trytraitsGENUS.txt', sep='\t')

#########################################################
#########################################################
#########################################################
#########################################################

#BGC data - part2
#Read-in bgcTABLE data filled with different data sources (bgc papers, TRY spp/genus, TRY genus)
#Fill out trait table with (1) bgc.traits, (2) GStry.traits, and (3) Gtry.traits in that order

#need these txt files from Step1:
#'bgcTABLE_bgctraits.txt' 
#'bgcTABLE_trytraitsGENUSSPP.txt'
#'bgcTABLE_trytraitsGENUS.txt'

#this code will produce these txt files:
#'bgcTABLE.txt'

setwd("~/Desktop/BGC_R")

#########################################################
bgc.traits<-read.table("bgcTABLE_bgctraits.txt",header=TRUE,sep="\t") # from Step1
GStry.traits<-read.table("bgcTABLE_trytraitsGENUSSPP.txt",header=TRUE,sep="\t") # from Step1
Gtry.traits<-read.table("bgcTABLE_trytraitsGENUS.txt",header=TRUE,sep="\t") #from Step1

#########################################################
#Isolate the trait data from each trait table
idcols<-bgc.traits[,(1:11)]
bgc.traitcols<-bgc.traits[,-(1:11)]
GStry.traitcols<-GStry.traits[,-(1:11)]
Gtry.traitcols<-Gtry.traits[,-(1:11)]
ncols<-dim(bgc.traitcols)[2]

#########################################################
#(1a) Identify where in the trait table there is bgc.traits info
occupdata<-numeric(0)
for (i in 1:ncols){
  col<-!is.na(bgc.traitcols[,i])
  occupdata<-cbind(occupdata,col)
}
colnames(occupdata)<-colnames(bgc.traitcols)
bgc.traitcolsO<-occupdata

#(1b) Insert GStry.trait data into empty cells of bgc.traitcols -> traitcols1
#pseudocode: 
#initialize an empty trait table matrix, traitcols
tmp<-dim(bgc.traitcolsO)[1]*dim(bgc.traitcolsO)[2] 
x<-rep(NA,tmp) 
traitcols<-matrix(x,nr=dim(bgc.traitcolsO)[1],nc=dim(bgc.traitcolsO)[2])
#loop through columns of bgc.traitcolsO. For each column,
#loop through rows. for each row,
#if bgc.traitcols0[k,i] == 0, then replace with GStry.traitcols[k,i]
#if bgc.traitcols0[k,i] == 1, then leave this cell alone (aka replace with bgc.traitcols[k,i])
for (i in 1:dim(bgc.traitcolsO)[2]){ #each column
  for (k in 1:dim(bgc.traitcolsO)[1]){ #each row
    if(bgc.traitcolsO[k,i] == 0){traitcols[k,i]<-GStry.traitcols[k,i]}
    if(bgc.traitcolsO[k,i] == 1){traitcols[k,i]<-bgc.traitcols[k,i]}
  }
}
colnames(traitcols)<-colnames(bgc.traitcols)
traitcols1<-traitcols

#########################################################
#(2a) Identify where in the trait table there is traitcols1 (bgc.traitcols + GStry.traitcols) info
occupdata<-numeric(0)
for (i in 1:ncols){
  col<-!is.na(traitcols1[,i])
  occupdata<-cbind(occupdata,col)
}
colnames(occupdata)<-colnames(bgc.traitcols)
traitcols1O<-occupdata

#(2b) Insert Gtry.trait data into empty cells of traitcols1 -> traitcols2
#pseudocode: 
#initialize an empty trait table matrix, traitcols
traitcols<-matrix(x,nr=dim(bgc.traitcolsO)[1],nc=dim(bgc.traitcolsO)[2]) #this will have the same dims as above
#loop through columns of bgc.traitcols10. For each column,
#loop through rows. for each row,
#if traitcols10[k,i] == 0, then replace with Gtry.traitcols[k,i]
#if traitcols10[k,i] == 1, then leave this cell alone (aka replace with traitcols1[k,i])
for (i in 1:dim(traitcols1O)[2]){ #each column
  for (k in 1:dim(traitcols1O)[1]){ #each row
    if(traitcols1O[k,i] == 0){traitcols[k,i]<-Gtry.traitcols[k,i]}
    if(traitcols1O[k,i] == 1){traitcols[k,i]<-traitcols1[k,i]}
  }
}
colnames(traitcols)<-colnames(bgc.traitcols)
traitcols2<-traitcols

#########################################################
#Add idcols and save the final traitcols data table
traits<-cbind(idcols,traitcols2)
write.table(traits, file='bgcTABLE.txt', sep='\t') #this has bgc.traitcols + GStry.traitcols + Gtry.traitcols in that order

#########################################################
#(3) What kind of data coverage is there?
#Identify where in the trait table there is traitcols2 (bgc.traitcols + GStry.traitcols + Gtry.traitcols) info
occupdata<-numeric(0)
for (i in 1:ncols){
  col<-!is.na(traitcols2[,i])
  occupdata<-cbind(occupdata,col)
}
colnames(occupdata)<-colnames(bgc.traitcols)
traitcols2O<-occupdata

subobs.per.traits<-colSums(traitcols2O)
subobs.per.traits

traits.per.subobs<-rowSums(traitcols2O)
traits.per.subobs


