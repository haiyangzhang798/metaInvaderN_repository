#TRY data - part1
# Compile and Clean raw TRY data

#will need these txt files: 
#"TRY_Proposal_117_Data_Release_2012_08_26.txt"
#"TRY_Proposal_117_Data_Release_2012_09_20.txt"

#this code will produce these txt files:
#'TRYdataT.txt' #trait TRY data w/o dups
#'TRY_traitsdatakey.txt' #list of unique dataid categories and their units


setwd("~/Desktop/BGC_R")

#########################################################
#########################################################
#Coalescing raw data parts 1 and 2 into trait data and non-trait data
dataA<-read.table("TRY_Proposal_117_Data_Release_2012_08_26.txt",header=TRUE,sep="\t") #raw data
dataB<-read.table("TRY_Proposal_117_Data_Release_2012_09_20.txt",header=TRUE,sep="\t") #raw data
data<-rbind(dataA,dataB)

#make sure that data types are identified correctly
data$ObservationID<-as.factor(data$ObservationID)
data$ObsDataID<-as.factor(data$ObsDataID)
data$TraitID<-as.factor(data$TraitID)
data$DataID<-as.factor(data$DataID)
data$OrigObsDataID<-as.factor(data$OrigObsDataID)

#########################################################
#General Info about the dataset/Column headers

#LastName
#FileName
#SpeciesName: original name of the species
#AccSpeciesName: accepted species name (USE THIS)
#ObservationID: identifies which ObsDataIDs are related to one another (USE THIS)
# e.g. two traits measured on the same leaf
# e.g. if plants were grown under experimental conditions, this is reported as a covariate entry with the same ObservationID
#ObsDataID: identifies the row - can be either a trait or a covariate (USE THIS)
#TraitVsNonTrait: identifies rows with trait info (USE THIS)
#TraitID (USE THIS)
#TraitName
#DataID: identifies the trait subgroup (USE THIS)
#DataName
#OrigValueStr: original value as a text string
#OrigUncertaintyStr: original uncertainty as text string
#OrigUnitStr: original unit as text string
#Unit.UnitName:	Unit name
#OrigValue: Original value
#ValueKindName:	Value kind (single measurement, mean, median, etc.)
#StdValue:	Standardized values; not available in all cases (USE THIS)
#Unit_1.UnitName:	Standard unit: Always available for standardized traits (USE THIS)
#RelUncertainty%:	Relative uncertainty in %
#UncertaintyName:	Kind of uncertainty (standard deviation, standard error,...)
#OrigObsDataID:	indicates that that row is a duplicate, contains the ObsDataID of the original entry
#NonTraitCategories:	Type of ancillary data

#########################################################
#########################################################
#########################################################
#Produce a non-trait dataset
dataNT<-subset(data, !TraitVsNonTrait=='x') #subset the rows containing non-trait data
summary(dataNT)
#still need to do this...
#why are there two rows with the same ObsDataID?
ID<-as.vector(unique(dataNT1$ObsDataID))
dup.list<-character(0)
for (i in 1:length(ID)){ #loop through unique ID list
  temp<-subset(dataNT, ObsDataID==ID[i]) # for each ID, see how many rows there are
  if(dim(temp)[1]>1){dup.list<-c(dup.list,ID[i])} #if there is more than one row, then store that ID num in a duplicate list
}
dup.listNT1<-dup.list
temp<-subset(dataNT1, ObsDataID==dup.listNT1[1000]) # checked some 'dup' rows and they are infact dups
#remove the dups (if necessary) using dup.listNT1
#write.table(dataNT, file='TRYdataNT.txt',sep='\t')

#########################################################
#########################################################
#########################################################
#Produce a trait dataset

#Remove some columns
colR<-c('LastName','FileName','SpeciesName', 'OrigValueStr','OrigUncertaintyStr','OrigUnitStr','Unit_UnitName','OrigValue','ValueKindName','RelUncertainty.','UncertaintyName') 
colRn<-numeric(0)
for (i in (1:length(colR))){
  k<-which(colnames(data)==colR[i])
  colRn<-c(colRn,k)}
data1<-data[,-colRn]

#Remove duplicate observations, ie rows without NA in OrigObsDataID
data1$OrigObsDataID<-as.numeric(data1$OrigObsDataID)
sum(data1$OrigObsDataID==0)
data2<-data1[is.na(data1$OrigObsDataID),]

#Remove the OrigObsDataID column
k<-which(colnames(data2)=='OrigObsDataID')
data3<-data2[,-k]

#....Trait-only dataset con't
dataT<-subset(data3, TraitVsNonTrait=='x') #subset the rows containing trait data

#Remove the TraitVsNonTrait column
k<-which(colnames(dataT)=='TraitVsNonTrait')
dataT<-dataT[,-k]

#get rid of the rows where StdValue=NA
temp<-!is.na(dataT$StdValue)
dataT<-dataT[temp,]
sum(is.na(dataT$StdValue)) #check

#why are there two rows with the same ObsDataID?
ID<-as.vector(unique(dataT$ObsDataID))
dup.list<-character(0)
for (i in 1:length(ID)){ #loop through unique ID list
  temp<-dataT$ObsDataID==ID[i] # for each ID, see how many rows there are
  if(sum(temp)>1){dup.list<-c(dup.list,ID[i])} #if there is more than one row, then store that ID num in a duplicate list
}
dup.listT<-dup.list
temp<-subset(dataT, ObsDataID==dup.list[100]) # checked some 'dup' rows and they are infact dups

#get rid of dups
for (i in 1:length(dup.list)){ #at start, dataT has 99631 rows
  temp<-which(dataT$ObsDataID==dup.list[i]) # which row number matches dup.list element?
  dataT<-dataT[-temp[2],] # delete the second element of 'which' from dataT
}

#Remove the NonTraitCategories column
k<-which(colnames(dataT)=='NonTraitCategories')
dataT<-dataT[,-k]

write.table(dataT, file='TRYdataT.txt', sep='\t') 

#########################################################
#########################################################
#########################################################
dataT<-read.table('TRYdataT.txt', sep='\t', header=T) 

#list of traits in dataT
traits<-unique(dataT$TraitID) # TraitID nums
traitnames<-as.character(unique(dataT$TraitName)) #TraitName

#list of subtraits (dataID) associated with each trait
ids.tab<-numeric(0)
for (i in 1:length(traits)){
  temp<-subset(dataT, TraitID==traits[i])
  dataids<-unique(temp$DataID)
  if (length(dataids)==1){row<-c(dataids,NA)}
  if (length(dataids)==2){row<-c(dataids)}
  print(row)
  ids.tab<-rbind(ids.tab,row)
}
ids.tab # DataID nums

#make a list of corresponding DataNames...
#for column1 of ids.tab
k<-ids.tab[,1]
dn1<-rep(NA,length(k))
for (i in 1:length(k)){
  temp<-subset(dataT, DataID==k[i])
  dn1[i]<-as.character(temp$DataName[1])
}
dn1

#for column2 of ids.tab
k<-ids.tab[,2]
dn2<-rep(NA,length(k))
for (i in 1:length(k)){
  temp<-subset(dataT, DataID==k[i])
  dn2[i]<-as.character(temp$DataName[1])
}
dn2
#for all unique DataIDs
uniqueDataID<-unique(dataT$DataID)
dn<-rep(NA,length(uniqueDataID))
for (i in 1:length(uniqueDataID)){
  temp<-subset(dataT, DataID==uniqueDataID[i])
  dn[i]<-as.character(temp$DataName[1])
}
dn

#make a list for the corresponding Units
uniqueDataID<-unique(dataT$DataID)
length(uniqueDataID) # check against ids.tab
ids.units<-character(0)
for (i in 1:length(uniqueDataID)){
  temp<-subset(dataT, DataID==uniqueDataID[i])
  row<-unique(temp$Unit_1_UnitName)
  ids.units<-rbind(ids.units,as.character(row))
} # all 'rows' were 1 element long, meaning that only 1 unit type is used for each DataID
ids.units


#bind everything together into a TraitID table
traitid.table<-cbind(traits,traitnames,ids.tab[,1],dn1,ids.tab[,2],dn2)
colnames(traitid.table)<-c('TraitID','TraitName','DataID_1','DataName_1','DataID_2','DataName_2')
rownames(traitid.table)<-seq(1:dim(traitid.table)[1])
write.table(traitid.table, file='TRY_traitsdatakey.txt', sep='\t')



#########################################################
