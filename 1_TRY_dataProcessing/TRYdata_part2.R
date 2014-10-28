#TRY data - part2
# Reformat dataT 

#will need these txt files: 
#'TRYdataT.txt' #TRY trait data, no dups
#'TRY_traitsdatakey.txt' #list of unique dataid categories and their units

#this code will produce these txt files:
#'TRY_dataidsBYsppAVGs.txt'
#'TRY_dataidsBYgenusAVGs.txt'

setwd("~/Desktop/BGC_R")

#########################################################
#########################################################
dataT<-read.table("TRYdataT.txt",header=TRUE,sep="\t") #from TRYdata_part1 code
dataid.table<-read.table("TRY_traitsdatakey.txt",sep="\t") # from TRYdata_part1 code

View(dataT)
View(dataid.table)

#########################################################
#Reformat the TRY trait data
#TRYdataT is in a long format with single columns identifying the obsID, trait category, and the value of the trait
#Reformatted table will have 1 row for each ObsID, and a separate column for each trait category to hold the corresponding trait values
##stuff required for the ObsDataID x DataID loop
DATAIDLIST<-unique(dataT$DataID); #make a list of the unique traitid numbers
length(DATAIDLIST); dim(dataid.table)[1] #check against trait.table
dataT$AccSpeciesName<-as.character(dataT$AccSpeciesName); #make sure species names are identified as characters
studyNumbers<-unique(dataT$ObsDataID); #make a list of the unique ObsDataIDs
length(studyNumbers); dim(dataT)[1] #check to make sure that there is a unique ObsDataID for each row of dataT
finalData<-c(); #initialize a vector
#the ObsDataID x unique traits loop
for(studynumber in 1:length(studyNumbers)) #loop through the list of unique 'obsid' values
  {
    studyData<-subset(dataT,ObsDataID==studyNumbers[studynumber]); #select rows that match that 'ObsDataID' value
    row<-rep(NA,times=(2+length(DATAIDLIST))); #initialize a vector called 'row' into which all the trait values can fit
    row[1]<-studyData$AccSpeciesName[1]; #fill the first element in the row with the species name
    row[2]<-studyData$ObsDataID[1]; #fill the second element in the row with the ObsDataID value
    for(i in 1:length(DATAIDLIST)) #loop through the list of unique dataid numbers
    {
      traitData<-subset(studyData,DataID==DATAIDLIST[i]); #select rows in the obsid-subsetted dataset that match that dataid
      if(dim(traitData)[1] !=0){row[2+i]<-mean(traitData$StdValue)}; #if there is 1 or more selected rows, take the mean of 'value' and put that number into the [2+i]th column of the row
      if(dim(traitData)[1] >1){print(dim(traitData[1]))}; #let me know if there is more than one row 
    }
    finalData<-rbind(finalData,row); #bind that row to others to create the 'finalData' matrix with [2+i] columns and [unique species and ObsDataID]rows
    print(dim(finalData));
  }   
colnames(finalData)<-c("AccSpeciesName","ObsDataID",paste('DataID',DATAIDLIST)) #assign column names
row.names(finalData)<-seq(1:dim(finalData)[1])

#Add a 'genus' column
tmp<-finalData[,1]
Genus<-sub(patter=" .*",replacement="",x=tmp, perl=T)
dataG<-data.frame(finalData[,1],as.factor(Genus),finalData[,-1])

write.table(dataG, file='TRY_dataidsBYobsids.txt', sep='\t')

#########################################################
#########################################################
#Now, take the Reformated dataset of ObsDataID (rows) by DataID (columns) and make aggregated datasets
##a. A dataset of Species (rows) by DataID (dataID) by aggregating over ObsDataID
##b. dataset of Genus (rows) by DataID (dataID) by aggregating over ObsDataID

data<-read.table(file='TRY_dataidsBYobsids.txt',sep='\t')
colnames(data)[1]<-'GenusSpecies'

#can get rid of this code once I re-run the code above and produce an updated version of 'TRY_dataidsBYobsids.txt'
tmp<-data[,1] 
Genus<-sub(patter=" .*",replacement="",x=tmp, perl=T)
data$Genus<-as.factor(Genus)

library(reshape)
library(doBy)

#Manually calc the trait means for a test 
#species and genus
rows.testspp<-which(data$GenusSpecies=='Ulex gallii')
Ulexgallii.as.num<-as.numeric(data[rows.testspp[1],1])
table.testspp<-data[rows.testspp,] #all rows with 'Ulex gallii'
means.testspp<-colMeans(table.testspp[,4:26], na.rm=T) #take the col means
real.testspp<-c(Ulexgallii.as.num, means.testspp)
#genus
rows.testg<-which(data$Genus=='Ulex')
Ulex.as.num<-as.numeric(data[rows.testg[1],2])
table.testg<-data[rows.testg,] #all rows with 'Ulex gallii'
means.testg<-colMeans(table.testg[,4:26], na.rm=T) #take the col means
real.testg<-c(Ulex.as.num, means.testg)

#Make an index of data$GenusSpecies and data$Genus name as character strings, as numeric
GenusSpecies.index<-cbind(as.character(data$GenusSpecies), as.numeric(data$GenusSpecies))
Genus.index<-cbind(as.character(data$Genus), as.numeric(data$Genus))

#Prepare to melt data
data$GenusSpecies<-as.factor(as.numeric(data$GenusSpecies))
data$Genus<-as.factor(as.numeric(data$Genus))
data$ObsDataID<-as.factor(data$ObsDataID)

#Melt data by averaging (cast.mean) and counting cells (cast.n) across ObsDataID for each GenusSpecies and Genus
melted.data<-melt(data, id=c('GenusSpecies','Genus','ObsDataID'), na.rm=T)
GScast.mean<-cast(melted.data,GenusSpecies~variable, mean)
GScast.n<-cast(melted.data,GenusSpecies~variable, length)
dim(GScast.mean)
Gcast.mean<-cast(melted.data,Genus~variable, mean)
Gcast.n<-cast(melted.data,Genus~variable, length)
dim(Gcast.mean)

#Make an index of all unique species as numeric and character AccSpecies
chrspp<-unique(GenusSpecies.index[,1])
numspp<-unique(GenusSpecies.index[,2])
numspp<-as.numeric(numspp)
GSsppid<-data.frame(numspp,chrspp)
str(GSsppid)
chrspp<-unique(Genus.index[,1])
numspp<-unique(Genus.index[,2])
numspp<-as.numeric(numspp)
Gsppid<-data.frame(numspp,chrspp)
str(Gsppid)

#Check that test species is numbered correctly
which(GSsppid==Ulexgallii.as.num); GSsppid[57,] #check
which(Gsppid==Ulex.as.num); Gsppid[48,] #check

#Check that cast.mean results match manually-calc results for Ulex
row<-which(GScast.mean==Ulexgallii.as.num)
result.testspp<-GScast.mean[row,]
testing<-rbind(real.testspp,result.testspp)
View(testing) #check
row<-which(Gcast.mean==Ulex.as.num)
result.testg<-Gcast.mean[row,]
testing<-rbind(real.testg,result.testg)
View(testing) #check

#Order sppid by num to assign cast.mean character string species names
dim(GScast.mean); dim(GSsppid) #check that rows of cast.mean are unique species
ordered.GSsppid<-orderBy(~numspp, GSsppid) 
GScast.mean$GenusSpecies<-ordered.GSsppid$chrspp 
GScast.n$GenusSpecies<-ordered.GSsppid$chrspp
dim(Gcast.mean); dim(Gsppid) #check that rows of cast.mean are unique species
ordered.Gsppid<-orderBy(~numspp, Gsppid) 
Gcast.mean$Genus<-ordered.Gsppid$chrspp 
Gcast.n$Genus<-ordered.Gsppid$chrspp


write.table(GScast.mean, file='TRY_dataidsBYsppAVGs.txt', sep='\t')
#write.table(GScast.n, file='TRY_dataidsBYsppNs.txt', sep='\t')

write.table(Gcast.mean, file='TRY_dataidsBYgenusAVGs.txt', sep='\t')
#write.table(Gcast.n, file='TRY_dataidsBYgenusNs.txt', sep='\t')

