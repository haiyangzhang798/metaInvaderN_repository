#BGCdata_reformat

#purpose: reformat raw data means extacted from papers
#old format: 1 row per observation
#new format: simplify columns to...
#obsID and obsID metadata
#measurement
#unit
#mean_C (control study mean)
#mean_T (treatment study mean)
#var_C
#var_T
#n_C
#n_T
#vartype (SE,SD,95%CI)
#qualitynotes
#notes

setwd("~/Desktop/MetaAnalysis_R/2_BGC data processing")

#########################################################
data<-read.table("obsID_fromPapers.txt",header=TRUE,sep="\t") # raw

View(data)
colnames(data)
dim(data)

meas<-as.character(c('nh','no','toti',
        'ammonif','nitrif','nminz',
        'soilN','SOM','soilcn','microbialcn',
        'soilmoi','pH',
        'plantcov','biom','litterbiom',
        'percN','cn','litterpercN','littercn'))
meanTcol<-as.numeric(c(12,14,16,
            19,21,23,
            26,29,32,35,
            37,40,
            42,45,48,
            51,54,57,60))
meanCcol<-as.numeric(c(13,15,17,
            20,22,24,
            27,30,33,36,
            38,41,
            43,46,49,
            52,55,58,61))
unitcol<-as.numeric(c(18,18,18,
           25,25,25,
           28,31,34,NA,
           39,NA,
           44,47,50,
           53,56,59,62))
measmat<-data.frame(meas,meanCcol,meanTcol,unitcol)
View(measmat)


rows<-numeric(0)
#loop through each row/obsID
for (i in 1:dim(data)[1]){ 
  #loop through each measurement
  for (k in 1:dim(measmat)[1]){ 
    
    #isolate the current obsID,i
    d<-data[i,] 
    
    #pull out the appropriate cells from the row given the current measurement,k
    author<-d[3]
    location<-d[5]
    ecosystemName<-d[6]
    studyType<-d[7]
    ecosystemID<-d[8]
    multimeas<-d[9]
    measurement<-as.character(measmat[k,'meas'])
    meanC<-d[measmat[k,'meanCcol']]       
    meanT<-d[measmat[k,'meanTcol']]
    if(!is.na(measmat[k,'unitcol'])){
      unit<-d[measmat[k,'unitcol']]
    } else {
      unit<-NA
    }
    
    #bind the new cells into a row
    row<-data.frame(d[1:2],author,d[4],location,ecosystemName,studyType,ecosystemID,multimeas,
                    measurement, meanC, meanT, unit)
    names(row)<-c('obsID','articleID','author','year','location','ecosystemName','studyType','ecosystemID','multimeas',
                  'measurement','meanC','meanT','unit')
    
    #bind the rows together
    rows<-rbind(rows,row)
  
  }

}
View(rows)

write.table(rows, file='metaanalysis_fromPapers_longform.txt',sep='\t', row.names=F,col.names=T)



