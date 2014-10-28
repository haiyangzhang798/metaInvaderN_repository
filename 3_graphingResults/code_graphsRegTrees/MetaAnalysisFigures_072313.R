#7/23/13
#MetaAnalysis
#########################################################
#########################################################
setwd("~/Desktop/MetaAnalysis_R/3_Graphing results")

#Read-in data for Figures (Parts 1-4)
#########################################################
#########################################################
#obs ids etc
idstemp<-read.table('invadersONLY.txt',header=TRUE, sep='\t')
ids<-idstemp[,c(1:4,27)]
initials<-ids[,5]
obsids<-ids[,1]

#measurement id table
traitskey<-read.table("data_traitids_unitconv.txt",header=TRUE,sep="\t")

#soil responses - log response ratio (greater than 1 means higher values in invaded areas)
rrsoil<-read.table('rrsoil.txt',header=TRUE, sep='\t')
colnames(rrsoil)<-c('NH4+ LogRR','NO3- LogRR','Inorganic N LogRR','Ammonification LogRR','Nitrification LogRR','Mineralization LogRR')

#invader trait values
invtraits<-read.table('invaders.txt',header=TRUE, sep='\t')
colnames(invtraits)<-c('Leaf C:N','Litter C:N','Leaf %N','Litter %N')

#native area community-weighted mean trait values -- over 60%
natCWMcalc<-read.table('natCWMcalc.txt',header=TRUE, sep='\t')
colnames(natCWMcalc)<-c('Leaf C:N','Litter C:N','Leaf %N','Litter %N')

#native area soil responses
natsoil<-read.table('natsoil.txt',header=TRUE, sep='\t')
colnames(natsoil)<-c('NH4+ (ppm)','NO3- (ppm)','Inorganic N (ppm)','Ammonification (ppm/d)','Nitrification (ppm/d)','Mineralization (ppm/d)')

#invaded area soil responses
invsoil<-read.table('invsoil.txt',header=TRUE, sep='\t')
colnames(invsoil)<-c('NH4+ (ppm)','NO3- (ppm)','Inorganic N (ppm)','Ammonification (ppm/d)','Nitrification (ppm/d)','Mineralization (ppm/d)')

#response ratio community-weighted mean trait values -- over 60%
rrCWMcalc<-read.table('rrCWMcalc.txt',header=TRUE, sep='\t')
colnames(rrCWMcalc)<-c('Leaf C:N LogRR','Litter C:N LogRR','Leaf %N LogRR','Litter %N LogRR')

