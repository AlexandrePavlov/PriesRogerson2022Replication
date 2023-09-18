#This R script reads the raw "fs" (firm-size) data for each of the 30 states, 
#keeps the variables of interest and saves the dataset in a file called qwi_fs.csv

#loading libraries
library(dplyr)

#set working directory where data folder is
wd = "C:/Users/Alex/OneDrive - Universite de Montreal/Replication"
setwd(wd)

#vector of states
statelist <- c('al','ak','az','ar','ca','co','ct','de','fl','ga','hi','id','il',
               'in','ia','ks','ky','la','me','md','mi','mn','ms','mo','mt','ne',
               'nv','nh','nj','nm','ny','nc','nd','oh','ok','or','pa','ri','sc',
               'sd','tn','tx','ut','vt','va','wa','wv','wi','wy')

#loading data and only keeping columns of interest
loc_start = "/data/qwi_"
loc_end = "_sa_fs_gs_ns_op_u.csv"
data_fs <- read.csv(paste(wd,loc_start,"ak",loc_end,sep=""))
data_fs <- select(data_fs,c('geography','industry','sex','agegrp','year',
                            'quarter','Emp','EmpEnd','EmpTotal','HirA',
                            'HirAEnd','HirAS','Sep','SepBeg','firmsize'))

for (i in statelist[-1]){
  df <- read.csv(paste(wd,loc_start,i,loc_end,sep=""))
  df <- select(df,c('geography','industry','sex','agegrp','year',
                      'quarter','Emp','EmpEnd','EmpTotal','HirA',
                      'HirAEnd','HirAS','Sep','SepBeg','firmsize'))
  data_fs <- rbind(data_fs,df)
}

#sort geography, firmsize, year, quarter
data_fs <- arrange(data_fs,geography, firmsize,year,quarter)

#adding date and time column
data_fs$date = data_fs$year+data_fs$quarter/4
data_fs$time = data_fs$year*4+data_fs$quarter

#saving dataframe as .csv
write.csv(data_fs,file="qwi_fs_2006.csv",row.names=FALSE)