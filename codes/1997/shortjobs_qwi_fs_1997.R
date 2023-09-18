#This R script reads the raw "fs" (firm-size) data for each of the 30 states, 
#keeps the variables of interest and saves the dataset in a file called qwi_fs.csv

#loading libraries
library(dplyr)

#set working directory where data folder is
wd = "C:/Users/Alex/OneDrive - Universite de Montreal/Replication"
setwd(wd)

#vector of states
statelist <- c('ca','co','ct','hi','id','il','ks','la','md','me','mn','mo','mt',
               'nc','nj','nm','tx','wa','wv')

#loading data and only keeping columns of interest
loc_start = "/Pries Rogerson (2022)/data/qwi_"
loc_end = "_sa_fs_gs_ns_op_u.csv"
data_fs <- read.csv(paste(wd,loc_start,"ca",loc_end,sep=""))
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
write.csv(data_fs,file="qwi_fs_1997.csv",row.names=FALSE)