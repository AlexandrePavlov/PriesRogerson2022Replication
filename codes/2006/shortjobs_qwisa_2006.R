#This R script reads the raw "fs" (firm-size) data for each of the 30 states, 
#keeps the variables of interest and saves the dataset in a file called qwisa.csv

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

#loading data and only keeping variables of interest
loc_start = "/data/qwi_"
loc_end = "_sa_f_gs_ns_op_u.csv"
data_sa <- read.csv(paste(wd,loc_start,"ak",loc_end,sep=""))
data_sa <- select(data_sa,c('geography','industry','sex','agegrp','year',
                            'quarter','Emp','EmpEnd','EmpTotal','HirA',
                            'HirAEnd','HirAS','Sep','SepBeg'))

for (i in statelist[-1]){
  df <- read.csv(paste(wd,loc_start,i,loc_end,sep=""))
  df <- select(df,c('geography','industry','sex','agegrp','year',
                      'quarter','Emp','EmpEnd','EmpTotal','HirA',
                      'HirAEnd','HirAS','Sep','SepBeg'))
  data_sa <- rbind(data_sa,df)
}

#sort geography, firmsize, year, quarter
data_sa <- arrange(data_sa,geography,sex,year,quarter)

#adding date and time column
data_sa$date = data_sa$year+(data_sa$quarter-1)/4
data_sa$time = data_sa$year*4+data_sa$quarter

#saving dataframe as .csv
write.csv(data_sa,file="qwisa_2006.csv",row.names=FALSE)