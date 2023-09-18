#loading libraries
library(ggplot2)
library(dplyr)
library(plm)

#set working directory where data folder is
wd = "C:/Users/Alex/OneDrive - Universite de Montreal/Replication"
setwd(wd)

data_fs <- read.csv(paste(wd,"qwi_fs.csv",sep="/"))

#keep if sex == 0
data_fs <- data_fs[(data_fs$sex == 0), ]
#drop sex
data_fs <- subset(data_fs, select = -c(sex))
#keep if agegrp=="A00"
data_fs <- data_fs[(data_fs$agegrp == "A00"), ]
data_fs <- subset(data_fs, select = -c(agegrp))
#keep if industry=="00"
data_fs <- data_fs[(data_fs$industry == "00"), ]
data_fs <- subset(data_fs, select = -c(industry))
#drop if firmsize==0
data_fs <- data_fs[!(data_fs$firmsize == 0), ]

#generate variables of interest
data_fs <- arrange(data_fs,geography,firmsize,year,quarter)

#egen cell=group(firmsize year quarter)
data_fs <- data_fs %>%
  mutate(cell = group_indices(., firmsize, year, quarter))

#aggregate over geography

#bysort cell: egen allhira=total(hira)

#hires in the quarter
data_fs <- data_fs %>%
  arrange(cell) %>%
  group_by(cell) %>%
  #returns sum if some rows are not NA, otherwise returns NA
  mutate(allhira := ifelse(all(is.na(HirA)), NA, sum(HirA, na.rm = T))) %>%
  ungroup()

#bysort cell: egen allhiras=total(hiras)

#"stable" hires, workers in second quarter of employment
data_fs <- data_fs %>%
  arrange(cell) %>%
  group_by(cell) %>%
  mutate(allhiras := ifelse(all(is.na(HirAS)), NA, sum(HirAS, na.rm = T))) %>%
  ungroup()

#bysort cell: egen allhiraend=total(hiraend)

#all hires that also survive into second quarter
data_fs <- data_fs %>%
  arrange(cell) %>%
  group_by(cell) %>%
  mutate(allhiraend := ifelse(all(is.na(HirAEnd)), NA, sum(HirAEnd, na.rm = T))) %>%
  ungroup()

#bysort cell: egen allemp=total(emp)

#beginning of quarter employment (had earnings last quarter and this quarter)
data_fs <- data_fs %>%
  arrange(cell) %>%
  group_by(cell) %>%
  mutate(allemp := ifelse(all(is.na(Emp)), NA, sum(Emp, na.rm = T))) %>%
  ungroup()

#bysort cell: egen allempend=total(empend)

#end of quarter employment (have earnings this quarter and next quarter)
data_fs <- data_fs %>%
  arrange(cell) %>%
  group_by(cell) %>%
  mutate(allempend := ifelse(all(is.na(EmpEnd)), NA, sum(EmpEnd, na.rm = T))) %>%
  ungroup()

#bysort cell: egen allemptot=total(emptotal)

#total count of people employed in the quarter
data_fs <- data_fs %>%
  arrange(cell) %>%
  group_by(cell) %>%
  mutate(allemptot := ifelse(all(is.na(EmpTotal)), NA, sum(EmpTotal, na.rm = T))) %>%
  ungroup()

#bysort cell: egen allsep=total(sep)

#total separations
data_fs <- data_fs %>%
  arrange(cell) %>%
  group_by(cell) %>%
  mutate(allsep := ifelse(all(is.na(Sep)), NA, sum(Sep, na.rm = T))) %>%
  ungroup()

#bysort cell: egen allsepbeg=total(sepbeg)

#total separations of employment spells that existed in previous quarter
data_fs <- data_fs %>%
  arrange(cell) %>%
  group_by(cell) %>%
  mutate(allsepbeg := ifelse(all(is.na(SepBeg)), NA, sum(SepBeg, na.rm = T))) %>%
  ungroup()

#bysort cell: keep if _n==1
data_fs <- data_fs %>% 
  arrange(cell) %>% 
  group_by(cell) %>% 
  filter(row_number() == 1) %>% 
  ungroup()

#drop geography
data_fs <- subset(data_fs, select = -c(geography))

#egen cellid=group(firmsize)
data_fs <- data_fs %>%
  mutate(cellid = group_indices(., firmsize))

#generate employ=(allemp+allempend)/2
data_fs$employ = (data_fs$allemp+data_fs$allempend)/2

#generate oneqsepsrate1=(allhira-allhiraend)/employ

#spells that begin and end in the same quarter, as fraction of average of
#beginning and end employment
data_fs$oneqsepsrate1=(data_fs$allhira-data_fs$allhiraend)/data_fs$employ

#generate oneqsepsrate=(allhira-allhiraend)/allemptot

#spells that begin and end in the same quarter, as fraction of total count of 
#employment spells in the quarter
data_fs$oneqsepsrate=(data_fs$allhira-data_fs$allhiraend)/data_fs$allemptot

#generate oneqhazrate=(allhira-allhiraend)/allhira

#fraction of all hires in the quarter that also end in the quarter
data_fs$oneqhazrate=(data_fs$allhira-data_fs$allhiraend)/data_fs$allhira

#generate twoplussepsrate=(allsepbeg)/allemptot

#separations among spells with 2+ quarters of tenure, as fraction total count of
#employment spells in the quarter
data_fs$twoplussepsrate=data_fs$allsepbeg/data_fs$allemptot

#generate twoplushazrate=(allsepbeg)/(allemptot-allhira)

#separations among spells with 2+ quarters of tenure, as fraction of all such spells
data_fs$twoplushazrate=data_fs$allsepbeg/(data_fs$allemptot-data_fs$allhira)

#xtset cellid time
data_fs <- pdata.frame(data_fs, index = c("cellid", "time"))

#generate twoqsepsrate=(L.allhiraend-allhiras)/allemptot

#spells that separate by end of second quarter (all that started two quarters 
#ago, minus the ones that we know continue past current quarter), as fraction of
#total count emp. spells in quarter
data_fs$twoqsepsrate=(lag(data_fs$allhiraend, 1)-data_fs$allhiras)/data_fs$allemptot

#generate twoqhazrate=(L.allhiraend-allhiras)/L.allhiraend

#spells that separate by end of second quarter, as fraction of spells that have 
#lasted two quarters
data_fs$twoqhazrate=(lag(data_fs$allhiraend, 1)-data_fs$allhiras)/lag(data_fs$allhiraend, 1)

#generate threeplussepsrate=(allsep-(L.allhiraend-allhiras)-(allhira-allhiraend))/allemptot

#separations among spells that have lasted three or more quarters, as fraction of
#total count of employment spells in the quarter
data_fs$threeplussepsrate=(data_fs$allsep-(lag(data_fs$allhiraend,1)-data_fs$allhiras)-(data_fs$allhira-data_fs$allhiraend))/data_fs$allemptot

#generate threeplushazrate=(allsep-(L.allhiraend-allhiras)-(allhira-allhiraend))/(allemptot-L.allhiraend-allhira)

#separations among spells that have lasted three or more quarters, as fraction 
#of all such spells
data_fs$threeplushazrate=(data_fs$allsep-(lag(data_fs$allhiraend,1)-data_fs$allhiras)-(data_fs$allhira-data_fs$allhiraend))/(data_fs$allemptot-lag(data_fs$allhiraend,1)-data_fs$allhira)

#generate sepsrate=allsep/allemptot

#overall separation rate
data_fs$sepsrate=data_fs$allsep/data_fs$allemptot

#generate hirerate1=allhira/employ

#hires as a fraction of average of beginning and end employment
data_fs$hirerate1=data_fs$allhira/data_fs$employ

#generate hirerate=allhira/allemptot

#hires as a fraction of total count of employmen spells in the quarter
data_fs$hirerate=data_fs$allhira/data_fs$allemptot

#drop if time<=1998*4+4 | time>2018*4

#can't compare with factors so I need to transform as numeric
#7961 --> 1
#1998*4+4=7996 --> 36
#2018*4=8072 --> 112
#hence condition should be to drop if time<=36 | time>112
data_fs <- data_fs[!(as.numeric(data_fs$time) <= 36 | as.numeric(data_fs$time) > 112), ]

#generate empty variables (e.g. generate oneqsepsrate1sa=.)
data_fs$oneqsepsrate1sa <- NA
data_fs$oneqsepsratesa <- NA
data_fs$oneqhazratesa <- NA
data_fs$twoplussepsratesa <- NA
data_fs$twoplushazratesa <- NA
data_fs$twoqsepsratesa <- NA
data_fs$twoqhazratesa <- NA
data_fs$threeplussepsratesa <- NA
data_fs$threeplushazratesa <- NA
data_fs$sepsratesa <- NA
data_fs$hirerate1sa <- NA
data_fs$hireratesa <- NA

#sort cell date
data_fs <- arrange(data_fs,cell,date)

#by cell: egen oneqsepsrate1means=mean(oneqsepsrate1)
data_fs <- data_fs %>%
  group_by(cell) %>%
  mutate(oneqsepsrate1means = mean(oneqsepsrate1,na.rm=TRUE)) %>%
  ungroup()

#by cell: egen oneqsepsratemeans=mean(oneqsepsrate)
data_fs <- data_fs %>%
  group_by(cell) %>%
  mutate(oneqsepsratemeans = mean(oneqsepsrate,na.rm=TRUE)) %>%
  ungroup()

#by cell: egen oneqhazratemeans=mean(oneqhazrate)
data_fs <- data_fs %>%
  group_by(cell) %>%
  mutate(oneqhazratemeans = mean(oneqhazrate,na.rm=TRUE)) %>%
  ungroup()

#by cell: egen twoplussepsratemeans=mean(twoplussepsrate)
data_fs <- data_fs %>%
  group_by(cell) %>%
  mutate(twoplussepsratemeans = mean(twoplussepsrate,na.rm=TRUE)) %>%
  ungroup()

#by cell: egen twoplushazratemeans=mean(twoplushazrate)
data_fs <- data_fs %>%
  group_by(cell) %>%
  mutate(twoplushazratemeans = mean(twoplushazrate,na.rm=TRUE)) %>%
  ungroup()

#by cell: egen twoqsepsratemeans=mean(twoqsepsrate)
data_fs <- data_fs %>%
  group_by(cell) %>%
  mutate(twoqsepsratemeans = mean(twoqsepsrate,na.rm=TRUE)) %>%
  ungroup()

#by cell: egen twoqhazratemeans=mean(twoqhazrate)
data_fs <- data_fs %>%
  group_by(cell) %>%
  mutate(twoqhazratemeans = mean(twoqhazrate,na.rm=TRUE)) %>%
  ungroup()

#by cell: egen threeplussepsratemeans=mean(threeplussepsrate)
data_fs <- data_fs %>%
  group_by(cell) %>%
  mutate(threeplussepsratemeans = mean(threeplussepsrate,na.rm=TRUE)) %>%
  ungroup()

#by cell: egen threeplushazratemeans=mean(threeplushazrate)
data_fs <- data_fs %>%
  group_by(cell) %>%
  mutate(threeplushazratemeans = mean(threeplushazrate,na.rm=TRUE)) %>%
  ungroup()

#by cell: egen sepsratemeans=mean(sepsrate)
data_fs <- data_fs %>%
  group_by(cell) %>%
  mutate(sepsratemeans = mean(sepsrate,na.rm=TRUE)) %>%
  ungroup()

#by cell: egen hirerate1means=mean(hirerate1)
data_fs <- data_fs %>%
  group_by(cell) %>%
  mutate(hirerate1means = mean(hirerate1,na.rm=TRUE)) %>%
  ungroup()

#by cell: egen hireratemeans=mean(hirerate)
data_fs <- data_fs %>%
  group_by(cell) %>%
  mutate(hireratemeans = mean(hirerate,na.rm=TRUE)) %>%
  ungroup()

for (i in 1:5){
  #reg oneqsepsrate1 i.quarter if cellid==i
  model <- lm(oneqsepsrate1 ~ factor(quarter), data = subset(data_fs, cellid == i))
  #predict double sa, residual 
  data_fs$sa <- residuals(model)
  #replace oneqsepsrate1sa=sa+oneqsepsrate1means if cellid==i
  data_fs$oneqsepsrate1sa[data_fs$cellid == i] <- data_fs$sa[data_fs$cellid == i] + data_fs$oneqsepsrate1means[data_fs$cellid == i]
  #drop sa
  data_fs <- subset(data_fs, select = -c(sa))
  
  #reg oneqsepsrate i.quarter if cellid==i
  #predict double sa, residual 
  #replace oneqsepsratesa=sa+oneqsepsratemeans if cellid==i
  #drop sa
  model <- lm(oneqsepsrate ~ factor(quarter), data = subset(data_fs, cellid == i))
  data_fs$sa <- residuals(model)
  data_fs$oneqsepsratesa[data_fs$cellid == i] <- data_fs$sa[data_fs$cellid == i] + data_fs$oneqsepsratemeans[data_fs$cellid == i]
  data_fs <- subset(data_fs, select = -c(sa))
  
  #reg oneqhazrate i.quarter if cellid==i
  #predict double sa, residual 
  #replace oneqhazratesa=sa+oneqhazratemeans if cellid==i
  #drop sa
  model <- lm(oneqhazrate ~ factor(quarter), data = subset(data_fs, cellid == i))
  data_fs$sa <- residuals(model)
  data_fs$oneqhazratesa[data_fs$cellid == i] <- data_fs$sa[data_fs$cellid == i] + data_fs$oneqhazratemeans[data_fs$cellid == i]
  data_fs <- subset(data_fs, select = -c(sa))
  
  #reg twoplussepsrate i.quarter if cellid==i
  #predict double sa, residual 
  #replace twoplussepsratesa=sa+twoplussepsratemeans if cellid==i
  #drop sa
  model <- lm(twoplussepsrate ~ factor(quarter), data = subset(data_fs, cellid == i))
  data_fs$sa <- residuals(model)
  data_fs$twoplussepsratesa[data_fs$cellid == i] <- data_fs$sa[data_fs$cellid == i] + data_fs$twoplussepsratemeans[data_fs$cellid == i]
  data_fs <- subset(data_fs, select = -c(sa))
  
  #reg twoplushazrate i.quarter if cellid==i
  #predict double sa, residual 
  #replace twoplushazratesa=sa+twoplushazratemeans if cellid==i
  #drop sa
  model <- lm(twoplushazrate ~ factor(quarter), data = subset(data_fs, cellid == i))
  data_fs$sa <- residuals(model)
  data_fs$twoplushazratesa[data_fs$cellid == i] <- data_fs$sa[data_fs$cellid == i] + data_fs$twoplushazratemeans[data_fs$cellid == i]
  data_fs <- subset(data_fs, select = -c(sa))
  
  #reg twoqsepsrate i.quarter if cellid==i
  #predict double sa, residual 
  #replace twoqsepsratesa=sa+twoqsepsratemeans if cellid==i
  #drop sa
  model <- lm(twoqsepsrate ~ factor(quarter), data = subset(data_fs, cellid == i))
  data_fs$sa <- residuals(model)
  data_fs$twoqsepsratesa[data_fs$cellid == i] <- data_fs$sa[data_fs$cellid == i] + data_fs$twoqsepsratemeans[data_fs$cellid == i]
  data_fs <- subset(data_fs, select = -c(sa))
  
  #reg twoqhazrate i.quarter if cellid==i
  #predict double sa, residual 
  #replace twoqhazratesa=sa+twoqhazratemeans if cellid==i
  #drop sa
  model <- lm(twoqhazrate ~ factor(quarter), data = subset(data_fs, cellid == i))
  data_fs$sa <- residuals(model)
  data_fs$twoqhazratesa[data_fs$cellid == i] <- data_fs$sa[data_fs$cellid == i] + data_fs$twoqhazratemeans[data_fs$cellid == i]
  data_fs <- subset(data_fs, select = -c(sa))
  
  #reg threeplussepsrate i.quarter if cellid==i
  #predict double sa, residual 
  #replace threeplussepsratesa=sa+threeplussepsratemeans if cellid==i
  #drop sa
  model <- lm(threeplussepsrate ~ factor(quarter), data = subset(data_fs, cellid == i))
  data_fs$sa <- residuals(model)
  data_fs$threeplussepsratesa[data_fs$cellid == i] <- data_fs$sa[data_fs$cellid == i] + data_fs$threeplussepsratemeans[data_fs$cellid == i]
  data_fs <- subset(data_fs, select = -c(sa))
  
  #reg threeplushazrate i.quarter if cellid==i
  #predict double sa, residual 
  #replace threeplushazratesa=sa+threeplushazratemeans if cellid==i
  #drop sa
  model <- lm(threeplushazrate ~ factor(quarter), data = subset(data_fs, cellid == i))
  data_fs$sa <- residuals(model)
  data_fs$threeplushazratesa[data_fs$cellid == i] <- data_fs$sa[data_fs$cellid == i] + data_fs$threeplushazratemeans[data_fs$cellid == i]
  data_fs <- subset(data_fs, select = -c(sa))
  
  #reg sepsrate i.quarter if cellid==i
  #predict double sa, residual 
  #replace sepsratesa=sa+sepsratemeans if cellid==i
  #drop sa
  model <- lm(sepsrate ~ factor(quarter), data = subset(data_fs, cellid == i))
  data_fs$sa <- residuals(model)
  data_fs$sepsratesa[data_fs$cellid == i] <- data_fs$sa[data_fs$cellid == i] + data_fs$sepsratemeans[data_fs$cellid == i]
  data_fs <- subset(data_fs, select = -c(sa))
  
  #reg hirerate1 i.quarter if cellid==i
  #predict double sa, residual 
  #replace hirerate1sa=sa+hirerate1means if cellid==i
  #drop sa
  model <- lm(hirerate1 ~ factor(quarter), data = subset(data_fs, cellid == i))
  data_fs$sa <- residuals(model)
  data_fs$hirerate1sa[data_fs$cellid == i] <- data_fs$sa[data_fs$cellid == i] + data_fs$hirerate1means[data_fs$cellid == i]
  data_fs <- subset(data_fs, select = -c(sa))
  
  #reg hirerate i.quarter if cellid==i
  #predict double sa, residual 
  #replace hireratesa=sa+hireratemeans if cellid==i
  #drop sa
  model <- lm(hirerate ~ factor(quarter), data = subset(data_fs, cellid == i))
  data_fs$sa <- residuals(model)
  data_fs$hireratesa[data_fs$cellid == i] <- data_fs$sa[data_fs$cellid == i] + data_fs$hireratemeans[data_fs$cellid == i]
  data_fs <- subset(data_fs, select = -c(sa))
}

write.csv(data_fs,file="shortjobs_fs.csv")

#sort cellid time
data_fs <- arrange(data_fs,cellid,time)

#by cellid: generate oneqsepsratesa_first=(oneqsepsratesa[1]+oneqsepsratesa[2]+oneqsepsratesa[3]+oneqsepsratesa[4])/4
data_fs <- data_fs %>%
  group_by(cellid) %>%
  mutate(oneqsepsratesa_first = (nth(oneqsepsratesa,1) +
                                   nth(oneqsepsratesa,2) +
                                   nth(oneqsepsratesa,3) +
                                   nth(oneqsepsratesa,4))/4) %>%
  ungroup()

#by cellid: generate oneqsepsratesa_last=(oneqsepsratesa[_N]+oneqsepsratesa[_N-1]+oneqsepsratesa[_N-2]+oneqsepsratesa[_N-3])/4
data_fs <- data_fs %>%
  group_by(cellid) %>%
  mutate(oneqsepsratesa_last = (nth(oneqsepsratesa,-1) +
                                   nth(oneqsepsratesa,-2) +
                                   nth(oneqsepsratesa,-3) +
                                   nth(oneqsepsratesa,-4))/4) %>%
  ungroup()

#by cellid: generate hireratesa_first=(hireratesa[1]+hireratesa[2]+hireratesa[3]+hireratesa[4])/4
data_fs <- data_fs %>%
  group_by(cellid) %>%
  mutate(hireratesa_first = (nth(hireratesa, 1) +
                               nth(hireratesa, 2) +
                               nth(hireratesa, 3) +
                               nth(hireratesa, 4)) / 4) %>%
  ungroup()

#by cellid: generate hireratesa_last=(hireratesa[_N]+hireratesa[_N-1]+hireratesa[_N-2]+hireratesa[_N-3])/4
data_fs <- data_fs %>%
  group_by(cellid) %>%
  mutate(hireratesa_last = (nth(hireratesa, -1) +
                               nth(hireratesa, -2) +
                               nth(hireratesa, -3) +
                               nth(hireratesa, -4)) / 4) %>%
  ungroup()

#by cellid: keep if _n==1
#keeps first observation in each grouping
data_fs <- data_fs %>% 
  group_by(cellid) %>%
  slice(1) %>%
  ungroup()

fs_label <- c("0-19","20-49","50-249","250-499","500+")

ggplot() +
  geom_point(data = data_fs, aes(x = oneqsepsratesa_first, y = oneqsepsratesa_last), color = "blue") +
  geom_abline(slope = 1, intercept = 0, color = "black") +
  xlim(0.04, 0.14) +
  ylim(0.04, 0.14) +
  labs(x = expression("1999 One quarter incidence rate (q"[1]*")"),
       y = expression("2017 One quarter incidence rate (q"[1]*")")) +
  geom_text(data = data_fs,aes(x = oneqsepsratesa_first, y = oneqsepsratesa_last,label=fs_label),nudge_x=0.002,nudge_y=0.002) +
  theme(panel.background = element_rect(fill = 'white', colour = 'white'))

ggsave('oneqseps_firstlast_firmsize.pdf')

ggplot() +
  geom_point(data = data_fs, aes(x = hireratesa_first, y = hireratesa_last), color = "blue") +
  geom_abline(slope = 1, intercept = 0, color = "black") +
  xlim(0.14, 0.3) +
  ylim(0.14, 0.3) +
  labs(x = "1999 Hires rate (h)",
       y = "2017 Hires rate (h)") +
  geom_text(data = data_fs,aes(x = hireratesa_first, y = hireratesa_last,label=fs_label),nudge_x=0.0025,nudge_y=0.0025) +
  theme(panel.background = element_rect(fill = 'white', colour = 'white'))

ggsave('hirerate_firstlast_firmsize.pdf')