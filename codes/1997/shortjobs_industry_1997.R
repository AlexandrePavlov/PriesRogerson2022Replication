#loading libraries
library(ggplot2)
library(dplyr)
library(plm)

#set working directory where data folder is
wd = "C:/Users/Alex/OneDrive - Universite de Montreal/Replication"
setwd(wd)

data_sa <- read.csv(paste(wd,"qwisa_1997.csv",sep="/"))

#keep if sex == 0
data_sa <- data_sa[(data_sa$sex == 0), ]
#drop sex
data_sa <- subset(data_sa, select = -c(sex))
#keep if agegrp=="A00"
data_sa <- data_sa[(data_sa$agegrp == "A00"), ]
data_sa <- subset(data_sa, select = -c(agegrp))
#drop if industry=="00" | industry=="92"
data_sa <- data_sa[!(data_sa$industry == "00" | data_sa$industry == "92"), ]

#generate variables of interest
data_sa <- arrange(data_sa,geography,industry,year,quarter)

#egen cell=group(industry year quarter)
data_sa <- data_sa %>%
  mutate(cell = group_indices(., industry, year, quarter))

#aggregate over geography

#bysort cell: egen allhira=total(hira)

#hires in the quarter
data_sa <- data_sa %>%
  arrange(cell) %>%
  group_by(cell) %>%
  #returns sum if some rows are not NA, otherwise returns NA
  mutate(allhira := ifelse(all(is.na(HirA)), NA, sum(HirA, na.rm = T))) %>%
  ungroup()

#bysort cell: egen allhiras=total(hiras)

#"stable" hires, workers in second quarter of employment
data_sa <- data_sa %>%
  arrange(cell) %>%
  group_by(cell) %>%
  mutate(allhiras := ifelse(all(is.na(HirAS)), NA, sum(HirAS, na.rm = T))) %>%
  ungroup()

#bysort cell: egen allhiraend=total(hiraend)

#all hires that also survive into second quarter
data_sa <- data_sa %>%
  arrange(cell) %>%
  group_by(cell) %>%
  mutate(allhiraend := ifelse(all(is.na(HirAEnd)), NA, sum(HirAEnd, na.rm = T))) %>%
  ungroup()

#bysort cell: egen allemp=total(emp)

#beginning of quarter employment (had earnings last quarter and this quarter)
data_sa <- data_sa %>%
  arrange(cell) %>%
  group_by(cell) %>%
  mutate(allemp := ifelse(all(is.na(Emp)), NA, sum(Emp, na.rm = T))) %>%
  ungroup()

#bysort cell: egen allempend=total(empend)

#end of quarter employment (have earnings this quarter and next quarter)
data_sa <- data_sa %>%
  arrange(cell) %>%
  group_by(cell) %>%
  mutate(allempend := ifelse(all(is.na(EmpEnd)), NA, sum(EmpEnd, na.rm = T))) %>%
  ungroup()

#bysort cell: egen allemptot=total(emptotal)

#total count of people employed in the quarter
data_sa <- data_sa %>%
  arrange(cell) %>%
  group_by(cell) %>%
  mutate(allemptot := ifelse(all(is.na(EmpTotal)), NA, sum(EmpTotal, na.rm = T))) %>%
  ungroup()

#bysort cell: egen allsep=total(sep)

#total separations
data_sa <- data_sa %>%
  arrange(cell) %>%
  group_by(cell) %>%
  mutate(allsep := ifelse(all(is.na(Sep)), NA, sum(Sep, na.rm = T))) %>%
  ungroup()

#bysort cell: egen allsepbeg=total(sepbeg)

#total separations of employment spells that existed in previous quarter
data_sa <- data_sa %>%
  arrange(cell) %>%
  group_by(cell) %>%
  mutate(allsepbeg := ifelse(all(is.na(SepBeg)), NA, sum(SepBeg, na.rm = T))) %>%
  ungroup()

#bysort cell: keep if _n==1
data_sa <- data_sa %>% 
  arrange(cell) %>% 
  group_by(cell) %>% 
  filter(row_number() == 1) %>% 
  ungroup()

#drop geography
data_sa <- subset(data_sa, select = -c(geography))

#egen cellid=group(industry)
data_sa <- data_sa %>%
  mutate(cellid = group_indices(., industry))

#generate employ=(allemp+allempend)/2
data_sa$employ = (data_sa$allemp+data_sa$allempend)/2

#generate oneqsepsrate1=(allhira-allhiraend)/employ

#spells that begin and end in the same quarter, as fraction of average of
#beginning and end employment
data_sa$oneqsepsrate1=(data_sa$allhira-data_sa$allhiraend)/data_sa$employ

#generate oneqsepsrate=(allhira-allhiraend)/allemptot

#spells that begin and end in the same quarter, as fraction of total count of 
#employment spells in the quarter
data_sa$oneqsepsrate=(data_sa$allhira-data_sa$allhiraend)/data_sa$allemptot

#generate oneqhazrate=(allhira-allhiraend)/allhira

#fraction of all hires in the quarter that also end in the quarter
data_sa$oneqhazrate=(data_sa$allhira-data_sa$allhiraend)/data_sa$allhira

#generate twoplussepsrate=(allsepbeg)/allemptot

#separations among spells with 2+ quarters of tenure, as fraction total count of
#employment spells in the quarter
data_sa$twoplussepsrate=data_sa$allsepbeg/data_sa$allemptot

#generate twoplushazrate=(allsepbeg)/(allemptot-allhira)

#separations among spells with 2+ quarters of tenure, as fraction of all such spells
data_sa$twoplushazrate=data_sa$allsepbeg/(data_sa$allemptot-data_sa$allhira)

#xtset cellid time
data_sa <- pdata.frame(data_sa, index = c("cellid", "time"))

#generate twoqsepsrate=(L.allhiraend-allhiras)/allemptot

#spells that separate by end of second quarter (all that started two quarters 
#ago, minus the ones that we know continue past current quarter), as fraction of
#total count emp. spells in quarter
data_sa$twoqsepsrate=(lag(data_sa$allhiraend, 1)-data_sa$allhiras)/data_sa$allemptot

#generate twoqhazrate=(L.allhiraend-allhiras)/L.allhiraend

#spells that separate by end of second quarter, as fraction of spells that have 
#lasted two quarters
data_sa$twoqhazrate=(lag(data_sa$allhiraend, 1)-data_sa$allhiras)/lag(data_sa$allhiraend, 1)

#generate threeplussepsrate=(allsep-(L.allhiraend-allhiras)-(allhira-allhiraend))/allemptot

#separations among spells that have lasted three or more quarters, as fraction of
#total count of employment spells in the quarter
data_sa$threeplussepsrate=(data_sa$allsep-(lag(data_sa$allhiraend,1)-data_sa$allhiras)-(data_sa$allhira-data_sa$allhiraend))/data_sa$allemptot

#generate threeplushazrate=(allsep-(L.allhiraend-allhiras)-(allhira-allhiraend))/(allemptot-L.allhiraend-allhira)

#separations among spells that have lasted three or more quarters, as fraction 
#of all such spells
data_sa$threeplushazrate=(data_sa$allsep-(lag(data_sa$allhiraend,1)-data_sa$allhiras)-(data_sa$allhira-data_sa$allhiraend))/(data_sa$allemptot-lag(data_sa$allhiraend,1)-data_sa$allhira)

#generate sepsrate=allsep/allemptot

#overall separation rate
data_sa$sepsrate=data_sa$allsep/data_sa$allemptot

#generate hirerate1=allhira/employ

#hires as a fraction of average of beginning and end employment
data_sa$hirerate1=data_sa$allhira/data_sa$employ

#generate hirerate=allhira/allemptot

#hires as a fraction of total count of employmen spells in the quarter
data_sa$hirerate=data_sa$allhira/data_sa$allemptot

#drop if time<=1996*4+4 | time>2018*4

#can't compare with factors so I need to transform as numeric
#7961 --> 1
#1996*4+4=7988 --> 28
#2018*4=8072 --> 112
#hence condition should be to drop if time<=28 | time>112
data_sa <- data_sa[!(as.numeric(data_sa$time) <= 28 | as.numeric(data_sa$time) > 112), ]

#generate empty variables (e.g. generate oneqsepsrate1sa=.)
data_sa$oneqsepsrate1sa <- NA
data_sa$oneqsepsratesa <- NA
data_sa$oneqhazratesa <- NA
data_sa$twoplussepsratesa <- NA
data_sa$twoplushazratesa <- NA
data_sa$twoqsepsratesa <- NA
data_sa$twoqhazratesa <- NA
data_sa$threeplussepsratesa <- NA
data_sa$threeplushazratesa <- NA
data_sa$sepsratesa <- NA
data_sa$hirerate1sa <- NA
data_sa$hireratesa <- NA

#sort cellid date
data_sa <- arrange(data_sa,cellid,date)

#by cellid: egen oneqsepsrate1means=mean(oneqsepsrate1)
data_sa <- data_sa %>%
  group_by(cellid) %>%
  mutate(oneqsepsrate1means = mean(oneqsepsrate1,na.rm=TRUE)) %>%
  ungroup()

#by cellid: egen oneqsepsratemeans=mean(oneqsepsrate)
data_sa <- data_sa %>%
  group_by(cellid) %>%
  mutate(oneqsepsratemeans = mean(oneqsepsrate,na.rm=TRUE)) %>%
  ungroup()

#by cellid: egen oneqhazratemeans=mean(oneqhazrate)
data_sa <- data_sa %>%
  group_by(cellid) %>%
  mutate(oneqhazratemeans = mean(oneqhazrate,na.rm=TRUE)) %>%
  ungroup()

#by cellid: egen twoplussepsratemeans=mean(twoplussepsrate)
data_sa <- data_sa %>%
  group_by(cellid) %>%
  mutate(twoplussepsratemeans = mean(twoplussepsrate,na.rm=TRUE)) %>%
  ungroup()

#by cellid: egen twoplushazratemeans=mean(twoplushazrate)
data_sa <- data_sa %>%
  group_by(cellid) %>%
  mutate(twoplushazratemeans = mean(twoplushazrate,na.rm=TRUE)) %>%
  ungroup()

#by cellid: egen twoqsepsratemeans=mean(twoqsepsrate)
data_sa <- data_sa %>%
  group_by(cellid) %>%
  mutate(twoqsepsratemeans = mean(twoqsepsrate,na.rm=TRUE)) %>%
  ungroup()

#by cellid: egen twoqhazratemeans=mean(twoqhazrate)
data_sa <- data_sa %>%
  group_by(cellid) %>%
  mutate(twoqhazratemeans = mean(twoqhazrate,na.rm=TRUE)) %>%
  ungroup()

#by cellid: egen threeplussepsratemeans=mean(threeplussepsrate)
data_sa <- data_sa %>%
  group_by(cellid) %>%
  mutate(threeplussepsratemeans = mean(threeplussepsrate,na.rm=TRUE)) %>%
  ungroup()

#by cellid: egen threeplushazratemeans=mean(threeplushazrate)
data_sa <- data_sa %>%
  group_by(cellid) %>%
  mutate(threeplushazratemeans = mean(threeplushazrate,na.rm=TRUE)) %>%
  ungroup()

#by cellid: egen sepsratemeans=mean(sepsrate)
data_sa <- data_sa %>%
  group_by(cellid) %>%
  mutate(sepsratemeans = mean(sepsrate,na.rm=TRUE)) %>%
  ungroup()

#by cellid: egen hirerate1means=mean(hirerate1)
data_sa <- data_sa %>%
  group_by(cellid) %>%
  mutate(hirerate1means = mean(hirerate1,na.rm=TRUE)) %>%
  ungroup()

#by cellid: egen hireratemeans=mean(hirerate)
data_sa <- data_sa %>%
  group_by(cellid) %>%
  mutate(hireratemeans = mean(hirerate,na.rm=TRUE)) %>%
  ungroup()

for (i in 1:19){
  #reg oneqsepsrate1 i.quarter if cellid==i
  model <- lm(oneqsepsrate1 ~ factor(quarter), data = subset(data_sa, cellid == i))
  #predict double sa, residual 
  data_sa$sa <- residuals(model)
  #replace oneqsepsrate1sa=sa+oneqsepsrate1means if cellid==i
  data_sa$oneqsepsrate1sa[data_sa$cellid == i] <- data_sa$sa[data_sa$cellid == i] + data_sa$oneqsepsrate1means[data_sa$cellid == i]
  #drop sa
  data_sa <- subset(data_sa, select = -c(sa))
  
  #reg oneqsepsrate i.quarter if cellid==i
  #predict double sa, residual 
  #replace oneqsepsratesa=sa+oneqsepsratemeans if cellid==i
  #drop sa
  model <- lm(oneqsepsrate ~ factor(quarter), data = subset(data_sa, cellid == i))
  data_sa$sa <- residuals(model)
  data_sa$oneqsepsratesa[data_sa$cellid == i] <- data_sa$sa[data_sa$cellid == i] + data_sa$oneqsepsratemeans[data_sa$cellid == i]
  data_sa <- subset(data_sa, select = -c(sa))
  
  #reg oneqhazrate i.quarter if cellid==i
  #predict double sa, residual 
  #replace oneqhazratesa=sa+oneqhazratemeans if cellid==i
  #drop sa
  model <- lm(oneqhazrate ~ factor(quarter), data = subset(data_sa, cellid == i))
  data_sa$sa <- residuals(model)
  data_sa$oneqhazratesa[data_sa$cellid == i] <- data_sa$sa[data_sa$cellid == i] + data_sa$oneqhazratemeans[data_sa$cellid == i]
  data_sa <- subset(data_sa, select = -c(sa))
  
  #reg twoplussepsrate i.quarter if cellid==i
  #predict double sa, residual 
  #replace twoplussepsratesa=sa+twoplussepsratemeans if cellid==i
  #drop sa
  model <- lm(twoplussepsrate ~ factor(quarter), data = subset(data_sa, cellid == i))
  data_sa$sa <- residuals(model)
  data_sa$twoplussepsratesa[data_sa$cellid == i] <- data_sa$sa[data_sa$cellid == i] + data_sa$twoplussepsratemeans[data_sa$cellid == i]
  data_sa <- subset(data_sa, select = -c(sa))
  
  #reg twoplushazrate i.quarter if cellid==i
  #predict double sa, residual 
  #replace twoplushazratesa=sa+twoplushazratemeans if cellid==i
  #drop sa
  model <- lm(twoplushazrate ~ factor(quarter), data = subset(data_sa, cellid == i))
  data_sa$sa <- residuals(model)
  data_sa$twoplushazratesa[data_sa$cellid == i] <- data_sa$sa[data_sa$cellid == i] + data_sa$twoplushazratemeans[data_sa$cellid == i]
  data_sa <- subset(data_sa, select = -c(sa))
  
  #reg twoqsepsrate i.quarter if cellid==i
  #predict double sa, residual 
  #replace twoqsepsratesa=sa+twoqsepsratemeans if cellid==i
  #drop sa
  model <- lm(twoqsepsrate ~ factor(quarter), data = subset(data_sa, cellid == i))
  data_sa$sa <- residuals(model)
  data_sa$twoqsepsratesa[data_sa$cellid == i] <- data_sa$sa[data_sa$cellid == i] + data_sa$twoqsepsratemeans[data_sa$cellid == i]
  data_sa <- subset(data_sa, select = -c(sa))
  
  #reg twoqhazrate i.quarter if cellid==i
  #predict double sa, residual 
  #replace twoqhazratesa=sa+twoqhazratemeans if cellid==i
  #drop sa
  model <- lm(twoqhazrate ~ factor(quarter), data = subset(data_sa, cellid == i))
  data_sa$sa <- residuals(model)
  data_sa$twoqhazratesa[data_sa$cellid == i] <- data_sa$sa[data_sa$cellid == i] + data_sa$twoqhazratemeans[data_sa$cellid == i]
  data_sa <- subset(data_sa, select = -c(sa))
  
  #reg threeplussepsrate i.quarter if cellid==i
  #predict double sa, residual 
  #replace threeplussepsratesa=sa+threeplussepsratemeans if cellid==i
  #drop sa
  model <- lm(threeplussepsrate ~ factor(quarter), data = subset(data_sa, cellid == i))
  data_sa$sa <- residuals(model)
  data_sa$threeplussepsratesa[data_sa$cellid == i] <- data_sa$sa[data_sa$cellid == i] + data_sa$threeplussepsratemeans[data_sa$cellid == i]
  data_sa <- subset(data_sa, select = -c(sa))
  
  #reg threeplushazrate i.quarter if cellid==i
  #predict double sa, residual 
  #replace threeplushazratesa=sa+threeplushazratemeans if cellid==i
  #drop sa
  model <- lm(threeplushazrate ~ factor(quarter), data = subset(data_sa, cellid == i))
  data_sa$sa <- residuals(model)
  data_sa$threeplushazratesa[data_sa$cellid == i] <- data_sa$sa[data_sa$cellid == i] + data_sa$threeplushazratemeans[data_sa$cellid == i]
  data_sa <- subset(data_sa, select = -c(sa))
  
  #reg sepsrate i.quarter if cellid==i
  #predict double sa, residual 
  #replace sepsratesa=sa+sepsratemeans if cellid==i
  #drop sa
  model <- lm(sepsrate ~ factor(quarter), data = subset(data_sa, cellid == i))
  data_sa$sa <- residuals(model)
  data_sa$sepsratesa[data_sa$cellid == i] <- data_sa$sa[data_sa$cellid == i] + data_sa$sepsratemeans[data_sa$cellid == i]
  data_sa <- subset(data_sa, select = -c(sa))
  
  #reg hirerate1 i.quarter if cellid==i
  #predict double sa, residual 
  #replace hirerate1sa=sa+hirerate1means if cellid==i
  #drop sa
  model <- lm(hirerate1 ~ factor(quarter), data = subset(data_sa, cellid == i))
  data_sa$sa <- residuals(model)
  data_sa$hirerate1sa[data_sa$cellid == i] <- data_sa$sa[data_sa$cellid == i] + data_sa$hirerate1means[data_sa$cellid == i]
  data_sa <- subset(data_sa, select = -c(sa))
  
  #reg hirerate i.quarter if cellid==i
  #predict double sa, residual 
  #replace hireratesa=sa+hireratemeans if cellid==i
  #drop sa
  model <- lm(hirerate ~ factor(quarter), data = subset(data_sa, cellid == i))
  data_sa$sa <- residuals(model)
  data_sa$hireratesa[data_sa$cellid == i] <- data_sa$sa[data_sa$cellid == i] + data_sa$hireratemeans[data_sa$cellid == i]
  data_sa <- subset(data_sa, select = -c(sa))
}

write.csv(data_sa,file="shortjobs_industry_1997.csv")

#sort cellid time
data_sa <- arrange(data_sa,cellid,time)

#by cellid: generate oneqsepsratesa_first=(oneqsepsratesa[1]+oneqsepsratesa[2]+oneqsepsratesa[3]+oneqsepsratesa[4])/4
data_sa <- data_sa %>%
  group_by(cellid) %>%
  mutate(oneqsepsratesa_first = (nth(oneqsepsratesa,1) +
                                   nth(oneqsepsratesa,2) +
                                   nth(oneqsepsratesa,3) +
                                   nth(oneqsepsratesa,4))/4) %>%
  ungroup()

#by cellid: generate oneqsepsratesa_last=(oneqsepsratesa[_N]+oneqsepsratesa[_N-1]+oneqsepsratesa[_N-2]+oneqsepsratesa[_N-3])/4
data_sa <- data_sa %>%
  group_by(cellid) %>%
  mutate(oneqsepsratesa_last = (nth(oneqsepsratesa,-1) +
                                   nth(oneqsepsratesa,-2) +
                                   nth(oneqsepsratesa,-3) +
                                   nth(oneqsepsratesa,-4))/4) %>%
  ungroup()

#by cellid: generate hireratesa_first=(hireratesa[1]+hireratesa[2]+hireratesa[3]+hireratesa[4])/4
data_sa <- data_sa %>%
  group_by(cellid) %>%
  mutate(hireratesa_first = (nth(hireratesa, 1) +
                               nth(hireratesa, 2) +
                               nth(hireratesa, 3) +
                               nth(hireratesa, 4)) / 4) %>%
  ungroup()

#by cellid: generate hireratesa_last=(hireratesa[_N]+hireratesa[_N-1]+hireratesa[_N-2]+hireratesa[_N-3])/4
data_sa <- data_sa %>%
  group_by(cellid) %>%
  mutate(hireratesa_last = (nth(hireratesa, -1) +
                               nth(hireratesa, -2) +
                               nth(hireratesa, -3) +
                               nth(hireratesa, -4)) / 4) %>%
  ungroup()

#by cellid: keep if _n==1
#keeps first observation in each grouping
data_sa <- data_sa %>% 
  group_by(cellid) %>%
  slice(1) %>%
  ungroup()

ind_label <- c("Agric","Mining","Utilities","Const","Manuf","Wholesale","Retail",
               "Transport","Infor","FIRE","RealEst","ProfServ","Manag","Admin",
               "Educ","Health","Arts","AccomRest","Other")

ggplot() +
  geom_point(data = data_sa, aes(x = oneqsepsratesa_first, y = oneqsepsratesa_last), color = "blue") +
  geom_abline(slope = 1, intercept = 0, color = "black") +
  xlim(0, 0.3) +
  ylim(0, 0.3) +
  labs(x = expression("1997 One quarter incidence rate (q"[1]*")"),
       y = expression("2017 One quarter incidence rate (q"[1]*")")) +
  geom_text(data = data_sa,aes(x = oneqsepsratesa_first, y = oneqsepsratesa_last,label=ind_label),nudge_x=0.006,nudge_y=0.006) +
  theme(panel.background = element_rect(fill = 'white', colour = 'white'))

ggsave('oneqseps_firstlast_industry_1997.pdf')

ggplot() +
  geom_point(data = data_sa, aes(x = hireratesa_first, y = hireratesa_last), color = "blue") +
  geom_abline(slope = 1, intercept = 0, color = "black") +
  xlim(0, 0.5) +
  ylim(0, 0.5) +
  labs(x = "1997 Hires rate (h)",
       y = "2017 Hires rate (h)") +
  geom_text(data = data_sa,aes(x = hireratesa_first, y = hireratesa_last,label=ind_label),nudge_x=0.01,nudge_y=0.01) +
  theme(panel.background = element_rect(fill = 'white', colour = 'white'))

ggsave('hirerate_firstlast_industry_1997.pdf')