#loading libraries
library(ggplot2)
library(dplyr)

#set working directory where data folder is
wd = "C:/Users/Alex/OneDrive - Universite de Montreal/Replication"
setwd(wd)

data_sa <- read.csv(paste(wd,"qwisa_1997.csv",sep="/"))

#drop if agegrp=="A00"
data_sa <- data_sa[!(data_sa$agegrp == "A00"), ]
#drop if sex == 0
data_sa <- data_sa[!(data_sa$sex == 0), ]
#keep if industry=="00"
data_sa <- data_sa[data_sa$industry == "00", ]
#drop industry
data_sa <- subset(data_sa, select = -c(industry))

#generate variables of interest
data_sa <- arrange(data_sa,geography,sex,agegrp,year,quarter)

#egen cellid=group(year quarter)
data_sa <- data_sa %>%
  mutate(cellid = group_indices(., year, quarter))

#aggregate over geography, sex, age

#bysort cellid: egen allhira=total(hira)

#hires in the quarter
data_sa <- data_sa %>%
  arrange(cellid) %>%
  group_by(cellid) %>%
  #returns sum if some rows are not NA, otherwise returns NA
  mutate(allhira := ifelse(all(is.na(HirA)), NA, sum(HirA, na.rm = T))) %>%
  ungroup()

#bysort cellid: egen allhiras=total(hiras)

#"stable" hires, workers in second quarter of employment
data_sa <- data_sa %>%
  arrange(cellid) %>%
  group_by(cellid) %>%
  mutate(allhiras := ifelse(all(is.na(HirAS)), NA, sum(HirAS, na.rm = T))) %>%
  ungroup()

#bysort cellid: egen allhiraend=total(hiraend)

#all hires that also survive into second quarter
data_sa <- data_sa %>%
  arrange(cellid) %>%
  group_by(cellid) %>%
  mutate(allhiraend := ifelse(all(is.na(HirAEnd)), NA, sum(HirAEnd, na.rm = T))) %>%
  ungroup()

#bysort cellid: egen allemp=total(emp)

#beginning of quarter employment (had earnings last quarter and this quarter)
data_sa <- data_sa %>%
  arrange(cellid) %>%
  group_by(cellid) %>%
  mutate(allemp := ifelse(all(is.na(Emp)), NA, sum(Emp, na.rm = T))) %>%
  ungroup()

#bysort cellid: egen allempend=total(empend)

#end of quarter employment (have earnings this quarter and next quarter)
data_sa <- data_sa %>%
  arrange(cellid) %>%
  group_by(cellid) %>%
  mutate(allempend := ifelse(all(is.na(EmpEnd)), NA, sum(EmpEnd, na.rm = T))) %>%
  ungroup()

#bysort cellid: egen allemptot=total(emptotal)

#total count of people employed in the quarter
data_sa <- data_sa %>%
  arrange(cellid) %>%
  group_by(cellid) %>%
  mutate(allemptot := ifelse(all(is.na(EmpTotal)), NA, sum(EmpTotal, na.rm = T))) %>%
  ungroup()

#bysort cellid: egen allsep=total(sep)

#total separations
data_sa <- data_sa %>%
  arrange(cellid) %>%
  group_by(cellid) %>%
  mutate(allsep := ifelse(all(is.na(Sep)), NA, sum(Sep, na.rm = T))) %>%
  ungroup()

#bysort cellid: egen allsepbeg=total(sepbeg)

#total separations of employment spells that existed in previous quarter
data_sa <- data_sa %>%
  arrange(cellid) %>%
  group_by(cellid) %>%
  mutate(allsepbeg := ifelse(all(is.na(SepBeg)), NA, sum(SepBeg, na.rm = T))) %>%
  ungroup()

#bysort cellid: keep if _n==1
data_sa <- data_sa %>% 
  arrange(cellid) %>% 
  group_by(cellid) %>% 
  filter(row_number() == 1) %>% 
  ungroup()

#drop geography sex agegrp
data_sa <- subset(data_sa, select = -c(geography))
data_sa <- subset(data_sa, select = -c(sex))
data_sa <- subset(data_sa, select = -c(agegrp))

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
data_sa <- data_sa[!(as.numeric(data_sa$time) <= 1996*4+4 | as.numeric(data_sa$time) > 2018*4), ]

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

#egen oneqsepsrate1means=mean(oneqsepsrate1)
data_sa$oneqsepsrate1means = mean(data_sa$oneqsepsrate1,na.rm=TRUE)

#egen oneqsepsratemeans=mean(oneqsepsrate)
data_sa$oneqsepsratemeans = mean(data_sa$oneqsepsrate,na.rm=TRUE)

#egen oneqhazratemeans=mean(oneqhazrate)
data_sa$oneqhazratemeans = mean(data_sa$oneqhazrate,na.rm=TRUE)

#egen twoplussepsratemeans=mean(twoplussepsrate)
data_sa$twoplussepsratemeans = mean(data_sa$twoplussepsrate,na.rm=TRUE)

#egen twoplushazratemeans=mean(twoplushazrate)
data_sa$twoplushazratemeans = mean(data_sa$twoplushazrate,na.rm=TRUE)

#egen twoqsepsratemeans=mean(twoqsepsrate)
data_sa$twoqsepsratemeans = mean(data_sa$twoqsepsrate,na.rm=TRUE)

#egen twoqhazratemeans=mean(twoqhazrate)
data_sa$twoqhazratemeans = mean(data_sa$twoqhazrate,na.rm=TRUE)

#egen threeplussepsratemeans=mean(threeplussepsrate)
data_sa$threeplussepsratemeans = mean(data_sa$threeplussepsrate,na.rm=TRUE)

#egen threeplushazratemeans=mean(threeplushazrate)
data_sa$threeplushazratemeans = mean(data_sa$threeplushazrate,na.rm=TRUE)

#egen sepsratemeans=mean(sepsrate)
data_sa$sepsratemeans = mean(data_sa$sepsrate,na.rm=TRUE)

#egen hirerate1means=mean(hirerate1)
data_sa$hirerate1means = mean(data_sa$hirerate1,na.rm=TRUE)

#egen hireratemeans=mean(hirerate)
data_sa$hireratemeans = mean(data_sa$hirerate,na.rm=TRUE)

#reg oneqsepsrate1 i.quarter
model <- lm(oneqsepsrate1 ~ factor(quarter), data = data_sa)
#predict double sa, residual 
data_sa$sa <- residuals(model)
#replace oneqsepsrate1sa=sa+oneqsepsrate1means
data_sa$oneqsepsrate1sa <- data_sa$sa + data_sa$oneqsepsrate1means
#drop sa
data_sa <- subset(data_sa, select = -c(sa))

#reg oneqsepsrate i.quarter
#predict double sa, residual 
#replace oneqsepsratesa=sa+oneqsepsratemeans
#drop sa
model <- lm(oneqsepsrate ~ factor(quarter), data = data_sa)
data_sa$sa <- residuals(model)
data_sa$oneqsepsratesa <- data_sa$sa + data_sa$oneqsepsratemeans
data_sa <- subset(data_sa, select = -c(sa))

#reg oneqhazrate i.quarter
#predict double sa, residual 
#replace oneqhazratesa=sa+oneqhazratemeans
#drop sa
model <- lm(oneqhazrate ~ factor(quarter), data = data_sa)
data_sa$sa <- residuals(model)
data_sa$oneqhazratesa <- data_sa$sa + data_sa$oneqhazratemeans
data_sa <- subset(data_sa, select = -c(sa))

#reg twoplussepsrate i.quarter
#predict double sa, residual 
#replace twoplussepsratesa=sa+twoplussepsratemeans
#drop sa
model <- lm(twoplussepsrate ~ factor(quarter), data = data_sa)
data_sa$sa <- residuals(model)
data_sa$twoplussepsratesa <- data_sa$sa + data_sa$twoplussepsratemeans
data_sa <- subset(data_sa, select = -c(sa))

#reg twoplushazrate i.quarter
#predict double sa, residual 
#replace twoplushazratesa=sa+twoplushazratemeans
#drop sa
model <- lm(twoplushazrate ~ factor(quarter), data = data_sa)
data_sa$sa <- residuals(model)
data_sa$twoplushazratesa <- data_sa$sa + data_sa$twoplushazratemeans
data_sa <- subset(data_sa, select = -c(sa))

#reg twoqsepsrate i.quarter
#predict double sa, residual 
#replace twoqsepsratesa=sa+twoqsepsratemeans
#drop sa
model <- lm(twoqsepsrate ~ factor(quarter), data = data_sa)
data_sa$sa <- residuals(model)
data_sa$twoqsepsratesa <- data_sa$sa + data_sa$twoqsepsratemeans
data_sa <- subset(data_sa, select = -c(sa))

#reg twoqhazrate i.quarter
#predict double sa, residual 
#replace twoqhazratesa=sa+twoqhazratemeans
#drop sa
model <- lm(twoqhazrate ~ factor(quarter), data = data_sa)
data_sa$sa <- residuals(model)
data_sa$twoqhazratesa <- data_sa$sa + data_sa$twoqhazratemeans
data_sa <- subset(data_sa, select = -c(sa))

#reg threeplussepsrate i.quarter
#predict double sa, residual 
#replace threeplussepsratesa=sa+threeplussepsratemeans
#drop sa
model <- lm(threeplussepsrate ~ factor(quarter), data = data_sa)
data_sa$sa <- residuals(model)
data_sa$threeplussepsratesa <- data_sa$sa + data_sa$threeplussepsratemeans
data_sa <- subset(data_sa, select = -c(sa))

#reg threeplushazrate i.quarter
#predict double sa, residual 
#replace threeplushazratesa=sa+threeplushazratemeans
#drop sa
model <- lm(threeplushazrate ~ factor(quarter), data = data_sa)
data_sa$sa <- residuals(model)
data_sa$threeplushazratesa <- data_sa$sa + data_sa$threeplushazratemeans
data_sa <- subset(data_sa, select = -c(sa))

#reg sepsrate i.quarter
#predict double sa, residual 
#replace sepsratesa=sa+sepsratemeans
#drop sa
model <- lm(sepsrate ~ factor(quarter), data = data_sa)
data_sa$sa <- residuals(model)
data_sa$sepsratesa <- data_sa$sa + data_sa$sepsratemeans
data_sa <- subset(data_sa, select = -c(sa))

#reg hirerate1 i.quarter
#predict double sa, residual 
#replace hirerate1sa=sa+hirerate1means
#drop sa
model <- lm(hirerate1 ~ factor(quarter), data = data_sa)
data_sa$sa <- residuals(model)
data_sa$hirerate1sa <- data_sa$sa + data_sa$hirerate1means
data_sa <- subset(data_sa, select = -c(sa))

#reg hirerate i.quarter
#predict double sa, residual 
#replace hireratesa=sa+hireratemeans
#drop sa
model <- lm(hirerate ~ factor(quarter), data = data_sa)
data_sa$sa <- residuals(model)
data_sa$hireratesa <- data_sa$sa + data_sa$hireratemeans
data_sa <- subset(data_sa, select = -c(sa))

#figure 1
ggplot(data_sa, aes(x = date)) +
  geom_line(aes(y = hireratesa, color = "h"), size = 1) +
  geom_line(aes(y = sepsratesa, color = "s"), size = 1) +
  labs(x="Year",y="",color="") +
  scale_color_manual(values=c("h"="blue","s"="red")) +
  theme(panel.background = element_rect(fill = 'white', colour = 'white'))

ggsave('hire_sep_graph_1997.pdf')

#figure 5 (left)
ggplot(data_sa[data_sa$time>=8001,], aes(x = date)) +
  geom_line(aes(y = hireratesa), color="blue", size = 1) +
  labs(x="Year",y="hireratesa") +
  theme(panel.background = element_rect(fill = 'white', colour = 'white'))

ggsave('hiresrate_graph_1997.pdf')

#figure 5 (right)
ggplot(data_sa[data_sa$time>=8001,], aes(x = date)) +
  geom_line(aes(y = oneqhazratesa), color="blue", size = 1) +
  labs(x="Year",y="oneqhazratesa") +
  theme(panel.background = element_rect(fill = 'white', colour = 'white'))

ggsave('oneqhazratesa_graph_1997.pdf')

#figure 6
ggplot(data_sa, aes(x = date)) +
  geom_line(aes(y = oneqhazratesa, color = "d1"), size = 1) +
  geom_line(aes(y = twoqhazratesa, color = "d2"), size = 1) +
  geom_line(aes(y = threeplushazratesa, color = "d3"), size = 1) +
  labs(x="Year",y="Hazard rate",color="") +
  scale_color_manual(values=c("d1"="blue","d2"="red","d3"="darkgreen")) +
  theme(panel.background = element_rect(fill = 'white', colour = 'white'))

ggsave('allthreehazrates_graph_1997.pdf')

#generate onetwoqsepsrate=oneqsepsratesa+twoqsepsratesa
#generate onetwothreesepsrate=oneqsepsratesa+twoqsepsratesa+threeplussepsratesa
#generate twothreeqsepsrate=threeplussepsratesa+twoqsepsratesa
onetwoqsepsrate <- data_sa$oneqsepsratesa+data_sa$twoqsepsratesa
onetwothreesepsrate <- data_sa$oneqsepsratesa+data_sa$twoqsepsratesa+data_sa$threeplussepsratesa
twothreeqsepsrate <- data_sa$threeplussepsratesa + data_sa$twoqsepsratesa

#figure 3
ggplot(data_sa, aes(x = date)) +
  geom_area(aes(y = onetwothreesepsrate, fill = "One quarter"), alpha = 1) +
  geom_area(aes(y = twothreeqsepsrate, fill = "Two quarters"), alpha = 1) +
  geom_area(aes(y = threeplussepsratesa, fill = "Three or more quarters"), alpha = 1) +
  labs(x="Year",y="Separations rate",fill="") +
  scale_fill_manual(values=c("One quarter"="blue","Two quarters"="red","Three or more quarters"="darkgreen")) +
  theme(panel.background = element_rect(fill = 'white', colour = 'white'))

ggsave('sepsratestack_graph_1997.pdf')

#Produce the statistics reported throughout the text, and in Tables 1 and 2

#generate sepsratesa_beg=(sepsratesa[1]+sepsratesa[2]+sepsratesa[3]+sepsratesa[4])/4
sepsratesa_beg <- (nth(data_sa$hireratesa, 1)+nth(data_sa$hireratesa, 2)+
                     nth(data_sa$hireratesa, 3)+nth(data_sa$hireratesa, 4))/4
#generate sepsratesa_end=(sepsratesa[_N]+sepsratesa[_N-1]+sepsratesa[_N-2]+sepsratesa[_N-3])/4
sepsratesa_end <- (nth(data_sa$hireratesa, -1)+nth(data_sa$hireratesa, -2)+
                     nth(data_sa$hireratesa, -3)+nth(data_sa$hireratesa, -4))/4

#generate oneqsepsratesa_beg=(oneqsepsratesa[1]+oneqsepsratesa[2]+oneqsepsratesa[3]+oneqsepsratesa[4])/4
oneqsepsratesa_beg <- (nth(data_sa$oneqsepsratesa, 1)+nth(data_sa$oneqsepsratesa, 2)+
                     nth(data_sa$oneqsepsratesa, 3)+nth(data_sa$oneqsepsratesa, 4))/4

#generate oneqsepsratesa_end=(oneqsepsratesa[_N]+oneqsepsratesa[_N-1]+oneqsepsratesa[_N-2]+oneqsepsratesa[_N-3])/4
oneqsepsratesa_end <- (nth(data_sa$oneqsepsratesa, -1)+nth(data_sa$oneqsepsratesa, -2)+
                         nth(data_sa$oneqsepsratesa, -3)+nth(data_sa$oneqsepsratesa, -4))/4

#generate twoqsepsratesa_beg=(twoqsepsratesa[1]+twoqsepsratesa[2]+twoqsepsratesa[3]+twoqsepsratesa[4])/4
twoqsepsratesa_beg <- (nth(data_sa$twoqsepsratesa, 1)+nth(data_sa$twoqsepsratesa, 2)+
                         nth(data_sa$twoqsepsratesa, 3)+nth(data_sa$twoqsepsratesa, 4))/4

#generate twoqsepsratesa_end=(twoqsepsratesa[_N]+twoqsepsratesa[_N-1]+twoqsepsratesa[_N-2]+twoqsepsratesa[_N-3])/4
twoqsepsratesa_end <- (nth(data_sa$twoqsepsratesa, -1)+nth(data_sa$twoqsepsratesa, -2)+
                         nth(data_sa$twoqsepsratesa, -3)+nth(data_sa$twoqsepsratesa, -4))/4

#generate threeplussepsratesa_beg=(threeplussepsratesa[1]+threeplussepsratesa[2]+threeplussepsratesa[3]+threeplussepsratesa[4])/4
threeplussepsratesa_beg <- (nth(data_sa$threeplussepsratesa, 1)+nth(data_sa$threeplussepsratesa, 2)+
                         nth(data_sa$threeplussepsratesa, 3)+nth(data_sa$threeplussepsratesa, 4))/4

#generate threeplussepsratesa_end=(threeplussepsratesa[_N]+threeplussepsratesa[_N-1]+threeplussepsratesa[_N-2]+threeplussepsratesa[_N-3])/4
threeplussepsratesa_end <- (nth(data_sa$threeplussepsratesa, -1)+nth(data_sa$threeplussepsratesa, -2)+
                              nth(data_sa$threeplussepsratesa, -3)+nth(data_sa$threeplussepsratesa, -4))/4

#generate sepsratesa_change=sepsratesa_end-sepsratesa_beg
sepsratesa_change <- sepsratesa_end-sepsratesa_beg

#generate oneqsepsratesa_change=oneqsepsratesa_end-oneqsepsratesa_beg
oneqsepsratesa_change <- oneqsepsratesa_end-oneqsepsratesa_beg

#generate twoqsepsratesa_change=twoqsepsratesa_end-twoqsepsratesa_beg
twoqsepsratesa_change <- twoqsepsratesa_end-twoqsepsratesa_beg

#generate threeplussepsratesa_change=threeplussepsratesa_end-threeplussepsratesa_beg
threeplussepsratesa_change <- threeplussepsratesa_end-threeplussepsratesa_beg

#Numbers mentioned in Introduction, second paragraph
oneqsepsratesa_beg
oneqsepsratesa_end
sepsratesa_beg
sepsratesa_end

#generate oneqhazratesa_beg=(oneqhazratesa[1]+oneqhazratesa[2]+oneqhazratesa[3]+oneqhazratesa[4])/4
oneqhazratesa_beg <- (nth(data_sa$oneqhazratesa, 1)+nth(data_sa$oneqhazratesa, 2)+
                     nth(data_sa$oneqhazratesa, 3)+nth(data_sa$oneqhazratesa, 4))/4

#generate oneqhazratesa_end=(oneqhazratesa[_N]+oneqhazratesa[_N-1]+oneqhazratesa[_N-2]+oneqhazratesa[_N-3])/4
oneqhazratesa_end <- (nth(data_sa$oneqhazratesa, -1)+nth(data_sa$oneqhazratesa, -2)+
                        nth(data_sa$oneqhazratesa, -3)+nth(data_sa$oneqhazratesa, -4))/4

#generate twoqhazratesa_beg=(twoqhazratesa[1]+twoqhazratesa[2]+twoqhazratesa[3]+twoqhazratesa[4])/4
twoqhazratesa_beg <- (nth(data_sa$twoqhazratesa, 1)+nth(data_sa$twoqhazratesa, 2)+
                        nth(data_sa$twoqhazratesa, 3)+nth(data_sa$twoqhazratesa, 4))/4

#generate twoqhazratesa_end=(twoqhazratesa[_N]+twoqhazratesa[_N-1]+twoqhazratesa[_N-2]+twoqhazratesa[_N-3])/4
twoqhazratesa_end <- (nth(data_sa$twoqhazratesa, -1)+nth(data_sa$twoqhazratesa, -2)+
                        nth(data_sa$twoqhazratesa, -3)+nth(data_sa$twoqhazratesa, -4))/4

#generate threeplushazratesa_beg=(threeplushazratesa[1]+threeplushazratesa[2]+threeplushazratesa[3]+threeplushazratesa[4])/4
threeplushazratesa_beg <- (nth(data_sa$threeplushazratesa, 1)+nth(data_sa$threeplushazratesa, 2)+
                        nth(data_sa$threeplushazratesa, 3)+nth(data_sa$threeplushazratesa, 4))/4

#generate threeplushazratesa_end=(threeplushazratesa[_N]+threeplushazratesa[_N-1]+threeplushazratesa[_N-2]+threeplushazratesa[_N-3])/4
threeplushazratesa_end <- (nth(data_sa$threeplushazratesa, -1)+nth(data_sa$threeplushazratesa, -2)+
                             nth(data_sa$threeplushazratesa, -3)+nth(data_sa$threeplushazratesa, -4))/4

#generate hireratesa_beg=(hireratesa[1]+hireratesa[2]+hireratesa[3]+hireratesa[4])/4
hireratesa_beg <- (nth(data_sa$hireratesa, 1)+nth(data_sa$hireratesa, 2)+
                             nth(data_sa$hireratesa, 3)+nth(data_sa$hireratesa, 4))/4

#generate hireratesa_end=(hireratesa[_N]+hireratesa[_N-1]+hireratesa[_N-2]+hireratesa[_N-3])/4
hireratesa_end <- (nth(data_sa$hireratesa, -1)+nth(data_sa$hireratesa, -2)+
                     nth(data_sa$hireratesa, -3)+nth(data_sa$hireratesa, -4))/4

#generate oneqhazratesa_change=oneqhazratesa_end-oneqhazratesa_beg
oneqhazratesa_change <- oneqhazratesa_end-oneqhazratesa_beg

#generate twoqhazratesa_change=twoqhazratesa_end-twoqhazratesa_beg
twoqhazratesa_change <- twoqhazratesa_end-twoqhazratesa_beg

#generate threeplushazratesa_change=threeplushazratesa_end-threeplushazratesa_beg
threeplushazratesa_change <- threeplushazratesa_end-threeplushazratesa_beg

#generate hireratesa_change=hireratesa_end-hireratesa_beg
hireratesa_change <- hireratesa_end-hireratesa_beg

#Table 1
oneqhazratesa_beg
twoqhazratesa_beg 
threeplushazratesa_beg
hireratesa_beg
#Table 2
oneqhazratesa_change
twoqhazratesa_change
threeplushazratesa_change
hireratesa_change