if(!require(knitr)) install.packages("knitr")

knitr::opts_chunk$set(echo = TRUE)

if(!require(dplyr)) install.packages("dplyr")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(xtable)) install.packages("xtable")
if(!require(devtools)) install.packages("devtools")
if(!require(cowplot)) install.packages("cowplot")
if(!require(magrittr)) install.packages("magrittr")
if(!require(multipanelfigure)) install.packages("multipanelfigure")

library(dplyr)
library(devtools)
library(ggplot2)
library(xtable)
library(cowplot)
library(magrittr)
library(multipanelfigure)

## Read ALS Data
workdir <- "F:\\Yatsenko\\ALEX\\R\\Initial_Research\\ALS"
setwd(workdir) 

finenameALSFRS <- "PROACT_ALSFRS.csv"
finenameALSFRSHistory <- "PROACT_ALSHISTORY.csv"
finenameDeath <- "PROACT_DEATHDATA.csv"
finenameRiluzole <- "PROACT_RILUZOLE.csv"

alsrfs <-  read.csv(file = paste(workdir,finenameALSFRS,sep="/"))
alsrfs$subject_id <- as.factor(alsrfs$subject_id)
print(str(alsrfs))

alsrfsH <-  read.csv(file = paste(workdir,finenameALSFRSHistory,sep="/"))
alsrfsH$subject_id <- as.factor(alsrfsH$subject_id)
print(str(alsrfsH))

deathdata <-  read.csv(file = paste(workdir,finenameDeath,sep="/"))
deathdata$subject_id <- as.factor(deathdata$subject_id)
print(str(deathdata))

riluzole <-  read.csv(file = paste(workdir,finenameRiluzole,sep="/"))
riluzole$subject_id <- as.factor(riluzole$subject_id)
print(str(riluzole))

columnsALSFRS1 <- c("subject_id","ALSFRS_Delta","ALSFRS_R_Total")
columnsALSFRS2 <- c("subject_id","ALSFRS_Delta","avg_ALSFRS_R_Total")
columnsALSFRSH <- c("subject_id",	"Subject_ALS_History_Delta", "Onset_Delta", "Diagnosis_Delta")

## Death Data
# subject_id is not unique here. 
# it is one person with two death date, i will choose the last
f_deathdata <- 
  deathdata  %>% 
  unique() %>%
  group_by(subject_id,Subject_Died) %>%
  summarise(Death_Days_max = max(Death_Days)) %>%
  ungroup()

colnames(f_deathdata)[colnames(f_deathdata) == "Death_Days_max"] = "Death_Days"
print(str(f_deathdata))  

## Riluzole  Data
# subject_id is unique here. I have checked
f_riluzole <- 
  riluzole  %>% 
  unique() 

print(str(f_riluzole))

## ALSRFRS Historical  Data
# ALSRFRS Historical data
f_alsrfsH <- 
  alsrfsH %>% 
  select(all_of(columnsALSFRSH)) 

# Unique subject_id alsrfsHistory 
print(length(unique(f_alsrfsH$subject_id)))
# Unique subject_id alsrfsHistory with not empty Onset_Delta
f1 <- filter(f_alsrfsH,!is.na(Onset_Delta))
print(dim(unique(f1)))
# Unique subject_id alsrfsHistory with not empty Onset_Delta
#
f2 <- filter(f1,!is.na(Diagnosis_Delta))
print(dim(unique(f2)))

f_alsrfsH_O <- 
  f_alsrfsH %>% 
  filter(!is.na(Onset_Delta)) %>%
  unique() 

f_alsrfsH_O$Onset_weeks  <- round( f_alsrfsH_O$Onset_Delta/ 7.0 )
f_alsrfsH_O$Onset_months <-round( f_alsrfsH_O$Onset_Delta/ 30.0 )

f_alsrfsH_OD <- 
  f_alsrfsH_O %>% 
  filter(!is.na(Diagnosis_Delta)) %>%
  unique() 

f_alsrfsH_OD$Diagnosis_weeks <- round( f_alsrfsH_OD$Diagnosis_Delta/ 7.0 )
f_alsrfsH_OD$Diagnosis_months <- round(f_alsrfsH_OD$Diagnosis_Delta/ 30.0 )

f_alsrfsH_OD$Delta_OD_days <- 
  f_alsrfsH_OD$Diagnosis_Delta - f_alsrfsH_OD$Onset_Delta

f_alsrfsH_OD$Delta_OD_weeks <- round( f_alsrfsH_OD$Delta_OD_days/ 7.0 )
f_alsrfsH_OD$Delta_OD_months <- round( f_alsrfsH_OD$Delta_OD_days/ 30.0 )

print(str(f_alsrfsH_O))
print(str(f_alsrfsH_OD))

## ------------------------------------------------------
## Stat Onsite and Diagnosis time, months
print(summary(f_alsrfsH_OD$Onset_months))
print(summary(f_alsrfsH_OD$Diagnosis_months))
print(summary(f_alsrfsH_OD$Delta_OD_months))

xmin <- -10
xmax <- 100
xstep<-10 

## Histogram: Difference between Onsite and Diagnosis time, months
p1 <- ggplot(f_alsrfsH_OD, aes(x =  Delta_OD_months)) + 
  geom_histogram(color="blue", fill="lightblue", binwidth=1) +
  xlim(xmin, xmax)  +
  scale_x_continuous(breaks = seq(xmin, xmax, xstep), lim = c(xmin, xmax)) +
  geom_vline(aes(xintercept = mean(Delta_OD_months)), color = "#000000", size = 1) +
  geom_vline(aes(xintercept = median(Delta_OD_months)), color = "#101000", size = 1) +
  ggtitle("ALSRFS History") +
  xlab("Diagnostic-Onset, Time, Months")

p2 <- ggplot(f_alsrfsH_OD, aes(x =  Delta_OD_months)) + 
  geom_histogram(aes(y=after_stat(density)), color="blue", fill="lightblue", binwidth=1)+
  geom_density(alpha=.2, fill="#FF6666") +
  xlim(xmin, xmax) +
  scale_x_continuous(breaks = seq(xmin, xmax, xstep), lim = c(xmin, xmax)) +
  geom_vline(aes(xintercept = mean(Delta_OD_months)), color = "#000000", size = 1) +
  ggtitle("ALSRFS History")+
  xlab("Diagnostic-Onset, Time, Months")


## ------------------------------------------------------
## Intersection Original ALSRFS History with Death Data
f_alsrfsH_ODDI <-
  f_alsrfsH_OD  %>%
  inner_join(f_deathdata, by=c("subject_id" = "subject_id"))

f_alsrfsH_ODDI$Death_weeks <- round( f_alsrfsH_ODDI$Death_Days/ 7.0 )
f_alsrfsH_ODDI$Death_months <- round(f_alsrfsH_ODDI$Death_Days/ 30.0 )

f_alsrfsH_ODDI$Delta_DD_days <- 
  f_alsrfsH_ODDI$Death_Days - f_alsrfsH_ODDI$Diagnosis_Delta
f_alsrfsH_ODDI$Delta_DD_weeks <- round( f_alsrfsH_ODDI$Delta_DD_days/ 7.0 )
f_alsrfsH_ODDI$Delta_DD_months <- round(f_alsrfsH_ODDI$Delta_DD_days/ 30.0 )

f_alsrfsH_ODDT <-
  f_alsrfsH_ODDI  %>%
  filter(Subject_Died=="Yes") %>%
 # filter(!is.na(Death_Days))

## ------------------------------------------------------
## Stat Onsite and Diagnosis, Death time months 
## Historical ALSRFS intersect with Death Data (Yes and No)
print(summary(f_alsrfsH_ODDI$Onset_months))
print(tapply(f_alsrfsH_ODDI$Onset_months,f_alsrfsH_ODDI$Subject_Died, summary))
print(summary(f_alsrfsH_ODDI$Diagnosis_months))
print(tapply(f_alsrfsH_ODDI$Diagnosis_months,f_alsrfsH_ODDI$Subject_Died, summary))
print(summary(f_alsrfsH_ODDI$Delta_OD_months))
print(tapply(f_alsrfsH_ODDI$Delta_OD_months,f_alsrfsH_ODDI$Subject_Died, summary))
print(summary(f_alsrfsH_ODDI$Death_months))
print(tapply(f_alsrfsH_ODDI$Death_months,f_alsrfsH_ODDI$Subject_Died, summary))
print(summary(f_alsrfsH_ODDI$Delta_DD_months))
print(tapply(f_alsrfsH_ODDI$Delta_DD_months,f_alsrfsH_ODDI$Subject_Died, summary))

## Histogram: Difference between Onsite and Diagnosis time, months
#p3 <- 
  ggplot(f_alsrfsH_ODDI, aes(x =  Delta_OD_months)) + 
  geom_histogram(color="blue", fill="lightblue", binwidth=1) +
  xlim(xmin, xmax) +
  scale_x_continuous(breaks = seq(xmin, xmax, xstep), lim = c(xmin, xmax)) +
  geom_vline(aes(xintercept = mean(Delta_OD_months, na.rm=TRUE)), color = "#000000", size = 1) +
  geom_vline(aes(xintercept = median(Delta_OD_months, na.rm=TRUE)), color = "#000000", size = 1) +
  ggtitle("ALSRFS History intersection with death info")  + 
  facet_grid(Subject_Died~., margins = TRUE, scales='free', labeller=label_both) +
  xlab("Diagnostic-Onset, Time, Months")

#p4 <- 
  ggplot(f_alsrfsH_ODDI, aes(x =  Delta_OD_months)) + 
  geom_histogram(aes(y=after_stat(density)), color="blue", fill="lightblue", binwidth=1)+
  geom_density(alpha=.2, fill="#FF6666") +
  xlim(xmin, xmax) +
  scale_x_continuous(breaks = seq(xmin, xmax, xstep), lim = c(xmin, xmax)) +
  geom_vline(aes(xintercept = mean(Delta_OD_months, na.rm=TRUE)), color = "#000000", size = 1) +
  geom_vline(aes(xintercept = median(Delta_OD_months, na.rm=TRUE)), color = "#000000", size = 1) +
  ggtitle("ALSRFS History intersection with death info") + 
  facet_grid(Subject_Died~., margins = TRUE, scales='free', labeller=label_both) +
  xlab("Diagnostic-Onset, Time, Months")


## Histogram: ALSFRS History, Dead time month, 
# difference=(Death time - diagnosis time) month
xmin <- -5
xmax <- 70
xstep<- 5

#p5 <- 
  ggplot(filter(f_alsrfsH_ODDI,Subject_Died=="Yes"), aes(x =  Death_months)) + 
  geom_histogram(color="blue", fill="lightblue", binwidth=1) +
  xlim(xmin, xmax) +
  scale_x_continuous(breaks = seq(xmin, xmax, xstep), lim = c(xmin, xmax)) +
  geom_vline(aes(xintercept = mean(Death_months, na.rm=TRUE)), color = "#000000", size = 1) +
  geom_vline(aes(xintercept = median(Death_months, na.rm=TRUE)), color = "#000000", size = 1) +
  #facet_grid(Subject_Died~., margins = TRUE, scales='free', labeller=label_both) +
  ggtitle("ALSRFS History intersection with dead") +
  xlab("Death, Time, Months")


#p6 <-  
  ggplot(filter(f_alsrfsH_ODDI,Subject_Died=="Yes"), aes(x =  Delta_DD_months)) + 
  geom_histogram(color="blue", fill="lightblue", binwidth=1) +
  xlim(xmin, xmax) +
  scale_x_continuous(breaks = seq(xmin, xmax, xstep), lim = c(xmin, xmax)) +
  geom_vline(aes(xintercept = mean(Delta_DD_months, na.rm=TRUE)), color = "#000000", size = 1) +
  geom_vline(aes(xintercept = median(Delta_DD_months, na.rm=TRUE)), color = "#000000", size = 1) +
 # facet_grid(Subject_Died~., margins = TRUE, scales='free', labeller=label_both) +
  ggtitle("ALSRFS History intersection with dead")  +
  xlab("Death - Diagnostic, Time, Months")


##  -------------
## ALSRFRS  Data
# select columns "subject_id","ALSFRS_Delta","ALSFRS_R_Total"
# filter rows with values in columns "ALSFRS_Delta","ALSFRS_R_Total"
# average value of "ALSFRS_R_Total" in a certain in moment of time
# we have several records some time


f_alsrfs <- 
  alsrfs %>% 
  select(all_of(columnsALSFRS1)) %>% 
  filter(!is.na(ALSFRS_R_Total)) %>%
  filter(!is.na(ALSFRS_Delta))  %>%
  group_by(subject_id, ALSFRS_Delta) %>%
  summarise(avg_ALSFRS_R_Total = mean(ALSFRS_R_Total)) %>%
  select(all_of(columnsALSFRS2)) %>%
  ungroup() 


colnames(f_alsrfs)[colnames(f_alsrfs) == "avg_ALSFRS_R_Total"] = "ALSFRS_R_Total"
f_alsrfs$ALSFRS_R_Total <- round( f_alsrfs$ALSFRS_R_Total )

print(str(f_alsrfs))

## Original statistics preparation
# Calculate simple statistics: number of measurement (visits), day of first visit, last visit
# time between first and last visit days, weeks, months
sf_alsrfs <- 
  f_alsrfs %>%
  group_by(subject_id) %>%
  summarise( Number_visits = n(), 
             first_ALSFRS = min(ALSFRS_Delta), 
             last_ALSFRS = max(ALSFRS_Delta), 
             interval_ALSFRS = 1+max(ALSFRS_Delta) - min(ALSFRS_Delta) ) 


sf_alsrfs$first_ALSFRS_weeks <- round( sf_alsrfs$first_ALSFRS / 7.0 )
sf_alsrfs$first_ALSFRS_months <- round( sf_alsrfs$first_ALSFRS / 30.0 )
sf_alsrfs$last_ALSFRS_weeks <- round( sf_alsrfs$last_ALSFRS / 7.0 )
sf_alsrfs$last_ALSFRS_months <- round( sf_alsrfs$last_ALSFRS / 30.0 )
sf_alsrfs$interval_ALSFRS_weeks <- round( sf_alsrfs$interval_ALSFRS / 7.0 )
sf_alsrfs$interval_ALSFRS_months <- round( sf_alsrfs$interval_ALSFRS / 30.0 )

# Add value of ALSFRS_R in the first measurement
sf_alsrfs <- 
  sf_alsrfs  %>%
  inner_join(f_alsrfs, by=c("subject_id" = "subject_id", "first_ALSFRS" =  "ALSFRS_Delta")) %>%
  unique()

colnames(sf_alsrfs)[colnames(sf_alsrfs) == "ALSFRS_R_Total"] = "intial_ALSFRS_R_Total"

# Add value of ALSFRS_R in the last measurement
sf_alsrfs <- 
  sf_alsrfs  %>%
  inner_join(f_alsrfs, by=c("subject_id" = "subject_id", "last_ALSFRS" =  "ALSFRS_Delta")) %>%
  unique()

colnames(sf_alsrfs)[colnames(sf_alsrfs) == "ALSFRS_R_Total"] = "last_ALSFRS_R_Total"

print(str(sf_alsrfs))

## Inner intersection
## ALSRFS History, Dead info, with table ALSRFS value

sf_alsrfsH_ODDI <-
  sf_alsrfs  %>%
  inner_join(f_alsrfsH_ODDI, by=c("subject_id" = "subject_id"))

sf_alsrfsH_ODDI$fDelta_OFA_days <- 
  sf_alsrfsH_ODDI$first_ALSFRS - sf_alsrfsH_ODDI$Onset_Delta
sf_alsrfsH_ODDI$fDelta_DFA_days <- 
  sf_alsrfsH_ODDI$first_ALSFRS - sf_alsrfsH_ODDI$Diagnosis_Delta

sf_alsrfsH_ODDI$lDelta_DLA_days <- 
  sf_alsrfsH_ODDI$Death_Days - sf_alsrfsH_ODDI$last_ALSFRS


sf_alsrfsH_ODDI$fDelta_OFA_weeks <- round(sf_alsrfsH_ODDI$fDelta_OFA_days /7.0)
sf_alsrfsH_ODDI$fDelta_OFA_months <- round(sf_alsrfsH_ODDI$fDelta_OFA_days/30.0)
sf_alsrfsH_ODDI$fDelta_DFA_weeks <- round(sf_alsrfsH_ODDI$fDelta_DFA_days /7.0)
sf_alsrfsH_ODDI$fDelta_DFA_months <- round(sf_alsrfsH_ODDI$fDelta_DFA_days/30.0)
sf_alsrfsH_ODDI$lDelta_DLA_weeks <- round(sf_alsrfsH_ODDI$lDelta_DLA_days /7.0)
sf_alsrfsH_ODDI$lDelta_DLA_months <- round(sf_alsrfsH_ODDI$lDelta_DLA_days/30.0)


## ------------------------------------------------------
## Stat Onsite and Diagnosis, Death time months 
## Historical ALSRFS intersect with Death Data (Yes and No)

print(summary(sf_alsrfsH_ODDI$Onset_months))
print(tapply(sf_alsrfsH_ODDI$Onset_months,sf_alsrfsH_ODDI$Subject_Died, summary))
print(summary(sf_alsrfsH_ODDI$Diagnosis_months))
print(tapply(sf_alsrfsH_ODDI$Diagnosis_months,sf_alsrfsH_ODDI$Subject_Died, summary))
print(summary(sf_alsrfsH_ODDI$Delta_OD_months))
print(tapply(sf_alsrfsH_ODDI$Delta_OD_months,sf_alsrfsH_ODDI$Subject_Died, summary))
print(summary(sf_alsrfsH_ODDI$Death_months))
print(tapply(sf_alsrfsH_ODDI$Death_months,sf_alsrfsH_ODDI$Subject_Died, summary))
print(summary(sf_alsrfsH_ODDI$Delta_DD_months))
print(tapply(sf_alsrfsH_ODDI$Delta_DD_months,sf_alsrfsH_ODDI$Subject_Died, summary))

print(summary(sf_alsrfsH_ODDI$first_ALSFRS))
print(tapply(sf_alsrfsH_ODDI$first_ALSFRS,sf_alsrfsH_ODDI$Subject_Died, summary))
print(summary(sf_alsrfsH_ODDI$fDelta_OFA_months))
print(tapply(sf_alsrfsH_ODDI$fDelta_OFA_months,sf_alsrfsH_ODDI$Subject_Died, summary))
print(summary(sf_alsrfsH_ODDI$fDelta_DFA_months))
print(tapply(sf_alsrfsH_ODDI$fDelta_DFA_months,sf_alsrfsH_ODDI$Subject_Died, summary))
print(summary(sf_alsrfsH_ODDI$Death_months ))
print(tapply(sf_alsrfsH_ODDI$Death_months ,sf_alsrfsH_ODDI$Subject_Died, summary))
print(summary(sf_alsrfsH_ODDI$lDelta_DLA_months))
print(tapply(sf_alsrfsH_ODDI$lDelta_DLA_months,sf_alsrfsH_ODDI$Subject_Died, summary))

xmin <- 0
xmax <- 65
xstep<- 5

#p7 <-
ggplot(sf_alsrfsH_ODDI, aes(x =  first_ALSFRS)) + 
  geom_histogram(color="blue", fill="lightblue", binwidth=1) +
  xlim(xmin, xmax) +
  scale_x_continuous(breaks = seq(xmin, xmax, xstep), lim = c(xmin, xmax)) +
  geom_vline(aes(xintercept = mean(first_ALSFRS, na.rm=TRUE)), color = "#000000", size = 1) +
  geom_vline(aes(xintercept = median(first_ALSFRS, na.rm=TRUE)), color = "#000000", size = 1) +
  ggtitle("ALSRFS History intersection with dead and ALSRFS")  +
  xlab("first_ALSFRS, Time, Months")

#p8 <-
  ggplot(sf_alsrfsH_ODDI, aes(x =  fDelta_OFA_months)) + 
  geom_histogram(color="blue", fill="lightblue", binwidth=1) +
  xlim(xmin, xmax) +
  scale_x_continuous(breaks = seq(xmin, xmax, xstep), lim = c(xmin, xmax)) +
  geom_vline(aes(xintercept = mean(fDelta_OFA_months, na.rm=TRUE)), color = "#000000", size = 1) +
  geom_vline(aes(xintercept = median(fDelta_OFA_months, na.rm=TRUE)), color = "#000000", size = 1) +
  ggtitle("ALSRFS History intersection with dead and ALSRFS")  +
  xlab("first_ALSFRS - Onset_Delta, Time, Months")


xmin <- -5
xmax <- 60
xstep<- 5

#p9 <-  
  ggplot(sf_alsrfsH_ODDI, aes(x =  fDelta_DFA_months)) + 
  geom_histogram(color="blue", fill="lightblue", binwidth=1) +
  xlim(xmin, xmax) +
  scale_x_continuous(breaks = seq(xmin, xmax, xstep), lim = c(xmin, xmax)) +
  geom_vline(aes(xintercept = mean(fDelta_DFA_months, na.rm=TRUE)), color = "#000000", size = 1) +
  geom_vline(aes(xintercept = median(fDelta_DFA_months, na.rm=TRUE)), color = "#000000", size = 1) +
  ggtitle("ALSRFS History intersection with dead and ALSRFS")  +
  xlab("Death - Diagnostic, Time, Months")

  
  xmin <- 0
  xmax <- 70
  xstep<- 5
  
  #p10 <-  
  ggplot(sf_alsrfsH_ODDI, aes(x =  Death_months )) + 
    geom_histogram(color="blue", fill="lightblue", binwidth=1) +
    xlim(xmin, xmax) +
    scale_x_continuous(breaks = seq(xmin, xmax, xstep), lim = c(xmin, xmax)) +
    geom_vline(aes(xintercept = mean( Death_months, na.rm=TRUE)), color = "#000000", size = 1) +
    geom_vline(aes(xintercept = median( Death_months, na.rm=TRUE)), color = "#000000", size = 1) +
    ggtitle("ALSRFS History intersection with dead and ALSRFS")   +
    xlab("Death, Time, Months")  
  
xmin <- -2.5
xmax <- 35
xstep<- 2.5

#p11 <-  
  ggplot(sf_alsrfsH_ODDI, aes(x =  lDelta_DLA_months)) + 
  geom_histogram(color="blue", fill="lightblue", binwidth=1) +
  xlim(xmin, xmax) +
  scale_x_continuous(breaks = seq(xmin, xmax, xstep), lim = c(xmin, xmax)) +
  geom_vline(aes(xintercept = mean(lDelta_DLA_months, na.rm=TRUE)), color = "#000000", size = 1) +
  geom_vline(aes(xintercept = median(lDelta_DLA_months, na.rm=TRUE)), color = "#000000", size = 1) +
  ggtitle("ALSRFS History intersection with dead and ALSRFS")   +
  xlab("Death - last ALSRFS, Time, Months")

####
  
  col <- c("intial_ALSFRS_R_Total","last_ALSFRS_R_Total","fDelta_OFA_months","fDelta_DFA_months",
          "first_ALSFRS_months","last_ALSFRS_months")
  stat <-
    sf_alsrfsH_ODDI   %>%
    select(col)
  
  stat_yes <-
    sf_alsrfsH_ODDI   %>%
    filter(Subject_Died=="Yes") %>%
    select(col)
  
  stat_no <-
    sf_alsrfsH_ODDI   %>%
    filter(Subject_Died=="No") %>%
    select(col)
  
  print(cor(stat, use = "complete.obs"))
  print(cor(stat_yes, use = "complete.obs"))
  print(cor(stat_no, use = "complete.obs"))
    
  
  ## Initial ALFRS_R depends on time interval  fDelta_DFA_months (months)

  p12<- ggplot(sf_alsrfsH_ODDI, aes(x = fDelta_DFA_months, y = intial_ALSFRS_R_Total)) +
    geom_point(size=2,aes(color = intial_ALSFRS_R_Total))
  p13<- p12 + facet_grid(Subject_Died~., margins = TRUE, scales='free', labeller=label_both)
  show(p12)
  show(p13)  
  
  