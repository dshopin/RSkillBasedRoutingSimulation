setwd('~\\Optimization')
source('C:\\Users\\e6on6gv\\Documents\\GitHub\\RSkillBasedRoutingSimulation\\SBR.simulation.R')
source('generate.tasks.R')

library(tictoc)
library(parallel)

skills <- c('ZoneA','ZoneB','ZoneD','ZoneE','ZoneG','ZoneGov','ZoneI','ZoneM','ZoneQ','ZoneR','ZoneS','ZoneV')

ofr <- data.frame(skill=c('ZoneA','ZoneB','ZoneD','ZoneE','ZoneG','ZoneGov','ZoneI','ZoneM','ZoneQ','ZoneR','ZoneS','ZoneV','ZoneAOverflow','ZoneBOverflow','ZoneDOverflow','ZoneEOverflow','ZoneGOverflow','ZoneROverflow','ZoneVOverflow')
                  ,overflow.skill=c('ZoneAOverflow','ZoneBOverflow','ZoneDOverflow','ZoneEOverflow','ZoneGOverflow','ZoneGovOverflow','ZoneIOverflow','ZoneMOverflow','ZoneQOverflow','ZoneROverflow','ZoneSOverflow','ZoneVOverflow','DeptSouthOverflow','DeptSouthOverflow','DeptSouthOverflow','DeptNorthOverflow','DeptNorthOverflow','DeptNorthOverflow','DeptNorthOverflow')
                  ,threshold=c(40,40,40,40,40,40,40,40,40,40,40,40,20,20,20,20,20,20,20)
                  ,stringsAsFactors = FALSE
)

dists <- read.csv(file='DISTS.CSV', fileEncoding = 'UTF-8-BOM')
dists$zone1 <- as.character(dists$zone1)


servers <- read.csv(file='AGENTS.CSV', fileEncoding = 'UTF-8-BOM')
servers$server_id <- servers$ResourceLoginID
servers$date <- as.Date(servers$date, format='%d%b%Y')

realdata <- read.csv(file='QUEUESTATES.CSV', fileEncoding = 'UTF-8-BOM')
realdata$date <- as.Date(realdata$date, format='%d%b%Y')
realdata$zone1 <- as.character(realdata$zone1)

date <- as.Date('2016-10-4')
hour <- 12

tasks <- generate.tasks(skill.names=dists[dists$hour==hour, 'zone1']
                        ,iat.shapes=dists[dists$hour==hour, 'iat_Shape']
                        ,iat.scales=dists[dists$hour==hour, 'iat_Scale']
                        ,serv.shapes=dists[dists$hour==hour, 'talk_Shape']
                        ,serv.scales=dists[dists$hour==hour, 'talk_Scale']
                        ,warmup=0.4
                        ,duration=3600)

###############################################
##Comparing performance of different branches##
###############################################
time.branch <- sapply(1:20, function(x) system.time(
qmodel <- SBR.simulation(tasks=tasks, servers=servers[servers$hour==hour & servers$date==date,], overflow=ofr, warmup=0.4, plots=FALSE)
)[3])


time.master <- sapply(1:100, function(x) system.time(
  qmodel <- SBR.simulation(tasks=tasks, servers=servers[servers$hour==hour & servers$date==date,], overflow=ofr, warmup=0.4, plots=FALSE)
)[3])

paste(mean(time.branch)-1.96*sd(time.branch)/10, ' - ', mean(time.branch)+1.96*sd(time.branch)/10)
paste(mean(time.master)-1.96*sd(time.master)/10, ' - ', mean(time.master)+1.96*sd(time.master)/10)
################################################





##############################################################
##Looking for cases when negative values emerge in QUEUESTAT##
##############################################################

for(seed in 1:100){
  set.seed(seed)
  tasks <- generate.tasks(skill.names=dists[dists$hour==hour, 'zone1']
                          ,iat.shapes=dists[dists$hour==hour, 'iat_Shape']
                          ,iat.scales=dists[dists$hour==hour, 'iat_Scale']
                          ,serv.shapes=dists[dists$hour==hour, 'talk_Shape']
                          ,serv.scales=dists[dists$hour==hour, 'talk_Scale']
                          ,warmup=0.4
                          ,duration=3600)
  qmodel <- SBR.simulation(tasks=tasks, servers=servers[servers$hour==hour & servers$date==date,], overflow=ofr, warmup=0.4, plots=FALSE)
  queuestat <- qmodel$queuestat
  if(any(queuestat$L.by.skill<0) || any(queuestat$duration.by.skill<0) || any(queuestat$L.total<0) || any(queuestat$duration.total<0)){
    print(seed)
    break
  }
}



set.seed(94)
tasks <- generate.tasks(skill.names=dists[dists$hour==hour, 'zone1']
                        ,iat.shapes=dists[dists$hour==hour, 'iat_Shape']
                        ,iat.scales=dists[dists$hour==hour, 'iat_Scale']
                        ,serv.shapes=dists[dists$hour==hour, 'talk_Shape']
                        ,serv.scales=dists[dists$hour==hour, 'talk_Scale']
                        ,warmup=0.4
                        ,duration=3600)
qmodel <- SBR.simulation(tasks=tasks, servers=servers[servers$hour==hour & servers$date==date,], overflow=ofr, warmup=0.4, plots=FALSE)
queuestat <- qmodel$queuestat
##############################################################






