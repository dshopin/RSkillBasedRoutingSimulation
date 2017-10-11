
rm(list=ls())
setwd('C:\\Users\\e6on6gv\\Documents\\GitHub\\RSkillBasedRoutingSimulation\\')
source('SBR.simulation.R')
source('generate.tasks.R')

library(tictoc)
library(parallel)

skills <- c('ZoneA','ZoneB','ZoneD','ZoneE','ZoneG','ZoneGov','ZoneI','ZoneM','ZoneQ','ZoneR','ZoneS','ZoneV')

ofr <- data.frame(skill=c('ZoneA','ZoneB','ZoneD','ZoneE','ZoneG','ZoneGov','ZoneI','ZoneM','ZoneQ','ZoneR','ZoneS','ZoneV','ZoneAOverflow','ZoneBOverflow','ZoneDOverflow','ZoneEOverflow','ZoneGOverflow','ZoneROverflow','ZoneVOverflow')
                  ,overflow.skill=c('ZoneAOverflow','ZoneBOverflow','ZoneDOverflow','ZoneEOverflow','ZoneGOverflow','ZoneGovOverflow','ZoneIOverflow','ZoneMOverflow','ZoneQOverflow','ZoneROverflow','ZoneSOverflow','ZoneVOverflow','DeptSouthOverflow','DeptSouthOverflow','DeptSouthOverflow','DeptNorthOverflow','DeptNorthOverflow','DeptNorthOverflow','DeptNorthOverflow')
                  ,threshold=c(40,40,40,40,40,40,40,40,40,40,40,40,20,20,20,20,20,20,20)
                  ,stringsAsFactors = FALSE
)

dists <- read.csv(file='C:\\Users\\e6on6gv\\Documents\\Optimization\\DISTS.CSV', fileEncoding = 'UTF-8-BOM')
dists$zone1 <- as.character(dists$zone1)


servers <- read.csv(file='C:\\Users\\e6on6gv\\Documents\\Optimization\\AGENTS.CSV', fileEncoding = 'UTF-8-BOM')
servers$server_id <- servers$ResourceLoginID
servers$date <- as.Date(servers$date, format='%d%b%Y')

realdata <- read.csv(file='C:\\Users\\e6on6gv\\Documents\\Optimization\\QUEUESTATES.CSV', fileEncoding = 'UTF-8-BOM')
realdata$date <- as.Date(realdata$date, format='%d%b%Y')
realdata$zone1 <- as.character(realdata$zone1)



##Wrapper function for a single simulation
single.sim <- function(skill, date, hour, warmup){
  tasks <- generate.tasks(skill.names=dists[dists$hour==hour, 'zone1']
                          ,iat.shapes=dists[dists$hour==hour, 'iat_Shape']
                          ,iat.scales=dists[dists$hour==hour, 'iat_Scale']
                          ,serv.shapes=dists[dists$hour==hour, 'talk_Shape']
                          ,serv.scales=dists[dists$hour==hour, 'talk_Scale']
                          ,warmup=warmup
                          ,duration=3600)
  qmodel <- SBR.simulation(tasks=tasks, servers=servers[servers$hour==hour & servers$date==date,], overflow=ofr, warmup=warmup)
  return(qmodel)
}

cores <- detectCores()-1
cl <- makeCluster(cores)
dates <- unique(realdata[,c('date','hour')])$date
hours <- unique(realdata[,c('date','hour')])$hour
ntrial <- 100

clusterExport(cl,c("single.sim","SBR.simulation","generate.tasks","dists","servers", "ofr", "ntrial"))

system.time(
  val <-  mapply(function(date, hour) parLapply(cl, 1:ntrial, function(x) single.sim(date=date, hour=hour, warmup=0.4)), date=dates[1:30], hour=hours[1:30])
)
stopCluster(cl)

sim.points <- sapply(val, function(x) x$summary$wait.total[[1]])
sim.points.num <- rep(1:30, times=rep(100,30))
obs.w <- cbind(aggregate(realdata$wait_mean*realdata$num_calls, by=list(realdata$date, realdata$hour), sum)[3]
               ,aggregate(realdata$num_calls, by=list(realdata$date, realdata$hour), sum)[3]
)
obs.w <- obs.w[1:30,1]/obs.w[1:30,2]
obs.w <- rep(obs.w, times=rep(100,30))

plot(sim.points.num, sim.points, pch=20)
lines(sim.points.num, obs.w, col='red')





