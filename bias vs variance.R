##Here we'll check the relation between:

# long simulations (provide baseline waiting time with little variance)
# multiple short simulation without warmup (large variance, should be biased against 1)
# multiple short simulation with warmup (large variance, should not be biased against 1)
library(tictoc)
library(parallel)

rm(list=ls())
setwd('C:\\Users\\e6on6gv\\Documents\\GitHub\\RSkillBasedRoutingSimulation')
source('SBR.simulation.R')
source('generate.tasks.R')


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

##specific hour and date
dists <- dists[dists$hour==11,]
servers <- servers[servers$date==as.Date('2017-09-01') & servers$hour==11,]



#1. Long simulations

#1a. How long is long enough?
get.wait.time <- function(duration){
  tasks <- generate.tasks(skill.names=dists$zone1
                          ,iat.shapes=dists$iat_Shape
                          ,iat.scales=dists$iat_Scale
                          ,serv.shapes=dists$talk_Shape
                          ,serv.scales=dists$talk_Scale
                          ,warmup=0
                          ,duration=duration)
  qmodel <- SBR.simulation(tasks=tasks, servers=servers, overflow=ofr, warmup=0)
  return(qmodel$summary$wait.total[[1]])
}

cores <- detectCores()-1


##Comparing different loop organization, parallel or not
# durations <- 3600*c(1, 3, 5)
# ntrial <- 15
# 
# 
# cl <- makeCluster(cores)
# clusterExport(cl,c("durations", "get.wait.time","SBR.simulation","generate.tasks","dists","servers", "ofr", "ntrial"))
# tic("sapply(clusterMap)")
# long1 <- sapply(1:ntrial, function(x) clusterMap(cl, get.wait.time, durations))
# toc()
# stopCluster(cl)
# 
# cl <- makeCluster(cores)
# clusterExport(cl,c("durations", "get.wait.time","SBR.simulation","generate.tasks","dists","servers", "ofr", "ntrial"))
# tic("parSapply(mapply)")
# long2 <- parSapply(cl, 1:ntrial, function(x) mapply(get.wait.time, durations))
# toc()
# stopCluster(cl)
# 
# cl <- makeCluster(cores)
# clusterExport(cl,c("durations", "get.wait.time","SBR.simulation","generate.tasks","dists","servers", "ofr", "ntrial"))
# tic("mapply(parSapply)")
# long3 <- mapply(function(duration) parSapply(cl, 1:ntrial, function(x) get.wait.time(duration)), durations)
# toc()
# stopCluster(cl)
# 
# cl <- makeCluster(cores)
# clusterExport(cl,c("durations", "get.wait.time","SBR.simulation","generate.tasks","dists","servers", "ofr", "ntrial"))
# tic("clusterMap(sapply)")
# long4 <- clusterMap(cl, function(duration) sapply(1:ntrial, function(x) get.wait.time(duration)), durations)
# toc()
# stopCluster(cl)

## Best when sapply is parallelized (more trials than durations),  mapply(parSapply) is better than parSapply(mapply) because result matrix already transponsed

cl <- makeCluster(cores)
durations <- 3600*c(1, 100, 1000, 10000)
ntrial <- 100
clusterExport(cl,c("durations", "get.wait.time","SBR.simulation","generate.tasks","dists","servers", "ofr", "ntrial"))

system.time(
  long <-  mapply(function(duration) parSapply(cl, 1:ntrial, function(x) get.wait.time(duration)), durations)
)
stopCluster(cl)


long <- data.frame(durations=rep(durations, times=rep(ntrial,length(durations))),wait=unlist(long))

plot(jitter(log(long$durations)), long$wait, pch=20)


# 2. Multiple short simulations with different warmup periods
get.wait.time.warm <- function(warmup){
  tasks <- generate.tasks(skill.names=dists$zone1
                          ,iat.shapes=dists$iat_Shape
                          ,iat.scales=dists$iat_Scale
                          ,serv.shapes=dists$talk_Shape
                          ,serv.scales=dists$talk_Scale
                          ,warmup=warmup
                          ,duration=3600)
  qmodel <- SBR.simulation(tasks=tasks, servers=servers, overflow=ofr, warmup=warmup)
  return(qmodel$summary$wait.total[[1]])
}
cl <- makeCluster(cores)
warmups <- seq(0,0.9,0.1)
ntrial <- 100
clusterExport(cl,c("warmups", "get.wait.time.warm","SBR.simulation","generate.tasks","dists","servers", "ofr", "ntrial"))

system.time(
  short <-  mapply(function(warmup) parSapply(cl, 1:ntrial, function(x) get.wait.time.warm(warmup)), warmups)
)
stopCluster(cl)

short <- data.frame(warmups=rep(warmups, times=rep(ntrial,length(warmups))),wait=as.vector(short))

plot(jitter(short$warmups), short$wait, pch=20)
lines(unique(short$warmups), tapply(short$wait, short$warmups, mean), col='red')



##CLT for one point
cl <- makeCluster(cores)
warmups <- c(0.4)
ntrial <- 1000
clusterExport(cl,c("warmups", "get.wait.time.warm","SBR.simulation","generate.tasks","dists","servers", "ofr", "ntrial"))

system.time(
  cltcheck <-  mapply(function(warmup) parSapply(cl, 1:ntrial, function(x) get.wait.time.warm(warmup)), warmups)
)
stopCluster(cl)

hist(cltcheck - mean(cltcheck), breaks=50)




##0.4 warmup looks enough.





























