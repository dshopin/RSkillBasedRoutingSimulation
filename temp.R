setwd('~\\Optimization')
source('C:\\Users\\e6on6gv\\Documents\\GitHub\\RSkillBasedRoutingSimulation\\SBR.simulation.R')
source('C:\\Users\\e6on6gv\\Documents\\GitHub\\RSkillBasedRoutingSimulation\\generate.tasks.R')

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
warmup <- 0.4



set.seed(1)
tasks <- generate.tasks(skill.names=dists[dists$hour==hour, 'zone1']
                        ,iat.shapes=dists[dists$hour==hour, 'iat_Shape']
                        ,iat.scales=dists[dists$hour==hour, 'iat_Scale']
                        ,serv.shapes=dists[dists$hour==hour, 'talk_Shape']
                        ,serv.scales=dists[dists$hour==hour, 'talk_Scale']
                        ,warmup=0.4
                        ,duration=3600)


servers <- servers[servers$hour==hour & servers$date==date,]
overflow <- ofr

# qmodel <- SBR.simulation(tasks=tasks, servers=servers[servers$hour==hour & servers$date==date,], overflow=ofr, warmup=0.4, plots=FALSE)

  
  tasks$dequeue.time=NA
  tasks$dequeue.skill=''
  tasks$release.time=NA
  tasks$waiting.time=NA
  #add iat within skill
  tasks <- tasks[order(tasks$skill, tasks$arrival.time),]
  tasks$iat.by.skill <- c(tasks[1,'arrival.time'],sapply(2:nrow(tasks), function(x) {if(tasks[x-1,'skill']==tasks[x,'skill']){tasks[x,'arrival.time']-tasks[x-1,'arrival.time']} else tasks[x,'arrival.time']}))
  #add total iat
  tasks <- tasks[order(tasks$arrival.time),]
  tasks$iat.total <- c(tasks[1,'arrival.time'],sapply(2:nrow(tasks), function(x) tasks[x,'arrival.time']-tasks[x-1,'arrival.time']))
  
  
  
  servers <- cbind(servers,data.frame(idle.time=rep(0,nrow(servers)), release.time=rep(NA,nrow(servers)), task_id=rep(NA,nrow(servers))))
  
  queue <- data.frame(task_id=as.numeric(NULL), enqueue.time=as.numeric(NULL), service.time=as.numeric(NULL), overflow.time=as.numeric(NULL), skill=as.character(NULL), stringsAsFactors = FALSE)
  
  
  #data frames for simulation statistics
  serverstat <- data.frame(server_id=as.character(NULL), event=as.character(NULL), clock=as.numeric(NULL), task_id=as.numeric(NULL), stringsAsFactors = FALSE)
  queuestat <- data.frame(task_id=as.numeric(NULL), event=as.character(NULL), clock=as.numeric(NULL), skill=as.character(NULL), stringsAsFactors = FALSE)
  
  start.clock <- min(tasks$arrival.time)
  
  next.event <- start.clock
  clock <- start.clock
  
  
  for(row in 1:nrow(tasks)){
    
    while(clock < tasks[row,'arrival.time']){
      
      
      #######################
      #  Releasing servers  #
      #######################
      for (s in servers[servers$release.time==clock & !is.na(servers$task_id),'server_id']){
        
        serverstat <- rbind(serverstat,data.frame(server_id=s, event='release', clock=clock, task_id=servers[servers$server_id==s,'task_id'], stringsAsFactors = FALSE))
        
        servers[servers$server_id==s,'task_id'] <- NA
        servers[servers$server_id==s,'release.time'] <- NA
        servers[servers$server_id==s,'idle.time'] <- 0
      }
      
      
      ###################################
      #Overflow those reached thresholds#
      ###################################
      for (t in queue[queue$overflow.time==clock & !is.na(queue$overflow.time),'task_id']){
        
        overflow.skill <- overflow[overflow$skill==queue[queue$task_id==t,'skill'], 'overflow.skill']
        overflow.threshold <- overflow[overflow$skill==overflow.skill, 'threshold']
        
        queue[queue$task_id==t,'skill'] <- overflow.skill
        if(length(overflow.threshold)>0) queue[queue$task_id==t,'overflow.time'] <- clock + overflow.threshold
        else queue[queue$task_id==t,'overflow.time'] <- NA
        
        queuestat <- rbind(queuestat,data.frame(task_id=t, event='overflow', clock=clock, skill=overflow.skill, stringsAsFactors = FALSE))
      }
      
      ######################################
      #Transfer calls from queue to servers#
      ######################################
      servers <- servers[order(-servers$idle.time),] #order by descending idle time
      
      for(t in queue$task_id){
        
        
        available.server <- servers[is.na(servers$task_id) & servers[,queue[queue$task_id==t,'skill']]==1, 'server_id'][1]
        if (!is.na(available.server)){
          
          servers[servers$server_id==available.server,'task_id'] <- queue[queue$task_id==t,'task_id']
          servers[servers$server_id==available.server,'release.time'] <- clock + queue[queue$task_id==t,'service.time']
          servers[servers$server_id==available.server,'idle.time'] <- NA
          
          serverstat <- rbind(serverstat,data.frame(server_id=available.server, event='engage', clock=clock, task_id=t, stringsAsFactors = FALSE))
          queuestat <- rbind(queuestat,data.frame(task_id=t, event='dequeue', clock=clock, skill=queue[queue$task_id==t,'skill'], stringsAsFactors = FALSE))
          
          tasks[tasks$task_id==t,'dequeue.time'] <- clock
          tasks[tasks$task_id==t,'waiting.time'] <- clock - tasks[tasks$task_id==t,'arrival.time']
          tasks[tasks$task_id==t,'dequeue.skill'] <- queue[queue$task_id==t,'skill']
          tasks[tasks$task_id==t,'release.time'] <- clock + queue[queue$task_id==t,'service.time']
          
          queue <- queue[!(queue$task_id==t),]
        }
        
        
      }
      
      #####################
      #Find the next event#
      #####################
      
      next.event <- min(tasks[row,'arrival.time'], queue$overflow.time, servers$release.time, na.rm=TRUE)
      servers$idle.time <- servers$idle.time + (next.event - clock)
      clock <- next.event
      
    }
    
    
    #####################
    #  New call arrives #
    #####################
    overflow.threshold <- overflow[overflow$skill==tasks[row,'skill'], 'threshold']
    if(length(overflow.threshold)>0) overflow.time <- clock + overflow.threshold
    else overflow.time <- NA
    queue <- rbind(queue,data.frame(task_id=tasks[row,'task_id'], enqueue.time=clock, service.time=tasks[row,'service.time'], overflow.time=overflow.time, skill=tasks[row,'skill'], stringsAsFactors = FALSE))
    
    queuestat <- rbind(queuestat,data.frame(task_id=tasks[row,'task_id'], event='enqueue', clock=clock, skill=tasks[row,'skill'], stringsAsFactors = FALSE))
    
  }
  
  
  
  
  ###########################
  # Post-process queue stats#
  ###########################
  
  skill.num <- length(unique(queuestat$skill))
 
  queuestat <- rbind(data.frame(task_id=rep(0,skill.num), event=rep('start',skill.num), clock=rep(start.clock,skill.num), skill=unique(queuestat$skill)), queuestat)
  queuestat$change <- (queuestat$event %in% c('enqueue','overflow')) - (queuestat$event=='dequeue')
  queuestat <- aggregate(queuestat$change, by=list(skill=queuestat$skill, clock=queuestat$clock), FUN=sum)
  queuestat <- queuestat[queuestat$x != 0 | queuestat$clock == start.clock,]
  
  #L by skills
  queuestat <- queuestat[order(queuestat$skill, queuestat$clock),]
  queuestat$L.by.skill <- ave(queuestat$x,by=queuestat$skill, FUN=cumsum)
  queuestat$duration.by.skill <- c(sapply(1:(nrow(queuestat)-1)
                                          , function(x) {if(queuestat[x+1,'skill']==queuestat[x,'skill']){queuestat[x+1,'clock']-queuestat[x,'clock']} else max(tasks$arrival.time)-queuestat[x,'clock']}), max(tasks$arrival.time-queuestat[nrow(queuestat)-1,'clock']))
  
  
  ####create state of skill queues at clock=0 and delete all warmup history
  queuestat <- queuestat[order(queuestat$clock),]
  queuestat2 <- queuestat
  #get state of each queue before 0
  zero.states <- queuestat[sapply(unique(queuestat$skill), function(x) max(which(queuestat[queuestat$clock <= 0,'skill']==x))),]
  zero.states$duration.by.skill <- zero.states$duration.by.skill + zero.states$clock
  zero.states$clock <- 0
  zero.states$x <- 0
  queuestat <- rbind(zero.states,queuestat[queuestat$clock > 0,])
  
  #L total
  queuestat$L.total <- cumsum(queuestat$x) + sum(queuestat[queuestat$clock == 0, 'L.by.skill'])
  queuestat$duration.total <- c(sapply(1:(nrow(queuestat)-1), function(x) queuestat[x+1,'clock']-queuestat[x,'clock']),max(tasks$arrival.time)-queuestat[nrow(queuestat)-1,'clock'])
  
  queuestat$x <- NULL
  
  
  
 
  
  
  