tic("Total")
rm(list=ls())
setwd('~\\GitHub\\RSkillBasedRoutingSimulation')


ntask <- 5000

main.skills <- c('ZoneA','ZoneB','ZoneD','ZoneE','ZoneG','ZoneGov','ZoneI','ZoneM','ZoneQ','ZoneR','ZoneS','ZoneV')
overflow.skills <- c('ZoneAOverflow','ZoneBOverflow','ZoneDOverflow','ZoneEOverflow','ZoneGOverflow','ZoneGovOverflow','ZoneIOverflow','ZoneMOverflow','ZoneQOverflow','ZoneROverflow','ZoneSOverflow','ZoneVOverflow','DeptSouthOverflow','DeptNorthOverflow')
#overflow routing data frame
ofr <- data.frame(skill=c('ZoneA','ZoneB','ZoneD','ZoneE','ZoneG','ZoneGov','ZoneI','ZoneM','ZoneQ','ZoneR','ZoneS','ZoneV','ZoneAOverflow','ZoneBOverflow','ZoneDOverflow','ZoneEOverflow','ZoneGOverflow','ZoneROverflow','ZoneVOverflow')
                  ,overflow.skill=c('ZoneAOverflow','ZoneBOverflow','ZoneDOverflow','ZoneEOverflow','ZoneGOverflow','ZoneGovOverflow','ZoneIOverflow','ZoneMOverflow','ZoneQOverflow','ZoneROverflow','ZoneSOverflow','ZoneVOverflow','DeptSouthOverflow','DeptSouthOverflow','DeptSouthOverflow','DeptNorthOverflow','DeptNorthOverflow','DeptNorthOverflow','DeptNorthOverflow')
                  ,threshold=c(40,40,40,40,40,40,40,40,40,40,40,40,20,20,20,20,20,20,20)
                  ,stringsAsFactors = FALSE
                  )

iat.shape <- c(0.963187,0.930654,0.914509,0.92554,0.955998,1.289547,0.855745,1.011346,1.25718,0.926373,1.063419,1.404157)
iat.scale <- c(343.5583,349.3116,377.0777,348.1639,309.2764,1038.923,1186.025,944.5012,1163.363,359.8592,1280.771,1021.215)
serv.shape <- c(0.762129,0.743385,0.706864,0.72219,0.713124,0.877112,0.728817,0.743795,0.721065,0.733499,0.658369,0.833826)
serv.scale <- c(492.0019,605.55,501.7605,416.0421,371.2114,479.6999,406.7151,226.3426,183.3927,485.3072,294.1322,332.4179)


#generate tasks
set.seed(1)
tasks <- data.frame(arrival.time=as.numeric(NULL)
                    ,iat=as.numeric(NULL)
                    ,service.time=as.numeric(NULL)
                    ,skill=as.character(NULL)
                    ,dequeue.time=as.numeric(NULL)
                    ,dequeue.skill=as.character(NULL)
                    ,release.time=as.numeric(NULL)
                    ,stringsAsFactors = FALSE)

for(i in 1:length(main.skills)){
  
  #tasks for i-th skill
  skill.tasks <- data.frame(iat=rgamma(ntask,shape=iat.shape[i], scale=iat.scale[i])
                            ,service.time=rgamma(ntask,shape=serv.shape[i], scale=serv.scale[i])
                            ,skill=main.skills[i]
                            ,dequeue.time=NA
                            ,dequeue.skill=''
                            ,release.time=NA
                            ,stringsAsFactors = FALSE
                            )
  skill.tasks$arrival.time <- cumsum(skill.tasks$iat)
  
  #combining with all tasks
  tasks <- rbind(tasks, skill.tasks)
}
tasks <- tasks[order(tasks$arrival.time),]
tasks <- tasks[1:ntask,]
tasks$task_id <- 1:nrow(tasks)



servers <- read.csv(file='asm.csv', fileEncoding = 'UTF-8-BOM')
servers <- cbind(servers,data.frame(idle.time=rep(0,nrow(servers)), release.time=rep(NA,nrow(servers)), task_id=rep(NA,nrow(servers))))

queue <- data.frame(task_id=as.numeric(NULL), enqueue.time=as.numeric(NULL), service.time=as.numeric(NULL), overflow.time=as.numeric(NULL), skill=as.character(NULL), stringsAsFactors = FALSE)
# queue <- data.frame(task_id=1, enqueue.time=1, service.time=1, overflow.time=1, skill='ZoneA', stringsAsFactors = FALSE)



#data frames for simulation statistics
serverstat <- data.frame(server_id=as.character(NULL), event=as.character(NULL), clock=as.numeric(NULL), task_id=as.numeric(NULL), stringsAsFactors = FALSE)
queuestat <- data.frame(task_id=as.numeric(NULL), event=as.character(NULL), clock=as.numeric(NULL), skill=as.character(NULL), stringsAsFactors = FALSE)


next.event <- 0
clock <- 0


timer <- data.frame(task=as.numeric(NULL)
                    ,releases=as.numeric(NULL)
                    ,release.time=as.numeric(NULL)
                    ,overflows=as.numeric(NULL)
                    ,overflow.time=as.numeric(NULL)
                    ,transfers=as.numeric(NULL)
                    ,transfer.time=as.numeric(NULL)
                    ,startqlen=as.numeric(NULL)
                    ,total=as.numeric(NULL))

for(row in 1:nrow(tasks)){
  
  releases <- 0
  release.time <- 0
  overflows <- 0
  overflow.time <- 0
  transfers <- 0
  transfer.time <- 0
  startqlen <- nrow(queue)
  
  t01 <- Sys.time()
  
  
  while(clock < tasks[row,'arrival.time']){
    
    
    #######################
    #  Releasing servers  #
    #######################
    t1 <- Sys.time()
    for (s in servers[servers$release.time==clock & !is.na(servers$task_id),'server_id']){
      releases <- releases + 1
      
      serverstat <- rbind(serverstat,data.frame(server_id=s, event='release', clock=clock, task_id=servers[servers$server_id==s,'task_id'], stringsAsFactors = FALSE))
      
      servers[servers$server_id==s,'task_id'] <- NA
      servers[servers$server_id==s,'release.time'] <- NA
      servers[servers$server_id==s,'idle.time'] <- 0
    }
    t2 <- Sys.time()
    release.time <- release.time + (t2-t1)
    
    ###################################
    #Overflow those reached thresholds#
    ###################################
    t1 <- Sys.time()
    for (t in queue[queue$overflow.time==clock & !is.na(queue$overflow.time),'task_id']){
      overflows <- overflows + 1
      
      overflow.skill <- ofr[ofr$skill==queue[queue$task_id==t,'skill'], 'overflow.skill']
      overflow.threshold <- ofr[ofr$skill==overflow.skill, 'threshold']
        
      queue[queue$task_id==t,'skill'] <- overflow.skill
      if(length(overflow.threshold)>0) queue[queue$task_id==t,'overflow.time'] <- clock + overflow.threshold
      else queue[queue$task_id==t,'overflow.time'] <- NA
        
      queuestat <- rbind(queuestat,data.frame(task_id=t, event='overflow', clock=clock, skill=overflow.skill, stringsAsFactors = FALSE))
    }
    t2 <- Sys.time()
    overflow.time <- overflow.time + (t2-t1)
    
    ######################################
    #Transfer calls from queue to servers#
    ######################################
    servers <- servers[order(-servers$idle.time),] #order by descending idle time
    
    t1 <- Sys.time()
    for(t in queue$task_id){
      
      
      available.server <- servers[is.na(servers$task_id) & servers[,queue[queue$task_id==t,'skill']]==1, 'server_id'][1]
      if (!is.na(available.server)){
        transfers <- transfers + 1
        
        servers[servers$server_id==available.server,'task_id'] <- queue[queue$task_id==t,'task_id']
        servers[servers$server_id==available.server,'release.time'] <- clock + queue[queue$task_id==t,'service.time']
        servers[servers$server_id==available.server,'idle.time'] <- NA
        
        serverstat <- rbind(serverstat,data.frame(server_id=available.server, event='engage', clock=clock, task_id=t, stringsAsFactors = FALSE))
        queuestat <- rbind(queuestat,data.frame(task_id=t, event='dequeue', clock=clock, skill=queue[queue$task_id==t,'skill'], stringsAsFactors = FALSE))
        
        tasks[tasks$task_id==t,'dequeue.time'] <- clock
        tasks[tasks$task_id==t,'dequeue.skill'] <- queue[queue$task_id==t,'skill']
        tasks[tasks$task_id==t,'release.time'] <- clock + queue[queue$task_id==t,'service.time']
        
        queue <- queue[!(queue$task_id==t),]
      }
     
      
    }
    
    t2 <- Sys.time()
    transfer.time <- transfer.time + (t2-t1)
    
    
    #####################
    #Find the next event#
    #####################
    
    next.event <- min(tasks[row,'arrival.time'], queue$overflow.time, servers$release.time, na.rm=TRUE)
    servers$idle.time <- servers$idle.time + (next.event - clock)
    clock <- next.event
    
  }
  
  t02 <- Sys.time()
  total <- t02-t01
  
  timer <- rbind(timer, data.frame(task=row
                                   ,releases=releases
                                   ,release.time=release.time
                                   ,overflows=overflows
                                   ,overflow.time=overflow.time
                                   ,transfers=transfers
                                   ,transfer.time=transfer.time
                                   ,startqlen=startqlen
                                   ,total=total))
 
  #####################
  #  New call arrives #
  #####################
  overflow.threshold <- ofr[ofr$skill==tasks[row,'skill'], 'threshold']
  queue <- rbind(queue,data.frame(task_id=tasks[row,'task_id'], enqueue.time=clock, service.time=tasks[row,'service.time'], overflow.time=clock+overflow.threshold, skill=tasks[row,'skill'], stringsAsFactors = FALSE))
  
  queuestat <- rbind(queuestat,data.frame(task_id=tasks[row,'task_id'], event='enqueue', clock=clock, skill=tasks[row,'skill'], stringsAsFactors = FALSE))
  
  
}

sum(timer$total)







