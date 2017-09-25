###########################################################################
#              Function to simulate Skill-based routing queueing system   #
###########################################################################
#
# tasks - data frame with the following mandatory variables:
#         'task_id' (numeric)
#         'arrival.time' (numeric)
#         'service.time' (numeric)
#         'skill' (character)

# servers - data frame with mandatory variable 'server_id'. All remaining variables are interpreted as different skills and 
#           must have values 0 or 1 (if a server has or has not the skill)

# overflow - data frame with information on overflow routing with mandatory variables:
#     'skill' (character)
#     'overflow.skill' (character)
#     'threshold' (numeric)

#
# The function produces a named list containing the following data frames:
#   tasks - all tasksk with initial info + release time, dequeue time, dequeue skill.tasks
#   serverstat - all events of engaging/releasing servers
#   queuestat  - all events of enqueueing/dequeueing/overflowing
#   


SBR.simulation <- function(tasks, servers, overflow){
  
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
  
  
  next.event <- 0
  clock <- 0
  
  
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
    queue <- rbind(queue,data.frame(task_id=tasks[row,'task_id'], enqueue.time=clock, service.time=tasks[row,'service.time'], overflow.time=clock+overflow.threshold, skill=tasks[row,'skill'], stringsAsFactors = FALSE))
    
    queuestat <- rbind(queuestat,data.frame(task_id=tasks[row,'task_id'], event='enqueue', clock=clock, skill=tasks[row,'skill'], stringsAsFactors = FALSE))
    
    
  }
  
  
  ###########################
  # Post-process queue stats#
  ###########################

  skill.num <- length(unique(queuestat$skill))
  queuestat <- rbind(data.frame(task_id=rep(0,skill.num), event=rep('start',skill.num), clock=rep(0,skill.num), skill=unique(queuestat$skill)), queuestat)
  queuestat$change <- (queuestat$event %in% c('enqueue','overflow')) - (queuestat$event=='dequeue')
  queuestat <- aggregate(queuestat$change, by=list(skill=queuestat$skill, clock=queuestat$clock), FUN=sum)
  queuestat <- queuestat[queuestat$x != 0 | queuestat$clock==0,]
  
  #L by skills
  queuestat <- queuestat[order(queuestat$skill, queuestat$clock),]
  queuestat$L.by.skill <- ave(queuestat$x,by=queuestat$skill, FUN=cumsum)
  queuestat$duration.by.skill <- c(sapply(1:(nrow(queuestat)-1)
                                          , function(x) {if(queuestat[x+1,'skill']==queuestat[x,'skill']){queuestat[x+1,'clock']-queuestat[x,'clock']} else max(tasks$arrival.time)}), max(tasks$arrival.time))
  
  #L total
  queuestat <- queuestat[order(queuestat$clock),]
  queuestat$L.total <- cumsum(queuestat$x)
  queuestat$duration.total <- c(sapply(1:(nrow(queuestat)-1), function(x) queuestat[x+1,'clock']-queuestat[x,'clock']),max(tasks$arrival.time))
  
  queuestat$x <- NULL
  
  
  
  
  ###########################
  #    Summary              #
  ###########################
  
  mysummary <- function(var){
  by.skill <- aggregate(var, by=list(skill=tasks$skill)
                        ,FUN=function(x) c(mean=mean(x, na.rm = TRUE),quantile(x,prob=c(0.8,0.9,0.95,0.99), na.rm = TRUE))
                  )
  total <-  c(mean=mean(var, na.rm = TRUE), quantile(var,prob=c(0.8,0.9,0.95,0.99), na.rm = TRUE))
  
  return(list(by.skill, total))
  }
  
  iat.by.skill <- mysummary(tasks$iat.by.skill)[1]
  iat.total  <- mysummary(tasks$iat.by.skill)[2]
  service.by.skill <- mysummary(tasks$service.time)[1]
  service.total  <- mysummary(tasks$service.time)[2]
  wait.by.skill <- mysummary(tasks$waiting.time)[1]
  wait.total  <- mysummary(tasks$waiting.time)[2]
  
  
  L.mean.by.skill <- aggregate(queuestat$L.by.skill*queuestat$duration.by.skill, by=list(skill=queuestat$skill)
                              ,FUN=sum)

  L.mean.by.skill$x <- L.mean.by.skill$x / aggregate(queuestat$duration.by.skill, by=list(skill=queuestat$skill)
                                                     ,FUN=sum)$x
  
  L.mean.total <- sum(queuestat$L.total*queuestat$duration.total)/sum(queuestat$duration.total)

  

  

  
  ###########################
  # Plots                   #
  ###########################
  library(ggplot2)
  
  L.by.skill<-ggplot(queuestat, aes(x = clock, y = L.by.skill, color = skill, shape=skill)) +
    scale_shape_manual(values=1:nlevels(queuestat$skill)) +
    geom_step()+
    geom_point(size=2)+
    ylab("Queue (L)")+
    xlab("Time")
  
  L.total <-ggplot(queuestat, aes(x = clock, y = L.total)) +
    geom_step()+
    ylab("Queue (L)")+
    xlab("Time")
  
  Wq.total <- ggplot(tasks[!is.na(tasks$waiting.time),], aes(x = waiting.time)) +
           geom_histogram(binwidth=1)+
           xlab("Waiting time (Wq)")
         
  Wq.by.skill <- ggplot(tasks[!is.na(tasks$waiting.time),], aes(x = waiting.time, color=skill, fill=skill)) +
           geom_histogram(binwidth=1)+
           facet_grid(skill~.)
         xlab("Waiting time (Wq)")
  
  
  model <- list(tasks=tasks, queuestat=queuestat, serverstat=serverstat
                ,plots=list(L.by.skill=L.by.skill, L.total=L.total, Wq.total=Wq.total, Wq.by.skill=Wq.by.skill)
                ,summary=list( iat.by.skill=iat.by.skill
                              ,iat.total=iat.total
                              ,service.by.skill=service.by.skill
                              ,service.total=service.total
                              ,wait.by.skill=wait.by.skill
                              ,wait.total=wait.total
                              ,L.mean.by.skill=L.mean.by.skill
                              ,L.mean.total=L.mean.total
                ))
  return(model)
  
}









