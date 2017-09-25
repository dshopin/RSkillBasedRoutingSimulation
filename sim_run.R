
rm(list=ls())
setwd('~\\GitHub\\RSkillBasedRoutingSimulation')
source('SBR.simulation.R')



ntask<-1000

skills <- c('ZoneA','ZoneB','ZoneD','ZoneE','ZoneG','ZoneGov','ZoneI','ZoneM','ZoneQ','ZoneR','ZoneS','ZoneV')

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
                    ,stringsAsFactors = FALSE)

for(i in 1:length(skills)){
  
  #tasks for i-th skill
  skill.tasks <- data.frame(iat.by.skill=rgamma(ntask,shape=iat.shape[i], scale=iat.scale[i])
                            ,service.time=rgamma(ntask,shape=serv.shape[i], scale=serv.scale[i])
                            ,skill=skills[i]
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


qmodel <- SBR.simulation(tasks=tasks, servers=servers, overflow=ofr)




tasks <- qmodel$tasks

queuestat <- qmodel$queuestat
serverstat <- qmodel$serverstat
