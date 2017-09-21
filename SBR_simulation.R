
setwd('~\\GitHub\\RSkillBasedRoutingSimulation')


ntask <- 1000
main.skills <- c('ZoneA','ZoneB','ZoneD','ZoneE','ZoneG','ZoneGov','ZoneI','ZoneM','ZoneQ','ZoneR','ZoneS','ZoneV')
overflow.skills <- c('ZoneAOverflow','ZoneBOverflow','ZoneDOverflow','ZoneEOverflow','ZoneGOverflow','ZoneGovOverflow','ZoneIOverflow','ZoneMOverflow','ZoneQOverflow','ZoneROverflow','ZoneSOverflow','ZoneVOverflow','DeptSouthOverflow','DeptNorthOverflow')
#overflow routing data frame
ofr <- data.frame(from=c('ZoneA','ZoneB','ZoneD','ZoneE','ZoneG','ZoneGov','ZoneI','ZoneM','ZoneQ','ZoneR','ZoneS','ZoneV','ZoneAOverflow','ZoneBOverflow','ZoneDOverflow','ZoneEOverflow','ZoneGOverflow','ZoneROverflow','ZoneVOverflow')
                  ,to=c('ZoneAOverflow','ZoneBOverflow','ZoneDOverflow','ZoneEOverflow','ZoneGOverflow','ZoneGovOverflow','ZoneIOverflow','ZoneMOverflow','ZoneQOverflow','ZoneROverflow','ZoneSOverflow','ZoneVOverflow','DeptSouthOverflow','DeptSouthOverflow','DeptSouthOverflow','DeptNorthOverflow','DeptNorthOverflow','DeptNorthOverflow','DeptNorthOverflow')
                  ,threshold=c(40,40,40,40,40,40,40,40,40,40,40,40,20,20,20,20,20,20,20)
                  )

iat.shape <- c(0.963187,0.930654,0.914509,0.92554,0.955998,1.289547,0.855745,1.011346,1.25718,0.926373,1.063419,1.404157)
iat.scale <- c(343.5583,349.3116,377.0777,348.1639,309.2764,1038.923,1186.025,944.5012,1163.363,359.8592,1280.771,1021.215)
serv.shape <- c(0.762129,0.743385,0.706864,0.72219,0.713124,0.877112,0.728817,0.743795,0.721065,0.733499,0.658369,0.833826)
serv.scale <- c(492.0019,605.55,501.7605,416.0421,371.2114,479.6999,406.7151,226.3426,183.3927,485.3072,294.1322,332.4179)

#generate tasks
tasks <- data.frame(iat=as.numeric(NULL), servtime=as.numeric(NULL), skill=as.character(NULL))
for(i in 1:length(skills)){
  
  #tasks for i-th skill
  skill.tasks <- data.frame(iat=rgamma(ntask,shape=iat.shape[i], scale=iat.scale[i])
                            ,servtime=rgamma(ntask,shape=serv.shape[i], scale=serv.scale[i])
                            ,skill=skills[i]
                            )
  skill.tasks$arrival <- cumsum(skill.tasks$iat)
  
  #combining with all tasks
  tasks <- rbind(tasks, skill.tasks)
}
tasks <- tasks[order(tasks$arrival),]
tasks <- tasks[1:ntask,]
tasks$task_id <- 1:nrow(tasks)





  
 
#agent-skill matrix

