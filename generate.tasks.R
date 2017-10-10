
#  This function generates list of tasks for different skills with different distribution parameters.
#  Interrarival distributed as Gamma, service time - as Weibull
#  Number of tasks defined by desired duration of simulation AND warmup period
#  All warmup tasks have negative clock time, experiment tasks start after 0

generate.tasks <- function(skill.names, iat.shapes, iat.scales, serv.shapes, serv.scales, duration, warmup=0){
  
  library(data.table)
  warmup.start <- duration * warmup / (warmup - 1)
  
  #this function to use in MAPPLY below; generates required amount of tasks for 1 skill
  one.skill.tasks <- function(skill.names, iat.shapes, iat.scales, serv.shapes, serv.scales, duration){
    iat <- c()
    service.time <- c()
    
    while(sum(iat) <= duration - warmup.start){
      iat <- c(iat, rgamma(1,shape=iat.shapes, scale=iat.scales))
      service.time <- c(service.time, rweibull(1,shape=serv.shapes, scale=serv.scales) + 90) #work time=90
    }
    
    arrival.time <- cumsum(iat) + warmup.start
    skill <- rep(skill.names,length(iat))
    return(data.frame(arrival.time, iat, service.time, skill,stringsAsFactors = FALSE))
  }
    
  #generate and combine tasks for all skills
  all.tasks <- mapply(FUN=one.skill.tasks
                      ,skill.names=skill.names
                      ,iat.shapes=iat.shapes
                      ,iat.scales=iat.scales
                      ,serv.shapes=serv.shapes
                      ,serv.scales=serv.scales
                      ,duration=duration
                      ,SIMPLIFY=FALSE)
  all.tasks <- as.data.frame(rbindlist(all.tasks))
  all.tasks <- all.tasks[order(all.tasks$arrival.time),]
  all.tasks <- all.tasks[all.tasks$arrival.time <= duration,]
  all.tasks$task_id <- 1:nrow(all.tasks)
  return(all.tasks)
  
}
# generate.tasks(skill.names=c('A','B'), iat.shapes=c(0.88,0.84), iat.scales=c(833,895), serv.shapes=c(0.9,0.8), serv.scales=c(687,682), duration=3600, warmup=0.3)

