This is an R program for simulation of a queuing system with SBR (skill-based routing):
-every incoming task belong to one of N possible classes (C1, C2, ... Cn)
-every server has 1 or more skills allowing to take tasks of 1 or more classes

Also the concept of overflow included, when after a specific threshold waiting time the task switches to another class (queue) in hope that there is a server available
with corresponding skill