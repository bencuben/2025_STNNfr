
source("Functions_hpc.R")

scenario_i = commandArgs(TRUE)
scenario_i = as.numeric(scenario_i)

print(paste0("Scenario Knn = ",scenario_i))
load(file="5_27_2025_Simulation Grid.Rdata")


distri=simulation_grid[scenario_i,1]
n=simulation_grid[scenario_i,2]
m = simulation_grid[scenario_i,3]
p=simulation_grid[scenario_i,4]
k= simulation_grid[scenario_i,5]
g= simulation_grid[scenario_i,6]
name = paste0(getwd(),"/data/data",distri,"_",n,"_",m,"_",p,".RData")
load(file = name)


# Simulation --------------------------------------------------------------
#install.packages("pryr")


mem_knn = mem_change({ 
  time_knn = system.time({
    result_knn =knn.st(query_set = data$query_set,training_set = data$training_set,g=g,kmax=k)
    } ) 
  })




save(result_knn,file= paste0(getwd(),"/results/result_knn_",scenario_i,".RData") )
save(time_knn,file= paste0(getwd(),"/results/time_knn_",scenario_i,".RData") )
save(mem_knn,file= paste0(getwd(),"/results/mem_knn_",scenario_i,".RData") )
