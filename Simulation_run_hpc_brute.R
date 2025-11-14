
source("Functions_hpc.R")

scenario_i = commandArgs(TRUE)
scenario_i = as.numeric(scenario_i)

print(paste0("Scenario Brute = ",scenario_i))
load(file="5_27_2025_Simulation Grid2.Rdata")


distri=simulation_grid2[scenario_i,1]
n=simulation_grid2[scenario_i,2]
m = simulation_grid2[scenario_i,3]
p=simulation_grid2[scenario_i,4]
k= simulation_grid2[scenario_i,5]
name = paste0(getwd(),"/data/data",distri,"_",n,"_",m,"_",p,".RData")
load(file = name)


# Simulation --------------------------------------------------------------
mem_brute = mem_change({ 
  time_brute = system.time({
    result_brute= archaic_knn(query = data$query_set,data = data$training_set,k = k) 
    })
})



save(result_brute,file= paste0(getwd(),"/results/result_brute_",scenario_i,".RData") )
save(time_brute,file= paste0(getwd(),"/results/time_brute_",scenario_i,".RData") )
save(mem_brute,file= paste0(getwd(),"/results/mem_brute_",scenario_i,".RData") )