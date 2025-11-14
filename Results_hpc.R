

load("5_27_2025_Simulation Grid.Rdata")
load("5_27_2025_Simulation Grid2.Rdata")

#simulation_grid[1,]
#simulation_grid2[1,]

# Finding agreement in neighbors ------------------------------------------
reps= 25

agreement = matrix(NA,nrow= nrow(simulation_grid2)/reps, 4)
counter =1 
for(i in seq(1,nrow(simulation_grid2 ),reps)  ){
  
  
  load(paste0("results/result_brute_",i,".RData"))
  knn_brute =  t(sapply(result_brute[1:(length(result_brute)) ], function(x) x$knn))
  knn_brute =  if (nrow(knn_brute)==1) t(knn_brute) else knn_brute
  
  #j =4 for the number of k considered
  for(j in 1:4){
    print(paste0(counter,"---------------------"))
    #print(simulation_grid2[i,])
    #print(simulation_grid[(i+150*(counter-1) +50*(j-1) ),])
    #Sys.sleep(1)
    
    load(paste0("results/result_knn_",(i+(reps*3)*(counter-1) +reps*(j-1) ),".RData"))
    #load(paste0("results/time_knn_",i,".RData"))
    #load(paste0("results/time_brute_",i,".RData"))
    
    knn_st = do.call(rbind,(result_knn$knn[as.character(1:(length(result_knn$knn))) ]))
    
    
    agreement[counter,j]= mean(knn_st== knn_brute)
    
    remove(knn_st)
  }
  
  counter = counter+1
}

mean(agreement)
# 100% agreement in neighbors



# Finding the number of iteration and time in knn st ------------------------------

simulation_grid_results = as.data.frame(simulation_grid)
#simulation_grid_results$iteration_max = NA
#simulation_grid_results$iteration_mean = NA
#simulation_grid_results$iteration_median = NA

simulation_results_kprime = list()
for(i in 1:nrow(simulation_grid_results)){
  print(paste0("i = ",i,"------------"))
  load(paste0("results/result_knn_",i,".RData"))
  load(paste0("results/time_knn_",i,".RData"))
  load(paste0("results/mem_knn_",i,".RData"))
  
  simulation_grid_results$iteration_max[i] = max(unlist(result_knn$iterations) )
  simulation_grid_results$iteration_mean[i] = mean(unlist(result_knn$iterations) )
  simulation_grid_results$iteration_median[i] = median(unlist(result_knn$iterations) )
  
  simulation_results_kprime[i] = unlist(result_knn$k_prime) 
  
  simulation_grid_results$CPU_time[i] = time_knn[2] 
  simulation_grid_results$elapsed[i] = time_knn[3] 
  simulation_grid_results$memory[i] = mem_knn
  remove(result_knn,time_knn,mem_knn)
}


# Exporting the results
save(simulation_grid_results,file="simulation_grid_results_5_27_2025.RData")


# Finding the number of iteration  in knn  brute ------------------------------

simulation_grid2_results = as.data.frame(simulation_grid2)
#simulation_grid_results$iteration_max = NA
#simulation_grid_results$iteration_mean = NA
#simulation_grid_results$iteration_median = NA
for(i in 1:nrow(simulation_grid2_results)){
  print(paste0("i = ",i,"------------"))
  load(paste0("results/result_brute_",i,".RData"))
  load(paste0("results/time_brute_",i,".RData"))
  load(paste0("results/mem_brute_",i,".RData"))
  # 
  # simulation_grid2_results$iteration_max[i] = max(unlist(result_brute$iterations) )
  # simulation_grid2_results$iteration_mean[i] = mean(unlist(result_brute$iterations) )
  # simulation_grid2_results$iteration_median[i] = median(unlist(result_brute$iterations) )
  
  simulation_grid2_results$CPU_time[i] = time_brute[2] 
  simulation_grid2_results$elapsed[i] = time_brute[3] 
  simulation_grid2_results$memory[i] = mem_brute
  
  
  remove(result_brute,time_brute,mem_brute)
}


# Exporting the results
save(simulation_grid2_results,file="simulation_grid2_results_5_27_2025.RData")


#time_knn
#time_brute


# 
# 
# #load("C:\\Brahian Cano\\KNN\\23_9_Simulation Results1parallel.Rdata")
# load("C:\\Brahian Cano\\KNN\\29_9_Simulation Results1parallel.Rdata")
# # Breaking down the results into different object
# 
# knn_st = lapply(my_simulation_results_paralel, function(x) do.call(rbind,(x$knn[as.character(1:(length(x$knn))) ]))  ) 
# #save(knn_st,file="C:\\Brahian Cano\\KNN\\23_9_Simulation Results1parallel_knn.Rdata")
# save(knn_st,file="C:\\Brahian Cano\\KNN\\29_9_Simulation Results1parallel_knn.Rdata")
# remove(knn_st)
# gc()
# 
# knn_dist_st = lapply(my_simulation_results_paralel, function(x) do.call(rbind,(x$knn_dist[as.character(1:(length(x$knn_dist))) ]))  ) 
# #save(knn_dist_st,file="C:\\Brahian Cano\\KNN\\23_9_Simulation Results1parallel_knn_dist.Rdata")
# save(knn_dist_st,file="C:\\Brahian Cano\\KNN\\29_9_Simulation Results1parallel_knn_dist.Rdata")
# remove(knn_dist_st)
# 
# 
# mean_iterations_st = sapply(sapply(my_simulation_results_paralel, function(x) unlist(x$iterations) ), mean)
# #save(mean_iterations_st,file="C:\\Brahian Cano\\KNN\\23_9_Simulation Results1parallel_iterations.Rdata")
# save(mean_iterations_st,file="C:\\Brahian Cano\\KNN\\29_9_Simulation Results1parallel_iterations.Rdata")
# remove(mean_iterations_st)
# 
# 
# CPU_time_knn_st = sapply(my_simulation_results_paralel,function(x) x$sys.self)
# #save(CPU_time_knn_st,file="C:\\Brahian Cano\\KNN\\23_9_Simulation Results1parallel_cpu.Rdata")
# save(CPU_time_knn_st,file="C:\\Brahian Cano\\KNN\\29_9_Simulation Results1parallel_cpu.Rdata")
# remove(CPU_time_knn_st)
# 
# elapsed_time_knn_st = sapply(my_simulation_results_paralel,function(x) x$elapsed)
# #save(elapsed_time_knn_st,file="C:\\Brahian Cano\\KNN\\23_9_Simulation Results1parallel_elapsed.Rdata")
# save(elapsed_time_knn_st,file="C:\\Brahian Cano\\KNN\\29_9_Simulation Results1parallel_elapsed.Rdata")
# remove(elapsed_time_knn_st)
# 
# 
# 
# 
# load("C:\\Brahian Cano\\KNN\\23_9_Simulation Results2parallel.Rdata")
# 
# 
# knn_arch=lapply(my_simulation_results2_paralel,function(x) t(sapply(x[1:(length(x)-5) ], function(x) x$knn)) )
# save(knn_arch,file ="C:\\Brahian Cano\\KNN\\23_9_Simulation Results2parallel_knn.Rdata")
# remove(knn_arch)
# 
# knn_dist_arch=lapply(my_simulation_results2_paralel,function(x) t(sapply(x[1:(length(x)-5) ], function(x) x$knn_dist)) )
# save(knn_dist_arch,file ="C:\\Brahian Cano\\KNN\\23_9_Simulation Results2parallel_knn_dist.Rdata")
# remove(knn_dist_arch)
# 
# CPU_time_knn_arch = sapply(my_simulation_results2_paralel,function(x) x$sys.self)
# save(CPU_time_knn_arch,file ="C:\\Brahian Cano\\KNN\\23_9_Simulation Results2parallel_cpu.Rdata")
# remove(CPU_time_knn_arch)
# 
# elapsed_time_knn_arch = sapply(my_simulation_results2_paralel,function(x) x$elapsed)
# save(elapsed_time_knn_arch,file ="C:\\Brahian Cano\\KNN\\23_9_Simulation Results2parallel_elapsed.Rdata")
# remove(elapsed_time_knn_arch)
# #load( "C:\\Brahian Cano\\KNN\\Simulation Results1.Rdata")
# #load( "C:\\Brahian Cano\\KNN\\Simulation Results2.Rdata")
# 
# 
# 
# 
# load("C:\\Brahian Cano\\KNN\\23_9_Simulation Grid.Rdata")
# load("C:\\Brahian Cano\\KNN\\23_9_Simulation Grid2.Rdata")


# Percentage of concordance for all scenarios -------------------------------
# i=1+ 1*50
# j=1+1*6*50
# simulation_grid2[i,]
# simulation_grid[j,]
# mean( t(sapply(my_simulation_results2_paralel[[i]][1:(length(my_simulation_results2_paralel[[i]])-5) ],function(x) x$knn))==
#         do.call(rbind,(my_simulation_results_paralel[[j]]$knn[as.character(1:(length(my_simulation_results_paralel[[j]]$knn))) ]))  )
# 


#load("C:\\Brahian Cano\\KNN\\23_9_Simulation Results1parallel_knn.Rdata")
# load("C:\\Brahian Cano\\KNN\\29_9_Simulation Results1parallel_knn.Rdata")
# load("C:\\Brahian Cano\\KNN\\23_9_Simulation Results2parallel_knn.Rdata")
# 
# knn_arch2 = sapply(knn_arch,function(x) if(nrow(x)==1) t(x) else x)
# knn_arch3 = rep(knn_arch2,each =6)
# # simulation_grid[1,]
# # simulation_grid2[1,]
# 
# agreement = c()
# for(i in 1:240000){
#   print(i)
#   agreement[i] =mean(knn_st[[i]] == knn_arch3[[i]])
# }
# table(agreement)
# 
# remove(knn_st,knn_arch,knn_arch2,knn_arch3)

# agreement = c()
# for(i in 1:nrow(simulation_grid )){
#   print(i)
#   #agreement[i] = mean(t(sapply(a2[[i]][1:(length(a2[[i]])-5) ],function(x) x$knn))==do.call(rbind,(a[[i]]$knn[as.character(1:(length(a[[i]]$knn))) ])))
#   agreement[i] = mean( t(sapply(my_simulation_results2_paralel[[i]][1:(length(my_simulation_results2_paralel[[i]])-5) ],function(x) x$knn))==
#                         do.call(rbind,(my_simulation_results_paralel[[i]]$knn[as.character(1:(length(my_simulation_results_paralel[[i]]$knn))) ]))  )
#   
# }
# mean(agreement)
# 100% agreement


# Number of iterations needed  ------------------------------

# Maximum number of iteration across observation in query set
# total_iterations = sapply(sapply(my_simulation_results_paralel, function(x) unlist(x$iterations) ), max)

#simulation_grid_withres= simulation_grid
#simulation_grid_withres$distribution = as.factor(simulation_grid_withres$distribution)

#for(i in 2:6){
#  simulation_grid_withres[,i]= as.numeric(simulation_grid_withres[,i])
#}
# 
# simulation_grid= simulation_grid[simulation_grid[,4]<=400,]
# 
# #load("C:\\Brahian Cano\\KNN\\23_9_Simulation Results1parallel_iterations.Rdata")
# load("C:\\Brahian Cano\\KNN\\29_9_Simulation Results1parallel_iterations.Rdata")
# simulation_grid = cbind(simulation_grid,iterations= mean_iterations_st)
# remove(mean_iterations_st)
# 
# #load("C:\\Brahian Cano\\KNN\\23_9_Simulation Results1parallel_cpu.Rdata")
# load("C:\\Brahian Cano\\KNN\\29_9_Simulation Results1parallel_cpu.Rdata")
# simulation_grid = cbind(simulation_grid,CPU_time= CPU_time_knn_st)
# remove(CPU_time_knn_st)
# 
# 
# #load("C:\\Brahian Cano\\KNN\\23_9_Simulation Results1parallel_elapsed.Rdata")
# load("C:\\Brahian Cano\\KNN\\29_9_Simulation Results1parallel_elapsed.Rdata")
# simulation_grid = cbind(simulation_grid,elapsed_time= elapsed_time_knn_st)
# remove(elapsed_time_knn_st)

#Averaging the results of the replications
# simulation_grid_aggregated = aggregate(cbind(iterations,CPU_time,elapsed_time)~distribution+n+m+p+k+g, FUN=mean,data=simulation_grid )
# #save(simulation_grid_aggregated, file="C:\\Brahian Cano\\KNN\\23_9_plotting_grid_1.Rdata" )
# save(simulation_grid_aggregated, file="C:\\Brahian Cano\\KNN\\29_9_plotting_grid_1.Rdata" )
########################################################################

# model1= lm(iterations~distribution+n+m+p+k+g,data=simulation_grid_aggregated)
# summary(model1)


# for(i in 1:6){
#   plot(simulation_grid_withres[,7]~as.factor(simulation_grid_withres[,i]),
#        ylab = "Number of Iterations required",
#        xlab = colnames(simulation_grid)[i])
# }
# 
# 
# plot(simulation_grid_withres[,7]~as.factor(I(simulation_grid_withres$k/simulation_grid_withres$n) ) ,
#      xlab="Proportion of k with respect to n",
#      ylab = "Number of Iterations required")

# 
# #distribution+n+m+p+k+g
# library(ggplot2)
# 
# #load("C:\\Brahian Cano\\KNN\\23_9_plotting_grid_1.Rdata" )
# load("C:\\Brahian Cano\\KNN\\29_9_plotting_grid_1.Rdata" )
# ggplot(data = simulation_grid_aggregated, aes(x=as.factor(distribution),y=iterations))+
#   geom_boxplot()
# 
# ggplot(data = simulation_grid_aggregated, aes(x=as.factor(n),y=iterations))+
#   geom_boxplot()
# 
# ggplot(data = simulation_grid_aggregated, aes(x=as.factor(m),y=iterations))+
#   geom_boxplot()
# 
# ggplot(data = simulation_grid_aggregated, aes(x=as.factor(p),y=iterations))+
#   geom_boxplot()
# 
# ggplot(data = simulation_grid_aggregated, aes(x=as.factor(k),y=iterations))+
#   geom_boxplot()
# 
# ggplot(data = simulation_grid_aggregated, aes(x=as.factor(g),y=iterations))+
#   geom_boxplot()
# 
# ggplot(data = simulation_grid_aggregated, aes(x=as.factor(k/n),y=iterations))+
#   geom_boxplot()+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
#   facet_wrap(~ as.factor(distribution))
# 
# 
# # library(dplyr)
# # 
# # mean_data <- simulation_grid_aggregated[,c(2,6,7)] %>%
# #   group_by(n,g) %>%
# #   summarise(iterations = mean(iterations))
# # 
# # 
# # ggplot(data = mean_data, aes(x=n,y=iterations,color =as.factor(g)))+
# #   geom_point()+ 
# #   geom_line()#+
# #   #facet_wrap(~ as.factor(distribution))
# 
# # ggplot(data = subset(simulation_grid_aggregated,n>=100), aes(x=iterations, y=CPU_time, color=as.factor(n))  )+
# #   stat_summary(geom = "point",fun = "mean")+
# #   stat_summary(geom = "line",fun = "mean")+
# #   facet_wrap(~ as.factor(distribution))
# 
# ggplot(data = subset(simulation_grid_aggregated,n>=100), aes(x=k/n, y=iterations, color =as.factor(g)))+
#   stat_summary(geom = "point",fun = "mean")+
#   stat_summary(geom = "line",fun = "mean")+
#   facet_wrap(~ as.factor(distribution))+
#   theme_bw()
# 
# ggplot(data = subset(simulation_grid_aggregated,n>=100), aes(x=n, y=CPU_time, color =as.factor(g)))+
#   stat_summary(geom = "point",fun = "mean")+
#   stat_summary(geom = "line",fun = "mean")+
#   facet_wrap(~ as.factor(distribution))
# 
# ggplot(data = simulation_grid_aggregated, aes(x=n, y=elapsed_time, color =as.factor(g)))+
#   stat_summary(geom = "point",fun = "mean")+
#   stat_summary(geom = "line",fun = "mean")+
#   facet_wrap(~ as.factor(distribution))
# 
# 
# 
# 
# # model2= lm(total_iterations~distribution+m+p+g+I(k/n),data=simulation_grid_withres)
# # summary(model2)
# 
# #simulation_grid_aggregated$k_proportion = simulation_grid_aggregated$k /simulation_grid_aggregated$n
# 
# 
# 
# ggplot(data = simulation_grid_aggregated, aes(x=as.factor(k/n),y=iterations,color=as.factor(distribution)))+
#   geom_boxplot()
# 
# ggplot(data = simulation_grid_aggregated, aes(x=k/n,y=iterations,color=as.factor(g) ))+
#   geom_smooth()+
#   facet_wrap(~ as.factor(distribution))
# 
# 
# # Computational time ------------------------------------------------------
# 
# 
# load("C:\\Brahian Cano\\KNN\\23_9_Simulation Results2parallel_cpu.Rdata")
# simulation_grid2 = cbind(simulation_grid2,CPU_time= CPU_time_knn_arch)
# remove(CPU_time_knn_arch)
# 
# 
# load("C:\\Brahian Cano\\KNN\\23_9_Simulation Results2parallel_elapsed.Rdata")
# simulation_grid2 = cbind(simulation_grid2,elapsed_time= elapsed_time_knn_arch)
# remove(elapsed_time_knn_arch)
# 
# #Averaging the results of the replications
# simulation_grid2_aggregated = aggregate(cbind(CPU_time,elapsed_time)~distribution+n+m+p+k, FUN=mean,data=simulation_grid2)
# ########################################################################
# 
# 
# simulation_grid2_aggregated = cbind(simulation_grid2_aggregated,g=NA)
# simulation_grid2_aggregated = cbind(simulation_grid2_aggregated,iterations=NA)
# 
# simulation_grid_aggregated = cbind(simulation_grid_aggregated,method=0) # 0 to indicate my method
# simulation_grid2_aggregated = cbind(simulation_grid2_aggregated,method=1) #1 to indicate brute force
# 
# 
# simulation_grid_combined = rbind(simulation_grid_aggregated,simulation_grid2_aggregated)
# 
# save(simulation_grid_combined,file ="C:\\Brahian Cano\\KNN\\29_9_plotting_grid_2.Rdata")
#   # simulation_grid_withres= simulation_grid
# # simulation_grid_withres$distribution = as.factor(simulation_grid_withres$distribution)
# # 
# # for(i in 2:6){
# #   simulation_grid_withres[,i]= as.numeric(simulation_grid_withres[,i])
# # }
# # simulation_grid_withres$CPU_time_knn_st = sapply(my_simulation_results_paralel,function(x) x$sys.self)
# # simulation_grid_withres$CPU_time_knn_arch = sapply(my_simulation_results2_paralel,function(x) x$sys.self)
# # 
# # 
# # simulation_grid_withres$elapsed_time_knn_st = sapply(my_simulation_results_paralel,function(x) x$elapsed)
# # simulation_grid_withres$elapsed_time_knn_arch = sapply(my_simulation_results2_paralel,function(x) x$elapsed)
# # 
# # 
# 
# library(tidyr)
# library(ggplot2)
# 
# # aux1 = pivot_longer(simulation_grid_withres[,-c(9,10)],col = !c(distribution,n,m,p,k,g),names_to = "Method",values_to = "CPU_time")
# # 
# # boxplot(aux1$CPU_time[aux1$Method=="CPU_time_knn_arch"]-aux1$CPU_time[aux1$Method=="CPU_time_knn_st"])
# # 
# # aux = aggregate(CPU_time~distribution+n+m+p+k+Method, FUN=mean,data=subset(aux1,Method=="CPU_time_knn_arch") )
# # aux$g = 0
# # aux1 = rbind(subset(aux1,Method=="CPU_time_knn_st"),aux)
# # remove(aux)
# 
# # sapply(split(aux1$CPU_time,aux1$Method),summary)
# # ggplot(data = aux1, aes(y=CPU_time,x=Method))+
# #   geom_boxplot()
# 
# #load("C:\\Brahian Cano\\KNN\\23_9_plotting_grid_2.Rdata")
# load("C:\\Brahian Cano\\KNN\\29_9_plotting_grid_2.Rdata")
# ggplot(data= simulation_grid_combined, aes(x=as.factor(distribution),y=CPU_time,color=as.factor(method) ))+
#   geom_boxplot()
# 
# ggplot(data= simulation_grid_combined, aes(x=as.factor(n),y=CPU_time,color=as.factor(method) ))+
#   geom_boxplot()
# ggplot(data = simulation_grid_combined, aes(x=n, y=CPU_time, color =as.factor(method)))+
#   stat_summary(geom = "point",fun = "mean")+
#   stat_summary(geom = "line",fun = "mean")
# 
# 
# ggplot(data= simulation_grid_combined, aes(x=as.factor(m),y=CPU_time,color=as.factor(method) ))+
#   geom_boxplot()
# ggplot(data = simulation_grid_combined, aes(x=m, y=CPU_time, color =as.factor(method)))+
#   stat_summary(geom = "point",fun = "mean")+
#   stat_summary(geom = "line",fun = "mean")
# 
# ggplot(data = simulation_grid_combined, aes(x=as.factor(p),y=CPU_time,color=as.factor(method) ))+
#   geom_boxplot()
# ggplot(data = simulation_grid_combined, aes(x=p, y=CPU_time, color =as.factor(method)))+
#   stat_summary(geom = "point",fun = "mean")+
#   stat_summary(geom = "line",fun = "mean")
# 
# ggplot(data = simulation_grid_combined, aes(x=as.factor(k),y=CPU_time,color=as.factor(method) ))+
#   geom_boxplot()
# ggplot(data = simulation_grid_combined, aes(x=k, y=CPU_time, color =as.factor(method)))+
#   stat_summary(geom = "point",fun = "mean")+
#   stat_summary(geom = "line",fun = "mean")
# 
# ggplot(data = simulation_grid_combined, aes(x=as.factor(g),y=CPU_time,color=as.factor(method) ))+
#   geom_boxplot()
# ggplot(data = simulation_grid_combined, aes(x=g, y=CPU_time ))+
#   stat_summary(geom = "point",fun = "mean")+
#   stat_summary(geom = "line",fun = "mean")+
#   facet_wrap(~ as.factor(distribution))
# 
# ggplot(data = simulation_grid_combined, aes(x=as.factor(k/n),y=CPU_time,color=as.factor(method) ))+
#   geom_boxplot()
# ggplot(data = simulation_grid_combined, aes(x=k/n, y=CPU_time, color =as.factor(method)))+
#   stat_summary(geom = "point",fun = "mean")+
#   stat_summary(geom = "line",fun = "mean")
# 
# 
# 
# 
# # ggplot(data = aux1, aes(x=distribution,y=CPU_time,color=Method))+
# #   geom_boxplot()
# # 
# # ggplot(data = aux1, aes(x=as.factor(n),y=CPU_time,color=Method))+
# #   geom_boxplot()
# # 
# # ggplot(data = aux1, aes(x=as.factor(m),y=CPU_time,color=Method))+
# #   geom_boxplot()
# # 
# # ggplot(data = aux1, aes(x=as.factor(p),y=CPU_time,color=Method))+
# #   geom_boxplot(notch = T)
# # 
# # ggplot(data = aux1, aes(x=as.factor(k),y=CPU_time,color=Method))+
# #   geom_boxplot()
# # 
# # ggplot(data = aux1, aes(x=as.factor(g),y=CPU_time,color=Method))+
# #   geom_boxplot()
# 
# 
# 
# 
# 
# 
# # Converting into long format
# # aux2 = pivot_longer(simulation_grid_withres[,-c(7,8)],col = !c(distribution,n,m,p,k,g),names_to = "Method",values_to = "Elapsed_time")
# # 
# # boxplot(aux2$Elapsed_time[aux2$Method=="elapsed_time_knn_arch"]-aux2$Elapsed_time[aux2$Method=="elapsed_time_knn_st"])
# # 
# # ggplot(data = aux2, aes(x=distribution,y=Elapsed_time,color=Method))+
# #   geom_boxplot()
# # 
# # ggplot(data = aux2, aes(x=as.factor(n),y=Elapsed_time,color=Method))+
# #   geom_boxplot()
# # 
# # ggplot(data = aux2, aes(x=as.factor(m),y=Elapsed_time,color=Method))+
# #   geom_boxplot()
# # 
# # ggplot(data = aux2, aes(x=as.factor(p),y=Elapsed_time,color=Method))+
# #   geom_boxplot()
# # 
# # ggplot(data = aux2, aes(x=as.factor(k),y=Elapsed_time,color=Method))+
# #   geom_boxplot()
# # 
# # ggplot(data = aux2, aes(x=as.factor(g),y=Elapsed_time,color=Method))+
# #   geom_boxplot()
# # 
# # 
# 
# 
# 
# 
# 
# ggplot(data= simulation_grid_combined, aes(x=as.factor(distribution),y=elapsed_time,color=as.factor(method) ))+
#   geom_boxplot()
# 
# ggplot(data= simulation_grid_combined, aes(x=as.factor(n),y=elapsed_time,color=as.factor(method) ))+
#   geom_boxplot()
# ggplot(data = simulation_grid_combined, aes(x=n, y=elapsed_time, color =as.factor(method)))+
#   stat_summary(geom = "point",fun = "mean")+
#   stat_summary(geom = "line",fun = "mean")
# 
# ggplot(data= simulation_grid_combined, aes(x=as.factor(m),y=elapsed_time,color=as.factor(method) ))+
#   geom_boxplot()
# ggplot(data = simulation_grid_combined, aes(x=m, y=elapsed_time, color =as.factor(method)))+
#   stat_summary(geom = "point",fun = "mean")+
#   stat_summary(geom = "line",fun = "mean")
# 
# ggplot(data = simulation_grid_combined, aes(x=as.factor(p),y=elapsed_time,color=as.factor(method) ))+
#   geom_boxplot()
# ggplot(data = simulation_grid_combined, aes(x=p, y=elapsed_time, color =as.factor(method)))+
#   stat_summary(geom = "point",fun = "mean")+
#   stat_summary(geom = "line",fun = "mean")
# 
# ggplot(data = simulation_grid_combined, aes(x=as.factor(k),y=elapsed_time,color=as.factor(method) ))+
#   geom_boxplot()
# ggplot(data = simulation_grid_combined, aes(x=k, y=elapsed_time, color =as.factor(method)))+
#   stat_summary(geom = "point",fun = "mean")+
#   stat_summary(geom = "line",fun = "mean")
# 
# 
# ggplot(data = simulation_grid_combined, aes(x=as.factor(g),y=elapsed_time,color=as.factor(method) ))+
#   geom_boxplot()
# ggplot(data = simulation_grid_combined, aes(x=g, y=elapsed_time) )+
#   stat_summary(geom = "point",fun = "mean")+
#   stat_summary(geom = "line",fun = "mean")
# 
# ggplot(data = simulation_grid_combined, aes(x=as.factor(k/n),y=elapsed_time,color=as.factor(method) ))+
#   geom_boxplot()
# ggplot(data = simulation_grid_combined, aes(x=k/n, y=elapsed_time, color =as.factor(method)))+
#   stat_summary(geom = "point",fun = "mean")+
#   stat_summary(geom = "line",fun = "mean")
# 
# 
# # Considering pairwise differences ----------------------------------------
# simulation_grid2_aggregated_copy = simulation_grid2_aggregated[,-c(8,9,10)]
# colnames(simulation_grid2_aggregated_copy) = c("distribution","n","m","p","k","CPU_time1","elapsed_time1")
# simulation_combined = merge(simulation_grid_aggregated,simulation_grid2_aggregated_copy,by=c("distribution","n","m","p","k"),all.x = T)
# simulation_combined = as.data.frame(simulation_combined)
# simulation_combined$CPU_time_diff = simulation_combined$CPU_time1 -simulation_combined$CPU_time#Time from Brute vs my method
# simulation_combined$elapsed_time_diff = simulation_combined$elapsed_time1 -simulation_combined$elapsed_time#Time from Brute vs my method
# 
# 
# save(simulation_combined,file ="C:\\Brahian Cano\\KNN\\29_9_plotting_grid_3.Rdata")
# 
# 
# 
# #load("C:\\Brahian Cano\\KNN\\23_9_plotting_grid_3.Rdata")
# load("C:\\Brahian Cano\\KNN\\29_9_plotting_grid_3.Rdata")
# ggplot(data= simulation_combined, aes(x=as.factor(distribution),y=CPU_time_diff) )+
#   geom_boxplot()
# 
# ggplot(data= simulation_combined, aes(x=as.factor(n),y=CPU_time_diff) )+
#   geom_boxplot()
# 
# ggplot(data= simulation_combined, aes(x=as.factor(m),y=CPU_time_diff) )+
#   geom_boxplot()
# 
# ggplot(data= simulation_combined, aes(x=as.factor(p),y=CPU_time_diff) )+
#   geom_boxplot()
# 
# ggplot(data= simulation_combined, aes(x=as.factor(k),y=CPU_time_diff) )+
#   geom_boxplot()
# 
# ggplot(data= simulation_combined, aes(x=as.factor(g),y=CPU_time_diff) )+
#   geom_boxplot()
# 
# ggplot(data= simulation_combined, aes(x=as.factor(k/n),y=CPU_time_diff) )+
#   geom_boxplot()
# 
# 
# 
# ggplot(data = simulation_combined, aes(x=n, y=CPU_time_diff, color =as.factor(g)) )+
#   stat_summary(geom = "point",fun = "mean")+
#   stat_summary(geom = "line",fun = "mean")
# 
# ggplot(data = simulation_combined, aes(x=k, y=CPU_time_diff, color =as.factor(g)) )+
#   stat_summary(geom = "point",fun = "mean")+
#   stat_summary(geom = "line",fun = "mean")
# 
# ggplot(data = simulation_combined, aes(x=p, y=CPU_time_diff, color =as.factor(g)) )+
#   stat_summary(geom = "point",fun = "mean")+
#   stat_summary(geom = "line",fun = "mean")
# 
# ggplot(data = simulation_combined, aes(x=m, y=CPU_time_diff, color =as.factor(g)) )+
#   stat_summary(geom = "point",fun = "mean")+
#   stat_summary(geom = "line",fun = "mean")+
#   facet_wrap(~ as.factor(distribution))
#   
# 
# 
# 
# 
# ggplot(data = subset(simulation_combined,n>=100 & k>=4), aes(x=n, y=CPU_time_diff, color =as.factor(m)))+
#   stat_summary(geom = "point",fun = "mean")+
#   stat_summary(geom = "line",fun = "mean")+
#   facet_wrap(~ as.factor(distribution))+ 
#   theme_bw() 
# ggplot(data = subset(simulation_combined,n>=100 & k>=4), aes(x=n, y=CPU_time_diff, color =as.factor(m)))+
#   geom_smooth()+
#   facet_wrap(~ as.factor(distribution))+ 
#   theme_bw()
# 
# 
# ggplot(data = subset(simulation_combined,k>=4 & n>=100), aes(x=k, y=CPU_time_diff, color =as.factor(m)))+
#   geom_smooth()+
#   facet_wrap(~ as.factor(distribution))+ 
#   theme_bw()
# 
# 
# 
# 
# 
# 
# ggplot(data= simulation_combined, aes(x=as.factor(distribution),y=elapsed_time_diff) )+
#   geom_boxplot()
# 
# ggplot(data= simulation_combined, aes(x=as.factor(n),y=elapsed_time_diff) )+
#   geom_boxplot()
# 
# ggplot(data= simulation_combined, aes(x=as.factor(m),y=elapsed_time_diff) )+
#   geom_boxplot()
# 
# ggplot(data= simulation_combined, aes(x=as.factor(p),y=elapsed_time_diff) )+
#   geom_boxplot()
# 
# ggplot(data= simulation_combined, aes(x=as.factor(k),y=elapsed_time_diff) )+
#   geom_boxplot()
# 
# ggplot(data= simulation_combined, aes(x=as.factor(g),y=elapsed_time_diff) )+
#   geom_boxplot()
# 
# ggplot(data= simulation_combined, aes(x=as.factor(k/n),y=elapsed_time_diff) )+
#   geom_boxplot()
# 
