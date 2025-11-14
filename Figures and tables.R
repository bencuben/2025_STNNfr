


load("simulation_grid_results_5_27_2025.RData")
load("simulation_grid2_results_5_27_2025.RData")

#simulation_grid2_results = simulation_grid2_results[,-c(6,7,8)]


#Averaging the results of the replications
simulation_grid_aggregated = aggregate(cbind(iteration_max,iteration_mean,
                                             iteration_median,CPU_time,elapsed,
                                             memory)~
                                         distribution+n+m+p+k+g,
                                       FUN=mean,data=simulation_grid_results )

#save(simulation_grid_aggregated, file="C:\\Brahian Cano\\KNN\\23_9_plotting_grid_1.Rdata" )
#save(simulation_grid_aggregated, file="C:\\Brahian Cano\\KNN\\29_9_plotting_grid_1.Rdata" )
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


#distribution+n+m+p+k+g
library(ggplot2)

#load("C:\\Brahian Cano\\KNN\\23_9_plotting_grid_1.Rdata" )
#load("C:\\Brahian Cano\\KNN\\29_9_plotting_grid_1.Rdata" )

# Iteration mean for knn
ggplot(data = simulation_grid_aggregated, aes(x=as.factor(distribution),y=iteration_mean))+
  geom_boxplot()

ggplot(data = simulation_grid_aggregated, aes(x=as.factor(n),y=iteration_mean))+
  geom_boxplot()

ggplot(data = simulation_grid_aggregated, aes(x=as.factor(m),y=iteration_mean))+
  geom_boxplot()

ggplot(data = simulation_grid_aggregated, aes(x=as.factor(p),y=iteration_mean))+
  geom_boxplot()

ggplot(data = simulation_grid_aggregated, aes(x=as.factor(k),y=iteration_mean))+
  geom_boxplot()

ggplot(data = simulation_grid_aggregated, aes(x=as.factor(g),y=iteration_mean))+
  geom_boxplot()

ggplot(data = simulation_grid_aggregated, aes(x=as.factor(k/n),y=iteration_mean))+
  geom_boxplot()+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  facet_wrap(~ as.factor(distribution))



# Iteration max for knn
ggplot(data = simulation_grid_aggregated, aes(x=as.factor(distribution),y=iteration_max))+
  geom_boxplot()

ggplot(data = simulation_grid_aggregated, aes(x=as.factor(n),y=iteration_max))+
  geom_boxplot()

ggplot(data = simulation_grid_aggregated, aes(x=as.factor(m),y=iteration_max))+
  geom_boxplot()

ggplot(data = simulation_grid_aggregated, aes(x=as.factor(p),y=iteration_max))+
  geom_boxplot()

ggplot(data = simulation_grid_aggregated, aes(x=as.factor(k),y=iteration_max))+
  geom_boxplot()

ggplot(data = simulation_grid_aggregated, aes(x=as.factor(g),y=iteration_max))+
  geom_boxplot()

ggplot(data = simulation_grid_aggregated, aes(x=as.factor(k/n),y=iteration_max))+
  geom_boxplot()+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  facet_wrap(~ as.factor(distribution))


ggplot(data = simulation_grid_aggregated, aes(x=log(n),y=iteration_max,color=as.factor(m)) )+
  geom_boxplot()+ #theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  facet_wrap(~ as.factor(distribution))

# library(dplyr)
# 
# mean_data <- simulation_grid_aggregated[,c(2,6,7)] %>%
#   group_by(n,g) %>%
#   summarise(iterations = mean(iterations))
# 
# 
# ggplot(data = mean_data, aes(x=n,y=iterations,color =as.factor(g)))+
#   geom_point()+ 
#   geom_line()#+
#   #facet_wrap(~ as.factor(distribution))

# ggplot(data = subset(simulation_grid_aggregated,n>=100), aes(x=iterations, y=CPU_time, color=as.factor(n))  )+
#   stat_summary(geom = "point",fun = "mean")+
#   stat_summary(geom = "line",fun = "mean")+
#   facet_wrap(~ as.factor(distribution))

ggplot(data = simulation_grid_aggregated, aes(x=k/n, y=iteration_mean, color =as.factor(g)))+
  stat_summary(geom = "point",fun = "mean")+
  stat_summary(geom = "line",fun = "mean")+
  facet_wrap(~ as.factor(distribution))+
  theme_bw()

ggplot(data = simulation_grid_aggregated, aes(x=log10(n), y=CPU_time, color =as.factor(g)))+
  stat_summary(geom = "point",fun = "mean")+
  stat_summary(geom = "line",fun = "mean")+
  facet_wrap(~ as.factor(distribution))

ggplot(data = simulation_grid_aggregated, aes(x=log10(n), y=elapsed, color =as.factor(g)))+
  stat_summary(geom = "point",fun = "mean")+
  stat_summary(geom = "line",fun = "mean")+
  facet_wrap(~ as.factor(distribution))




# model2= lm(total_iterations~distribution+m+p+g+I(k/n),data=simulation_grid_withres)
# summary(model2)

#simulation_grid_aggregated$k_proportion = simulation_grid_aggregated$k /simulation_grid_aggregated$n



ggplot(data = simulation_grid_aggregated, aes(x=as.factor(k/n),y=iteration_mean,color=as.factor(distribution)))+
  geom_boxplot()

ggplot(data = simulation_grid_aggregated, aes(x=k/n,y=iteration_mean,color=as.factor(g) ))+
  geom_smooth()+
  facet_wrap(~ as.factor(distribution))


# Computational time ------------------------------------------------------
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

#Averaging the results of the replications
simulation_grid2_aggregated = aggregate(cbind(CPU_time,elapsed,memory)~distribution+n+m+p+k,
                                        FUN=mean,data=simulation_grid2_results)
########################################################################


simulation_grid2_aggregated = cbind(simulation_grid2_aggregated,g=NA)
simulation_grid2_aggregated = cbind(simulation_grid2_aggregated,iteration_max=NA)
simulation_grid2_aggregated = cbind(simulation_grid2_aggregated,iteration_mean=NA)
simulation_grid2_aggregated = cbind(simulation_grid2_aggregated,iteration_median=NA)

simulation_grid_aggregated = cbind(simulation_grid_aggregated,method=0) # 0 to indicate my method
simulation_grid2_aggregated = cbind(simulation_grid2_aggregated,method=1) #1 to indicate brute force


simulation_grid_combined = rbind(simulation_grid_aggregated,simulation_grid2_aggregated)

#save(simulation_grid_combined,file ="C:\\Brahian Cano\\KNN\\29_9_plotting_grid_2.Rdata")
# simulation_grid_withres= simulation_grid
# simulation_grid_withres$distribution = as.factor(simulation_grid_withres$distribution)
# 
# for(i in 2:6){
#   simulation_grid_withres[,i]= as.numeric(simulation_grid_withres[,i])
# }
# simulation_grid_withres$CPU_time_knn_st = sapply(my_simulation_results_paralel,function(x) x$sys.self)
# simulation_grid_withres$CPU_time_knn_arch = sapply(my_simulation_results2_paralel,function(x) x$sys.self)
# 
# 
# simulation_grid_withres$elapsed_time_knn_st = sapply(my_simulation_results_paralel,function(x) x$elapsed)
# simulation_grid_withres$elapsed_time_knn_arch = sapply(my_simulation_results2_paralel,function(x) x$elapsed)
# 
# 

library(tidyr)
library(ggplot2)

# aux1 = pivot_longer(simulation_grid_withres[,-c(9,10)],col = !c(distribution,n,m,p,k,g),names_to = "Method",values_to = "CPU_time")
# 
# boxplot(aux1$CPU_time[aux1$Method=="CPU_time_knn_arch"]-aux1$CPU_time[aux1$Method=="CPU_time_knn_st"])
# 
# aux = aggregate(CPU_time~distribution+n+m+p+k+Method, FUN=mean,data=subset(aux1,Method=="CPU_time_knn_arch") )
# aux$g = 0
# aux1 = rbind(subset(aux1,Method=="CPU_time_knn_st"),aux)
# remove(aux)

# sapply(split(aux1$CPU_time,aux1$Method),summary)
# ggplot(data = aux1, aes(y=CPU_time,x=Method))+
#   geom_boxplot()

#load("C:\\Brahian Cano\\KNN\\23_9_plotting_grid_2.Rdata")
#load("C:\\Brahian Cano\\KNN\\29_9_plotting_grid_2.Rdata")

ggplot(data= simulation_grid_combined, aes(x=as.factor(distribution),y=CPU_time,color=as.factor(method) ))+
  geom_boxplot()

ggplot(data= simulation_grid_combined, aes(x=as.factor(n),y=CPU_time,color=as.factor(method) ))+
  geom_boxplot()

ggplot(data = simulation_grid_combined, aes(x=log10(n), y=CPU_time, color =as.factor(method)))+
  stat_summary(geom = "point",fun = "mean")+
  stat_summary(geom = "line",fun = "mean")


ggplot(data= simulation_grid_combined, aes(x=as.factor(m),y=CPU_time,color=as.factor(method) ))+
  geom_boxplot()
ggplot(data = simulation_grid_combined, aes(x=m, y=CPU_time, color =as.factor(method)))+
  stat_summary(geom = "point",fun = "mean")+
  stat_summary(geom = "line",fun = "mean")

ggplot(data = simulation_grid_combined, aes(x=as.factor(p),y=CPU_time,color=as.factor(method) ))+
  geom_boxplot()
ggplot(data = simulation_grid_combined, aes(x=p, y=CPU_time, color =as.factor(method)))+
  stat_summary(geom = "point",fun = "mean")+
  stat_summary(geom = "line",fun = "mean")

ggplot(data = simulation_grid_combined, aes(x=as.factor(k),y=CPU_time,color=as.factor(method) ))+
  geom_boxplot()
ggplot(data = simulation_grid_combined, aes(x=k, y=CPU_time, color =as.factor(method)))+
  stat_summary(geom = "point",fun = "mean")+
  stat_summary(geom = "line",fun = "mean")

ggplot(data = simulation_grid_combined, aes(x=as.factor(g),y=CPU_time,color=as.factor(method) ))+
  geom_boxplot()
ggplot(data = simulation_grid_combined, aes(x=g, y=CPU_time ))+
  stat_summary(geom = "point",fun = "mean")+
  stat_summary(geom = "line",fun = "mean")+
  facet_wrap(~ as.factor(distribution))

ggplot(data = simulation_grid_combined, aes(x=as.factor(k/n),y=CPU_time,color=as.factor(method) ))+
  geom_boxplot()

ggplot(data = simulation_grid_combined, aes(x=k/n, y=CPU_time, color =as.factor(method)))+
  stat_summary(geom = "point",fun = "mean")+
  stat_summary(geom = "line",fun = "mean")




# ggplot(data = aux1, aes(x=distribution,y=CPU_time,color=Method))+
#   geom_boxplot()
# 
# ggplot(data = aux1, aes(x=as.factor(n),y=CPU_time,color=Method))+
#   geom_boxplot()
# 
# ggplot(data = aux1, aes(x=as.factor(m),y=CPU_time,color=Method))+
#   geom_boxplot()
# 
# ggplot(data = aux1, aes(x=as.factor(p),y=CPU_time,color=Method))+
#   geom_boxplot(notch = T)
# 
# ggplot(data = aux1, aes(x=as.factor(k),y=CPU_time,color=Method))+
#   geom_boxplot()
# 
# ggplot(data = aux1, aes(x=as.factor(g),y=CPU_time,color=Method))+
#   geom_boxplot()






# Converting into long format
# aux2 = pivot_longer(simulation_grid_withres[,-c(7,8)],col = !c(distribution,n,m,p,k,g),names_to = "Method",values_to = "Elapsed_time")
# 
# boxplot(aux2$Elapsed_time[aux2$Method=="elapsed_time_knn_arch"]-aux2$Elapsed_time[aux2$Method=="elapsed_time_knn_st"])
# 
# ggplot(data = aux2, aes(x=distribution,y=Elapsed_time,color=Method))+
#   geom_boxplot()
# 
# ggplot(data = aux2, aes(x=as.factor(n),y=Elapsed_time,color=Method))+
#   geom_boxplot()
# 
# ggplot(data = aux2, aes(x=as.factor(m),y=Elapsed_time,color=Method))+
#   geom_boxplot()
# 
# ggplot(data = aux2, aes(x=as.factor(p),y=Elapsed_time,color=Method))+
#   geom_boxplot()
# 
# ggplot(data = aux2, aes(x=as.factor(k),y=Elapsed_time,color=Method))+
#   geom_boxplot()
# 
# ggplot(data = aux2, aes(x=as.factor(g),y=Elapsed_time,color=Method))+
#   geom_boxplot()
# 
# 





ggplot(data= simulation_grid_combined, aes(x=as.factor(distribution),y=elapsed,color=as.factor(method) ))+
  geom_boxplot()

ggplot(data= simulation_grid_combined, aes(x=as.factor(n),y=elapsed,color=as.factor(method) ))+
  geom_boxplot()
ggplot(data = simulation_grid_combined, aes(x=log10(n), y=elapsed, color =as.factor(method)))+
  stat_summary(geom = "point",fun = "mean")+
  stat_summary(geom = "line",fun = "mean")

ggplot(data= simulation_grid_combined, aes(x=as.factor(m),y=elapsed,color=as.factor(method) ))+
  geom_boxplot()
ggplot(data = simulation_grid_combined, aes(x=m, y=elapsed, color =as.factor(method)))+
  stat_summary(geom = "point",fun = "mean")+
  stat_summary(geom = "line",fun = "mean")

ggplot(data = simulation_grid_combined, aes(x=as.factor(p),y=elapsed,color=as.factor(method) ))+
  geom_boxplot()
ggplot(data = simulation_grid_combined, aes(x=p, y=elapsed, color =as.factor(method)))+
  stat_summary(geom = "point",fun = "mean")+
  stat_summary(geom = "line",fun = "mean")

ggplot(data = simulation_grid_combined, aes(x=as.factor(k),y=elapsed,color=as.factor(method) ))+
  geom_boxplot()
ggplot(data = simulation_grid_combined, aes(x=k, y=elapsed, color =as.factor(method)))+
  stat_summary(geom = "point",fun = "mean")+
  stat_summary(geom = "line",fun = "mean")


ggplot(data = simulation_grid_combined, aes(x=as.factor(g),y=elapsed,color=as.factor(method) ))+
  geom_boxplot()
ggplot(data = simulation_grid_combined, aes(x=g, y=elapsed) )+
  stat_summary(geom = "point",fun = "mean")+
  stat_summary(geom = "line",fun = "mean")

ggplot(data = simulation_grid_combined, aes(x=as.factor(k/n),y=elapsed,color=as.factor(method) ))+
  geom_boxplot()
ggplot(data = simulation_grid_combined, aes(x=k/n, y=elapsed, color =as.factor(method)))+
  stat_summary(geom = "point",fun = "mean")+
  stat_summary(geom = "line",fun = "mean")


# Considering pairwise differences ----------------------------------------

simulation_grid2_aggregated_copy = simulation_grid2_aggregated[,-c(9:13)]
colnames(simulation_grid2_aggregated_copy) = c("distribution","n","m","p","k","CPU_time1","elapsed_time1","memory1")
simulation_combined = merge(simulation_grid_aggregated,
                            simulation_grid2_aggregated_copy,
                            by=c("distribution","n","m","p","k"),all.x = T)
simulation_combined = as.data.frame(simulation_combined)
simulation_combined$CPU_time_diff = simulation_combined$CPU_time1 -simulation_combined$CPU_time#Time from Brute vs my method
simulation_combined$elapsed_time_diff = simulation_combined$elapsed_time1 -simulation_combined$elapsed#Time from Brute vs my method
simulation_combined$memory_diff = simulation_combined$memory1 -simulation_combined$memory#Time from Brute vs my method


#save(simulation_combined,file ="C:\\Brahian Cano\\KNN\\29_9_plotting_grid_3.Rdata")



#load("C:\\Brahian Cano\\KNN\\23_9_plotting_grid_3.Rdata")
#load("C:\\Brahian Cano\\KNN\\29_9_plotting_grid_3.Rdata")
ggplot(data= simulation_combined, aes(x=as.factor(distribution),y=CPU_time_diff) )+
  geom_boxplot()

ggplot(data= simulation_combined, aes(x=as.factor(n),y=CPU_time_diff) )+
  geom_boxplot()

ggplot(data= simulation_combined, aes(x=as.factor(m),y=CPU_time_diff) )+
  geom_boxplot()

ggplot(data= simulation_combined, aes(x=as.factor(p),y=CPU_time_diff) )+
  geom_boxplot()

ggplot(data= simulation_combined, aes(x=as.factor(k),y=CPU_time_diff) )+
  geom_boxplot()

ggplot(data= simulation_combined, aes(x=as.factor(g),y=CPU_time_diff) )+
  geom_boxplot()

ggplot(data= simulation_combined, aes(x=as.factor(k/n),y=CPU_time_diff) )+
  geom_boxplot()



ggplot(data = simulation_combined, aes(x=log10(n), y=CPU_time_diff, color =as.factor(g)) )+
  stat_summary(geom = "point",fun = "mean")+
  stat_summary(geom = "line",fun = "mean")+ 
  theme_bw()+
  geom_hline(yintercept = 0, linetype = "dotted", color = "black")

ggplot(data = simulation_combined, aes(x=k, y=CPU_time_diff, color =as.factor(g)) )+
  stat_summary(geom = "point",fun = "mean")+
  stat_summary(geom = "line",fun = "mean")+ 
  theme_bw()+
  geom_hline(yintercept = 0, linetype = "dotted", color = "black")

ggplot(data = simulation_combined, aes(x=p, y=CPU_time_diff, color =as.factor(g)) )+
  stat_summary(geom = "point",fun = "mean")+
  stat_summary(geom = "line",fun = "mean")+ 
  theme_bw()+
  geom_hline(yintercept = 0, linetype = "dotted", color = "black")

ggplot(data = simulation_combined, aes(x=m, y=CPU_time_diff, color =as.factor(g)) )+
  stat_summary(geom = "point",fun = "mean")+
  stat_summary(geom = "line",fun = "mean")+
  facet_wrap(~ as.factor(distribution))+ 
  theme_bw()+
  geom_hline(yintercept = 0, linetype = "dotted", color = "black")





ggplot(data = simulation_combined, aes(x=log10(n), y=CPU_time_diff, color =as.factor(m)))+
  stat_summary(geom = "point",fun = "mean")+
  stat_summary(geom = "line",fun = "mean")+
  facet_wrap(~ as.factor(p))+ 
  theme_bw()+
  geom_hline(yintercept = 0, linetype = "dotted", color = "black")


ggplot(data = simulation_combined, aes(x=log10(n), y=CPU_time_diff, color =as.factor(m)))+
  geom_smooth()+
  facet_wrap(~ as.factor(distribution))+ 
  theme_bw()+
  geom_hline(yintercept = 0, linetype = "dotted", color = "black")


ggplot(data = simulation_combined, aes(x=k, y=CPU_time_diff, color =as.factor(m)))+
  geom_smooth()+
  facet_wrap(~ as.factor(distribution))+ 
  theme_classic()+
  geom_hline(yintercept = 0, linetype = "dotted", color = "black")

ggplot(data = simulation_combined, aes(x=k/n, y=CPU_time_diff, color =as.factor(m)))+
  stat_summary(geom = "point",fun = "mean")+
  stat_summary(geom = "line",fun = "mean")+
  facet_wrap(~ as.factor(p),scales = "free")+ 
  theme_bw()+
  geom_hline(yintercept = 0, linetype = "dotted", color = "black")



# Elapsed time ------------------------------------------------------------



ggplot(data= simulation_combined, aes(x=as.factor(distribution),y=elapsed_time_diff) )+
  geom_boxplot()

ggplot(data= simulation_combined, aes(x=as.factor(n),y=elapsed_time_diff) )+
  geom_boxplot()

ggplot(data= simulation_combined, aes(x=as.factor(m),y=elapsed_time_diff) )+
  geom_boxplot()

ggplot(data= simulation_combined, aes(x=as.factor(p),y=elapsed_time_diff) )+
  geom_boxplot()

ggplot(data= simulation_combined, aes(x=as.factor(k),y=elapsed_time_diff) )+
  geom_boxplot()

ggplot(data= simulation_combined, aes(x=as.factor(g),y=elapsed_time_diff) )+
  geom_boxplot()

ggplot(data= simulation_combined, aes(x=as.factor(k/n),y=elapsed_time_diff) )+
  geom_boxplot()




ggplot(data = simulation_combined, aes(x=log10(n), y=elapsed_time_diff, color =as.factor(g)) )+
  stat_summary(geom = "point",fun = "mean")+
  stat_summary(geom = "line",fun = "mean")+ 
  theme_bw()+
  geom_hline(yintercept = 0, linetype = "dotted", color = "black")

ggplot(data = simulation_combined, aes(x=k, y=elapsed_time_diff, color =as.factor(g)) )+
  stat_summary(geom = "point",fun = "mean")+
  stat_summary(geom = "line",fun = "mean")+ 
  theme_bw()+
  geom_hline(yintercept = 0, linetype = "dotted", color = "black")

ggplot(data = simulation_combined, aes(x=p, y=elapsed_time_diff, color =as.factor(g)) )+
  stat_summary(geom = "point",fun = "mean")+
  stat_summary(geom = "line",fun = "mean")+ 
  theme_bw()+
  geom_hline(yintercept = 0, linetype = "dotted", color = "black")

ggplot(data = simulation_combined, aes(x=m, y=elapsed_time_diff, color =as.factor(g)) )+
  stat_summary(geom = "point",fun = "mean")+
  stat_summary(geom = "line",fun = "mean")+
  facet_wrap(~ as.factor(distribution))+ 
  theme_bw()+
  geom_hline(yintercept = 0, linetype = "dotted", color = "black")





ggplot(data = simulation_combined, aes(x=log10(n), y=elapsed_time_diff, color =as.factor(m)))+
  stat_summary(geom = "point",fun = "mean")+
  stat_summary(geom = "line",fun = "mean")+
  facet_wrap(~ as.factor(p))+ 
  theme_bw()+
  geom_hline(yintercept = 0, linetype = "dotted", color = "black")


ggplot(data = simulation_combined, aes(x=log10(n), y=elapsed_time_diff, color =as.factor(m)))+
  geom_smooth()+
  facet_wrap(~ as.factor(distribution))+ 
  theme_bw()+
  geom_hline(yintercept = 0, linetype = "dotted", color = "black")


ggplot(data = simulation_combined, aes(x=k, y=elapsed_time_diff, color =as.factor(m)))+
  geom_smooth()+
  facet_wrap(~ as.factor(distribution))+ 
  theme_classic()+
  geom_hline(yintercept = 0, linetype = "dotted", color = "black")

ggplot(data = simulation_combined, aes(x=k/n, y=elapsed_time_diff, color =as.factor(m)))+
  stat_summary(geom = "point",fun = "mean")+
  stat_summary(geom = "line",fun = "mean")+
  facet_wrap(~ as.factor(p),scales = "free")+ 
  theme_bw()+
  geom_hline(yintercept = 0, linetype = "dotted",color = "black")




# Final Figures -----------------------------------------------------------

cc <- scales::seq_gradient_pal("#FC0103", "#7A1718", "Lab")(seq(0,1,length.out=3))

cc1 <- scales::seq_gradient_pal("#FFDE59", "#7A1718", "Lab")(seq(0,1,length.out=4))

dist_labeller <- function(variable,value){
  dist_names <- list(
    '0'= "Multivariate Normal",
    '1'='Multivariate t',
    '2'= "Spherical Uniform"
  )
  return(dist_names[value])
}

dist_labeller_p <- function(variable,value){
  dist_names <- list(
    '20'= "p = 20",
    '50'= "p = 50",
    '100'= "p = 100",
    '250'= "p = 250"
  )
  return(dist_names[value])
}


# Figure 4:

# Elapsed time vs n (sample size) by p (Dimension) with G as legend
# Subset for Spherical Uniform


png("plots/Figure4.png",height=3.6,width = 5.4, units = "in",res = 300)

ggplot(data = subset(simulation_grid_aggregated,distribution==2), aes(x=log10(n), y=elapsed, color =as.factor(g)))+
  stat_summary(geom = "point",fun = "mean")+
  stat_summary(geom = "line",fun = "mean")+
  facet_wrap(~ as.factor(p),labeller=dist_labeller_p)+ 
  theme_bw() +labs(color = "G", y= "Elapsed time (Secs)")+
  scale_colour_manual(values=cc1)+
  #eliminates background, gridlines, and chart border
  theme(
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    #panel.border = element_blank()
  ) +
  
  #draws x and y axis line
  theme(axis.line = element_line(color = 'black'))

dev.off()

# CPU time vs n (sample size) by p (Dimension) with G as legend


ggplot(data = subset(simulation_grid_aggregated,distribution==2), aes(x=log10(n), y=CPU_time, color =as.factor(g)))+
  stat_summary(geom = "point",fun = "mean")+
  stat_summary(geom = "line",fun = "mean")+
  facet_wrap(~ as.factor(p),labeller=dist_labeller_p)+ 
  theme_bw() +labs(color = "G", y= "CPU time (Secs)")+
  scale_colour_manual(values=cc1)+
  #eliminates background, gridlines, and chart border
  theme(
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    #panel.border = element_blank()
  ) +
  
  #draws x and y axis line
  theme(axis.line = element_line(color = 'black'))


# Memory  vs n (sample size) by p (Dimension) with G as legend


ggplot(data = subset(simulation_grid_aggregated,distribution==2), 
       aes(x=log10(n), y=memory/(1000*1000), color =as.factor(g)))+
  stat_summary(geom = "point",fun = "mean")+
  stat_summary(geom = "line",fun = "mean")+
  facet_wrap(~ as.factor(p),labeller=dist_labeller_p)+ 
  theme_bw() +labs(color = "G", y= "Memory allocation (MB)")+
  scale_colour_manual(values=cc1)+
  #eliminates background, gridlines, and chart border
  theme(
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    #panel.border = element_blank()
  ) +
  
  #draws x and y axis line
  theme(axis.line = element_line(color = 'black'))



# Fig 5:
# plot of effect of m, n vs CPU time considered by distribution

png("plots/Figure5.png",height=3.6,width = 5.4, units = "in",res = 300)
ggplot(data = subset(simulation_combined,distribution==2 & g==1), aes(x=log10(n), y=CPU_time_diff,
                                       color =as.factor(m), linetype = as.factor(m)))+
  stat_summary(geom = "point",fun = "mean")+
  stat_summary(geom = "line",fun = "mean", linewidth = 1)+
  facet_wrap(~ as.factor(p),labeller=dist_labeller_p,scales = "free")+ 
  theme_bw()  +labs(color = "m",linetype="m", y= "Difference in CPU time (Secs)")+
  geom_hline(yintercept = 0, linetype = "dotted", linewidth = 1, color = "black")+
  scale_colour_manual(values=cc)+
  scale_linetype_manual(values =c("solid","dashed","dotdash"))+
  #eliminates background, gridlines, and chart border
  theme(
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    #panel.border = element_blank()
  ) #+
  
  #draws x and y axis line
  #theme(axis.line = element_line(color = 'black'),
  #      text=element_text(size=40))

dev.off()




# Fig 6:

png("plots/Figure6.png",height=3.6,width = 5.4, units = "in",res = 300)
ggplot(data = subset(simulation_combined,distribution==2 & g==1), aes(x=log10(n), y=elapsed_time_diff,
                                       color =as.factor(m), linetype = as.factor(m) ))+
  stat_summary(geom = "point",fun = "mean")+
  stat_summary(geom = "line",fun = "mean", linewidth = 1)+
  facet_wrap(~ as.factor(p),labeller=dist_labeller_p)+#,scales = "free")+ 
  theme_bw() +labs(color = "m",linetype="m", y= "Difference in Elapsed time(Secs)")+
  geom_hline(yintercept = 0, linetype = "dotted", linewidth = 1, color = "black")+
  scale_colour_manual(values=cc)+
  #eliminates background, gridlines, and chart border
  theme(
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    #panel.border = element_blank()
  ) #+
  
  #draws x and y axis line
  #theme(axis.line = element_line(color = 'black'),
  #      text=element_text(size=40))

dev.off()


png("plots/Figure7.png",height=3.6,width = 5.4, units = "in",res = 300)
ggplot(data = subset(simulation_combined,distribution==2 & g==1), aes(x=log10(n), y=memory_diff/(1000*1000),
                                                               color =as.factor(m), linetype = as.factor(m) ))+
  stat_summary(geom = "point",fun = "mean")+
  stat_summary(geom = "line",fun = "mean", linewidth = 1)+
  facet_wrap(~ as.factor(p),labeller=dist_labeller_p)+#,scales = "free")+ 
  theme_bw() +labs(color = "m",linetype="m", y= "Difference in Memory allocation (MB)")+
  geom_hline(yintercept = 0, linetype = "dotted", linewidth = 1, color = "black")+
  scale_colour_manual(values=cc)+
  #eliminates background, gridlines, and chart border
  theme(
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    #panel.border = element_blank()
  ) 
dev.off()
# 
# 
# aux = simulation_combined[,c("distribution","n","m","p","k","g","CPU_time_diff","elapsed_time_diff")]
# aux1 = pivot_longer(aux,col = c(CPU_time_diff,elapsed_time_diff),names_to = "metric",values_to = "y")
# 
# metric_labeller <- function(variable,value){
#   metric_names <- list(
#     'CPU_time_diff'= "CPU",
#     'elapsed_time_diff'= "Elapsed"
#   )
#   return(metric_names[value])
# }
# 
# ggplot(data = aux1, aes(x=log10(n), y=y, color =as.factor(m)))+
#   stat_summary(geom = "point",fun = "mean")+
#   stat_summary(geom = "line",fun = "mean")+
#   facet_wrap(~ as.factor(metric),scales="free", labeller = metric_labeller)+ 
#   theme_bw()  +labs(color = "m", y= "Difference in time (Seconds)")+
#   geom_hline(yintercept = 0, linetype = "dotted", linewidth = 0.7, color = "black")+
#   scale_colour_manual(values=cc)+
#   #eliminates background, gridlines, and chart border
#   theme(
#     plot.background = element_blank(),
#     panel.grid.major = element_blank(),
#     panel.grid.minor = element_blank(),
#     #panel.border = element_blank()
#   ) +
#   
#   #draws x and y axis line
#   theme(axis.line = element_line(color = 'black'))



# Appendix ------------------------------------------------------------------


ggplot(data = simulation_combined, aes(x=log10(n), y=CPU_time_diff,color =as.factor(g),
                                       linetype = as.factor(g)))+
  stat_summary(geom = "point",fun = "mean")+
  stat_summary(geom = "line",fun = "mean", linewidth = 1)+
  facet_wrap(~ as.factor(distribution),labeller=dist_labeller,scales = "fixed")+ 
  theme_bw()  +labs(color = "g",linetype="g", y= "Difference in CPU time (Secs)")+
  geom_hline(yintercept = 0, linetype = "dotted", linewidth = 1, color = "black")+
  #scale_colour_manual(values=cc1)+
  #scale_linetype_manual(values =c("solid","dashed","dotdash"))+
  #eliminates background, gridlines, and chart border
  theme(
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    #panel.border = element_blank()
  )

ggplot(data = simulation_combined, aes(x=log10(n), y=elapsed_time_diff,color =as.factor(g),
                                       linetype = as.factor(g)))+
  stat_summary(geom = "point",fun = "mean")+
  stat_summary(geom = "line",fun = "mean", linewidth = 1)+
  facet_wrap(~ as.factor(distribution),labeller=dist_labeller,scales = "fixed")+ 
  theme_bw()  +labs(color = "g",linetype="g", y= "Difference in elapsed time (Secs)")+
  geom_hline(yintercept = 0, linetype = "dotted", linewidth = 1, color = "black")+
  #scale_colour_manual(values=cc1)+
  #scale_linetype_manual(values =c("solid","dashed","dotdash"))+
  #eliminates background, gridlines, and chart border
  theme(
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    #panel.border = element_blank()
  )



ggplot(data = simulation_combined, aes(x=log10(n), y=memory_diff/(1000*1000),color =as.factor(g),
                                       linetype = as.factor(g)))+
  stat_summary(geom = "point",fun = "mean")+
  stat_summary(geom = "line",fun = "mean", linewidth = 1)+
  facet_wrap(~ as.factor(distribution),labeller=dist_labeller,scales = "fixed")+ 
  theme_bw()  +labs(color = "g",linetype="g", y= "Difference in memory allocation (MB)")+
  geom_hline(yintercept = 0, linetype = "dotted", linewidth = 1, color = "black")+
  #scale_colour_manual(values=cc1)+
  #scale_linetype_manual(values =c("solid","dashed","dotdash"))+
  #eliminates background, gridlines, and chart border
  theme(
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    #panel.border = element_blank()
  )







png("plots/Figure appendix 1.png",height=3.6,width = 5.4, units = "in",res = 300)
ggplot(data = subset(simulation_combined, g==1),
       aes(x=log10(n), y=CPU_time_diff,
           color =as.factor(m), linetype = as.factor(m)))+
  stat_summary(geom = "point",fun = "mean")+
  stat_summary(geom = "line",fun = "mean", linewidth = 1)+
  facet_wrap(~ as.factor(distribution),labeller=dist_labeller,scales = "fixed")+ 
  theme_bw()  +labs(color = "m",linetype="m", y= "Difference in CPU time (Secs)")+
  geom_hline(yintercept = 0, linetype = "dotted", linewidth = 1, color = "black")+
  scale_colour_manual(values=cc)+
  scale_linetype_manual(values =c("solid","dashed","dotdash"))+
  #eliminates background, gridlines, and chart border
  theme(
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    #panel.border = element_blank()
  ) #+
  
dev.off()



ggplot(data = subset(simulation_combined, g==1),
       aes(x=log10(n), y=elapsed_time_diff,
           color =as.factor(m), linetype = as.factor(m)))+
  stat_summary(geom = "point",fun = "mean")+
  stat_summary(geom = "line",fun = "mean", linewidth = 1)+
  facet_wrap(~ as.factor(distribution),labeller=dist_labeller,scales = "fixed")+ 
  theme_bw()  +labs(color = "m",linetype="m", y= "Difference in Elapsed time (Secs)")+
  geom_hline(yintercept = 0, linetype = "dotted", linewidth = 1, color = "black")+
  scale_colour_manual(values=cc)+
  scale_linetype_manual(values =c("solid","dashed","dotdash"))+
  #eliminates background, gridlines, and chart border
  theme(
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    #panel.border = element_blank()
  ) #+




ggplot(data = subset(simulation_combined, g==1),
       aes(x=log10(n), y=memory_diff/(1000*1000),
           color =as.factor(m), linetype = as.factor(m)))+
  stat_summary(geom = "point",fun = "mean")+
  stat_summary(geom = "line",fun = "mean", linewidth = 1)+
  facet_wrap(~ as.factor(distribution),labeller=dist_labeller,scales = "fixed")+ 
  theme_bw()  +labs(color = "m",linetype="m", y= "Difference in Memory allocation (MB)")+
  geom_hline(yintercept = 0, linetype = "dotted", linewidth = 1, color = "black")+
  scale_colour_manual(values=cc)+
  scale_linetype_manual(values =c("solid","dashed","dotdash"))+
  #eliminates background, gridlines, and chart border
  theme(
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    #panel.border = element_blank()
  ) #+
