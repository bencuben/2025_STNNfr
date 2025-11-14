source("Functions_hpc.R")
library(withr,lib.loc ="/home/bcanourrego/scratch/paper1/test/Rpackages")
library(farver,lib.loc ="/home/bcanourrego/scratch/paper1/test/Rpackages")
library(RColorBrewer,lib.loc ="/home/bcanourrego/scratch/paper1/test/Rpackages")
library(caret,lib.loc ="/home/bcanourrego/scratch/paper1/test/Rpackages")

#Read data ------------------------------------------------------------------------
data = read.csv("Alzheimer_data.csv")
# WE have only numeric covariates

#NA
#table(apply(data,2, function(x) mean(is.na(x))))
# 0 NA in the entire data set


# Normalization of the data
# Removing ID and Response variables
data_values = data[,-c(1,452)]


# Syntethic ---------------------------------------------------------------

Sigma = lapply(split(data_values,data$class), cov)
Mu = lapply(split(data_values,data$class),function(x) apply(x,2,mean)) 

library(MASS)
nh=50000
np=50000
set.seed(123)
data1 = mvrnorm(n=nh, mu=Mu[["H"]],Sigma = Sigma[["H"]])
set.seed(123)
data2 = mvrnorm(n=np, mu=Mu[["P"]],Sigma = Sigma[["P"]])
class_label = rep(c("H","P"),each=50000 )
data3 = rbind(data1,data2)

remove(data1,data2)


#library(caret)
set.seed(123)
inTrain <- createDataPartition(
  y = class_label,  ## the outcome data are needed
  p = .9,   ## The percentage of data in the
  ## training set
  list = FALSE
)


training <- data3[ inTrain,]
testing  <- data3[-inTrain,]

preProc = preProcess(training,method=c("center","scale") )
training <- predict(preProc, training)
testing     <- predict(preProc, testing)


rownames(training) = 1:nrow(training)
rownames(testing) = 1:nrow(testing)
training = as.data.frame(training)
testing = as.data.frame(testing)
#training = training[,1:400]
#testing = testing[,1:400]
#summary(training)
#summary(testing)
# Application of my method
g=1
kmax= 15

options(digits = 16)

remove(data3)

#head(training[,1:20])
#head(testing[,1:20])

mem_knn = mem_change({ 
  time1 = system.time({results1 = knn.st(query_set = testing, training_set =training ,g=g, kmax=kmax)})#,id="id")})
})
print("Done with STNNfr")

mem_brute=mem_change({ 
  time2 = system.time({results2 = archaic_knn(query=testing,data=training,k=kmax)})
})

print("Done with Brute-force")
# Application of Brute force
time1
time2

# Proportion in CPU time
time2[2]/time1[2]
# Proportion in Elapsed time
time2[3]/time1[3]
# Proportion of memory used
mem_knn/mem_brute 

results_data_application = list(time1,time2,mem_knn,mem_brute)

save(results_data_application,file= paste0(getwd(),"/Data_app_results.RData") )


load("Data_app_results.RData")



time1= results_data_application[[1]]
time2= results_data_application[[2]]
mem_knn= results_data_application[[3]]
mem_brute= results_data_application[[4]]



time2
mem_brute

time1
mem_knn
# Proportion in CPU time
time2[2]/time1[2]
# Proportion in Elapsed time
time2[3]/time1[3]
# Proportion of memory used
1-mem_knn/mem_brute 


# get_knn =  function(x) do.call(rbind,(x$knn[as.character(1:(length(x$knn))) ])) 
# knn1= get_knn(results1)
# 
# 
# get_knn_arch = function(x) t(sapply(x[1:(length(x)) ], function(x) x$knn))
# knn2= get_knn_arch(results2)
# 
# mean(knn1==knn2)



