
library(MASS,lib.loc ="/home/bcanourrego/scratch/paper1/test/Rpackages")
library(dbscan,lib.loc ="/home/bcanourrego/scratch/paper1/test/Rpackages")
library(zipfR,lib.loc ="/home/bcanourrego/scratch/paper1/test/Rpackages")
library(gmp,lib.loc ="/home/bcanourrego/scratch/paper1/test/Rpackages")
library(mvtnorm,lib.loc ="/home/bcanourrego/scratch/paper1/test/Rpackages")
library(pryr,lib.loc ="/home/bcanourrego/scratch/paper1/test/Rpackages")

# library(MASS)
# library(dbscan)
# library(zipfR)
# library(gmp)
# library(mvtnorm)
# library(pryr)
#Defining all the functions needed



# Note that for this function, there was a bug for which, big values of the parameters
# will result into a INF value or an NaN in the optimization
# I am not really sure yet, where are the boundaries.
# Function has been updated to use logarithms of big values and then exponential

# p=250 has worked so far


# ri: radius of the circle ci
# r: radius of the circle c defining the sample space
# tau: desired probability you want to obtain with circle ci
# query_point: xi vector of features
# centroid: Vectors of means of the training set
max_function2 = function(ri,r,tau,query_point,centroid){
  d = euclidean(centroid,query_point)
  
  
  #the point is inside
  #dbi=r-euclidean(centroid,query_point)
  
  # n here is the number of dimensions not the number of observations
  p= length(centroid)
  
  
  
  if(d>=r+ri | ri==0  | r==0){
    #There is no overlapping 
    volumen=0
    
  } 
  else if(d<=abs(r-ri)){
    # Volumen of smaller sphere 
    
    # https://www.storyofmathematics.com/hypersphere/#:~:text=The%20volume%20%28or%2C%20more%20accurately%2C%20%E2%80%9Ccontent%E2%80%9D%29%20of%20a,r%20n%20where%20%CE%93%20represents%20the%20gamma%20function.
    #volumen = (  pi^(n/2)   /(gamma(n/2 +1)) ) *(min(ri,r)^n)
    volumen_log =   as.bigz((p/2)* log(pi) + p* log(min(ri,r) ) - lgamma(p/2 +1) )
    
    volumen = exp(1)^volumen_log
    
  } else{
    # Case in which there is a partial overlap of spheres
    c1 = ((d^2)+(r^2)-(ri^2))/(2*d)
    
    c2 = ((d^2)-(r^2)+(ri^2))/(2*d)
    
    
    vn_cap= function(rad,a){
      if(a>=0){
        #res =1/2 *((pi^(n/2)) /(gamma(n/2+1)) )*(rad^n) * beta_inc((n+1)/2,1/2,1-(a^2 /rad^2))
        #library(gls)
        #res =exp(log(1/2) + (p/2)* log(pi) - (lgamma(p/2+1)) +p *log(rad) +log(beta_inc((p+1)/2,1/2,1-(a^2 /rad^2) ) ) )
        #library(zipfR)
        res =exp(1)^as.bigz(log(1/2) + (p/2)* log(pi) - (lgamma(p/2+1)) +p *log(rad) +Rbeta(1-(a^2 /rad^2), (p+1)/2, 1/2, log = T) ) 
      } else if(a<0){
        #res = ((pi^(n/2)) /(gamma(n/2+1)) )*(rad^n) - vn_cap(rad,-a)
        res = exp(1)^as.bigz( (p/2)* log(pi) -(lgamma(p/2+1)) +p*log(rad) )  - vn_cap(rad,-a)
      }
      
      return(res)
    }#end of vn_cap
    
    volumen = vn_cap(r,c1)+vn_cap(ri,c2)
  }
  
  
  # Generalize prob of the cap
  #sphere_prob = volumen/( (pi^(n/2)/(gamma(n/2 +1)) ) *(r^n) )
  sphere_prob = volumen/exp(1)^as.bigz((p/2)* log(pi) + p* log(r)  - lgamma(p/2 +1) )
  sphere_prob = as.double(sphere_prob)
   
  return(sphere_prob-tau)
  
  
  
  
}



# generate_point <- function(original_point, r) {
#   p <- length(original_point)
#   direction <- runif(p, min = -1, max = 1)
#   direction <- direction / sqrt(sum(direction^2))  # Normalize the direction vector
#   new_point <- original_point + r * direction
#   return(new_point)
# }

#Euclidean distance for two points a and b
euclidean <- function(a, b){
  return(sqrt(sum((a - b)^2)))
}



# nrPoints : desired amount of observations
# nrDim: Number of dimensions
# center: center of the sphere
# r = radius of the sphere
GenerateSpherePoints <- function(nrPoints,nrDim,center=rep(0,nrDim),r=3.5){
  #generate the polar coordinates!
  x <-  matrix(runif(nrPoints*nrDim,-pi,pi),ncol=nrDim)
  x[,nrDim] <- x[,nrDim]/2
  #recalculate them to cartesians
  sin.x <- sin(x)
  cos.x <- cos(x)
  cos.x[,nrDim] <- 1  # see the formula for n.spheres
  
  y <- sapply(1:nrDim, function(i){
    if(i==1){
      cos.x[,1]
    } else {
      cos.x[,i]*apply(sin.x[,1:(i-1),drop=F],1,prod)
    }
  })*sqrt(runif(nrPoints,0,r^2))
  
  y <-  as.data.frame(
    t(apply(y,1,'+',center))
  )
  
  names(y) <- make.names(seq_len(nrDim))
  y
}


# x: a batch of observations belonging to the same ring group, it must contain the p features.
#    then the p+1 columns must be an id
#    the p+2 columns must be ring group belonging

# data_ref: Training set
# ri_results_candidates: list of radius to use for searching with the fixed radius
#   search method. This is based on the ring group belonging
# kmax: Number of nearest neighbors to look up for
# p: number of dimensions


#Note: Function updated to receive a batch of observation from same group
# since their search would have the same radius, they can be imputed to the frNN()
# function simultaneously, but now I cannot omit the neighbors found in previous iterations
# to make it more efficient
# An inefficiency now, is that, the kd-tree for the search is created several times eventhough
# the training data doesn't chance, I need to create the structure and only do queries over it.
fixed_radius_knn = function(x,data_ref, ri_results_candidates, kmax,p,r){
  
  q = x[,1:p]
  
  
  id_query =x[,(p+1)]
  # There was a issue in which the query set names were being overwritten
  # I found that forcing the rownames to be this id query works
  rownames(q) = id_query
  
  q_group = x[1,(p+2)]

  
  
  # List of query set ids for which the kNN haven't been found
  need_neighbors = as.character(id_query)
  # list with the kNN ids from the training set for each point in the query set
  nn_res1 = list()
  # list with the kNN distances from the training set for each point in the query set
  nn_res2 = list()
  # list of k' neighbors used
  nn_res3 = list()
  
  # list containing how many radius was needed to expand in order to find kNN for each observation
  iteration_res = list()
  counter = 1
  
  # This process is repeated until all observations have obtained at least k neighbors
  while(length(need_neighbors)>0){
    
    
    # function to find observations at a distance "eps" from each point
    
    # If one exceded the prespecified radius search, then it will gradually increase ri
    # CHANGE!!! It should be needing more iterations than specified. MAYBE I NEED TO CONSIDER THE
    # MAXIMUM RADIUS BASED ON THE QUERY FARTEST DISTANCE
    rad_aux = ifelse(!is.na(ri_results_candidates[[as.character(q_group)]][counter]) ,
                     ri_results_candidates[[as.character(q_group)]][counter] ,
                     (counter+2)*r)
    q_aux = q[id_query %in% need_neighbors,]
    
    # if(length(need_neighbors)>1 ){
    #   q_aux = q[id_query %in% need_neighbors,]
    # } else if(length(need_neighbors)==1){
    #   q_aux = t(as.matrix(q[id_query %in% need_neighbors]) )
    # }
    
    
    
    
    # In the case that there is only one observation left, q would become a vector instead
    # of a matrix, causing problem, thus I force it to be a matrix again
    
    
    nn <- frNN(data_ref[,-(p+1)], # data ref without the id
               eps = rad_aux,
               query= q_aux)
    
    res1 = nn$id
    res2 = nn$dist
    res3 = lapply(nn$id,function(x) length(x))
    # Observations for which lesser than k nearest neighbor have been found
    need_neighbors= names(which(lengths(res1)<kmax) )
    
    # Removing these from the list, so they will be used in the next iteration
    res1[which(names(res1) %in%  need_neighbors)] <- NULL
    res2[which(names(res2) %in%  need_neighbors)] <- NULL
    res3[which(names(res3) %in%  need_neighbors)] <- NULL
    
    #At least 1 observation in the iteration with K neighbors
    if(length(res1)>0 ){
      
      # Extract only the k nearest neighbors
      res1 = lapply(res1,function(x) x[1:kmax])
      res2 = lapply(res2,function(x) x[1:kmax])
      
      nn_res1= c(res1,nn_res1)
      nn_res2= c(res2,nn_res2)
      nn_res3= c(res3,nn_res3)
      # Attach the iteration for which kNN where found 
      iteration_res[names(res1)]=counter
    }
    
    counter = counter+ 1
  }
  
  
  return(list(knn=nn_res1,knn_dist=nn_res2,k_prime= nn_res3,iterations = iteration_res))
  
  
}


# Auxiliar function
radius_candidate_finder= function(q,tau,r,centroid,max_r=3*r){
  return(stats::uniroot(max_function2,#interval = c(0,3*r),
                        lower = 0, upper = max_r,
                        r=r,tau=tau,query_point=q,centroid=centroid,
                        )$root)
}
radius_candidate_finder = Vectorize(radius_candidate_finder,vectorize.args="tau")


# query_set: set for observations for which kNN want to be found, NEEDS TO BE DATA FRAME
# training_set: where the neighbors need to come from, NEEDS TO BE DATA FRAME
# g: Number of ring groups
# kmax: Desired number of neighbors
knn.st = function(query_set,training_set,g,kmax){
  
  # Number of dimensions
  p = ncol(training_set)
  # Number of rows
  n = nrow(training_set)
  
  
  # Finding the centroid of training set
  centroid = apply(training_set,2, mean)
  
  
  # Calculating the distance of each point in training set to the centroid
  Distances_centroid = apply(training_set,1,euclidean, centroid)
  
  # Finding the point that is farther point from the centroid
  farthest = which.max(Distances_centroid)
  
  # Radius of the circle C defining the sample space
  r= as.numeric(Distances_centroid[farthest])
  
  
  #Find the group assignations
  # Generating the  g ring neighborhoods
  
  # group_aux = function(tau,rad){
  #   return(sqrt(tau*rad^2))
  # }
  # Finding groups from 0 to one, such that, there a total of g rings 
  # in addition to the g+1 out of sample space ring
  #ri_groups =group_aux(seq(0,1,length.out=g+1)[-1],rad = r)
  #ri_groups =sqrt( (seq(0,1,length.out=g+1)[-1] )*r^2)
  ri_groups =r*(seq(0,1,length.out=g+1)[-1] )^(1/p) 
  # Classifying the query observation into the groups
  
  Distances_centroid_query = apply(query_set,1,euclidean, centroid)
  
  #a point is classified as follow
  # 1 if in (0,ri_groups[1]]
  # 2 if in (ri_groups[1],ri_groups[2]]
  # ....
  # g if in (ri_groups[g-1],ri_groups[g]]
  # g+1 if > ri_groups[g]
  
  
  # The idea is to create homogeneous groups of observation in their of their location with respect
  # to the centroid
  groups_query= cut(Distances_centroid_query,c(0,ri_groups,Inf),labels=1:(g+1),include.lowest = T )
  groups_query = as.numeric(groups_query)
  
  
  # Changing the first coordinate minus de desired radius to create points at a fixed distance
  # from the centroid
  # note that for observations out of the sample space, 1.1*r is used as candidate
  
  # CHANGE!!!!!!!!!!!!---------------------------------------------------------------------------------
  #middle_points = apply(cbind(c(ri_groups,r*1.2),c(0,ri_groups)),1,mean)
  # I have decided to use the edge points for the candidate generation
  # since in high dimensional cases, points would be farther away from the center
  edge_points = c(ri_groups,r*1.2)
  groups_candidates_query = list()
  for(i in 1:(g+1)){
    groups_candidates_query[[as.character(i)]]=centroid
    #groups_candidates_query[[as.character(i)]][1]=groups_candidates_query[[as.character(i)]][1]-middle_points[i]
    groups_candidates_query[[as.character(i)]][1]=groups_candidates_query[[as.character(i)]][1]-edge_points[i]
  }
  
  # Problem if the candidate is farther away than 3*r from the centroid. Extrapolation
  
  
  # for each group we found at the middle of the ring, and then with the point we found 
  # the radius of spheres that have probabilities of  
  # 0.0500000 0.1555556 0.2611111 0.3666667 0.4722222 0.5777778 0.6833333 0.7888889 0.8944444 1.0000000
  
  #The idea would be finding good initial values for this tau, such that the algorithm converge at the
  # first iteration with high probability
  
  
  #initial_tau = ifelse(kmax==n,(kmax-0.01)/n, kmax/n)
  # This grid could be changed
  #tau_grid = if(kmax==n) seq( (kmax-0.01)/n,1,length.out=10) else seq(kmax/n,1,length.out=10) 
  tau_grid = if(kmax==n) c(1) else seq(kmax/n,0.99,length.out=10) 
  #something1(groups_candidates_query$``,tau=seq(initial_tau,1,length.out=10),r=r,centroid=centroid)
  ri_results_candidates= lapply(groups_candidates_query,radius_candidate_finder,
                                tau=tau_grid, max_r=max(Distances_centroid_query)+r,#2*r,
                                r=r,centroid=centroid)
  
  
  
  # ri_results_candidates = list()
  # 
  # 
  # #OPTIMIZE GET RID OF LOOPS!!!!!
  # for(g_aux in 1:(g+1) ){
  #    
  #   
  #   ri_results = c()
  #   
  #   initial_tau = ifelse(kmax==n,(kmax-0.01)/n, kmax/n)
  #   for(tau in seq(initial_tau,1,length.out=10) ){#seq(0.05,1,length.out=10) ){
  #     
  #     if(tau ==1 & g_aux<=g){
  #       # Worst case use the diameter of original sample space to find the neighbors
  #       ri_results= c(ri_results,2*r)
  #     } else{
  #       q = groups_candidates_query[[as.character(g_aux)]]
  #       interval=c(0,3*r ) # need to GENERALIZE THESE VALUES
  #       set.seed(123)
  #       ri_res= uniroot(max_function2,interval = interval,r=r,tau=tau,query_point=q,centroid=centroid)
  #       
  #       # This is the numerical results for ri
  #       ri_results= c(ri_results,ri_res$root)
  #     }
  #     
  #     
  #   }
  #   
  #   ri_results_candidates[[as.character(g_aux)]] = ri_results
  # }
  
  # Creating identifiers for both data
  #query_set= cbind(query_set,id = 1:nrow(query_set),groups= groups_query)
  query_set$id= 1:nrow(query_set)
  query_set$group= groups_query
  #training_set= cbind(training_set,id =1:nrow(training_set) )
  training_set$id= 1:nrow(training_set)
  
  # CHANGE, NO NEED TO STORE THIS OBJECT
  #data_temp_query = split.data.frame(query_set,groups_query)
  
  
  # For each ring group apply the fixed_radius_knn search
  aux = lapply(split.data.frame(query_set,groups_query),#data_temp_query,
               fixed_radius_knn,
               data_ref =training_set,ri_results_candidates,kmax,p,r)
  
  
  # Removing the ring group level from the list
  a= list(knn=list(),knn_dist=list(),k_prime=list(),iterations=list())
  
  # OPTIMIZE??
  for(i in names(aux)){
    a =  Map(c,aux[[i]],a)
  }
  
  return(a) 
  #return(c(a,list(r=r) )) 
  
}


# query: Query set for which kNN want to be found
# data: Training set from where to get the kNN
# k: Number of nearest neighbors
archaic_knn = function(query,data,k){
  
  res = apply(query,1,archaic_knn_aux,data,k)
  return(res)
}

# x: row of Query set for which kNN want to be found
# data_ref: Training set from where to get the kNN
archaic_knn_aux = function(x,data_ref,k){
  distances = apply(data_ref,1,euclidean,x)
  names(distances) = rownames(data_ref)
  
  distances = sort(distances)
  knn = as.numeric(names(distances)[1:k] )
  knn_dist = distances[1:k] 
  return(list(knn= knn,knn_dist=knn_dist))
} 




# x: vector with a set of parameters from the simulation grid
knn.st.simulation= function(x){
  # vector of parameters
  distri=x[[1]]
  n=x[[2]]
  m=x[[3]]
  p=x[[4]]
  k=x[[5]]
  g=x[[6]]
  
  # Importing the proper data set
  training_set = data_list[[as.character(distri)]][[as.character(n)]][[as.character(m)]][[as.character(p)]]$training_set
  query_set = data_list[[as.character(distri)]][[as.character(n)]][[as.character(m)]][[as.character(p)]]$query_set
  
  options(digits = 15)
  time1=system.time({
    res = knn.st(query_set,
                 training_set,
                 g=g,
                 kmax=k) })
  
  return(c(res,time1) )
  
  
  
}





# x: vector with a set of parameters from the simulation grid
archaic_knn_simulation= function(x){
  # vector of parameters
  distri=x[[1]]
  n=x[[2]]
  m=x[[3]]
  p=x[[4]]
  k=x[[5]]
  #g=as.numeric(x[[6]])
  # Note that g is not required for this method
  # Thus we we will have 1 vs g for each combinations for other parameters
  
  training_set = data_list[[as.character(distri)]][[as.character(n)]][[as.character(m)]][[as.character(p)]]$training_set
  query_set = data_list[[as.character(distri)]][[as.character(n)]][[as.character(m)]][[as.character(p)]]$query_set
  
  options(digits = 15)
  time3 = system.time({kkn_archaic_res = archaic_knn(query=query_set,data=training_set,k=k)})
  
  return(c(kkn_archaic_res,time3) )
  
  
  
}

