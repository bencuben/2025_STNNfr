load(file="5_27_2025_Simulation Grid2.Rdata")


simulation_index = 1:nrow(simulation_grid2)

my_files = list.files(paste0(getwd(),"/results") ) 

if(length(my_files)>0 ){
  # Find all numbers in the text
  # Find all entries that start with "result_brute_"
  matches <- grep("^result_brute_", my_files, value = TRUE)
  
  # Extract numbers from the matched entries
  numbers <- regmatches(matches, gregexpr("\\d+", matches))
  
  # Convert to numeric
  numbers <- as.numeric(unlist(numbers))
  
  
  simulation_index = simulation_index[!(simulation_index %in% numbers)]
}

simulation_index =sample(simulation_index)

for(simulation_i in simulation_index){
  cmd = paste("sbatch","  Multiple_jobs_brute.sh ",simulation_i,sep="")
  print(cmd)
  system(cmd)
}
