require(doParallel)
# cl <- makeCluster(2)
registerDoParallel(cores = 4)
foreach(i=1:3) %dopar% sqrt(i)
