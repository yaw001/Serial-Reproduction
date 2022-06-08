#Helper functions

# linear sum assignment problem (matching problem)
# When providing feedback, LSAP computes the minimum MEA matching solution.
LSAP<-function(truedots, respdots){
  dist.mat <- as.matrix(dist(rbind(truedots, respdots)))
  dist.mat <- dist.mat[(nrow(truedots)+1):nrow(dist.mat),1:(nrow(truedots))]
  assignment<-as.matrix(solve_LSAP(dist.mat))
  assignment<-cbind(1:nrow(assignment),assignment)
  errors<-dist.mat[assignment]
  mae<-mean(errors)
  return(mae)
}


#average distance to seed
dist_to_seed <- function(chain_dat){
  mae = c()
  seed = chain_dat %>% filter(Iter == 1) %>% dplyr::select(x,y)
  for(i in 2:20){
    ref_iter = chain_dat %>% filter(Iter==i) %>% dplyr::select(x,y)
    drift_error = LSAP(seed,ref_iter)
    mae=c(mae, drift_error)
  }
  return(mae)
}

#Nearest neighbor analysis
#Nearest neighbor analysis examines the distances between each point and the closest point to it, 
#and then compares these to expected values for a random sample of points from a CSR (complete spatial randomness) pattern.
#Compute Area and perimeter assuming a bounded regular rectangle 
area_perimeter <- function(dat){
  max_x = max(dat$x)
  min_x = min(dat$x)
  max_y = max(dat$y)
  min_y = min(dat$y)
  width = max_x - min_x
  height = max_y - min_y
  area = width * height
  perimeter = 2*(width + height)
  return(c(area,perimeter))
}

# max,min x and y values on data
max_min_xy <- function(dat){
  max_x = max(dat$x)
  min_x = min(dat$x)
  max_y = max(dat$y)
  min_y = min(dat$y)
  return(c(max_x,min_x,max_y,min_y))
}


#Compute the mean nearest neighbor distance
mean_nnd <- function(dat){
  return(mean(nndist(dat)))
}

#Compute expected value of the nearest neighbor distance in a random pattern assuming CSR (complete spatial randomness)
#Donnelly 1978 with correction for edge effect
#analysis confined within a bounded study region may well be biased because of the ignorance of the outside of the study region 
#as well as the inappropriateness of the theories.
#This problem of potential bias in spatial analysis is referred to as edge effects (or boundary effects).
mean_nnd_rand <- function(N,area,perimeter){
  return(0.5*sqrt(area/N)+0.0514*perimeter/N + 0.041*perimeter/N^(3/2))
}

#Compute the variance
var_nnd_rand <- function(N, area, perimeter){
  return(0.0703*area/N^2 + 0.037*perimeter*sqrt(area/N^5))
}

#Compute the z score for the observed mean nnd with edge effect correction
nnd_z <- function(dat){
  N = nrow(dat)
  area = area_perimeter(dat)[1]
  perimeter = area_perimeter(dat)[2]
  observed = mean_nnd(dat)
  mean_expected = mean_nnd_rand(N,area,perimeter)
  var_expected = var_nnd_rand(N,area,perimeter)
  z = (observed - mean_expected)/sqrt(var_expected/(N-1))
  return(z)
}

#Clark and Evans (1954) test of CSR without edge correction
mean_nnd_rand_no_edge <- function(N, area){
  return(1/(2*sqrt(N/area)))
}
var_nnd_rand_no_edge <- function(N, area){
  return((4-pi)/(4*pi*(N/area)))
}

#Two problems: 
#edge effect (ignored in our case)
#inflated variance due to reflexive points (62.15% points are reflexive nearest neighbors for CSR)
#When the elements of each sample in this process are positively correlated, when one value is high the others tend to be high, too. 
#Their mean will then be high. When one value is low the others tend to be low, too. 
#Their mean will then be low. Thus, the means tend either to be high or low.
#Conditioned on data
nnd_z_no_edge <- function(dat){
  N=nrow(dat)
  area = area_perimeter(dat)[1]
  observed = mean_nnd(dat)
  mean_expected = mean_nnd_rand_no_edge(N,area)
  var_expected = var_nnd_rand_no_edge(N,area)
  z = (observed - mean_expected)/sqrt(var_expected/(N))
  return(z)
}

#Conditioned on data
z_p_mc_sim_uniform <- function(dat){
  max_x = max_min_xy(dat)[1]
  min_x = max_min_xy(dat)[2]
  max_y = max_min_xy(dat)[3]
  min_y = max_min_xy(dat)[4]
  sampling_stats = rep(0,10000)
  for(i in 1:10000){
    rand_set = cbind(runif(15,min_x,max_x),runif(15,min_y,max_y))
    rand_mean_nnd = mean(nndist(rand_set))
    sampling_stats[i] = rand_mean_nnd
  }
  observed_mean_nnd = mean(nndist(dat))
  sampling_mean = mean(sampling_stats)
  se_mean = sd(sampling_stats)
  z_score = (observed_mean_nnd - sampling_mean)/se_mean
  if(z_score > 0) {
    p_value = sum(sampling_stats>=observed_mean_nnd)/10000
  } else {
    p_value = sum(sampling_stats<=observed_mean_nnd)/10000
  }
  return(list(p_value = p_value,
              z_score = z_score))
}

#Use Monte Carlo simulation to approximate p-value testing the randomization
#1. Estimated the mean and variance of the given iteration
#2. Generate samples from the truncated normal distribution and compute the mean NND 
#3. Compute observed mean NND
#4. Obtain the p-value
z_p_mc_sim <- function(dat){
  mean_dat = colMeans(dat)
  cov_dat = cov(dat)
  # cov_dat = diag(rep(max(diag(cov(dat))),2))
  sampling_stats = rep(0,10000)
  for(i in 1:10000){
    rand_set = rtmvnorm(15,mean_dat,cov_dat, lower = c(-1,-1), upper = c(1, 1))
    rand_mean_nnd = mean(nndist(rand_set))
    sampling_stats[i] = rand_mean_nnd
  }
  observed_mean_nnd = mean(nndist(dat))
  sampling_mean = mean(sampling_stats)
  se_mean = sd(sampling_stats)
  z_score = (observed_mean_nnd - sampling_mean)/se_mean
  if(z_score > 0) {
    p_value = sum(sampling_stats>=observed_mean_nnd)/10000
  } else {
    p_value = sum(sampling_stats<=observed_mean_nnd)/10000
  }
  return(list(p_value = p_value,
              z_score = z_score))
}

#dip test on pairwise distances (Dip-test)
#dip-dist tests for clusters in the set of dissimilarities using the Dip test. multiple modes
#suggests the presensce of multiple clusters.

dip_dist_test <- function(x,y){
  all_pair_dist = as.vector(dist(data.frame(x=x,y=y)))
  return(dip.test(all_pair_dist)$p.value)
}





