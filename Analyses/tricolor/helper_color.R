#shuffle the color index
color.rand<-function(col){
  index<-sample(length(col),replace=F)
  col.rand<-col[index]
  return(col.rand)
}


#Obtain the proportion of ratios that falls between the two thresholds
count.ratio <- function(ratio,threshold1,threshold2){
  return(counts = sum(ratio>threshold1&ratio<threshold2))
}
count.ratio.2 <- function(ratio,threshold1,threshold2){
  prop = sum(ratio>threshold1&ratio<threshold2)/30
  return(prop)
}

angle_with_axis <- function(eigenvec) {
  return(abs(atan(eigenvec[2]/eigenvec[1])*180/pi))
}

#Function to compute angle between two vectors 
angle.diff <- function(vec.1, vec.2){
  return(acos(vec.1%*%vec.2)*180/pi)
}


vec.extract <- function(list){
  len = length(list)
  vec = c()
  for(i in 1:len){
    vec = c(vec, list[[i]][,1])
  }
  return(matrix(vec,nrow = 2, ncol = len))
}

angles <- function(mat) {
  angle.list = c()
  if (ncol(mat) == 1) {
    return(0)
  }else{
    for(j in 1:(ncol(mat)-1)){
      for(i in (j+1):ncol(mat)){
        angle.list = c(angle.list, angle.diff(mat[,j],mat[,i]))
      }
    }
    return(mean(angle.list))
  }
}

min_angle <- function(vec.list) {
  len <-  length(vec.list[[1]])
  vec.list_all <- c(vec.list[[1]],Map("*",vec.list[[1]],-1))
  comb = combn(1:(2*len),len)
  if(len == 2) {
    comb = comb[,-(which(comb[2,] - comb[1,] == len))]
  }else{
    comb = comb[,-(which(comb[2,] - comb[1,] == len | comb[3,] - comb[1,] == len | comb[3,] - comb[2,] == len))]
  }
  list_sum = list()
  for(i in 1:ncol(comb)){
    list_sum = c(list_sum,list(Reduce("+",vec.list_all[comb[,i]])))
  }
  vec.sum = lapply(list_sum,norm,"2")
  return(max(unlist(vec.sum))/len)
}

sampling_sum <- function(size) {
  N = size*3
  x = runif(N,-1,1)
  sign=sample(c(1,-1),size=N,replace = T)
  y = sqrt(1-x^2)*sign
  z = cbind(x,y)
  z=split(z,seq(nrow(z)))
  random_list = tibble(index = rep(1:size,each =3), lengths=z)
  random_vec_sum = random_list %>% group_by(index) %>% dplyr::select(lengths) %>% group_map(~{min_angle(.)}) %>% unlist()
  mean_rd_sum = mean(random_vec_sum)
  return(mean_rd_sum)
}

get_mean_ci <- function(rd_mean){
  sort_mean = sort(rd_mean)
  mean_sampling = mean(rd_mean)
  ci_lower = sort_mean[250]
  ci_upper = sort_mean[9750]
  return(list(mean_sampling = mean_sampling,
              ci_lower = ci_lower,
              ci_upper = ci_upper))
}

dist.mat <- function(x){
  x <- sort(x)
  x <- (x-min(x))/(max(x)-min(x))
  m <- matrix(0, ncol=length(x), nrow=length(x))
  return(matrix(x[row(m)]-x[col(m)], ncol=length(x)))
}

#Null Hypothesis
uniform_null <- function(size){
  rd_sd = rep(0,size)
  for(i in 1:size){
    rd_dist = c(0,runif(3),1)
    sd = sd(diag(dist.mat(rd_dist)[-5,-1]))
    rd_sd[i] = sd
  }
  mean_rd_sd = mean(rd_sd)
  return(mean_rd_sd)
}



