library(tidyverse)
library(spatstat)
library(mvtnorm)
library(clue)
library(diptest)
# library(TruncatedNormal)

##load data
#Memory
setwd('/Users/young/Desktop/UCSD/Research/VWM_Iterated_Learning/Homogenous_dots/Data')
load('all.data_memory.Rdata')
all.data_memory = all.data_memory
all.data_memory$x = all.data_memory$x/275
all.data_memory$y = all.data_memory$y/275

#Perceptuomotor
load("all.data.Rdata")
all.data_motor = all.data
all.data_motor$x = all.data$x/275
all.data_motor$y = all.data$y/275

#Source files
setwd('/Users/young/Desktop/UCSD/Research/VWM_Iterated_Learning/Homogenous_dots/Results')
source("helper.R")

##Data mining
#Memory
tb.result_memory<-all.data_memory %>%
  group_by(Seed,Chain,Iter)%>% 
  do(tibble(xy = list(data.frame(x=.$x, y=.$y)),
            covariance=list(cov(cbind(.$x,.$y)))))%>%
  rowwise()%>%
  mutate(eigenval = list(eigen(covariance)),
         det = det(covariance),
         ratio = eigenval$values[1]/eigenval$values[2],
         proj.x = list(as.matrix(xy)%*%eigenval$vectors[,1]),
         proj.y = list(as.matrix(xy)%*%eigenval$vectors[,2]))

#Motor
tb.result_motor<-all.data_motor%>%
  group_by(Seed,Chain,Iter)%>%
  do(tibble(xy = list(data.frame(x=.$x, y=.$y)),
            covariance=list(cov(cbind(.$x,.$y)))))%>%
  rowwise()%>%
  mutate(eigenval = list(eigen(covariance)),
         det = det(covariance),
         ratio = eigenval$values[1]/eigenval$values[2],
         proj.x = list(as.matrix(xy)%*%eigenval$vectors[,1]))

#DTIS "Distance to initial seed" - measures the extent to which the iteration is dissimilar to the original seed

#Observed memory data 
mae_memory_seed = all.data_memory %>% group_by(Seed,Chain) %>% group_map(~{dist_to_seed(.)}) %>% do.call(cbind,.)
mae_memory_seed_df = data.frame(iteration = 1:19, mean_recall_error = apply(mae_memory_seed, MARGIN = 1, mean), se_error = apply(mae_memory_seed, MARGIN = 1, function(x) sd(x)/10))
mae_memory_seed_df$data = "memory"

#slope test for the DTIS with linear regression fit for the memory data
lm_memory_mae = mae_memory_seed_df %>% lm(formula = mean_recall_error~iteration)
summary(lm_memory_mae)
confint(lm_memory_mae,level =0.99)

#Observed Motor data 
mae_motor_seed = all.data_motor %>% group_by(Seed,Chain) %>% group_map(~{dist_to_seed(.)}) %>% do.call(cbind,.)
mae_motor_seed_df = data.frame(iteration = 1:19, mean_recall_error = apply(mae_motor_seed, MARGIN = 1, mean), se_error = apply(mae_motor_seed, MARGIN = 1, function(x) sd(x)/10))
mae_motor_seed_df$data = "motor"

#test
lm_motor_mae = mae_motor_seed_df %>% lm(formula = mean_recall_error~iteration)
confint(lm_motor_mae,level =0.99)

#combined
mae_seed_all = rbind(mae_motor_seed_df,mae_memory_seed_df)
mae_seed_all$iteration = mae_seed_all$iteration + 1
mae_seed_all %>% ggplot(aes(x=iteration,y=mean_recall_error,color=data))+geom_point(size=3)+
  # geom_line(size=1.5)+
  geom_errorbar(aes(ymin=mean_recall_error-se_error,ymax=mean_recall_error+se_error),width=0.5,size=1.1)+
  theme_bw()+
  scale_y_continuous(limits = c(0,0.7))+
  geom_smooth(method = "lm", se = F, linetype = "dashed")+
  scale_color_manual(breaks = c("motor","memory"), values = c("blue", "red"))+
  theme(axis.text = element_text(size= 20),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        legend.position = 'none')

#Paired t test for comparing the distance errors of the memory and percetuomotor data.
t.test(mae_motor_seed_df$mean_recall_error,mae_memory_seed_df$mean_recall_error, paired = T)


#Measure of dispersion, log Determinant of covariance matrix
#Memory
mean_log_det_memory = tb.result_memory %>% group_by(Iter) %>% summarise(mean_det = mean(log(det)),se_det = sd(log(det))/10)
mean_log_det_memory$data = "memory"

#correct standard error for seed (memory)
log_det_memory_0 = tb.result_memory %>% filter(Chain==1,Iter==1) %>% group_by(Iter) %>% 
  summarise(mean_det = mean(log(det)),se_det = sd(log(det))/sqrt(10)) %>% mutate(data = "memory")
mean_log_det_memory = rbind(log_det_memory_0,mean_log_det_memory[-1,])
#Percetuomotor
mean_log_det_motor = tb.result_motor %>% group_by(Iter) %>% summarise(mean_det = mean(log(det)),se_det = sd(log(det))/10)
mean_log_det_motor$data = "motor"

#correct standard error for seed
log_det_motor_0 = tb.result_motor %>% filter(Chain==1,Iter==1) %>% group_by(Iter) %>% 
  summarise(mean_det = mean(log(det)),se_det = sd(log(det))/sqrt(10)) %>% mutate(data = "motor")
mean_log_det_motor = rbind(log_det_motor_0,mean_log_det_motor[-1,])

mean_det_all = rbind(mean_log_det_memory,mean_log_det_motor)
mean_det_all %>% ggplot(aes(x=Iter,y=mean_det,color=data))+geom_point(size=3)+
  geom_errorbar(aes(ymin=mean_det-se_det,ymax=mean_det+se_det),width=0.5,size=1.1)+
  theme_bw()+
  scale_color_manual(values = c("red", "blue"))+
  geom_smooth(method = "lm", se = F, linetype = "dashed")+
  theme(axis.text = element_text(size= 20),
        # axis.title = element_text(size= 12),
        axis.title = element_blank(),
        # axis.title = element_blank(),
        panel.grid = element_blank(),
        legend.position = "none")

#Test slope for the determinant of covariance matrix with linear regression fit
#memory
lm_det_memory = mean_log_det_memory %>% lm(formula = mean_det~Iter)
lm_det_memory
confint(lm_det_memory,level =0.99)
#perceptuomotor
lm_det_motor = mean_log_det_motor %>% lm(formula = mean_det~Iter)
lm_det_motor
confint(lm_det_motor,level =0.99)

#Clusterability analysis 
#memory
#simulated p value assuming uniform within a defined window size
z_p_memory_uniform = all.data_memory %>% group_by(Seed,Chain,Iter) %>% select(x,y) %>% group_map(~{z_p_mc_sim_uniform(.)})

z_score = z_p_memory_uniform %>% lapply(function(x) {return(x$z_score)}) %>% unlist()
p_value = z_p_memory_uniform %>% lapply(function(x) {return(x$p_value)}) %>% unlist()

tb.result_memory$z_score = z_score
tb.result_memory$p_value = p_value

tb.result_memory = tb.result_memory %>% mutate(tail = ifelse(z_score>0, "upper","lower"),sig = ifelse(p_value<=0.05,"sig","non-sig"))
z_p_memory_uniform_result = tb.result_memory
# save(z_p_memory_uniform_result, file = "z_p_memory_uniform_result.Rdata")
load("z_p_memory_uniform_result.Rdata")

#Proportion of signiciant clustered patterns
prop_sig_memory = z_p_memory_uniform_result %>% 
  filter(z_score < 0, sig == "sig") %>% 
  group_by(Iter) %>% summarise(N=n(),prop_sig = N/100)

prop_0 = data.frame(Iter=1, N=0, prop_sig = 0)
prop_sig_memory = rbind(prop_0,prop_sig_memory)
prop_sig_memory$exp = "memory"

#Statistical test
prop_sig_lm = prop_sig_memory %>% lm(formula = prop_sig~Iter)
prop_sig_lm
confint(prop_sig_lm,level =0.99)

#motor
#simulated p value assuming uniform within a defined window size
z_p_motor_uniform = all.data_motor %>% group_by(Seed,Chain,Iter) %>% select(x,y) %>% group_map(~{z_p_mc_sim_uniform(.)})

z_score = z_p_motor_uniform %>% lapply(function(x) {return(x$z_score)}) %>% unlist()
p_value = z_p_motor_uniform %>% lapply(function(x) {return(x$p_value)}) %>% unlist()

tb.result_motor$z_score = z_score
tb.result_motor$p_value = p_value

tb.result_motor = tb.result_motor %>% mutate(tail = ifelse(z_score>0, "upper","lower"),sig = ifelse(p_value<=0.05,"sig","non-sig"))
z_p_motor_uniform_result = tb.result_motor
# save(z_p_motor_uniform_result, file = "z_p_motor_uniform_result.Rdata")
load("z_p_motor_uniform_result.Rdata")

#0 clustered patterns
prop_sig_motor = z_p_motor_uniform_result %>% filter(z_score < 0, sig == "sig") %>% group_by(Iter) %>% summarise(N=n(),prop_sig = N/100)

z_p_motor_uniform_result %>% filter(z_score > 0, sig == "sig") %>% group_by(Iter) %>% summarise(N=n(),prop_sig = N/100)

prop_sig_motor = data.frame(Iter=1:20, N = 0, prop_sig = 0, exp = "motor")

#plot memory and motor
all_prop_sig = rbind(prop_sig_memory,prop_sig_motor)
all_prop_sig %>% ggplot(aes(x=Iter,y=prop_sig,color=exp))+geom_point(size = 3) +
  theme_bw()+
  geom_smooth(method = "lm", se = F, linetype = "dashed")+
  scale_y_continuous(limits = c(0,0.25))+
  scale_color_manual(breaks = c("motor","memory"), values = c("blue", "red"))+
  theme(axis.text = element_text(size= 16),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        legend.position = "none")

#dip_dist test
# memory
dip_p_memory_summary = all.data_memory %>% group_by(Seed,Chain,Iter) %>% select(x,y) %>% summarise(dip_p_value = dip_dist_test(x,y),
                                                                                                   round_p = round(dip_p_value,digits = 2))

prop_cluster_memory = dip_p_memory_summary %>%  group_by(Iter) %>% summarise(N=sum(dip_p_value<=0.05),prop_sig = N/100) %>% 
  mutate(exp = "memory")

#Statistical test
prop_sig_lm_memory = prop_cluster_memory %>% lm(formula = prop_sig~Iter)
prop_sig_lm_memory
confint(prop_sig_lm_memory)

# motor
dip_p_motor_summary = all.data_motor %>% group_by(Seed,Chain,Iter) %>% select(x,y) %>% summarise(dip_p_value = dip_dist_test(x,y),
                                                                                                 round_p = round(dip_p_value,digits = 2))

prop_cluster_motor = dip_p_motor_summary %>% group_by(Iter) %>% summarise(N=sum(dip_p_value<=0.05),prop_sig = N/100) %>% 
  mutate(exp = "motor")

#Statistical test
prop_sig_lm_motor = prop_cluster_motor %>% lm(formula = prop_sig~Iter)
prop_sig_lm_motor
confint(prop_sig_lm_motor)


all_prop_sig = rbind(prop_cluster_memory,prop_cluster_motor)
all_prop_sig %>% ggplot(aes(x=Iter,y=prop_sig,color=exp))+geom_point(size = 3) +
  theme_bw()+
  geom_smooth(method = "lm", se = F, linetype = "dashed")+
  # scale_y_continuous(limits = c(0,0.17))+
  scale_color_manual(breaks = c("motor","memory"), values = c("blue", "red"))+
  theme(axis.text = element_text(size= 20),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        legend.position = "none")

#data check
#motor
dip_p_motor_summary %>% filter(Iter==20,round_p<=0.05)
all.data_motor %>% filter(Seed==8,Chain==3,Iter==1) %>% ggplot(aes(x=x,y=y))+geom_point(size=2)+coord_cartesian(xlim = c(-1,1),
                                                                                                                 ylim = c(-1,1))
all.data_motor %>% filter(Seed==1,Chain==1,Iter==1) %>% select(x,y) %>% dist() %>% hist()

#memory
dip_p_memory_summary %>% filter(Iter==20,round_p<=0.05) %>% print(n=100)

all.data_memory %>% filter(Seed==8,Chain==3,Iter==20) %>% ggplot(aes(x=x,y=y))+
  geom_point(size=2)+
  coord_cartesian(xlim = c(-1,1),ylim = c(-1,1))+
  theme_bw()+
  theme(panel.grid = element_blank())

#Plot histogram for distance
#unclustered pattern
pairwise_dist = all.data_motor %>% filter(Seed==8,Chain==1,Iter==1) %>%select(x,y) %>%  dist() %>% as.vector()

pairwise_dist_df = data.frame(dist = pairwise_dist)
pairwise_dist_df %>% ggplot(aes(x=dist)) + geom_histogram(aes(y=..density..),
                                                          position = "identity",
                                                          fill="white",
                                                          color="black",
                                                          binwidth = 0.05) +
  scale_y_continuous(limits = c(0,2.5))+
  geom_density(color="red",size=2)+
  theme_bw()+
  theme(axis.text = element_text(size= 16),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        legend.position = "none")

#clustered pattern
pairwise_dist = all.data_memory %>% filter(Seed==8,Chain==3,Iter==20)%>%select(x,y) %>%  dist() %>% as.vector()

pairwise_dist_df = data.frame(dist = pairwise_dist)
pairwise_dist_df %>% ggplot(aes(x=dist)) + geom_histogram(aes(y=..density..),
                                                          position = "identity",
                                                          fill="white",
                                                          binwidth = 0.05) +
  scale_y_continuous(limits = c(0,2.5))+
  geom_density(color="red",size=2)+
  theme_bw()+
  theme(axis.text = element_text(size= 16),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        legend.position = "none")

