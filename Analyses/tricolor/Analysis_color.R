#Data Analysis and Graphs
library(tidyverse)
library(ggfortify)
library(cluster)
library(broom)

# library(reshape)
setwd('/Users/young/Desktop/UCSD/Research/VWM_Iterated_Learning/Colored_dots/Data')
load('all.data_color.Rdata')
all.data<-filter(all.data,iter<=25)

#Source files
setwd('/Users/young/Desktop/UCSD/Research/VWM_Iterated_Learning/Colored_dots/Analysis')
source("helper_color.R")
#summary of the data 
tb.result_color<-all.data%>%filter(iter<=25) %>% 
  group_by(seed,chain,iter,color)%>% 
  do(tibble(xy = list(data.frame(x=.$x, y=.$y)),
            covariance=list(cov(cbind(.$x,.$y)))))%>%
  rowwise()%>%
  mutate(eigenval = list(eigen(covariance)),
         eigenvec = list(eigen(covariance)$vector),
         det = det(covariance),
         ratio = eigenval$values[1]/eigenval$values[2],
         correlation = cor(xy$x,xy$y),
         proj.x = list(as.matrix(xy)%*%eigenval$vectors[,1]),
         proj.y = list(as.matrix(xy)%*%eigenval$vectors[,2]),
         eigenvec_1 = list(eigenvec[,1]))

#The dispersion of color groups
#Mean determinant of Covariance matrix Contingent on Colors
log_mean_det <- tb.result_color %>% filter(iter>0) %>% group_by(iter,color)%>%
  summarise(N=n(),mean.det=mean(log(det)),se.mean=sd(log(det))/sqrt(N))

#For original seed
log_mean_det_0 <- tb.result_color %>% filter(chain==0,iter==0) %>% group_by(iter,color) %>% 
  summarise(N=n(),mean.det=mean(log(det)),se.mean=sd(log(det))/sqrt(N))

log_mean_det_all = rbind(log_mean_det_0,log_mean_det)
#Figure aggregating three color groups
log_mean_det_all%>%ggplot(aes(x=iter,mean,y=mean.det,color=color))+
  geom_point()+
  geom_line(size=1.1)+
  scale_color_manual(values = c('red'='red',
                                'green'='green2',
                                'blue'='blue')) +
  geom_errorbar(aes(ymin=mean.det-se.mean,ymax=mean.det+se.mean),width=0.5,size=1.1)+
  labs(x = 'iteration',
       y = 'Mean Determinant of Covariance Matrix')+
  theme_bw()+
  theme(text = element_text(family='Times New Roman', size= 15, face='bold'),
        panel.grid = element_blank(),
        plot.title = element_text(hjust=0.5),
        legend.position = 'none')

#The dispersion of the overall groups
mean.det_all<-all.data%>%group_by(seed,chain,iter)%>%
  summarise(det.cov=det(cov(cbind(x,y))))%>%
  group_by(iter)%>%
  summarise(mean.det=mean(log(det.cov)),se.mean=sd(log(det.cov))/sqrt(100))

mean.det_all%>%ggplot(aes(x=iter,y=mean.det))+
  geom_point()+
  geom_line(size=1.1)+
  geom_errorbar(aes(ymin=mean.det-se.mean,ymax=mean.det+se.mean),width=0.5,size=1.1)+
  labs(x = 'Iteration',
       y = 'Covariance Determinant (Pattern spread)')+
  theme_bw()+
  theme(text = element_text(size= 12),
        panel.grid = element_blank(),
        plot.title = element_text(hjust=0.5),
        legend.position = 'none',
        axis.title = element_blank())

lm_mean_det=mean.det_all %>% lm(formula = mean.det~iter)
lm_mean_det
confint(lm_mean_det,level =0.99)

#Color Randomization
#recreate the data with randomized color index
randcol<-as.vector(replicate(25*100,color.rand(rep(c('red','blue','green'),5))))
dat_randcol<-all.data %>% filter(iter>=1,iter<=25)
dat_randcol['color']=randcol

#For original seed
randcol_0 <- as.vector(replicate(10,color.rand(rep(c('red','blue','green'),5))))
dat_randcol_0 <- all.data %>% filter(chain==0,iter==0, iter<=25)
dat_randcol_0['color']=randcol_0

log_mean.det.rc_0<-dat_randcol_0%>%group_by(seed,chain,iter,color)%>%
  summarise(det.cov=det(cov(cbind(x,y))))%>%
  group_by(iter,color)%>%
  summarise(mean.det.rc=mean(log(det.cov)),se.mean.rc=sd(log(det.cov))/sqrt(10))

#compute the determinant of covariance matrix for the randomized color groups
log_mean.det.rc<-dat_randcol%>%group_by(seed,chain,iter,color)%>%
  summarise(det.cov=det(cov(cbind(x,y))))%>%
  group_by(iter,color)%>%
  summarise(mean.det.rc=mean(log(det.cov)),se.mean.rc=sd(log(det.cov))/sqrt(100))

#For all data including the random seed
mean.det.rc_all <- rbind(log_mean.det.rc_0,log_mean.det.rc)
mean.det_original <- log_mean_det_all
df.mean.det<-data.frame(iter=c(mean.det_original$iter,rep(0:25,each=3)),
                        color=c(mean.det_original$color,mean.det.rc_all$color),
                        mean.det=c(mean.det_original$mean.det,mean.det.rc_all$mean.det.rc),
                        se.mean=c(mean.det_original$se.mean,mean.det.rc_all$se.mean.rc),
                        category=c(rep('Observed',78),rep('Color randomized',78)))

#plot the mean determinant of covariance matrix three color groups with color randomization
#red groups plot
df.mean.det %>% filter(color == "red") %>% ggplot(aes(x=iter,y=mean.det))+geom_point(aes(shape=category),color="red",size=3)+
  geom_line(aes(linetype=category),color="red",size=1.5)+
  scale_linetype_manual(values = c('Observed'="solid",
                                   'Color randomized'="11"), name="category")+
  scale_shape_manual(values = c('Observed'=16,
                                'Color randomized'=17), name="category")+
  geom_errorbar(aes(ymin=mean.det-se.mean,ymax=mean.det+se.mean),width=0.4,size=0.9,color="red")+
  labs(x = 'Iteration',
       y = 'Covariance determinant (group spread)')+
  # including original seed
  scale_y_continuous(limits = c(-8,-3))+
  theme_bw()+
  theme(text = element_text(size= 26),
        panel.grid = element_blank(),
        plot.title = element_text(hjust=0.5),
        legend.position = 'none',
        axis.title = element_blank())

#slope test on red group
red_det_lm = df.mean.det %>% filter(color == "red", category == "Observed") %>% lm(formula = mean.det~iter)
red_det_lm
(red_det_lm)

#test 
t.test(df.mean.det %>% filter(iter>0, color == "red", category == "Observed") %>% pull(mean.det),
       df.mean.det %>% filter(iter>0, color == "red", category == "Color randomized") %>% pull(mean.det),
       paired = T,alternative="less")

#blue groups
df.mean.det %>% filter(color == "blue") %>% ggplot(aes(x=iter,y=mean.det))+geom_point(aes(shape=category),color="blue",size=3)+
  geom_line(aes(linetype=category),color="blue",size=1.5)+
  scale_linetype_manual(values = c('Observed'="solid",
                                   'Color randomized'="11"), name="category")+
  scale_shape_manual(values = c('Observed'=16,
                                'Color randomized'=17), name="category")+
  geom_errorbar(aes(ymin=mean.det-se.mean,ymax=mean.det+se.mean),width=0.4,size=0.9,color="blue")+
  # including the random seed
  scale_y_continuous(limits = c(-8,-3))+
  labs(x = 'Iteration',
       y = 'Covariance determinant (group spread)')+
  theme_bw()+
  theme(text = element_text(size= 26),
        panel.grid = element_blank(),
        plot.title = element_text(hjust=0.5),
        legend.position = 'none',
        axis.title = element_blank())

blue_det_lm = df.mean.det %>% filter(color == "blue", category == "Observed") %>% lm(formula = mean.det~iter)
blue_det_lm
confint(blue_det_lm,level =0.99)

t.test(df.mean.det %>% filter(iter>0, color == "blue", category == "Observed") %>% pull(mean.det),
       df.mean.det %>% filter(iter>0, color == "blue", category == "Color randomized") %>% pull(mean.det),
       paired = T,alternative="less")

# green groups
df.mean.det %>% filter(color == "green") %>% ggplot(aes(x=iter,y=mean.det))+geom_point(aes(shape=category),color="green",size=3)+
  geom_line(aes(linetype=category),color="green",size=1.5)+
  scale_linetype_manual(values = c('Observed'="solid",
                                   'Color randomized'="11"), name="category")+
  scale_shape_manual(values = c('Observed'=16,
                                'Color randomized'=17), name="category")+
  # including the original seed
  scale_y_continuous(limits = c(-8,-3))+
  geom_errorbar(aes(ymin=mean.det-se.mean,ymax=mean.det+se.mean),width=0.4,size=0.9,color="green")+
  labs(x = 'Iteration',
       y = 'Covariance determinant (group spread)')+
  theme_bw()+
  theme(text = element_text(size= 26),
        panel.grid = element_blank(),
        plot.title = element_text(hjust=0.5),
        legend.position = 'none',
        axis.title = element_blank())

green_det_lm = df.mean.det %>% filter(color == "green", category == "Observed") %>% lm(formula = mean.det~iter)
green_det_lm
confint(green_det_lm,level =0.99)

t.test(df.mean.det %>% filter(iter>0, color == "green", category == "Observed") %>% pull(mean.det),
       df.mean.det %>% filter(iter>0, color == "green", category == "Color randomized") %>% pull(mean.det),
       paired = T,alternative="less")


#anisotropy measured ratio of 1st eigenvalue/ 2nd eigenvalue conditioned on color groups.
#Red group: compute the log ratio
red_ratio = tb.result_color %>% filter(iter>0,color=="red") %>% group_by(iter) %>% summarise(mean_log_ratio=mean(log(ratio)), se_log_ratio = sd(log(ratio))/10) %>% 
  mutate(color="red")

red_ratio %>% ggplot(aes(x=iter,y=mean_log_ratio)) + 
  scale_y_continuous(limits = c(1,3))+
  geom_point(color="red") +
  geom_line(size=1.5,color="red") +
  geom_errorbar(aes(ymin=mean_log_ratio-se_log_ratio,ymax=mean_log_ratio+se_log_ratio),width=0.4,size=0.9,color="red")+
  theme_bw()+
  theme(text = element_text(size= 16),
        panel.grid = element_blank(),
        legend.position = 'none',
        axis.title = element_blank())

red_ratio_lm = red_ratio %>% lm(formula = mean_log_ratio~iter)
confint(red_ratio_lm)

#Green group: compute the log ratio
green_ratio = tb.result_color %>% filter(iter>0,color=="green") %>% group_by(iter) %>% summarise(mean_log_ratio=mean(log(ratio)), se_log_ratio = sd(log(ratio))/10) %>% 
  mutate(color = "green")
green_ratio %>% 
  ggplot(aes(x=iter,y=mean_log_ratio)) + 
  scale_y_continuous(limits = c(1,3))+
  geom_point(color="green") +
  geom_line(size=1.5,color="green") +
  geom_errorbar(aes(ymin=mean_log_ratio-se_log_ratio,ymax=mean_log_ratio+se_log_ratio),width=0.4,size=0.9,color="green")+
  theme_bw()+
  theme(text = element_text(size= 16),
        panel.grid = element_blank(),
        legend.position = 'none',
        axis.title = element_blank())

green_ratio_lm = green_ratio %>% lm(formula = mean_log_ratio~iter)
confint(green_ratio_lm)

#Blue group: compute the log ratio
blue_ratio = tb.result_color %>% filter(iter>0,color=="blue") %>% group_by(iter) %>% summarise(mean_log_ratio=mean(log(ratio)), se_log_ratio = sd(log(ratio))/10) %>% 
  mutate(color = "blue")
blue_ratio %>% 
  ggplot(aes(x=iter,y=mean_log_ratio)) + 
  scale_y_continuous(limits = c(1,3))+
  geom_point(color="blue") +
  geom_line(size=1.5,color="blue") +
  geom_errorbar(aes(ymin=mean_log_ratio-se_log_ratio,ymax=mean_log_ratio+se_log_ratio),width=0.4,size=0.9,color="blue")+
  theme_bw()+
  theme(text = element_text(size= 16),
        panel.grid = element_blank(),
        legend.position = 'none',
        axis.title = element_blank())

blue_ratio_lm = blue_ratio %>% lm(formula = mean_log_ratio~iter)
confint(blue_ratio_lm)

# combine 
log_ratio_all = rbind(red_ratio,green_ratio,blue_ratio)
log_ratio_all %>% 
  ggplot(aes(x=iter,y=mean_log_ratio,color=as.factor(color))) + 
  scale_y_continuous(limits = c(1,3))+
  geom_point() +
  geom_line(size=1.5) +
  geom_errorbar(aes(ymin=mean_log_ratio-se_log_ratio,ymax=mean_log_ratio+se_log_ratio),width=0.4,size=0.9)+
  scale_color_manual(values = c("blue","green","red"))+
  theme_bw()+
  theme(text = element_text(size= 16),
        panel.grid = element_blank(),
        legend.position = 'none',
        axis.title = element_blank())
#Ratio analysis.
#PC ratios are grouped into 3 categories (linear, anisotropic and isotropic)
ratio<-all.data%>%group_by(seed,chain,iter,color)%>%
  summarise(ratio=eigen(cov(cbind(x,y)))$values[1]/eigen(cov(cbind(x,y)))$values[2])

#Quartiles
quantile(ratio$ratio,probs = seq(0,1,0.25))
#Three quantiles
quantile(ratio$ratio,probs = seq(0,1,1/3))

quantile(ratio$ratio,0.425+0.15)
quantile(ratio$ratio,0.45)
quantile(ratio$ratio,0.75)
#Isotropic (first quartile)
threshold1=1
threshold2=2

count_result_isotropic = ratio%>%
  group_by(iter)%>%summarise(count=count.ratio(ratio,threshold1,threshold2))
prop.test(count_result_isotropic$count,n=rep(300,26))
prop.trend.test(count_result_isotropic$count,n=rep(300,26))

prop.result.2<-ratio%>%
  group_by(seed,iter)%>%summarise(prop=count.ratio.2(ratio,threshold1,threshold2))%>%
  group_by(iter)%>%summarise(mean.prop=mean(prop),se.prop=sd(prop)/sqrt(10))
df.ratio.2<-data.frame(iter=0:25,mean.prop=prop.result.2$mean.prop,prop.se=prop.result.2$se.prop)
df.ratio.2<-df.ratio.2 %>% mutate(type = "(3)isotropic(1<ratio<2.6)")

# anisotropic (second quartile + third)
threshold1=2
threshold2=14

prop.result.3<-ratio%>%
  group_by(seed,iter)%>%summarise(prop=count.ratio.2(ratio,threshold1,threshold2))%>%
  group_by(iter)%>%summarise(mean.prop=mean(prop),se.prop=sd(prop)/sqrt(10))

df.ratio.3<-data.frame(iter=0:25,mean.prop=prop.result.3$mean.prop,prop.se=prop.result.3$se.prop)
df.ratio.3<-df.ratio.3 %>% mutate(type = "(2)Anisotropic(2.6<ratio<14)")

# fourth quartile (linear)
threshold1 = 14
threshold2 = 100000000

prop.result.1<-ratio%>%
  group_by(seed,iter)%>%summarise(prop=count.ratio.2(ratio,threshold1,threshold2))%>%
  group_by(iter)%>%summarise(mean.prop=mean(prop),se.prop=sd(prop)/sqrt(10))

df.ratio.1<-data.frame(iter=0:25,mean.prop=prop.result.1$mean.prop,prop.se=prop.result.1$se.prop)
df.ratio.1 <- df.ratio.1 %>% mutate(type = "(1)linear(ratio>14)")

#Aggregate
ratio.df <- bind_rows(df.ratio.1,df.ratio.2,df.ratio.3)
ggplot(ratio.df, aes(iter,mean.prop)) + geom_area(aes(fill = type))+
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1,0.2))+
  scale_fill_manual(labels = c("Linear(ratio>15)","Anisotropic(2<ratio<15)","Isotropic(1<ratio<2)"),
                    values = c("#CC6666", "#9999CC", "#66CC99"))+
  theme_bw()+
  theme(text = element_text(size= 20),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        legend.title = element_blank(),
        legend.position = "none")


#Statistical test on slope
prop_lm_1 = prop.result.1 %>% lm(formula = mean.prop~iter)
confint(prop_lm_1)
prop_lm_2 = prop.result.2[-(1:5),]%>% lm(formula = mean.prop~iter)
confint(prop_lm_2)
prop_lm_3 = prop.result.3 %>% lm(formula = mean.prop~iter)
confint(prop_lm_3)

#only anisotropic and linear
# anisotropic
threshold1=1
threshold2=14

prop.result.3<-ratio%>%
  group_by(seed,iter)%>%summarise(prop=count.ratio.2(ratio,threshold1,threshold2))%>%
  group_by(iter)%>%summarise(mean.prop=mean(prop),se.prop=sd(prop)/sqrt(10))

df.ratio.3<-data.frame(iter=0:25,mean.prop=prop.result.3$mean.prop,prop.se=prop.result.3$se.prop)
df.ratio.3<-df.ratio.3 %>% mutate(type = "(2)Anisotropic(2.6<ratio<14)")

# linear
threshold1 = 14
threshold2 = 100000000

prop.result.1<-ratio%>%
  group_by(seed,iter)%>%summarise(prop=count.ratio.2(ratio,threshold1,threshold2))%>%
  group_by(iter)%>%summarise(mean.prop=mean(prop),se.prop=sd(prop)/sqrt(10))

df.ratio.1<-data.frame(iter=0:25,mean.prop=prop.result.1$mean.prop,prop.se=prop.result.1$se.prop)
df.ratio.1 <- df.ratio.1 %>% mutate(type = "(1)linear(ratio>14)")
# aggregate
ratio.df <- bind_rows(df.ratio.1,df.ratio.3)
ggplot(ratio.df, aes(iter,mean.prop)) + geom_area(aes(fill = type))+
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1,0.2))+
  scale_fill_manual(labels = c("Linear(ratio>15)","Anisotropic(2<ratio<15)"),
                    values = c("#CC6666", "#9999CC"))+
  theme_bw()+
  theme(text = element_text(size= 20),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        legend.title = element_blank(),
        legend.position = "none")


#Chain-specific analysis
ratio = ratio %>% mutate(log_ratio = log(ratio),
                         group = sprintf("%d%d%s",seed,chain,color))
isotropic_group = ratio %>% filter(iter==0,ratio<1.5) %>% pull(group) %>% unique()
anisotropic_group = ratio %>% filter(iter==0,ratio>=1.5, ratio<10) %>% pull(group) %>% unique()
linear_group = ratio %>% filter(iter==0,ratio>=10) %>% pull(group) %>% unique()
ratio_diff = ratio %>% group_by(group) %>% summarise(ratio_diff=ratio[26]-ratio[1]) %>% mutate(null = 0)

isotropic_diff = ratio_diff[ratio_diff$group%in%isotropic_group,]
t.test(isotropic_diff$ratio_diff,isotropic_diff$null)
hist(isotropic_diff$ratio_diff)

anisotropic_diff = ratio_diff[ratio_diff$group%in%anisotropic_group,]
t.test(anisotropic_diff$ratio_diff,anisotropic_diff$null)

hist(anisotropic_diff$ratio_diff)

linear_diff = ratio_diff[ratio_diff$group%in%linear_group,]
t.test(linear_diff$ratio_diff,linear_diff$null)

hist(linear_diff$ratio_diff)

# 40/300 = 13.33
ratio %>% filter(group%in%isotropic_group) %>% split(.$group) %>% map(~lm(log_ratio~iter,data=.x)) %>% map_df(tidy) %>% filter(term=="iter") %>%
  pull(estimate) %>% mean()

# 230/300
ratio %>% filter(group%in%anisotropic_group) %>% split(.$group) %>% map(~lm(log_ratio~iter,data=.x)) %>% map_df(tidy) %>% filter(term=="iter") %>%
  pull(estimate) %>% mean()

# 10%
ratio %>% filter(group%in%linear_group) %>% split(.$group) %>% map(~lm(log_ratio~iter,data=.x)) %>% map_df(tidy) %>% filter(term=="iter") %>%
  pull(estimate) %>% mean()

#Pearson correlation (No Good!)
tb.result_color %>% group_by(iter,color) %>% summarise(mean_cor = mean(abs(correlation)), se_cor = sd(correlation)/10) %>% ggplot(aes(x=iter,y=mean_cor,color=color))+
  geom_point() + geom_line() +
  geom_errorbar(aes(ymin=mean_cor-se_cor,ymax=mean_cor+se_cor),width=0.4,size=0.9)

#orientation analysis

#Vector sum
#filter the iterations that have 3 anisotropic (including linear) color groups
#Add label column to tb.result_color 
tb.result_color = tb.result_color %>% add_column(label = paste(.$seed,.$chain,.$iter))

#identify iterations that has at least 2 anisotropic groups

n_anisotropic = tb.result_color %>% group_by(seed,chain,iter) %>% summarise(n_anisotropic = sum(ratio>2))
# n_anisotropic = tb.result_color %>% group_by(seed,chain,iter) %>% summarise(n_anisotropic = sum(ratio>=1))

#Add label column
n_anisotropic = n_anisotropic %>% add_column(label = paste(.$seed,.$chain,.$iter))


#Pull out the labels of which the color groups have at least 2 anisotropic groups
selected_label = n_anisotropic %>% filter(n_anisotropic>2) %>% pull(label)

#Select them in the tb.result_color
anisotropic_groups_3 = tb.result_color[tb.result_color$label%in%(selected_label),]

#Run the min_angle function on these iteration, obtain the max vector sum
vec_sum_max = anisotropic_groups_3 %>% group_by(seed,chain,iter) %>% select(eigenvec) %>% group_map(~{min_angle(.)}) %>% unlist()

#add the vec_sum_max to the n_anisotropic df

vec_sum_df = n_anisotropic %>% filter(n_anisotropic>2) %>% add_column(vec_sum_max)

#summary of vec_sum_df
summary_vec_sum = vec_sum_df %>% filter(iter>0) %>% group_by(iter) %>% summarise(n=n(),mean_sum = mean(vec_sum_max), se_sum = sd(vec_sum_max)/sqrt(n))

# Sampling distribution by simulation
sample_size_list = summary_vec_sum$n
mean_rd_sum_list = list()
count = 1
for(n in sample_size_list){
  mean_rd_sum = rep(0,1000)
  for(i in 1:1000){
    mean_rd_sum[i] = sampling_sum(n)
  }
  mean_rd_sum_list[[count]] = mean_rd_sum
  count = count + 1
}

save(mean_rd_sum_list, file = "mean_rd_sum_2.Rdata")
load("mean_rd_sum_2.Rdata")
# load("mean_rd_sum.Rdata_4.4.Rdata")
# load("mean_rd_sum.Rdata_14.Rdata")
#Compute CI
# mean_ci = get_mean_ci(mean_rd_sum)
# ci_lower = mean_ci$ci_lower
# ci_upper = mean_ci$ci_upper

mean_ci_list = mean_rd_sum_list %>% lapply(function(x) get_mean_ci(x))
means = mean_ci_list %>% lapply(function(x) return(x$mean_sampling)) %>% unlist()
ci_lower = mean_ci_list %>% lapply(function(x) return(x$ci_lower)) %>% unlist()
ci_upper = mean_ci_list %>% lapply(function(x) return(x$ci_upper)) %>% unlist()

summary_vec_sum_ci = summary_vec_sum %>% mutate(ci_lower = ci_lower, ci_upper = ci_upper)
summary_vec_sum_ci= summary_vec_sum_ci %>% mutate(sig=ifelse(mean_sum>ci_upper,1,0))

#Analyze the original seed
# vec_sum_df_0 = vec_sum_df %>% filter(iter==0,chain==0)
# summary_vec_sum = vec_sum_df %>% filter(iter>0) %>% group_by(iter) %>% summarise(n=n(),mean_sum = mean(vec_sum_max), se_sum = sd(vec_sum_max)/sqrt(n))
# 
# rd_sum_0 = rep(0,1000)
# for(i in 1:1000){
#   rd_sum_0[i] = sampling_sum(7)
# }
# mean_0 = mean(rd_sum_0)
# ci_lower_0 = sort(rd_sum_0)[25]
# ci_upper_0 = sort(rd_sum_0)[975]
# summary_vec_sum_ci_0 = data.frame(iter=0,n=7,mean_sum = mean(vec_sum_df_0$vec_sum_max), se_sum = sd(vec_sum_df_0$vec_sum_max)/sqrt(7),ci_lower = ci_lower_0, ci_upper = ci_upper_0, sig = 0)
# 
# summary_vec_sum_all = rbind(summary_vec_sum_ci_0, summary_vec_sum_ci)

#Plot
summary_vec_sum_ci %>% ggplot()+
  geom_point(aes(x=iter,y=mean_sum,color = as.factor(sig)),size=4)+
  geom_errorbar(aes(iter,ymin=mean_sum-se_sum, ymax=mean_sum+se_sum, color = as.factor(sig)),width=0.5,size=1.1)+
  scale_color_manual(values = c("0" = "red","1" = "green"))+
  geom_ribbon(aes(x=iter,ymin = ci_lower, ymax = ci_upper),
              alpha = 1/5,
              fill = "blue")+
  geom_smooth(aes(x=iter,y=mean_sum),method = "lm", se = F,color="blue",linetype="dashed")+
  scale_y_continuous(limits = c(0.8,1))+
  theme_bw()+
  theme(text = element_text(size= 20),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        legend.title = element_blank(),
        legend.position = "none")

#last 10 iterations
# cutoffs: 2,5,26.6
n_anisotropic = tb.result_color %>% group_by(seed,chain,iter) %>% summarise(n_anisotropic = sum(ratio>2))
# n_anisotropic = tb.result_color %>% group_by(seed,chain,iter) %>% summarise(n_anisotropic = sum(ratio>=1))

#Add label column
n_anisotropic = n_anisotropic %>% add_column(label = paste(.$seed,.$chain,.$iter))

#Pull out the labels of which the color groups have at least 2 anisotropic groups
selected_label = n_anisotropic %>% filter(n_anisotropic>2) %>% pull(label)

#Select them in the tb.result_color
anisotropic_groups_3 = tb.result_color[tb.result_color$label%in%(selected_label),] 

#Run the min_angle function on these iteration, obtain the max vector sum
vec_sum_max = anisotropic_groups_3 %>% group_by(seed,chain,iter) %>% select(eigenvec) %>% group_map(~{min_angle(.)}) %>% unlist()

log_ratio_vec_sum = anisotropic_groups_3 %>% group_by(seed,chain,iter) %>% summarise(log_mean_ratio = log(mean(ratio))) %>% add_column(vec_sum_max = vec_sum_max)

ratio_vec_sum = anisotropic_groups_3 %>% group_by(seed,chain,iter) %>% summarise(mean_ratio = mean(ratio)) %>% add_column(vec_sum_max = vec_sum_max)

#ntile()/cut()
log_ratio_vec_sum = log_ratio_vec_sum %>% add_column(groups=(cut(log_ratio_vec_sum$log_mean_ratio,
                                             breaks = quantile(log_ratio_vec_sum$log_mean_ratio,prob=seq(0,1,1/5)),
                                             # labels = 1:5,
                                             include.lowest = TRUE)))

ratio_vec_sum = ratio_vec_sum %>% add_column(groups=(cut(ratio_vec_sum$mean_ratio,
                                                                 breaks = quantile(ratio_vec_sum$mean_ratio,prob=seq(0,1,1/5)),
                                                                 # labels = 1:5,
                                                                 include.lowest = TRUE)))


# library(trend)
# mk.test()
# cor.test(log_ratio_vec_sum$log_mean_ratio,log_ratio_vec_sum$vec_sum_max, method = "kendall")
# cor.test(log_ratio_vec_sum$log_mean_ratio,log_ratio_vec_sum$vec_sum_max, method = "spearman")

summary_ratio_vec_sum=ratio_vec_sum %>% 
  group_by(groups) %>% 
  summarise(n = n(),mean_vec_sum_max = mean(vec_sum_max),se_vec_sum_max = sd(vec_sum_max)/sqrt(n))
mean_rd_sum = rep(0,10000)
for(i in 1:10000){
  mean_rd_sum[i] = sampling_sum(343)
}

mean_ci = get_mean_ci(mean_rd_sum)
ci_lower = mean_ci$ci_lower
ci_upper = mean_ci$ci_upper
summary_ratio_vec_sum = summary_ratio_vec_sum %>% mutate(mean_ci = mean_ci$mean_sampling,
                                 ci_lower = ci_lower,
                                 ci_upper = ci_upper)
summary_ratio_vec_sum %>% ggplot(aes(x=groups,y=mean_vec_sum_max))+geom_point(size=4) +
  geom_errorbar(aes(groups,ymin=mean_vec_sum_max-se_vec_sum_max, ymax=mean_vec_sum_max+se_vec_sum_max),width=0.3,size=1.1)+
  geom_line(aes(x=as.numeric(groups),y=mean_ci),size=1.5,linetype="dashed",color="red")+
  geom_ribbon(aes(x=as.numeric(groups),ymin = ci_lower, ymax = ci_upper),
              alpha = 1/5,
              fill = "blue")+
  scale_y_continuous(limits = c(0.8,1))+
  scale_x_discrete(labels = c("1","2","3","4","5"))+
  theme_bw()+
  theme(text = element_text(size= 20),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        legend.title = element_blank(),
        legend.position = "none")



#add the vec_sum_max to the n_anisotropic df
vec_sum_df = n_anisotropic %>% filter(n_anisotropic>2) %>% add_column(vec_sum_max)

#summary of vec_sum_df
summary_vec_sum_2 = vec_sum_df %>% group_by(iter) %>% summarise(n=n(),mean_sum = mean(vec_sum_max), se_sum = sd(vec_sum_max)/sqrt(n))
summary_vec_sum_5 = vec_sum_df %>% group_by(iter) %>% summarise(n=n(),mean_sum = mean(vec_sum_max), se_sum = sd(vec_sum_max)/sqrt(n))
summary_vec_sum_26.6 = vec_sum_df %>% group_by(iter) %>% summarise(n=n(),mean_sum = mean(vec_sum_max), se_sum = sd(vec_sum_max)/sqrt(n))
summary_vec_sum_combined = rbind(summary_vec_sum_2,summary_vec_sum_5,summary_vec_sum_26.6) %>% mutate(type = rep(c(">2",">5",">26.6"),each=10))
summary_vec_sum_combined %>% ggplot(aes(x=iter,y=mean_sum,color=as.factor(type)))+
  geom_point(size=4)+
  geom_line(size=1.5)+
  geom_errorbar(aes(iter,ymin=mean_sum-se_sum, ymax=mean_sum+se_sum),width=0.5,size=1.1)+
  scale_color_manual(values = c(">2" = "red",">5" = "green",">26.6" = "blue"))+
  # geom_smooth(aes(x=iter,y=mean_sum),method = "lm", se = F,color="blue",linetype="dashed")+
  scale_y_continuous(limits = c(0.85,1))+
  scale_x_continuous(breaks = seq(16,25,1))+
  theme_bw()+
  theme(text = element_text(size= 20),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        legend.title = element_blank())+
  theme(legend.position = c(0.87, 0.13))

#Statistical Test
lm_vec_sum = summary_vec_sum_all %>% lm(formula = mean_sum~iter)
lm_vec_sum
confint(lm_vec_sum)



#Regularity analysis
## ratio > 14 linear patterns
paired.dist_sd_linear<-tb.result_color%>%filter(ratio > 14, iter > 0)%>%
  .$proj.x%>%lapply(dist.mat)%>%
  lapply(., function(x) diag(x[-nrow(x),-1]))%>%
  lapply(sd)

##Mean_dist_sd dataframe for linear patterns
linear_paired_sd = tb.result_color%>%filter(ratio>14, iter > 0)
linear_paired_sd = linear_paired_sd %>% add_column(dist_sd = unlist(paired.dist_sd_linear))
mean_dist_sd_linear = linear_paired_sd %>% group_by(iter) %>% summarise(n = n(), mean_dist_sd = mean(dist_sd), se_dist_sd = sd(dist_sd)/sqrt(n))

#Statistical test
lm_dist_sd = mean_dist_sd_linear %>% lm(formula = mean_dist_sd~iter)
lm_dist_sd
confint(lm_dist_sd)


#MC simulation for sampling distribution of mean of the standard deviations conditioned on the sample size for each iteration
sample_size_list_linear = mean_dist_sd_linear$n
#Simulations
mean_rd_sd_list_linear = list()
count = 1
for(n in sample_size_list_linear){
  mean_rd_sd_linear = rep(0,10000)
  for(i in 1:10000){
    mean_rd_sd_linear[i] = uniform_null(n)
  }
  mean_rd_sd_list_linear[[count]] = mean_rd_sd_linear
  count = count + 1
}

#save
# save(mean_rd_sd_list_linear, file = "mean_rd_sd_list_linear.Rdata")
load("mean_rd_sd_list_linear.Rdata")
#for linear groups
mean_ci_list_linear = mean_rd_sd_list_linear %>% lapply(function(x) get_mean_ci(x))
means_linear = mean_ci_list_linear %>% lapply(function(x) return(x$mean_sampling)) %>% unlist()
ci_lower_linear = mean_ci_list_linear %>% lapply(function(x) return(x$ci_lower)) %>% unlist()
ci_upper_linear = mean_ci_list_linear %>% lapply(function(x) return(x$ci_upper)) %>% unlist()

#summary for linear groups
summary_dist_sd_ci_linear = mean_dist_sd_linear %>% mutate(mean_sampling = means_linear, ci_lower = ci_lower_linear, ci_upper = ci_upper_linear)
summary_dist_sd_ci_linear = summary_dist_sd_ci_linear %>% mutate(sig=ifelse(mean_dist_sd<ci_lower,1,0))

#analyze on the original seed with linear groups
paired.dist_sd_0_linear<-tb.result_color%>%filter(ratio > 14,chain==0,iter==0)%>%
  .$proj.x%>%lapply(dist.mat)%>%
  lapply(., function(x) diag(x[-nrow(x),-1]))%>%
  lapply(sd)

linear_paired_sd_0 = tb.result_color%>%filter(ratio>14,chain==0,iter==0)
linear_paired_sd_0 = linear_paired_sd_0 %>% add_column(dist_sd = unlist(paired.dist_sd_0_linear))
mean_dist_sd_0_linear = linear_paired_sd_0 %>% group_by(iter) %>% 
  summarise(n = n(), mean_dist_sd = mean(dist_sd), se_dist_sd = sd(dist_sd)/sqrt(n))

#Simulation
sample_size_0_linear = mean_dist_sd_0_linear$n
rd_sd_0_linear = rep(0,10000)
for(i in 1:10000){
  rd_sd_0_linear[i] = uniform_null(sample_size_0_linear)
}

mean_0_linear = mean(rd_sd_0_linear)
ci_lower_0_linear = sort(rd_sd_0_linear)[250]
ci_upper_0_linear = sort(rd_sd_0_linear)[9750]
mean_dist_sd_0_linear=mean_dist_sd_0_linear %>% mutate(mean_sampling = mean_0_linear, ci_lower = ci_lower_0_linear, ci_upper = ci_upper_0_linear, sig = 0) 

#Summary
summary_dist_sd_ci_all_linear = rbind(mean_dist_sd_0_linear,summary_dist_sd_ci_linear)

#Plot 
summary_dist_sd_ci_all_linear %>% ggplot(aes(x=iter,y=mean_dist_sd))+
  geom_point(aes(x=iter,y=mean_dist_sd, color = as.factor(sig)),size=4)+
  geom_errorbar(aes(ymin=mean_dist_sd-se_dist_sd,ymax=mean_dist_sd+se_dist_sd,color = as.factor(sig)),width=0.5,size=1.1)+
  scale_color_manual(values = c("0" = "red","1" = "green"))+
  geom_ribbon(aes(x=iter,ymin = ci_lower, ymax = ci_upper),
              alpha = 1/5,
              fill = "blue")+
  theme_bw()+
  geom_smooth(method = "lm", se = F, linetype = "dashed")+
  scale_y_continuous(limits = c(0,0.31))+
  theme(text = element_text(size= 20),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        legend.title = element_blank(),
        legend.position = "none")
