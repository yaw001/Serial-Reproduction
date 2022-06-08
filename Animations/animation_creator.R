library(tidyverse)
library(gganimate)
theme_set(theme_bw())

setwd('/Users/young/Desktop/UCSD/Research/VWM_Iterated_Learning/Homogenous_dots/Data')
# setwd('/home/yaw001/iterated_learning_project/Data')

load('all.data_memory.Rdata')
all.data = all.data_memory
all.data$x = all.data$x/275
all.data$y = all.data$y/275

tb.result_memory<-all.data%>%
  group_by(Seed,Chain,Iter)%>% 
  do(tibble(xy = list(data.frame(x=.$x, y=.$y)),
            covariance=list(cov(cbind(.$x,.$y)))))%>%
  rowwise()%>%
  mutate(eigenval = list(eigen(covariance)),
         det = det(covariance),
         ratio = eigenval$values[1]/eigenval$values[2],
         proj.x = list(as.matrix(xy)%*%eigenval$vectors[,1]))

p <- all.data %>% 
  ggplot(aes(x=x, y=y))+
  facet_grid(Chain~Seed)+
  geom_point(size=1)+
  theme_bw()+
  coord_cartesian(xlim=c(-1,1), ylim=c(-1,1))+
  theme(axis.text = element_blank(),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        strip.text = element_blank(),
        legend.position = 'none')

p_anim = p + transition_states(states = Iter,
                               transition_length = 0,
                               state_length = 1)+
  labs(title = 'Iteration: {closest_state}')
p_anim

p_first <- all.data %>% filter(Iter==1) %>% 
  ggplot(aes(x=x, y=y))+
  facet_grid(Chain~Seed)+
  geom_point(size=1)+
  theme_bw()+
  coord_cartesian(xlim=c(-1,1), ylim=c(-1,1))+
  theme(axis.text = element_blank(),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        # strip.text = element_blank(),
        legend.position = 'none')

p_last <- all.data %>% filter(Iter==20) %>% 
  ggplot(aes(x=x, y=y))+
  facet_grid(Chain~Seed)+
  geom_point(size=1)+
  theme_bw()+
  coord_cartesian(xlim=c(-1,1), ylim=c(-1,1))+
  theme(axis.text = element_blank(),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        strip.text = element_blank(),
        legend.position = 'none')

anim_save("homogenous_memory.gif", animate(p_anim, nframes = 100, fps = 5, duration = 20, end_pause = 20, rewind = FALSE))

load("all.data.Rdata")
all.data$x = all.data$x/275
all.data$y = all.data$y/275
tb.result_motor<-all.data%>%
  group_by(Seed,Chain,Iter)%>%
  do(tibble(xy = list(data.frame(x=.$x, y=.$y)),
            covariance=list(cov(cbind(.$x,.$y)))))%>%
  rowwise()%>%
  mutate(eigenval = list(eigen(covariance)),
         det = det(covariance),
         ratio = eigenval$values[1]/eigenval$values[2],
         proj.x = list(as.matrix(xy)%*%eigenval$vectors[,1]))

p_motor <- all.data %>% 
  ggplot(aes(x=x, y=y))+
  facet_grid(Chain~Seed)+
  geom_point(size=1)+
  theme_bw()+
  coord_cartesian(xlim=c(-1,1), ylim=c(-1,1))+
  theme(axis.text = element_blank(),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        strip.text = element_blank(),
        legend.position = 'none')

p_motor_anim = p_motor + transition_states(states = Iter,
                               transition_length = 0,
                               state_length = 1)+
  labs(title = 'Iteration: {closest_state}')
p_motor_anim

anim_save("homogenous_motor.gif", animate(p_motor_anim, nframes = 100, fps = 5, duration = 20, end_pause = 20, rewind = FALSE))

setwd('/Users/young/Desktop/UCSD/Research/VWM_Iterated_Learning/Colored_dots/Data')
load('all.data_color.Rdata')
all.data<-filter(all.data,iter<=25)

p_color <- all.data %>% filter(iter <= 25) %>%
  ggplot(aes(x=x, y=y, color=color))+
  facet_grid(chain~seed)+
  geom_point(size=1)+
  theme_bw()+
  scale_color_manual(values=c("red"="red",
                              "green"="#00aa00",
                              "blue"="blue"))+
  coord_cartesian(xlim=c(-1,1), ylim=c(-1,1))+
  theme(axis.text = element_blank(),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        strip.text = element_blank(),
        legend.position = 'none')

p_color_first <- all.data %>% filter(iter == 25) %>%
  ggplot(aes(x=x, y=y, color=color))+
  facet_grid(chain~seed)+
  geom_point(size=1)+
  theme_bw()+
  scale_color_manual(values=c("red"="red",
                              "green"="#00aa00",
                              "blue"="blue"))+
  coord_cartesian(xlim=c(-1,1), ylim=c(-1,1))+
  theme(axis.text = element_blank(),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        strip.text = element_blank(),
        legend.position = 'none')

p_color_anim = p_color + transition_states(states = iter,
                                           transition_length = 0,
                                           state_length = 1)+
  labs(title = 'Iteration: {closest_state}')

anim_save("color_memory.gif", animate(p_color_anim, nframes = 125, fps = 5, duration = 25, end_pause = 20, rewind = FALSE))



