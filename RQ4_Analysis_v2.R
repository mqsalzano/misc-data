
#### load packages ####
library(readxl)
library(tidyverse)
library(ggplot2)
library(tidyr)
library(reshape2)
library(extrafont)
library(ggpubr)
library(lmerTest)
library(lme4)
library(car)
library(carData)
library(ggcorrplot)

#### load and extract data ####
masterfolder = "D:/UMass - HD/Brooks/WatchData/Mobile_Lab_Data_UMass"
setwd(paste0(masterfolder, "/Analysis"))
savefolder = paste0(getwd(), '')

runmetrics = as.data.frame(read.csv(paste0(masterfolder, '/runmetricsV3.csv')))
names(runmetrics)
str(runmetrics)

runmetrics$member_id = factor(runmetrics$member_id)
runmetrics$activity_id = factor(runmetrics$activity_id)
runmetrics$task_id = factor(runmetrics$task_id)
runmetrics$segment_name = factor(runmetrics$segment_name)
runmetrics$Persona_group = factor(runmetrics$Persona_group)

grid_theme = plot_theme =   theme(#text = element_text(family = "Calibri", face = "bold"),
  panel.border = element_blank(), 
  panel.grid.major = element_blank(),
  panel.background = element_blank(), 
  plot.title = element_text(face = "bold", size = 18, color = "black", hjust = 0.5),
  axis.text.x = element_text(face = "bold", size = 12, color = "black"),
  axis.title.x = element_text(face = "bold", size = 14, color = "black"),
  axis.title.y = element_text(face = "bold", size = 14, color = "black"),
  axis.text.y = element_text(face = "bold", size = 12, color = "black"))

#subset data
#predictors of interest: member_id(2), Persona_group(48), task_id(49), 
#   segment_name(50), converted_times(51), average_velocity_running(18)
#runmetric variables: average_kvert_running(21), average_stride_length_running(22),
#   average_cadence_running(19), average_gct_running(20), average_power_run(17)

#### plotting kvert for each segment type ####
kvert_data = runmetrics[,c(21,48,50,51)]
summary(kvert_data)
ggplot(data = kvert_data, aes(x = converted_times, y = average_kvert_running, color = segment_name)) +
  geom_point() #+ theme(legend.position = 'none')
kvert_lm = lm(average_kvert_running ~., data = kvert_data)
unqTasks = unique(kvert_data$segment_name)
kvert_plot_list = vector(mode = "list", length= length(unqTasks))
kvert_count_df = data.frame()

for (i in 1:length(unqTasks)) {
  
  segment = unqTasks[i]
  tmpdata = kvert_data[which(kvert_data$segment_name == segment),]
  kvert_plot_list[[i]] = ggplot(data = tmpdata, aes(x = converted_times, y = average_kvert_running, color = Persona_group)) +
    geom_point() + scale_color_manual(values = c('black', 'grey','blue','yellow'))+
    geom_smooth(method='lm', formula= y ~ x, color = 'red')+
    scale_x_continuous(limits = c(0, 24), breaks = seq(0, 24,6))+
    grid_theme  +
    labs(x = 'Hour in Day', y = 'kvert', title = segment) 
  kvert_count_df[i,1] = segment
  kvert_count_df[i,2] = nrow(tmpdata)
    
}

print(ggarrange(plotlist = kvert_plot_list, nrow= ceiling(sqrt(length(unqTasks))), ncol = ceiling(sqrt(length(unqTasks)))))

#### checking fun run data only ####
data_check = runmetrics[which(runmetrics$segment_name == 'fun run'),c(2,9,10,17:22,48,50,51)]
str(data_check)


#### subsetting fun run data to exclude values that don't make sense ####
#getting rid of following:
dist = which(data_check$total_distance > 1609.34 & data_check$total_distance < 25000) #1 mile and ~15.5 miles
duration = which(data_check$duration > 600 & data_check$duration < 10800) #10 minutes and 3 hours
vel = which(data_check$average_velocity_running < 5.95) #13.3 mph pace - min is 1.48 m/s pace
cadence = which(data_check$average_cadence_running > 100 & data_check$average_cadence_running < 250)
gct = which(data_check$average_gct_running > 165)# & data_check$average_gct_running < 300)
kvert = which(data_check$average_kvert_running > 55 & data_check$average_kvert_running < 12)
time = which(data_check$converted_times >4.49 & data_check$converted_times < 22.5)

common1 = intersect(dist,duration)
common2 = intersect(gct, cadence)
common3 = intersect(time, vel)

clean_data = data_check[intersect(intersect(common1,common2),common3),]
clean_data$duration = clean_data$duration/60


View(clean_data)

#### adding a new variable called "time_diff" which zeroes every runner's time at their earliest run time ####
unqID = unique(clean_data$member_id)

clean_data$time_diff = 0


for (i in 1:length(unqID)) {
  
  temp_id = unqID[i]
  id_loc = which(clean_data$member_id == temp_id)
  clean_data$time_diff[id_loc] = clean_data$converted_times[id_loc] -
    min(clean_data$converted_times[id_loc])
  
}

#### check correlations amongst predictors ####
#using converted_times
cor.data = clean_data[,c(2,3,4,5,12)]
names(cor.data)= c('Distance', 'Duration', 'Power', 'Velocity', 'Start Time')
cor1= round(cor(cor.data, use="complete.obs"),2)

Cor1Plot = ggcorrplot(cor1, lab = TRUE, lab_size = 6, show.legend = FALSE, type = "upper") +
 grid_theme + theme(axis.text.x = element_text(size = 16), axis.text.y = element_text(size = 16),
   axis.title.x = element_blank(), axis.title.y = element_blank())
print(Cor1Plot)
ggsave(paste0(savefolder, '/CorrelationPlot_RQ4.png'), plot = Cor1Plot, height = 6, width = 6)

#### plots ####

#### kvert plots ####
tmpkvert = clean_data[, c(1,7,9,11)]
kvert.time.lm = lm(average_kvert_running~converted_times, data = tmpkvert)
tmpkvert$fitted_vals = kvert.time.lm$fitted.values

kvert.time.lmm = lmer(average_kvert_running~converted_times + (1|member_id), data = tmpkvert)
tmpkvert$lmm_fit = fitted(kvert.time.lmm)

round(summary(kvert.time.lm)$adj.r.squared, digits = 3)
summary(kvert.time.lmm)
MuMIn::r.squaredGLMM(kvert.time.lmm)


simple.kvert.plot = ggplot() +
  geom_point(data = tmpkvert, aes(x = converted_times, 
                                  y = average_kvert_running), color = 'black') +
  geom_line(data = tmpkvert, aes(x = converted_times, 
                                 y = fitted_vals), color = 'red') + 
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0)+
  grid_theme + labs(x = 'Time', y = 'Kvert')
print(simple.kvert.plot)
ggsave(filename = paste0(savefolder, '/SimpleKvertTime.png'), simple.kvert.plot, height = 4, width = 6)

lmm.kvert.plot = ggplot() +
  geom_point(data = tmpkvert, aes(x = converted_times, 
                                  y = average_kvert_running), color = 'black') +
  geom_line(data = tmpkvert, aes(x = converted_times, 
                                 y = lmm_fit, group = member_id), color = 'red') + 
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0)+
  grid_theme + labs(x = 'Time', y = 'Kvert')
print(lmm.kvert.plot)
ggsave(filename = paste0(savefolder, '/LmmKvertTime.png'), lmm.kvert.plot, height = 4, width = 6)

kvert.vel.plotlist = vector(mode = 'list', length = 4)

unqPersona = sort(unique(clean_data$Persona_group))
dark.red = rgb(192/255, 80/255, 77/255)
mid.blue = rgb(79/255,129/255,189/255)
dark.yellow = rgb(255/255,192/255,0/255)
light.grey = rgb(191/255, 191/255, 191/255)
jade.green = rgb(155/255,187/255,89/255)
colorlist = c(dark.red, mid.blue, dark.yellow, jade.green)
linecolor = c('black', 'black', 'black', 'black')
for (grp in 1:4) {
  tmpdata = clean_data[which(clean_data$Persona_group == unqPersona[grp]),]
  tmp.lm = lm(average_kvert_running~I(log(average_velocity_running)), data = tmpdata)
  tmpdata$fitvals = tmp.lm$fitted.values
  kvert.vel.plotlist[[grp]] = ggplot() +
            geom_point(data = tmpdata, aes(x = average_velocity_running,
                                           y = average_kvert_running),
                       color = colorlist[grp]) +
            geom_line(data = tmpdata, aes(x = average_velocity_running,
                                          y = fitvals),
                      color = linecolor[grp]) +
    scale_y_continuous(limits = c(0,90)) +
    scale_x_continuous(limits = c(0,7)) +
    geom_hline(yintercept = 0) + geom_vline(xintercept = 0)+
            grid_theme + labs(title = unqPersona[grp], x = 'Velocity', y = 'KVert')
}
Kvert.Vel.Plots = ggarrange(plotlist = kvert.vel.plotlist, nrow = 2, ncol = 2)
print(Kvert.Vel.Plots)
ggsave(filename = paste0(savefolder, '/KvertVelocityPlots.png'), 
       plot = Kvert.Vel.Plots, height = 6, width = 10)

kvert.ConvTime.plotlist = vector(mode = 'list', length = 4)

for (grp in 1:4) {
  tmpdata = clean_data[which(clean_data$Persona_group == unqPersona[grp]),]
  tmp.lm = lm(average_kvert_running~converted_times, data = tmpdata)
  tmpdata$fitvals = tmp.lm$fitted.values
  kvert.ConvTime.plotlist[[grp]] = ggplot() +
    geom_point(data = tmpdata, aes(x = converted_times,
                                   y = average_kvert_running),
               color = colorlist[grp]) +
    geom_line(data = tmpdata, aes(x = converted_times,
                                  y = fitvals),
              color = linecolor[grp]) +
    scale_y_continuous(limits = c(0,90)) +
    scale_x_continuous(limits = c(0,24))+
    geom_hline(yintercept = 0) + geom_vline(xintercept = 0)+
  grid_theme + labs(title = unqPersona[grp], x = 'Time', y = 'KVert')
}
Kvert.ConvTime.Plots = ggarrange(plotlist = kvert.ConvTime.plotlist, nrow = 2, ncol = 2)
print(Kvert.ConvTime.Plots)
ggsave(filename = paste0(savefolder, '/KvertConvTimePlots.png'),
       plot = Kvert.ConvTime.Plots, height = 6, width = 10)

kvert.duration.plotlist = vector(mode = 'list', length = 4)

for (grp in 1:4) {
  tmpdata = clean_data[which(clean_data$Persona_group == unqPersona[grp]),]
  tmp.lm = lm(average_kvert_running~duration, data = tmpdata)
  tmpdata$fitvals = tmp.lm$fitted.values
  kvert.duration.plotlist[[grp]] = ggplot() +
    geom_point(data = tmpdata, aes(x = duration,
                                   y = average_kvert_running),
               color = colorlist[grp]) +
    geom_line(data = tmpdata, aes(x = duration,
                                  y = fitvals),
              color = linecolor[grp]) +
    scale_y_continuous(limits = c(0,90)) +
    geom_hline(yintercept = 0) + geom_vline(xintercept = 0)+
    #scale_x_continuous(limits = c(0,24))+
    grid_theme + labs(title = unqPersona[grp], x = 'Duration', y = 'KVert')
}
Kvert.duration.Plots = ggarrange(plotlist = kvert.duration.plotlist, nrow = 2, ncol = 2)
print(Kvert.duration.Plots)
ggsave(filename = paste0(savefolder, '/KvertDurationPlots.png'),
       plot = Kvert.duration.Plots, height = 6, width = 10)

#ggplot(data = clean_data, aes(x = Persona_group, y = average_velocity_running)) + 
#  geom_boxplot() + grid_theme + labs(x = 'Persona', y = 'Avg. Velocity')


kvert.log.mod = lm(average_kvert_running~I(log(average_velocity_running))*Persona_group, data = clean_data)

clean_data$KvertLogFit = kvert.log.mod$fitted.values

####  Kvert models: data not scaled; converted_times used ####
#run mixed model without interaction
kvert.lmm.full = lmer(average_kvert_running~
                                         I(log(average_velocity_running))+
                                         Persona_group +
                                         converted_times +
                                         duration + 
                                         (1|member_id), 
                                       data = clean_data)

clean_data$kvert.full.fit = fitted(kvert.lmm.full)

summary(kvert.lmm.full)

MuMIn::r.squaredGLMM(kvert.lmm.full)

#run mixed model without persona group
kvert.lmm.noGrp = lmer(average_kvert_running~
                        I(log(average_velocity_running))+
                        converted_times +
                        duration + 
                        (1|member_id), 
                      data = clean_data)

clean_data$kvert.noGrp.fit = fitted(kvert.lmm.noGrp)

summary(kvert.lmm.noGrp)

MuMIn::r.squaredGLMM(kvert.lmm.noGrp)

#run mixed model with time of day only
kvert.lmm.time = lmer(average_kvert_running~
                         converted_times +
                         (1|member_id), 
                       data = clean_data)

clean_data$kvert.time.fit = fitted(kvert.lmm.time)

summary(kvert.lmm.time)

MuMIn::r.squaredGLMM(kvert.lmm.time)

#
anova(kvert.lmm.full, kvert.lmm.noGrp, kvert.lmm.time)

#plot actual values vs. predicted values
plot.kvert.full = ggplot(data = clean_data, aes(x = average_kvert_running,
                                                   y = kvert.full.fit,
                                                   color = Persona_group)) + 
  geom_point() +
  scale_color_manual(values = colorlist) +
  grid_theme + theme(legend.position = 'none') +
  labs(y = 'Predicted', x = 'Actual', title = 'Kvert vs. Time of Day - Full Model')
print(plot.kvert.full)
ggsave(filename = paste0(savefolder, '/KvertFittedFullPlot.png'), plot.kvert.full,height = 4, width = 6)


plot.kvert.noGrp = ggplot(data = clean_data, aes(x = average_kvert_running,
                                                    y = kvert.noGrp.fit)) + 
  geom_point() +
  #scale_color_manual(values = colorlist) +
  grid_theme +
  labs(y = 'Predicted', x = 'Actual', title = 'Kvert vs. Time of Day - no Group')
print(plot.kvert.noGrp)
ggsave(filename = paste0(savefolder, '/KvertFittednoGrpPlot.png'), plot.kvert.noGrp,height = 4, width = 6)



plot.kvert.time = ggplot(data = clean_data, aes(x = average_kvert_running,
                                                 y = kvert.time.fit)) + 
  geom_point() +
  #scale_color_manual(values = colorlist) +
  grid_theme +
  labs(y = 'Predicted', x = 'Actual', title = 'Kvert vs. Time of Day only')
print(plot.kvert.time)
ggsave(filename = paste0(savefolder, '/KvertFittedTimePlot.png'), plot.kvert.time,height = 4, width = 6)


#### cadence plots ####
tmpcadence = clean_data[, c(1,5,6,10,12)]
cadence.time.lm = lm(average_cadence_running~converted_times, data = tmpcadence)
tmpcadence$fitted_vals = cadence.time.lm$fitted.values

cadence.time.lmm = lmer(average_cadence_running~converted_times + (1|member_id), data = tmpcadence)
tmpcadence$lmm_fit = fitted(cadence.time.lmm)

round(summary(cadence.time.lm)$adj.r.squared, digits = 3)
summary(cadence.time.lmm)
MuMIn::r.squaredGLMM(cadence.time.lmm)


simple.cadence.plot = ggplot() +
  geom_point(data = tmpcadence, aes(x = converted_times, 
                                    y = average_cadence_running), color = 'black') +
  geom_line(data = tmpcadence, aes(x = converted_times, 
                                   y = fitted_vals), color = 'red') + 
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0)+
  grid_theme + labs(x = 'Time', y = 'Cadence')
print(simple.cadence.plot)
ggsave(filename = paste0(savefolder, '/SimpleCadenceTime.png'), simple.cadence.plot, height = 4, width = 6)

lmm.cadence.plot = ggplot() +
  geom_point(data = tmpcadence, aes(x = converted_times, 
                                    y = average_cadence_running), color = 'black') +
  geom_line(data = tmpcadence, aes(x = converted_times, 
                                   y = lmm_fit, group = member_id), color = 'red') + 
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0)+
  grid_theme + labs(x = 'Time', y = 'Cadence')
print(lmm.cadence.plot)
ggsave(filename = paste0(savefolder, '/LmmCadenceTime.png'), lmm.cadence.plot, height = 4, width = 6)

cadence.vel.plotlist = vector(mode = 'list', length = 4)

unqPersona = sort(unique(clean_data$Persona_group))
dark.red = rgb(192/255, 80/255, 77/255)
mid.blue = rgb(79/255,129/255,189/255)
dark.yellow = rgb(255/255,192/255,0/255)
light.grey = rgb(191/255, 191/255, 191/255)
jade.green = rgb(155/255,187/255,89/255)
colorlist = c(dark.red, mid.blue, dark.yellow, jade.green)
linecolor = c('black', 'black', 'black', 'black')
for (grp in 1:4) {
  tmpdata = clean_data[which(clean_data$Persona_group == unqPersona[grp]),]
  tmp.lm = lm(average_cadence_running~I(log(average_velocity_running)), data = tmpdata)
  tmpdata$fitvals = tmp.lm$fitted.values
  cadence.vel.plotlist[[grp]] = ggplot() +
    geom_point(data = tmpdata, aes(x = average_velocity_running,
                                   y = average_cadence_running),
               color = colorlist[grp]) +
    geom_line(data = tmpdata, aes(x = average_velocity_running,
                                  y = fitvals),
              color = linecolor[grp]) +
    scale_y_continuous(limits = c(0,250)) +
    scale_x_continuous(limits = c(0,7)) +
    geom_hline(yintercept = 0) + geom_vline(xintercept = 0)+
    grid_theme + labs(title = unqPersona[grp], x = 'Velocity', y = 'cadence')
}
cadence.Vel.Plots = ggarrange(plotlist = cadence.vel.plotlist, nrow = 2, ncol = 2)
print(cadence.Vel.Plots)
ggsave(filename = paste0(savefolder, '/cadenceVelocityPlots.png'), 
       plot = cadence.Vel.Plots, height = 6, width = 10)

cadence.ConvTime.plotlist = vector(mode = 'list', length = 4)

for (grp in 1:4) {
  tmpdata = clean_data[which(clean_data$Persona_group == unqPersona[grp]),]
  tmp.lm = lm(average_cadence_running~converted_times, data = tmpdata)
  tmpdata$fitvals = tmp.lm$fitted.values
  cadence.ConvTime.plotlist[[grp]] = ggplot() +
    geom_point(data = tmpdata, aes(x = converted_times,
                                   y = average_cadence_running),
               color = colorlist[grp]) +
    geom_line(data = tmpdata, aes(x = converted_times,
                                  y = fitvals),
              color = linecolor[grp]) +
    scale_y_continuous(limits = c(0,250)) +
    scale_x_continuous(limits = c(0,24))+
    geom_hline(yintercept = 0) + geom_vline(xintercept = 0)+
    grid_theme + labs(title = unqPersona[grp], x = 'Time', y = 'cadence')
}
cadence.ConvTime.Plots = ggarrange(plotlist = cadence.ConvTime.plotlist, nrow = 2, ncol = 2)
print(cadence.ConvTime.Plots)
ggsave(filename = paste0(savefolder, '/cadenceConvTimePlots.png'),
       plot = cadence.ConvTime.Plots, height = 6, width = 10)

cadence.duration.plotlist = vector(mode = 'list', length = 4)

for (grp in 1:4) {
  tmpdata = clean_data[which(clean_data$Persona_group == unqPersona[grp]),]
  tmp.lm = lm(average_cadence_running~duration, data = tmpdata)
  tmpdata$fitvals = tmp.lm$fitted.values
  cadence.duration.plotlist[[grp]] = ggplot() +
    geom_point(data = tmpdata, aes(x = duration,
                                   y = average_cadence_running),
               color = colorlist[grp]) +
    geom_line(data = tmpdata, aes(x = duration,
                                  y = fitvals),
              color = linecolor[grp]) +
    scale_y_continuous(limits = c(0,250)) +
    geom_hline(yintercept = 0) + geom_vline(xintercept = 0)+
    #scale_x_continuous(limits = c(0,24))+
    grid_theme + labs(title = unqPersona[grp], x = 'Duration', y = 'cadence')
}
cadence.duration.Plots = ggarrange(plotlist = cadence.duration.plotlist, nrow = 2, ncol = 2)
print(cadence.duration.Plots)
ggsave(filename = paste0(savefolder, '/cadenceDurationPlots.png'),
       plot = cadence.duration.Plots, height = 6, width = 10)
####  cadence models: data not scaled; converted_times used ####


#run mixed model without interaction
cadence.lmm.full = lmer(average_cadence_running~
                          I(log(average_velocity_running))+
                          Persona_group +
                          converted_times +
                          duration + 
                          (1|member_id), 
                        data = clean_data)

clean_data$cadence.full.fit = fitted(cadence.lmm.full)

summary(cadence.lmm.full)

MuMIn::r.squaredGLMM(cadence.lmm.full)

#run mixed model without persona group
cadence.lmm.noGrp = lmer(average_cadence_running~
                           I(log(average_velocity_running))+
                           converted_times +
                           duration + 
                           (1|member_id), 
                         data = clean_data)

clean_data$cadence.noGrp.fit = fitted(cadence.lmm.noGrp)

summary(cadence.lmm.noGrp)

MuMIn::r.squaredGLMM(cadence.lmm.noGrp)

#run mixed model with time of day only
cadence.lmm.time = lmer(average_cadence_running~
                          converted_times +
                          (1|member_id), 
                        data = clean_data)

clean_data$cadence.time.fit = fitted(cadence.lmm.time)

summary(cadence.lmm.time)

MuMIn::r.squaredGLMM(cadence.lmm.time)

#
anova(cadence.lmm.full, cadence.lmm.noGrp, cadence.lmm.time)

#plot actual values vs. predicted values
plot.cadence.full = ggplot(data = clean_data, aes(x = average_cadence_running,
                                                  y = cadence.full.fit,
                                                  color = Persona_group)) + 
  geom_point() +
  scale_color_manual(values = colorlist) +
  grid_theme + theme(legend.position = "none") +
  labs(y = 'Predicted', x = 'Actual', title = 'Cadence vs. Time of Day - Full Model')
print(plot.cadence.full)
ggsave(filename = paste0(savefolder, '/CadenceFittedFullPlot.png'), plot.cadence.full,height = 4, width = 6)


plot.cadence.noGrp = ggplot(data = clean_data, aes(x = average_cadence_running,
                                                   y = cadence.noGrp.fit)) + 
  geom_point() +
  #scale_color_manual(values = colorlist) +
  grid_theme +
  labs(y = 'Predicted', x = 'Actual', title = 'Cadence vs. Time of Day - no Group')
print(plot.cadence.noGrp)
ggsave(filename = paste0(savefolder, '/CadenceFittednoGrpPlot.png'), plot.cadence.noGrp,height = 4, width = 6)



plot.cadence.time = ggplot(data = clean_data, aes(x = average_cadence_running,
                                                  y = cadence.time.fit)) + 
  geom_point() +
  #scale_color_manual(values = colorlist) +
  grid_theme +
  labs(y = 'Predicted', x = 'Actual', title = 'Cadence vs. Time of Day only')
print(plot.cadence.time)
ggsave(filename = paste0(savefolder, '/CadenceFittedTimePlot.png'), plot.cadence.time,height = 4, width = 6)

####  stride length plots ####
tmpSL = clean_data[, c(1,5,9,10,12)]
SL.time.lm = lm(average_stride_length_running~converted_times, data = tmpSL)
tmpSL$fitted_vals = SL.time.lm$fitted.values

SL.time.lmm = lmer(average_stride_length_running~converted_times + (1|member_id), data = tmpSL)
tmpSL$lmm_fit = fitted(SL.time.lmm)

round(summary(SL.time.lm)$adj.r.squared, digits = 3)
summary(SL.time.lmm)
MuMIn::r.squaredGLMM(SL.time.lmm)


simple.SL.plot = ggplot() +
  geom_point(data = tmpSL, aes(x = converted_times, 
                               y = average_stride_length_running), color = 'black') +
  geom_line(data = tmpSL, aes(x = converted_times, 
                              y = fitted_vals), color = 'red') + 
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0)+
  grid_theme + labs(x = 'Time', y = 'SL')
print(simple.SL.plot)
ggsave(filename = paste0(savefolder, '/SimpleSLTime.png'), simple.SL.plot, height = 4, width = 6)

lmm.SL.plot = ggplot() +
  geom_point(data = tmpSL, aes(x = converted_times, 
                               y = average_stride_length_running), color = 'black') +
  geom_line(data = tmpSL, aes(x = converted_times, 
                              y = lmm_fit, group = member_id), color = 'red') + 
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0)+
  grid_theme + labs(x = 'Time', y = 'SL')
print(lmm.SL.plot)
ggsave(filename = paste0(savefolder, '/LmmSLTime.png'), lmm.SL.plot, height = 4, width = 6)

SL.vel.plotlist = vector(mode = 'list', length = 4)

unqPersona = sort(unique(clean_data$Persona_group))
dark.red = rgb(192/255, 80/255, 77/255)
mid.blue = rgb(79/255,129/255,189/255)
dark.yellow = rgb(255/255,192/255,0/255)
light.grey = rgb(191/255, 191/255, 191/255)
jade.green = rgb(155/255,187/255,89/255)
colorlist = c(dark.red, mid.blue, dark.yellow, jade.green)
linecolor = c('black', 'black', 'black', 'black')
for (grp in 1:4) {
  tmpdata = clean_data[which(clean_data$Persona_group == unqPersona[grp]),]
  tmp.lm = lm(average_stride_length_running~(average_velocity_running), data = tmpdata)
  tmpdata$fitvals = tmp.lm$fitted.values
  SL.vel.plotlist[[grp]] = ggplot() +
    geom_point(data = tmpdata, aes(x = average_velocity_running,
                                   y = average_stride_length_running),
               color = colorlist[grp]) +
    geom_line(data = tmpdata, aes(x = average_velocity_running,
                                  y = fitvals),
              color = linecolor[grp]) +
    scale_y_continuous(limits = c(0,2.5)) +
    scale_x_continuous(limits = c(0,7)) +
    geom_hline(yintercept = 0) + geom_vline(xintercept = 0)+
    grid_theme + labs(title = unqPersona[grp], x = 'Velocity', y = 'SL')
}
SL.Vel.Plots = ggarrange(plotlist = SL.vel.plotlist, nrow = 2, ncol = 2)
print(SL.Vel.Plots)
ggsave(filename = paste0(savefolder, '/SLVelocityPlots.png'), 
       plot = SL.Vel.Plots, height = 6, width = 10)

SL.ConvTime.plotlist = vector(mode = 'list', length = 4)

for (grp in 1:4) {
  tmpdata = clean_data[which(clean_data$Persona_group == unqPersona[grp]),]
  tmp.lm = lm(average_stride_length_running~converted_times, data = tmpdata)
  tmpdata$fitvals = tmp.lm$fitted.values
  SL.ConvTime.plotlist[[grp]] = ggplot() +
    geom_point(data = tmpdata, aes(x = converted_times,
                                   y = average_stride_length_running),
               color = colorlist[grp]) +
    geom_line(data = tmpdata, aes(x = converted_times,
                                  y = fitvals),
              color = linecolor[grp]) +
    scale_y_continuous(limits = c(0,2.5)) +
    scale_x_continuous(limits = c(0,24))+
    geom_hline(yintercept = 0) + geom_vline(xintercept = 0)+
    grid_theme + labs(title = unqPersona[grp], x = 'Time', y = 'SL')
}
SL.ConvTime.Plots = ggarrange(plotlist = SL.ConvTime.plotlist, nrow = 2, ncol = 2)
print(SL.ConvTime.Plots)
ggsave(filename = paste0(savefolder, '/SLConvTimePlots.png'),
       plot = SL.ConvTime.Plots, height = 6, width = 10)

SL.duration.plotlist = vector(mode = 'list', length = 4)

for (grp in 1:4) {
  tmpdata = clean_data[which(clean_data$Persona_group == unqPersona[grp]),]
  tmp.lm = lm(average_stride_length_running~duration, data = tmpdata)
  tmpdata$fitvals = tmp.lm$fitted.values
  SL.duration.plotlist[[grp]] = ggplot() +
    geom_point(data = tmpdata, aes(x = duration,
                                   y = average_stride_length_running),
               color = colorlist[grp]) +
    geom_line(data = tmpdata, aes(x = duration,
                                  y = fitvals),
              color = linecolor[grp]) +
    scale_y_continuous(limits = c(0,2.5)) +
    geom_hline(yintercept = 0) + geom_vline(xintercept = 0)+
    #scale_x_continuous(limits = c(0,24))+
    grid_theme + labs(title = unqPersona[grp], x = 'Duration', y = 'SL')
}
SL.duration.Plots = ggarrange(plotlist = SL.duration.plotlist, nrow = 2, ncol = 2)
print(SL.duration.Plots)
ggsave(filename = paste0(savefolder, '/SLDurationPlots.png'),
       plot = SL.duration.Plots, height = 6, width = 10)
####  stride length models: data not scaled; converted_times used ####


#run mixed model without interaction
SL.lmm.full = lmer(average_stride_length_running~
                     average_velocity_running+
                     Persona_group +
                     converted_times +
                     duration + 
                     (1|member_id), 
                   data = clean_data)

clean_data$SL.full.fit = fitted(SL.lmm.full)

summary(SL.lmm.full)

MuMIn::r.squaredGLMM(SL.lmm.full)

#run mixed model without persona group
SL.lmm.noGrp = lmer(average_stride_length_running~
                      average_velocity_running +
                      converted_times +
                      duration + 
                      (1|member_id), 
                    data = clean_data)

clean_data$SL.noGrp.fit = fitted(SL.lmm.noGrp)

summary(SL.lmm.noGrp)

MuMIn::r.squaredGLMM(SL.lmm.noGrp)

#run mixed model with time of day only
SL.lmm.time = lmer(average_stride_length_running~
                     converted_times +
                     (1|member_id), 
                   data = clean_data)

clean_data$SL.time.fit = fitted(SL.lmm.time)

summary(SL.lmm.time)

MuMIn::r.squaredGLMM(SL.lmm.time)

#
anova(SL.lmm.full, SL.lmm.noGrp, SL.lmm.time)

#plot actual values vs. predicted values
plot.SL.full = ggplot(data = clean_data, aes(x = average_stride_length_running,
                                             y = SL.full.fit,
                                             color = Persona_group)) + 
  geom_point() +
  scale_color_manual(values = colorlist) +
  grid_theme + theme(legend.position = "none") +
  labs(y = 'Predicted', x = 'Actual', title = 'SL vs. Time of Day - Full Model')
print(plot.SL.full)
ggsave(filename = paste0(savefolder, '/SLFittedFullPlot.png'), plot.SL.full,height = 4, width = 6)


plot.SL.noGrp = ggplot(data = clean_data, aes(x = average_stride_length_running,
                                              y = SL.noGrp.fit)) + 
  geom_point() +
  #scale_color_manual(values = colorlist) +
  grid_theme +
  labs(y = 'Predicted', x = 'Actual', title = 'SL vs. Time of Day - no Group')
print(plot.SL.noGrp)
ggsave(filename = paste0(savefolder, '/SLFittednoGrpPlot.png'), plot.SL.noGrp,height = 4, width = 6)



plot.SL.time = ggplot(data = clean_data, aes(x = average_stride_length_running,
                                             y = SL.time.fit)) + 
  geom_point() +
  #scale_color_manual(values = colorlist) +
  grid_theme +
  labs(y = 'Predicted', x = 'Actual', title = 'SL vs. Time of Day only')
print(plot.SL.time)
ggsave(filename = paste0(savefolder, '/SLFittedTimePlot.png'), plot.SL.time,height = 4, width = 6)
#### velocity plots ####
tmpvelocity = clean_data[, c(1,5,9,10,12)]
velocity.time.lm = lm(average_velocity_running~converted_times, data = tmpvelocity)
tmpvelocity$fitted_vals = velocity.time.lm$fitted.values

velocity.time.lmm = lmer(average_velocity_running~converted_times + (1|member_id), data = tmpvelocity)
tmpvelocity$lmm_fit = fitted(velocity.time.lmm)

round(summary(velocity.time.lm)$adj.r.squared, digits = 3)
summary(velocity.time.lmm)
MuMIn::r.squaredGLMM(velocity.time.lmm)


simple.velocity.plot = ggplot() +
  geom_point(data = tmpvelocity, aes(x = converted_times, 
                                     y = average_velocity_running), color = 'black') +
  geom_line(data = tmpvelocity, aes(x = converted_times, 
                                    y = fitted_vals), color = 'red') + 
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0)+
  grid_theme + labs(x = 'Time', y = 'velocity')
print(simple.velocity.plot)
ggsave(filename = paste0(savefolder, '/SimplevelocityTime.png'), simple.velocity.plot, height = 4, width = 6)

lmm.velocity.plot = ggplot() +
  geom_point(data = tmpvelocity, aes(x = converted_times, 
                                     y = average_velocity_running), color = 'black') +
  geom_line(data = tmpvelocity, aes(x = converted_times, 
                                    y = lmm_fit, group = member_id), color = 'red') + 
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0)+
  grid_theme + labs(x = 'Time', y = 'velocity')
print(lmm.velocity.plot)
ggsave(filename = paste0(savefolder, '/LmmvelocityTime.png'), lmm.velocity.plot, height = 4, width = 6)
#### velocity models: data not scaled; converted_times used ####


#run mixed model without interaction
velocity.lmm.full = lmer(average_velocity_running~
                           Persona_group +
                           converted_times +
                           duration + 
                           (1|member_id), 
                         data = clean_data)

clean_data$velocity.full.fit = fitted(velocity.lmm.full)

summary(velocity.lmm.full)

MuMIn::r.squaredGLMM(velocity.lmm.full)

#run mixed model without persona group
velocity.lmm.noGrp = lmer(average_velocity_running~
                            converted_times +
                            duration + 
                            (1|member_id), 
                          data = clean_data)

clean_data$velocity.noGrp.fit = fitted(velocity.lmm.noGrp)

summary(velocity.lmm.noGrp)

MuMIn::r.squaredGLMM(velocity.lmm.noGrp)

#run mixed model with time of day only
velocity.lmm.time = lmer(average_velocity_running~
                           converted_times +
                           (1|member_id), 
                         data = clean_data)

clean_data$velocity.time.fit = fitted(velocity.lmm.time)

summary(velocity.lmm.time)

MuMIn::r.squaredGLMM(velocity.lmm.time)

#
anova(velocity.lmm.full, velocity.lmm.noGrp, velocity.lmm.time)

#plot actual values vs. predicted values
plot.velocity.full = ggplot(data = clean_data, aes(x = average_velocity_running,
                                                   y = velocity.full.fit,
                                                   color = Persona_group)) + 
  geom_point() +
  scale_color_manual(values = colorlist) +
  grid_theme + theme(legend.position = "none") +
  labs(y = 'Predicted', x = 'Actual', title = 'velocity vs. Time of Day - Full Model')
print(plot.velocity.full)
ggsave(filename = paste0(savefolder, '/velocityFittedFullPlot.png'), plot.velocity.full,height = 4, width = 6)


plot.velocity.noGrp = ggplot(data = clean_data, aes(x = average_velocity_running,
                                                    y = velocity.noGrp.fit)) + 
  geom_point() +
  #scale_color_manual(values = colorlist) +
  grid_theme +
  labs(y = 'Predicted', x = 'Actual', title = 'velocity vs. Time of Day - no Group')
print(plot.velocity.noGrp)
ggsave(filename = paste0(savefolder, '/velocityFittednoGrpPlot.png'), plot.velocity.noGrp,height = 4, width = 6)



plot.velocity.time = ggplot(data = clean_data, aes(x = average_velocity_running,
                                                   y = velocity.time.fit)) + 
  geom_point() +
  #scale_color_manual(values = colorlist) +
  grid_theme +
  labs(y = 'Predicted', x = 'Actual', title = 'velocity vs. Time of Day only')
print(plot.velocity.time)
ggsave(filename = paste0(savefolder, '/velocityFittedTimePlot.png'), plot.velocity.time,height = 4, width = 6)