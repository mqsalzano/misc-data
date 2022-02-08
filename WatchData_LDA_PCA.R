#### load packages ####
library(readxl)
library(tidyverse)
library(ggplot2)
library(tidyr)
library(reshape2)
library(extrafont)
library(ggpubr)
library(caret)
library(car)
library(carData)
library(ggcorrplot)
library(MASS)
library(sparseLDA)
library(glmnet)
library(randomForest)

#### load and extract data ####
masterfolder = "D:/UMass/Brooks/WatchData/Mobile_Lab_Data_UMass"
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

dark.red = rgb(192/255, 80/255, 77/255)
mid.blue = rgb(79/255,129/255,189/255)
dark.yellow = rgb(255/255,192/255,0/255)
light.grey = rgb(191/255, 191/255, 191/255)
jade.green = rgb(155/255,187/255,89/255)



#### create datasets for per run ####
per.run.data = runmetrics[which(runmetrics$segment_name == 'fun run'),c(2,6,9,10,16:22,48,50,51)]
#avg.run.data = runmetrics[which(runmetrics$segment_name == 'fun run'),c(2,6,16,17,23:29,48,50,51)]

dist = which(per.run.data$total_distance > 1609.34 & per.run.data$total_distance < 25000) #1 mile and ~15.5 miles
duration = which(per.run.data$duration > 600 & per.run.data$duration < 10800) #10 minutes and 3 hours
vel = which(per.run.data$average_velocity_running < 5.95) #13.3 mph pace - min is 1.48 m/s pace
cadence = which(per.run.data$average_cadence_running > 100 & per.run.data$average_cadence_running < 250)
gct = which(per.run.data$average_gct_running > 165)# & per.run.data$average_gct_running < 300)
#kvert = which(per.run.data$average_kvert_running > 55 & per.run.data$average_kvert_running < 12)
time = which(per.run.data$converted_times >4.49 & per.run.data$converted_times < 22.5)
hr = which(per.run.data$average_hr > 40)
pwr = which(per.run.data$average_power_run > 0)

common1 = intersect(dist,duration)
common2 = intersect(gct, cadence)
common3 = intersect(time, vel)
common4 = intersect(hr, pwr)

clean.run.data = per.run.data[intersect(intersect(intersect(common1,common2),common3),common4),]
clean.run.data$duration = clean.run.data$duration/60

####  plot data ####
plot.vars.list = vector(mode = "list", length = 9)
VOI = c('Distance (m)', 'Duration (s)', 'Heart Rate (bpm)',
        'Power (W)', 'Velocity (m/s)', 'Cadence (spm)',
        'GCT (s)', 'KVert (N/m)', 'Stride Length (m)')

for (i in 1:9) {
plot.vars.list[[i]] = ggplot(data = clean.run.data,
                             aes_string(x = clean.run.data$Persona_group,
                                        y = clean.run.data[,i+2],
                                        #color = clean.run.data$Persona_group,
                                        fill = clean.run.data$Persona_group)) +
  geom_violin(color = 'black') + 
  stat_summary(fun.y=mean, geom="point", shape =3, size=3, color="black") +
  labs(y = VOI[i])+
  scale_x_discrete(labels = c('G.S.', 'Pcr', 'S.R.', 'Trl'))+
  #scale_color_manual(values = c(dark.red, mid.blue, dark.yellow, jade.green)) +
  scale_fill_manual(values = c(dark.red, mid.blue, dark.yellow, jade.green)) +
  theme(legend.position = "none") +
  grid_theme
} 

plot.vars.grid = ggarrange(plotlist = plot.vars.list, nrow = 3, ncol = 3)
print(plot.vars.grid)
ggsave(paste0(savefolder,'/BoxplotGrid.png'), plot.vars.grid, height = 6, width = 10)


#### correlations
cor.data = clean.run.data[,c(3:11)]
names(cor.data) = c('Distance', 'Duration', 'Heart Rate',
                             'Power', 'Velocity', 'Cadence',
                             'GCT', 'KVert', 'Stride Length')
var.cor = cor(cor.data)

var.cor.plot = ggcorrplot(var.cor, lab = TRUE, hc.order = TRUE, lab_size = 3, show.legend = FALSE, type = "upper") +
  grid_theme + theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12),
                     axis.title.x = element_blank(), axis.title.y = element_blank())
print(var.corr.plot)
ggsave(paste0(savefolder, '/CorrelationPlot_RQ2.png'), plot = var.cor.plot, height = 6, width = 6)

#### LDA to define runner groups - using all runs ####

cv.control = trainControl(
                method = 'repeatedcv', 
                number = 5, repeats =10)

proc_params = preProcess(clean.run.data[,c(3:11,14)],
                         method = c('center', 'scale'))

# Running LDA using cross-validation - for prediction #
proc_data = predict(proc_params, clean.run.data[,c(3:11,14)])
proc_data = cbind(clean.run.data[,c(1,2,12)], proc_data)

clean.run.data.new = proc_data[complete.cases(proc_data[,c(3:13)]),]

unqID.all= unique(clean.run.data.new$member_id)

split.index = sample(unqID.all, size = length(unqID.all)*0.80, replace = FALSE)

idx = clean.run.data.new$member_id %in% split.index
train.run = clean.run.data.new[idx,]
test.run = clean.run.data.new[!idx,]


unqID.train = unique(train.run$member_id)
unqID.test = unique(test.run$member_id)

train.run = train.run[,-c(1,2,9,12)]
test.run = test.run[,-c(1,2,9,12)]

testLDA = train(Persona_group~.,
                data = train.run,
                method = 'lda',
                trControl = cv.control)

newdata = predict(testLDA, test.run)
confusionMatrix(newdata, test.run$Persona_group)

# Running LDA on all data for general evaluation #
testLDA2 = lda(Persona_group~., data= train.run)
newdata2 = predict(testLDA2, test.run)
confusionMatrix(newdata2$class,test.run$Persona_group)

lda.plot.data = cbind(as.data.frame(test.run$Persona_group), as.data.frame(newdata2$x))
names(lda.plot.data)[1] = 'Persona'
ggplot(data = lda.plot.data, aes(x = LD1, y = LD2, color = Persona)) +
  geom_point() + grid_theme + labs(x = 'LD1', y = 'LD2')
ldahist()
###

#### LDA to define runner groups - using an average of all runs for each runner ####
AvgData = data.frame()
newID = c(1:length(unqID.all))
for (i in 1:length(unqID.all)) {
  
  id = unqID.all[i]
  tmpdata = clean.run.data[which(clean.run.data$member_id == id),]
  tmpmeans = t(apply(tmpdata[,c(3:11,14)], 2, mean))
  tmprow = cbind(tmpdata[1,c(1,2,12,13)], tmpmeans)
  tmprow$member_id = newID[i]
  AvgData = rbind(AvgData, tmprow)
  
}

# Running LDA using cross-validation - for prediction #
proc_params = preProcess(AvgData[,c(5,6,7,8,10,12,13,14)],
                         method = c('center', 'scale'))

proc_avg_data = cbind(AvgData[,c(1,3)] ,
                      predict(proc_params, AvgData[,c(5,6,7,8,10,12,13,14)]))

new_avg_data = proc_avg_data[complete.cases(proc_avg_data),]

idx = createDataPartition(new_avg_data$Persona_group, p = 0.80, list = FALSE)
avg.train.run = new_avg_data[idx,-c(1)]
avg.test.run = new_avg_data[-idx,-c(1)]

avg.cv.LDA = train(Persona_group~.,
                   data =avg.train.run,
                   method = 'lda',
                   trControl = cv.control)

avg.cv.pred = predict(avg.cv.LDA, avg.test.run)
confusionMatrix(avg.cv.pred,avg.test.run$Persona_group)

# Running LDA on all data for general evaluation #
avgLDA = lda(Persona_group~.,
             data =avg.train.run)

avg.pred = predict(avgLDA, avg.test.run)
confusionMatrix(avg.pred$class, avg.test.run$Persona_group)


##not doing test/train
avg.LDA.all = lda(Persona_group~.,
                  data = new_avg_data[,-1])

pred.avg.LDA.all = predict(avg.LDA.all, new_avg_data[,-1])

avgLDA.plot.data = cbind(as.data.frame(pred.avg.LDA.all$class), as.data.frame(pred.avg.LDA.all$x))
names(avgLDA.plot.data)[1] = 'Persona'

avg.LDA.plot = ggplot(data = avgLDA.plot.data, aes(x = LD1, y = LD2, color = Persona)) +
  geom_point(size = 2.5) + grid_theme + labs(x = 'LD1', y = 'LD2') + 
  theme(legend.position = 'top') +
  scale_color_manual(values = c(dark.red, mid.blue, dark.yellow, jade.green))
print(avg.LDA.plot)
ggsave(paste0(savefolder, '/avgLDAplot.png'), avg.LDA.plot, height = 6, width = 6)

temp_pred = predict(avg.LDA.all)
ldahist(temp_pred$x[,1], g = Persona)
#dropping Trail runners
# avg.train.run = new_avg_data[idx,-c(1,8,11)]
# avg.test.run = new_avg_data[-idx,-c(1,8,11)]
# 
# avg.train.noTrail = avg.train.run[which(avg.train.run$Persona_group != 'Trail'),]
# avg.test.noTrail = avg.test.run[which(avg.test.run$Persona_group != 'Trail'),]
# avg.train.noTrail$Persona_group = factor(avg.train.noTrail$Persona_group)
# avg.test.noTrail$Persona_group = factor(avg.test.noTrail$Persona_group)
# 
# avg.cv.LDA.noTrail = train(Persona_group~.,
#                    data =avg.train.noTrail,
#                    method = 'lda',
#                    trControl = cv.control)
# 
# avg.cv.pred.noTrail = predict(avg.cv.LDA.noTrail, avg.test.noTrail)
# confusionMatrix(avg.cv.pred.noTrail, avg.test.noTrail$Persona_group )
# 
# avg.LDA.notrail = lda(Persona_group~.,
#                       data = avg.train.noTrail)
# 
# avg.pred.noTrail = predict(avg.LDA.notrail, avg.test.noTrail)
# confusionMatrix(avg.pred.noTrail$class, avg.test.noTrail$Persona_group)

#### Running LDA with one type of runner removed ####
# Trail is only group that defines runner based on where they run and not their goals of running
new_avg_data.noTrail = new_avg_data[which(new_avg_data$Persona_group != 'Trail'),-1]
new_avg_data.noTrail$Persona_group = factor(new_avg_data.noTrail$Persona_group)
avgLDA.noTrail = lda(Persona_group~.,
                     data = new_avg_data.noTrail)

pred.avgLDA.notrail = predict(avgLDA.noTrail, new_avg_data.noTrail)

avgLDA.notrail.plot.data = cbind(as.data.frame(new_avg_data.noTrail$Persona_group), as.data.frame(pred.avgLDA.notrail$x))
names(avgLDA.notrail.plot.data)[1] = 'Persona'

avg.LDA.notrail.plot = ggplot(data = avgLDA.notrail.plot.data, aes(x = LD1, y = LD2, color = Persona)) +
  geom_point(size = 2.5) + grid_theme + labs(x = 'LD1', y = 'LD2') + 
  theme(legend.position = 'top') +
  scale_color_manual(values = c(dark.red, mid.blue, dark.yellow))
print(avg.LDA.notrail.plot)
ggsave(paste0(savefolder, '/avgLDAplot_noTrail.png'), avg.LDA.notrail.plot, height = 6, width = 6)

confusionMatrix(pred.avgLDA.notrail$class, new_avg_data.noTrail$Persona_group)


#### Principal Compnents Analysis to determine if any sub-groups arise from Goal Seeker group ####
pca.data.GS = predict(proc_params, 
                      AvgData[which(AvgData$Persona_group == 'Goal Seeker'),c(5:14)])

pca.data.GS = pca.data.GS[complete.cases(pca.data.GS),]

tempPCA = prcomp(pca.data.GS)
dataPCA = as.data.frame(tempPCA$x)

pca.GS.plot = ggplot(data = dataPCA, aes(x = PC1,  y= PC2)) +
  geom_point(size = 2.5) + grid_theme + labs(x = 'PC1', y = 'PC2')
ggsave(paste0(savefolder, '/PCAforGS.png'), pca.GS.plot, height = 6, width = 6)
