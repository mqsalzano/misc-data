
library(readxl)
library(tidyverse)
BigCushAvgSock <- as.data.frame(read_excel("C:/Users/msalzano/Box/UMass/Brooks/BigCush/AvgSockBigCush2.xlsx"))
#View(BigCushSock)
headersSock = colnames(BigCushSock)

newAvgSock = BigCushAvgSock[,c(1,2,6,7,8:21, 29, 32, 70:72, 75, 76, 110:137, 
                                140, 141, 155, 163, 167, 
                                169, 179, 191, 196, 201, 206, 
                                209, 212, 215, 230:239, 241:245)]

BmchLDA = newAvgSock[, c(3:24,26:49, 52:55, 59)]

#keep normalized moments, discard non-normalized moments
BmchLDA = BmchLDA[,-c(3:8,33:38)]

names(BmchLDA)[c(1,2,11,12,13,26,36)] = c("AnkL_Stiff_F", "AnkL_Stiff_S", "Min_Ev_Angle",
                                                    "Ev_ROM", "Cadence", "Knee_Stiff_S", "Max_Ev_Vel")


for (i in c(1:38)) {
  
  BmchLDA[,i] = as.numeric(BmchLDA[,i])
}

BmchLDA_all = BmchLDA[complete.cases(BmchLDA), ]
BmchLDA_noHFA = BmchLDA_all[-c(which(BmchLDA_all$RunnerCategory == "HFA")),]
BmchLDA_GSvSR = BmchLDA_all[which(BmchLDA_all$RunnerCategory == "Goal Seeker" | 
                                BmchLDA_all$RunnerCategory == "Soul Runner"),]
BmchLDA_GSvP = BmchLDA_all[which(BmchLDA_all$RunnerCategory == "Goal Seeker" | 
                               BmchLDA_all$RunnerCategory == "Pacer"),]
BmchLDA_SRvP = BmchLDA_all[which(BmchLDA_all$RunnerCategory == "Soul Runner" | 
                               BmchLDA_all$RunnerCategory == "Pacer"),]


### Run LDA using all 4 runner categories - using Knee Abd/Add as separate variables
set.seed(123)
training.samples.all <- BmchLDA_all$RunnerCategory %>%
  createDataPartition(p = 0.8, list = FALSE)
train.data.all <- BmchLDA_all[training.samples.all, ]
test.data.all <- BmchLDA_all[-training.samples.all, ]

preproc.param.all <- train.data.all %>% 
  preProcess(method = c("center", "scale"))
# Transform the data using the estimated parameters
train.transformed.all <- preproc.param.all %>% predict(train.data.all)
test.transformed.all <- preproc.param.all %>% predict(test.data.all)


LDAmodel.tt.all = lda(RunnerCategory~., data = train.data.all)

predictions.all <- LDAmodel.tt.all %>% predict(train.data.all)

LDAmodel.full.all = lda(RunnerCategory ~., data = BmchLDA_all)
predictions.all.all <- LDAmodel.full.all %>% predict(BmchLDA_all)
y = integer(length(predictions.all.all$x)) + 1

lda.tt.all <- cbind(train.data.all, predict(LDAmodel.tt.all)$x)
ggplot(lda.tt.all, aes(LD1, LD2)) +
  geom_point(aes(color = RunnerCategory))  + scale_color_manual(values=c('#000000', '#CCCC00','#FF0000', '#0000FF')) +theme_classic()
ldahist(data = predict(LDAmodel.tt.all)$x[,1], g=train.data.all$RunnerCategory)
ldahist(data = predict(LDAmodel.tt.all)$x[,2], g=train.data.all$RunnerCategory)

lda.full.all <- cbind(BmchLDA_all, predict(LDAmodel.full.all)$x)
ggplot(lda.full.all, aes(LD1, LD2)) + 
  geom_point(aes(color = RunnerCategory)) + scale_color_manual(values=c('#000000', '#CCCC00','#FF0000', '#0000FF')) +theme_classic()
ldahist(data = predict(LDAmodel.full.all)$x[,1], g = BmchLDA_all$RunnerCategory)
ldahist(data = predict(LDAmodel.full.all)$x[,2], g = BmchLDA_all$RunnerCategory)


### Run LDA without HFA group - using Knee Abd/Add as separate variables
set.seed(123)
training.samples.noHFA <- BmchLDA_noHFA$RunnerCategory %>%
  createDataPartition(p = 0.8, list = FALSE)
train.data.noHFA <- BmchLDA_noHFA[training.samples.noHFA, ]
test.data.noHFA <- BmchLDA_noHFA[-training.samples.noHFA, ]

preproc.param.noHFA <- train.data.noHFA %>% 
  preProcess(method = c("center", "scale"))
# Transform the data using the estimated parameters
train.transformed.noHFA <- preproc.param.noHFA %>% predict(train.data.noHFA)
test.transformed.noHFA <- preproc.param.noHFA %>% predict(test.data.noHFA)


LDAmodel.tt.noHFA = lda(RunnerCategory~., data = train.data.noHFA)

predictions.noHFA <- LDAmodel.tt.noHFA %>% predict(train.data.noHFA)

LDAmodel.full.noHFA = lda(RunnerCategory ~., data = BmchLDA_noHFA)
predictions.all.noHFA <- LDAmodel.full.noHFA %>% predict(BmchLDA_noHFA)
y = integer(length(predictions.all.noHFA$x)) + 1

lda.tt.noHFA <- cbind(train.data.noHFA, predictions.noHFA$x)
ggplot(lda.tt.noHFA, aes(LD1, LD2)) +
  geom_point(aes(color = RunnerCategory)) + scale_color_manual(values=c('#000000','#FF0000', '#0000FF')) +theme_classic()
ldahist(data = predict(LDAmodel.tt.noHFA)$x[,1], g=train.data.noHFA$RunnerCategory)
ldahist(data = predict(LDAmodel.tt.noHFA)$x[,2], g=train.data.noHFA$RunnerCategory)

lda.full.noHFA <- cbind(BmchLDA_noHFA, predict(LDAmodel.full.noHFA)$x)
full.noHFA.plot = ggplot(lda.full.noHFA, aes(LD1, LD2)) +
  geom_point(aes(color = RunnerCategory)) + scale_color_manual(values=c('#000000','#FF0000', '#0000FF')) +theme_classic()
ldahist(data = predict(LDAmodel.full.all)$x[,1], g = BmchLDA_all$RunnerCategory)
ldahist(data = predict(LDAmodel.full.noHFA)$x[,1], g = BmchLDA_noHFA$RunnerCategory)
ldahist(data = predict(LDAmodel.full.noHFA)$x[,2], g = BmchLDA_noHFA$RunnerCategory)



### Run LDA - Goal Seeker (GS) v. Soul Runner (SR) - using Knee Abd/Add as separate variables
set.seed(123)
training.samples.GSvSR <- BmchLDA_GSvSR$RunnerCategory %>%
  createDataPartition(p = 0.8, list = FALSE)
train.data.GSvSR <- BmchLDA_GSvSR[training.samples.GSvSR, ]
test.data.GSvSR <- BmchLDA_GSvSR[-training.samples.GSvSR, ]

preproc.param.GSvSR <- train.data.GSvSR %>% 
  preProcess(method = c("center", "scale"))
# Transform the data using the estimated parameters
train.transformed.GSvSR <- preproc.param.GSvSR %>% predict(train.data.GSvSR)
test.transformed.GSvSR <- preproc.param.GSvSR %>% predict(test.data.GSvSR)


LDAmodel.tt.GSvSR = lda(RunnerCategory~., data = train.data.GSvSR)

predictions.GSvSR <- LDAmodel.tt.GSvSR %>% predict(train.data.GSvSR)

LDAmodel.full.GSvSR = lda(RunnerCategory ~., data = BmchLDA_GSvSR)
predictions.all.GSvsSR = LDAmodel.full.GSvSR %>% predict(BmchLDA_GSvSR)

lda.tt.GSvSR <- cbind(train.data.GSvSR, predict(LDAmodel.tt.GSvSR)$x)
ggplot(lda.tt.GSvSR, aes(LD1, 1)) + 
  geom_point(aes(color = RunnerCategory))  + scale_color_manual(values=c('#000000', '#0000FF')) + theme_classic() +
ldahist(data = predict(LDAmodel.tt.GSvSR)$x[,1], g=train.data.GSvSR$RunnerCategory)
ldahist(data = predict(LDAmodel.tt.GSvSR)$x[,2], g=train.data.GSvSR$RunnerCategory)

lda.full.GSvSR <- cbind(BmchLDA_GSvSR, predict(LDAmodel.full.GSvSR)$x) 
ggplot(lda.full.GSvSR, aes(LD1, 1)) + scale_color_manual(values=c('#000000', '#0000FF')) + theme_classic() +
  geom_point(aes(color = RunnerCategory))
ldahist(data = predict(LDAmodel.full.GSvSR)$x[,1], g = BmchLDA_GSvSR$RunnerCategory)
ldahist(data = predict(LDAmodel.full.GSvSR)$x[,2], g = BmchLDA_GSvSR$RunnerCategory)



### Run LDA - Goal Seeker (GS) v. Pacer (P) - using Knee Abd/Add as separate variables
set.seed(123)
training.samples.GSvP <- BmchLDA_GSvP$RunnerCategory %>%
  createDataPartition(p = 0.8, list = FALSE)
train.data.GSvP <- BmchLDA_GSvP[training.samples.GSvP, ]
test.data.GSvP <- BmchLDA_GSvP[-training.samples.GSvP, ]

preproc.param.GSvP <- train.data.GSvP %>% 
  preProcess(method = c("center", "scale"))
# Transform the data using the estimated parameters
train.transformed.GSvP <- preproc.param.GSvP %>% predict(train.data.GSvP)
test.transformed.GSvP <- preproc.param.GSvP %>% predict(test.data.GSvP)


LDAmodel.tt.GSvP = lda(RunnerCategory~., data = train.data.GSvP)

predictions.GSvP <- LDAmodel.tt.GSvP %>% predict(train.data.GSvP)

LDAmodel.full.GSvP = lda(RunnerCategory ~., data = BmchLDA_GSvP)
predictions.all.GSvsP = LDAmodel.full.GSvP %>% predict(BmchLDA_GSvP)


lda.tt.GSvP <- cbind(train.data.GSvP, predict(LDAmodel.tt.GSvP)$x)
ggplot(lda.tt.GSvP, aes(LD1, 1)) +
  geom_point(aes(color = RunnerCategory)) + scale_color_manual(values=c('#000000','#FF0000')) +theme_classic()
ldahist(data = predict(LDAmodel.tt.GSvP)$x[,1], g=train.data.GSvP$RunnerCategory)
ldahist(data = predict(LDAmodel.tt.GSvP)$x[,2], g=train.data.GSvP$RunnerCategory)

lda.full.GSvP <- cbind(BmchLDA_GSvP, predict(LDAmodel.full.GSvP)$x)
ggplot(lda.full.GSvP, aes(LD1, 1)) +
  geom_point(aes(color = RunnerCategory)) + scale_color_manual(values=c('#000000','#FF0000')) +theme_classic()
ldahist(data = predict(LDAmodel.full.GSvP)$x[,1], g = BmchLDA_GSvP$RunnerCategory)
ldahist(data = predict(LDAmodel.full.GSvP)$x[,2], g = BmchLDA_GSvP$RunnerCategory)


### Run LDA - Soul Runner (SR) v. Pacer (P) - using Knee Abd/Add as separate variables
set.seed(123)
training.samples.SRvP <- BmchLDA_SRvP$RunnerCategory %>%
  createDataPartition(p = 0.8, list = FALSE)
train.data.SRvP <- BmchLDA_SRvP[training.samples.SRvP, ]
test.data.SRvP <- BmchLDA_SRvP[-training.samples.SRvP, ]

preproc.param.SRvP <- train.data.SRvP %>% 
  preProcess(method = c("center", "scale"))
# Transform the data using the estimated parameters
train.transformed.SRvP <- preproc.param.SRvP %>% predict(train.data.SRvP)
test.transformed.SRvP <- preproc.param.SRvP %>% predict(test.data.SRvP)


LDAmodel.tt.SRvP = lda(RunnerCategory~., data = train.data.SRvP)

predictions.SRvP <- LDAmodel.tt.SRvP %>% predict(train.data.SRvP)

LDAmodel.full.SRvP = lda(RunnerCategory ~., data = BmchLDA_SRvP)
predictions.all.SRvP <- LDAmodel.full.SRvP %>% predict(BmchLDA_SRvP)
y = integer(length(predictions.all.SRvP$x)) + 1




lda.tt.SRvP <- cbind(train.data.SRvP, predict(LDAmodel.tt.SRvP)$x)
ggplot(lda.tt.SRvP, aes(LD1, 1)) +
  geom_point(aes(color = RunnerCategory)) + scale_color_manual(values=c('#FF0000', '#0000FF')) +theme_classic()
ldahist(data = predict(LDAmodel.tt.SRvP)$x[,1], g=train.data.SRvP$RunnerCategory)
ldahist(data = predict(LDAmodel.tt.SRvP)$x[,2], g=train.data.SRvPP$RunnerCategory)

lda.full.SRvP <- cbind(BmchLDA_SRvP, predict(LDAmodel.full.SRvP)$x)
ggplot(lda.full.SRvP, aes(LD1, 1)) +
  geom_point(aes(color = RunnerCategory)) + scale_color_manual(values=c('#FF0000', '#0000FF')) +theme_classic()
ldahist(data = predict(LDAmodel.full.SRvP)$x[,1], g = BmchLDA_SRvP$RunnerCategory)
ldahist(data = predict(LDAmodel.full.SRvP)$x[,2], g = BmchLDA_SRvP$RunnerCategory)






###run MANOVA
# MANOVA on LDA with all runner categories
var_all = integer(length(BmchLDA_all$RunnerCategory))
for (i in 1:38) {
  
  var_all = cbind(var_all, BmchLDA_all[,i])
  
}
var_all = var_all[,-1]
manova_all = manova(var_all ~ BmchLDA_all$RunnerCategory)
summary(manova_all, test = "Wilks", tol=0)

# MANOVA on LDA without HFA group
var_noHFA = integer(length(BmchLDA_noHFA$RunnerCategory))
for (i in 1:38) {
  
  var_noHFA = cbind(var_noHFA, BmchLDA_noHFA[,i])
  
}
var_noHFA = var_noHFA[,-1]
manova_noHFA = manova(var_noHFA ~ BmchLDA_noHFA$RunnerCategory)
summary(manova_noHFA, test = "Wilks", tol = 0)

# MANOVA on LDA with just Soul Runner vs. Pacer
var_SRvP = integer(length(BmchLDA_SRvP$RunnerCategory))
for (i in 1:38) {
  
  var_SRvP = cbind(var_SRvP, BmchLDA_SRvP[,i])
  
}
var_SRvP = var_SRvP[,-1]
manova_SRvP = manova(var_SRvP ~ BmchLDA_SRvP$RunnerCategory)
summary(manova_SRvP, test = "Wilks")

# MANOVA on LDA with just Goal Seeker vs. Pacer
var_GSvP = integer(length(BmchLDA_GSvP$RunnerCategory))
for (i in 1:38) {
  
  var_GSvP = cbind(var_GSvP, BmchLDA_GSvP[,i])
  
}
var_GSvP = var_GSvP[,-1]
manova_GSvP = manova(var_GSvP ~ BmchLDA_GSvP$RunnerCategory)
summary(manova_GSvP, test = "Wilks", tol = 0)

# MANOVA on LDA with just Goal Seeker vs. Soul Runner
var_GSvSR = integer(length(BmchLDA_GSvSR$RunnerCategory))
for (i in 1:38) {
  
  var_GSvSR = cbind(var_GSvSR, BmchLDA_GSvSR[,i])
  
}
var_GSvSR = var_GSvSR[,-1]
manova_GSvSR = manova(var_GSvSR ~ BmchLDA_GSvSR$RunnerCategory)
summary(manova_GSvSR, test = "Wilks")

