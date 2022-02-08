
library(readxl)
library(tidyverse)
BigCushAvgSock <- as.data.frame(read_excel("C:/Users/msalzano/Box/UMass/Brooks/BigCush/AvgSockBigCush2.xlsx"))
unqIDavg = unique(BigCushAvgSock$Initials)
idx = duplicated(BigCushAvgSock$Initials)
UnqSock = BigCushSock[idx,]

SubjVars = BigCushAvgSock[,c(76,134,135,166,169,179)]

for (i in c(1,2,3,6)) {
  
     SubjVars[,i] = as.numeric(SubjVars[,i])
    }

SubjVars$Gender = as.factor(SubjVars$Gender)
SubjVars$RunnerCategory = as.factor(SubjVars$RunnerCategory)
SubjVars$BMI = round(SubjVars$BodyMass/((SubjVars$Height)^2), digits = 1)
errsAge = which(SubjVars$Age_fixed_ < 0)  
SubjVars$Age_fixed_[errsAge] = NA
folderlocation = getwd()
VarList = c(1,2,3,6,7)
for (i in VarList) {
  VarName = names(SubjVars)[i]
  #jpeg(filename = paste0(VarName,".jpeg"))
  VarPlot = ggplot(SubjVars, aes(x = Gender, y = SubjVars[,i], fill = RunnerCategory))+
    geom_violin(trim = FALSE) +
    labs(y = VarName) +
   stat_summary(data = SubjVars, aes(x=Gender, y=SubjVars[,i], group=RunnerCategory), fun.y=mean, geom="point", shape =3, size=3, color="red") +
   stat_summary(fun.y=median, geom="point", shape=0, size=3, color = "blue") +
    theme_classic()
  ggsave(VarPlot, file=paste0(folderlocation, '/ViolinPlots/', VarName, '.png'), scale =2) 
}
