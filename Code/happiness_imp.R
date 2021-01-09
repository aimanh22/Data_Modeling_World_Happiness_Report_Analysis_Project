library(mice);library(miceMNAR)
library(VIM); library(lattice)

m<-new_happiness_missing_data <- read.csv("C:/Users/Home/Downloads/tab_inter3r.csv")
summary(m)
m<-m[complete.cases(m$Year),]
m<-m[complete.cases(m$X),]
m$X.1<-NULL
m$X<-NULL
head(m)
#####Scale###################
n<-m
###############################################
n$Happiness.Score<-floor(n$Happiness.Score)
n$happinesslevel[n$Happiness.Score<5]<-"Low"
n$happinesslevel[n$Happiness.Score==5]<-"Mid"
n$happinesslevel[n$Happiness.Score>5]<-"High"
n$happinesslevel<-ordered(n$happinesslevel,levels=c("Low","Mid","High"))
n$Perceptions.Corruption<-as.numeric(n$Perceptions.Corruption)
n$Country<-factor(n$Country)
n<-n[-which(n$Region=="Australia and New Zealand"),]
n$Region[n$Region=="Middle East and Northern Africa" | n$Region=="Sub-Saharan Africa"]<-"Middle East and Africa"
n$Region[n$Region=="Southern Asia" | n$Region=="Southeastern Asia"|n$Region=="Eastern Asia"]<-"Southern and Eastern Asia"
n$Region[n$Region=="Central and Eastern Europe" | n$Region=="Western Europe"]<-"Europe"
n$Region[n$Region=="North America"|n$Region=="Latin America and Caribbean"]<-"Americas"
#n$Region[n$Region=="NA"]<-NA
################################################################
#n$Region<-NULL#factor(n$Region)
m$Country<-factor(m$Country)
#m$Country<-factor(n$Country)
###############################Rescaling Variables##############################
#n$Log.GDP<-n$Log.GDP-mean(n$Log.GDP)
#n$GDP<-exp(n$Log.GDP)
################################Do this if required###############################
n$Generosity<-n$Generosity*100
n$Perceptions.Corruption<-n$Perceptions.Corruption*100
n$Freedom.Choices<-n$Freedom.Choices*100
n$Social.support<-n$Social.support*100
n$Generosity<-n$Generosity-mean(n$Generosity)
n$Perceptions.Corruption<-n$Perceptions.Corruption-mean(n$Perceptions.Corruption)
n$Freedom.Choices<-n$Freedom.Choices-mean(n$Freedom.Choices)
n$Social.support<-n$Social.support-mean(n$Social.support)
n$Life.Expectancy<-n$Life.Expectancy-median(n$Life.Expectancy)
n$Year<-factor(n$Year)
##############################################################################################
md.pattern(m)
aggr(m,col=c("lightblue3","darkred"),numbers=TRUE,sortVars=TRUE,
     labels=names(m),cex.axis=.7,gap=3,
     ylab=c("Proportion missing","Missingness pattern"))

par(mfrow=c(2,4))

for (a in colnames(n)){
  for (name in colnames(n)){
    marginplot(n[,c(name,a)],col=c("lightblue3","darkred"),cex.numbers=1.2,pch=19)
  }
}

#######################################Joint###############################################
JointModelEq <- generate_JointModelEq(data=m,varMNAR = "Perceptions.Corruption")

arg <- MNARargument(data=m,varMNAR="Perceptions.Corruption",JointModelEq=JointModelEq)
imputationNAR <- mice(data = arg$data_mod,
                    method = arg$method,
                    predictorMatrix = arg$predictorMatrix,
                    JointModelEq=arg$JointModelEq,
                    control=arg$control,
                    maxit=1,m=5)
#######################################################################################
imp_pmm <- mice(m,m=10,defaultMethod=c("pmm","logreg","polyreg","polr"),
                print=F)
pred<-imp_pmm$predictorMatrix
pred[9,3]<-0
pred[9,6]<-0
pred[9,8]<-0
pred[4,4:9]<-0
pred[5,4:6]<-0
#########Changed Predictor Matrix######################################
imp_pmm <- mice(m,m=15,defaultMethod=c("pmm","logreg","polyreg","polr"),
                print=F,predictorMatrix = pred)
densityplot(imp_pmm)
d1 <- complete(imp_pmm,1); sum(is.na(d1))
dim(d1[complete.cases(d1),])
write.csv(d1,"imputed1.csv")

d5 <- complete(imp_pmm,5); sum(is.na(d5))
dim(d5[complete.cases(d5),])
write.csv(d5,"imputed5.csv")
d5

#############################################################Pooling#############################
#does work but random intercept
reg_imp_pmm <- with(data=imp_pmm, model1)
reg_p <- pool(reg_imp_pmm)
summary(reg_p)