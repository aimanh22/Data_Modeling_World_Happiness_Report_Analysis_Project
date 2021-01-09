library(mice);library(miceMNAR)
library(VIM); library(lattice)
######################Loading the data from the merged file######
m<-new_happiness_missing_data <- read.csv("C:/Users/Home/Downloads/tab_inter3r.csv")
summary(m)
m<-m[complete.cases(m$Year),]
m<-m[complete.cases(m$X),]
m$X.1<-NULL
m$X<-NULL
m$Year<-NULL
m$Country<-NULL
head(m)
m$Region<-factor(m$Region)
############################################################################################## Missing Data Pattern######
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

####################################################################################### Multiple Imputation
num_imputation=5
imp_pmm <- mice(m,m=num_imputation,defaultMethod=c("pmm","logreg","polyreg","polr"),
                print=F)
densityplot(imp_pmm)
d<-rep(list(),num_imputation)
#####################################Preprocessing the data######################################
for (i in c(1:num_imputation)){
  d[[i]]<-complete(imp_pmm,i)
  d[[i]]$Happiness.Score<-floor(d[[i]]$Happiness.Score)
  d[[i]]$happinesslevel[d[[i]]$Happiness.Score<5]<-"Low"
  d[[i]]$happinesslevel[d[[i]]$Happiness.Score==5]<-"Mid"
  d[[i]]$happinesslevel[d[[i]]$Happiness.Score>5]<-"High"
  d[[i]]$happinesslevel<-ordered(d[[i]]$happinesslevel,levels=c("Low","Mid","High"))
  d[[i]]$Region[d[[i]]$Region=="Commonwealth of Independent States"]<-"Europe"
  d[[i]]$Region<-factor(d[[i]]$Region)
  d[[i]]<-d[[i]][complete.cases(d[[i]]),]
  table(d[[i]]$happinesslevel, d[[i]]$Region)
  d[[i]]$Log.GDP<-d[[i]]$Log.GDP-mean(d[[i]]$Log.GDP)
  d[[i]]$Generosity<-d[[i]]$Generosity*100
  d[[i]]$Perceptions.Corruption<-d[[i]]$Perceptions.Corruption*100
  d[[i]]$Freedom.Choices<-d[[i]]$Freedom.Choices*100
  d[[i]]$Social.support<-d[[i]]$Social.support*100
  d[[i]]$Generosity<-d[[i]]$Generosity-mean(d[[i]]$Generosity)
  d[[i]]$Perceptions.Corruption<-d[[i]]$Perceptions.Corruption-mean(d[[i]]$Perceptions.Corruption)
  d[[i]]$Freedom.Choices<-d[[i]]$Freedom.Choices-mean(d[[i]]$Freedom.Choices)
  d[[i]]$Social.support<-d[[i]]$Social.support-mean(d[[i]]$Social.support)
  d[[i]]$Life.Expectancy<-d[[i]]$Life.Expectancy-median(d[[i]]$Life.Expectancy)
}
####################################The EDA on the dataset#############################
library(ggplot2)
library(gridExtra)

plot_list <- list() 
for (i in c(1:num_imputation)){
  plot_list[[i]]<-ggplot(d[[i]],aes(x=happinesslevel))+geom_histogram(stat = "count",color="blue",fill=rainbow(3))+theme_classic()
}
grid.arrange(grobs=plot_list,ncol=5)

for (i in c(1:num_imputation)){
  print(table(d[[i]]$happinesslevel, d[[i]]$Region))
  print(prop.table(table(d[[i]]$happinesslevel, d[[i]]$Region), 2))
  print(chisq.test(table(d[[i]]$happinesslevel, d[[i]]$Region)))
  plot_list[[i]]<-ggplot(d[[i]],aes(x=happinesslevel,color=happinesslevel))+geom_histogram(stat = "count",color="blue",fill=rainbow(12))+theme_classic()+facet_wrap(~Region)  
}
grid.arrange(grobs=plot_list,ncol=5)

plot_list <- list() 
for (a in colnames(m)[c(2:7)])
{
  for (i in c(1:num_imputation)){
    plot_list[[i]]<-ggplot(d[[i]],aes_string(y="happinesslevel",x=a,color="happinesslevel"))+geom_boxplot()+theme(legend.position="none")
    #print(plot_list[[i]])
  }
  grid.arrange(grobs=plot_list,ncol=5)
}

plot_list <- list() 
for (a in colnames(m)[c(2:7)])
{
  for (i in c(1:num_imputation)){
    plot_list[[i]]<-ggplot(d[[i]],aes_string(y="happinesslevel",x=a,color="happinesslevel"))+geom_boxplot()+theme_classic()+facet_wrap(~Region)+theme(legend.position="none")
    #print(plot_list[[i]])
  }
  grid.arrange(grobs=plot_list,ncol=num_imputation)
}
##############################################Polr models##########################################
library(lmerTest)
library(MASS)
library(performance)
###Glancing at the AIC stepwise models
model_d<-rep(list(),num_imputation)
for (i in c(1:num_imputation)){
  NullModel<-polr(happinesslevel~1,data=d[[i]])
  summary(NullModel)
  FullModel<-polr(happinesslevel~(Life.Expectancy+Freedom.Choices+Perceptions.Corruption+Social.support+Generosity+Log.GDP)*Region+Log.GDP:Life.Expectancy+Log.GDP:Perceptions.Corruption+Log.GDP:Social.support+Life.Expectancy:Social.support+Log.GDP:Freedom.Choices+Generosity:Social.support+Generosity:Life.Expectancy, data = d[[i]])
  model_d[[i]] <- step(NullModel, scope = formula(FullModel),direction="both",trace=0)
  print(model_d[[i]]$call)
}
for (i in c(1:num_imputation)){
  print(model_d[[i]]$call)
}

#The final model is found using imputed dataset 1, the code for its model selectio can be found in the file model_selection.R#
#Applying the final model on all the datasets
my_model_d<-rep(list(),num_imputation)
for (i in c(1:num_imputation)){
  my_model_d[[i]]<-polr(happinesslevel ~ Log.GDP + Generosity + Social.support + Life.Expectancy+Freedom.Choices+ Log.GDP:Life.Expectancy+ Log.GDP:Social.support+Generosity:Social.support+(Log.GDP+Freedom.Choices)*Region, data = d[[i]])
  }

#Printing out the stats to check for consistency across datasets
for (i in c(1:num_imputation)){
  print(summary(my_model_d[[i]]))
  print(confint.default(my_model_d[[i]]))
  print(AIC(model_d[[i]]))
  print(AIC(my_model_d[[i]]))
  print(check_collinearity(my_model_d[[i]]))
  print(anova(my_model_d[[i]],model_d[[i]]))
}

####################################Polr model assessment#################
library(arm)
library(pROC)
library(e1071)
library(caret)

#Quick check for consistency across dataset. The main assessment done on the imputed dataset found in polr_diag.R file##
for (i in c(1:num_imputation)){
  predprobs <- fitted(my_model_d[[i]]) 
  predprobs[1:3,]
  rawresid1 <- (d[[i]]$happinesslevel=="Low") -  predprobs[,1]
  rawresid2 <- (d[[i]]$happinesslevel=="Mid") -  predprobs[,2]
  rawresid3 <- (d[[i]]$happinesslevel=="High") -  predprobs[,3]
  par(mfcol = c(1,3))
  print(binnedplot(d[[i]]$Log.GDP, rawresid1, xlab = "GDP", ylab = "Raw residuals", main = "Binned plot: viewcat = 1"))
  print(binnedplot(d[[i]]$Log.GDP, rawresid2, xlab = "GDP", ylab = "Raw residuals", main = "Binned plot: viewcat = 2"))
  print(binnedplot(d[[i]]$Log.GDP, rawresid3, xlab = "GDP", ylab = "Raw residuals", main = "Binned plot: viewcat = 3"))
  
  print(binnedplot(d[[i]]$Life.Expectancy, rawresid1, xlab = "LE", ylab = "Raw residuals", main = "Binned plot: viewcat = 1"))
  print(binnedplot(d[[i]]$Life.Expectancy, rawresid2, xlab = "LE", ylab = "Raw residuals", main = "Binned plot: viewcat = 2"))
  print(binnedplot(d[[i]]$Life.Expectancy, rawresid3, xlab = "LE", ylab = "Raw residuals", main = "Binned plot: viewcat = 3"))
  
  print(binnedplot(d[[i]]$Social.support, rawresid1, xlab = "SS", ylab = "Raw residuals", main = "Binned plot: viewcat = 1"))
  print(binnedplot(d[[i]]$Social.support, rawresid2, xlab = "SS", ylab = "Raw residuals", main = "Binned plot: viewcat = 2"))
  print(binnedplot(d[[i]]$Social.support, rawresid3, xlab = "SS", ylab = "Raw residuals", main = "Binned plot: viewcat = 3"))
  
  print(binnedplot(d[[i]]$Perceptions.Corruption, rawresid1, xlab = "PC", ylab = "Raw residuals", main = "Binned plot: viewcat = 1"))
  print(binnedplot(d[[i]]$Perceptions.Corruption, rawresid2, xlab = "PC", ylab = "Raw residuals", main = "Binned plot: viewcat = 2"))
  print(binnedplot(d[[i]]$Perceptions.Corruption, rawresid3, xlab = "PC", ylab = "Raw residuals", main = "Binned plot: viewcat = 3"))
  
  print(binnedplot(d[[i]]$Freedom.Choices, rawresid1, xlab = "FC", ylab = "Raw residuals", main = "Binned plot: viewcat = 1"))
  print(binnedplot(d[[i]]$Freedom.Choices, rawresid2, xlab = "FC", ylab = "Raw residuals", main = "Binned plot: viewcat = 2"))
  print(binnedplot(d[[i]]$Freedom.Choices, rawresid3, xlab = "FC", ylab = "Raw residuals", main = "Binned plot: viewcat = 3"))
  
  print(binnedplot(d[[i]]$Generosity, rawresid1, xlab = "Generosity", ylab = "Raw residuals", main = "Binned plot: viewcat = 1"))
  print(binnedplot(d[[i]]$Generosity, rawresid2, xlab = "Generosity", ylab = "Raw residuals", main = "Binned plot: viewcat = 2"))
  print(binnedplot(d[[i]]$Generosity, rawresid3, xlab = "Genrosity", ylab = "Raw residuals", main = "Binned plot: viewcat = 3"))
}

########################################CLMM models################################get
library(ordinal)

#The polr model converted to the clmm model for all datsets
model_clmm<-rep(list(),num_imputation)
for (i in c(1:num_imputation)){
  model_clmm[[i]]<-clmm(happinesslevel ~ Log.GDP + Generosity + Social.support + Life.Expectancy+Freedom.Choices+ Log.GDP:Life.Expectancy+ Log.GDP:Social.support+Generosity:Social.support+(Log.GDP+Freedom.Choices|Region), data = d[[i]])
}
for (i in c(1:num_imputation)){
  print(AIC(model_clmm[[i]]))
  print(summary(model_clmm[[i]]))
  print(check_collinearity(model_clmm[[i]]))
}
#############################################################Pooling#############################
#Pooling the clmm models for all the imputed data to get main effect estimates
reg_imp_pmm <- with(data=imp_pmm, model_clmm[[1]])
reg_p <- pool(reg_imp_pmm)
summary(reg_p)

#Getting the random effect by calculating the mean of random effects using all imputed datasets
r<-rep(list(),num_imputation)
for (i in c(1:num_imputation)){
  r[[i]]<-exp(ranef(model_clmm[[i]])$Region)
}
random_eff<-(r[[1]]+r[[2]]+r[[3]]+r[[4]]+r[[5]])/5

#Displaying dot product for imputed dataset 1
library(reshape2)
melt.ranef.clmm <- function(re,cv) {
  reList <- lapply(re,
                   function(x) {
                     x$id <- reorder(factor(rownames(x)),x[,1])
                     return(melt(x,id.var="id"))
                   })
  cvList <- lapply(cv,melt,id.var=NULL,value.name="se")
  mm <- Map(cbind,reList,cvList)
  return(mm)
}

ss<-rep(list(),num_imputation)
for (i in c(1:num_imputation)){
  ss[[i]]<- melt.ranef.clmm(ranef(model_clmm[[i]]),condVar(model_clmm[[i]]))
  ss[[i]]<- ss[[i]][[1]][,c(1,2,3,5)]
}
ss_mean<-ss[[1]]
ss_mean[,c(3)]<-(ss[[1]][[3]]+ss[[3]][[3]]+ss[[3]][[3]]+ss[[4]][[3]]+ss[[5]][[3]])/2

ss2<-ss[[1]] 
theme_set(theme_bw())
ggplot(ss[[1]],aes(value,id))+
  geom_errorbarh(aes(xmin=value-1.96*se,xmax=value+1.96*se),
                 height=0)+
  ggtitle(names(ss2))+
  geom_point(colour="blue")+
  facet_wrap(~variable,scale="free_x")
