library(ordinal)
library(MASS)
library(performance)
##############################Model Selection##########################
#AIC stepwise on the dataset 1
NullModel<-polr(happinesslevel~1,data=d[[i]])
summary(NullModel)
FullModel<-polr(happinesslevel~(Life.Expectancy+Freedom.Choices+Perceptions.Corruption+Social.support+Generosity+Log.GDP)*Region+Log.GDP:Life.Expectancy+Log.GDP:Perceptions.Corruption+Log.GDP:Social.support+Life.Expectancy:Social.support+Log.GDP:Freedom.Choices+Generosity:Social.support+Generosity:Life.Expectancy, data = d[[1]])
stepwise_AIC<- step(NullModel, scope = formula(FullModel),direction="both",trace=0)
####p_i is the AIC stepwise model
p_i<-polr(formula = happinesslevel ~ Log.GDP + Region + Freedom.Choices + 
           Generosity + Social.support + Life.Expectancy + Perceptions.Corruption+
           Region:Freedom.Choices + Log.GDP:Social.support + Log.GDP:Region + 
           Social.support:Life.Expectancy + Log.GDP:Freedom.Choices + 
           Log.GDP:Life.Expectancy + Region:Perceptions.Corruption + 
           Log.GDP:Perceptions.Corruption, data = d[[1]])
summary(p_i)
check_collinearity(p_i)

####Not defined variance covariance matrix
c_i<-clmm(happinesslevel ~ Log.GDP + Region + Freedom.Choices + 
        Generosity + Social.support + Life.Expectancy + Perceptions.Corruption+ Log.GDP:Social.support + Social.support:Life.Expectancy + Log.GDP:Freedom.Choices + 
           Log.GDP:Life.Expectancy + 
           Log.GDP:Perceptions.Corruption+(Log.GDP+Perceptions.Corruption+Freedom.Choices|Region), data = d[[1]])
summary(c_i)
check_collinearity(c_i)

#############removed gdp:pc
p1<-polr(formula = happinesslevel ~ Log.GDP + Region + Freedom.Choices + 
           Generosity + Social.support + Life.Expectancy + Perceptions.Corruption+
           Region:Freedom.Choices + Log.GDP:Social.support + Log.GDP:Region + 
           Social.support:Life.Expectancy + Log.GDP:Freedom.Choices + 
           Log.GDP:Life.Expectancy + Region:Perceptions.Corruption, data = d[[1]])
summary(p1)
check_collinearity(p1)

##Not defined variance-covariance matrix
c1<-clmm(happinesslevel ~ Log.GDP + Region + Freedom.Choices + 
           Generosity + Social.support + Life.Expectancy + Perceptions.Corruption+ Log.GDP:Social.support + Social.support:Life.Expectancy + Log.GDP:Freedom.Choices +  Log.GDP:Life.Expectancy+(Log.GDP+Perceptions.Corruption+Freedom.Choices|Region), data = d[[1]])
summary(c1)
check_collinearity(c1)

###################removed gdp:pc, reg:Pc
p1_rc<-polr(formula = happinesslevel ~ Log.GDP + Region + Freedom.Choices + 
           Generosity + Social.support + Life.Expectancy + Perceptions.Corruption+
           Region:Freedom.Choices + Log.GDP:Social.support + Log.GDP:Region + 
           Social.support:Life.Expectancy + Log.GDP:Freedom.Choices + 
           Log.GDP:Life.Expectancy , data = d[[1]])
summary(p1_rc)
check_collinearity(p1_rc)
anova(p1,p1_rc)

##Not defined variance-covariance matrix
c1_rc<-clmm(happinesslevel ~ Log.GDP + Region + Freedom.Choices + 
           Generosity + Social.support + Life.Expectancy + Perceptions.Corruption+ Log.GDP:Social.support + Social.support:Life.Expectancy + Log.GDP:Freedom.Choices +  Log.GDP:Life.Expectancy+(Log.GDP+Freedom.Choices|Region), data = d[[1]])
summary(c1_rc)
check_collinearity(c1_rc)


##############removed gdp:le and now it does have a defined matrix, corr an issue
p2<-polr(formula = happinesslevel ~ Log.GDP + Region + Freedom.Choices + 
           Generosity + Social.support + Life.Expectancy + Perceptions.Corruption+
           Region:Freedom.Choices + Log.GDP:Social.support + Log.GDP:Region + 
           Social.support:Life.Expectancy + Log.GDP:Freedom.Choices + 
           + Region:Perceptions.Corruption, data = d[[1]])
summary(p2)
check_collinearity(p2)

c2<-clmm(happinesslevel ~ Log.GDP + Region + Freedom.Choices + 
           Generosity + Social.support + Life.Expectancy + Perceptions.Corruption+ Log.GDP:Social.support + Social.support:Life.Expectancy + Log.GDP:Freedom.Choices +(Log.GDP+Perceptions.Corruption+Freedom.Choices|Region), data = d[[1]])
summary(c2)
check_collinearity(c2)

##################so removing le:ss, le:gdp, gdp:pc
p3<-polr(formula = happinesslevel ~ Log.GDP + Region + Freedom.Choices + 
           Generosity + Social.support + Life.Expectancy + Perceptions.Corruption+
           Region:Freedom.Choices + Log.GDP:Social.support + Log.GDP:Region + 
            + Log.GDP:Freedom.Choices + Region:Perceptions.Corruption, data = d[[1]])
summary(p3)
check_collinearity(p3)

##gdp:le makes the vcov undefined,corr bw fc and all an issue, gdp:ss and gdp:fc a problem
c3<-clmm(happinesslevel ~ Log.GDP + Region + Freedom.Choices + 
           Generosity + Social.support + Life.Expectancy + Perceptions.Corruption+ Log.GDP:Social.support + Log.GDP:Freedom.Choices +(Log.GDP+Perceptions.Corruption+Freedom.Choices|Region), data = d[[1]])
summary(c3)
check_collinearity(c3)

###########check r:pc, anova sig at 0.05, if for clmm an issue
p3_rc<-polr(formula = happinesslevel ~ Log.GDP + Region + Freedom.Choices + 
           Generosity + Social.support + Life.Expectancy + Perceptions.Corruption+
           Region:Freedom.Choices + Log.GDP:Social.support + Log.GDP:Region + 
           + Log.GDP:Freedom.Choices , data = d[[1]])
summary(p3_rc)
check_collinearity(p3_rc)

##gdp:le makes the vcov undefined,corr bw fc and all an issue, gdp:ss and gdp:fc a problem
c3_rc<-clmm(happinesslevel ~ Log.GDP + Region + Freedom.Choices + 
           Generosity + Social.support + Life.Expectancy + Perceptions.Corruption+ Log.GDP:Social.support + Log.GDP:Freedom.Choices +(Log.GDP+Freedom.Choices|Region), data = d[[1]])
summary(c3_rc)
check_collinearity(c3_rc)
anova(p3,p3_rc)
anova(c3,c3_rc)

##########################
##########removed le:ss, le:gdp
p4<-polr(formula = happinesslevel ~ Log.GDP + Region + Freedom.Choices + 
           Generosity + Social.support + Life.Expectancy + Perceptions.Corruption+
           Region:Freedom.Choices + Log.GDP:Social.support + Log.GDP:Region + 
           + Log.GDP:Freedom.Choices + Region:Perceptions.Corruption+Log.GDP:Perceptions.Corruption, data = d[[1]])
summary(p4)
check_collinearity(p4)

##Not defined variance-covariance matrix
c4<-clmm(happinesslevel ~ Log.GDP + Region + Freedom.Choices + 
           Generosity + Social.support + Life.Expectancy + Perceptions.Corruption+ Log.GDP:Social.support + Log.GDP:Freedom.Choices+Log.GDP:Perceptions.Corruption+(Log.GDP+Perceptions.Corruption+Freedom.Choices|Region), data = d[[1]])
summary(c4)
check_collinearity(c4)

##variables to be considered to be removed-pc/gen, ss:le, gdp:le,gdp:fc,gdp:pc(def),region:pc

#remove r:pc, gdp:pc, gdp:le #####does not converge
p5<-polr(formula = happinesslevel ~ Log.GDP + Region + Freedom.Choices + 
           Generosity + Social.support + Life.Expectancy + Perceptions.Corruption+
           Region:Freedom.Choices + Log.GDP:Social.support + Log.GDP:Region +Social.support:Life.EXpectancy+ 
           + Log.GDP:Freedom.Choices, data = d[[1]])
summary(p5)
check_collinearity(p5)

c5<-clmm(happinesslevel ~ Log.GDP + Region + Freedom.Choices + 
           Generosity + Social.support + Life.Expectancy + Perceptions.Corruption+ Log.GDP:Social.support + Log.GDP:Freedom.Choices +Social.support:Life.Expectancy+(Log.GDP+Perceptions.Corruption+Freedom.Choices|Region), data = d[[1]])
summary(c5)
check_collinearity(c5)


#remove r:pc, gdp:pc, ss:le
p6<-polr(formula = happinesslevel ~ Log.GDP + Region + Freedom.Choices + 
           Generosity + Social.support + Life.Expectancy + Perceptions.Corruption+
           Region:Freedom.Choices + Log.GDP:Social.support + Log.GDP:Region + 
           + Log.GDP:Freedom.Choices+Life.Expectancy:Log.GDP, data = d[[1]])
summary(p6)
check_collinearity(p6)

##gdp:le makes the vcov undefined,corr bw fc and all an issue, gdp:ss and gdp:fc a problem
c6<-clmm(happinesslevel ~ Log.GDP + Region + Freedom.Choices + 
           Generosity + Social.support + Life.Expectancy + Perceptions.Corruption+ Log.GDP:Social.support + Log.GDP:Freedom.Choices +Social.support:Log.GDP+Life.Expectancy:Log.GDP+(Log.GDP+Perceptions.Corruption+Freedom.Choices|Region), data = d[[1]])
summary(c6)
check_collinearity(c6)

###remove pc:r, gdp:pc, ss:le, gdp:le; polr ok, clmm has high vif
p2_rc<-polr(formula = happinesslevel ~ Log.GDP + Region + Freedom.Choices + 
              Generosity + Social.support + Life.Expectancy + Perceptions.Corruption+
              Region:Freedom.Choices + Log.GDP:Social.support + Log.GDP:Region + 
              Log.GDP:Freedom.Choices , data = d[[1]])
summary(p2_rc)
check_collinearity(p1_rc)
check_collinearity(p2_rc)
anova(p2,p1_rc, test = "Chisq")

c2_rc<-clmm(happinesslevel ~ Log.GDP + Region + Freedom.Choices + 
              Generosity + Social.support + Life.Expectancy + Perceptions.Corruption+ Log.GDP:Social.support+ Log.GDP:Freedom.Choices+(Log.GDP+Freedom.Choices|Region), data = d[[1]])
summary(c2_rc)
check_collinearity(c1_rc)
check_collinearity(c2_rc)
anova(c1_rc,c2_rc)
###############################After getting a converging model

################################################Without PC in Slope
#significant anova, vif still bad, corr is absolutely screwed
p3_rc1<-polr(formula = happinesslevel ~ Log.GDP + Region + Freedom.Choices + 
              Generosity + Social.support + Life.Expectancy + Perceptions.Corruption+
              Region:Freedom.Choices +Log.GDP:Region+Log.GDP:Social.support+ Log.GDP:Freedom.Choices , data = d[[1]])
summary(p3_rc1)
check_collinearity(p3_rc1)

##gdp:le makes the vcov undefined,corr between fc and all an issue, gdp:ss and gdp:fc a problem
c3_rc1<-clmm(happinesslevel ~ Log.GDP + Region + Freedom.Choices + 
              Generosity + Social.support + Life.Expectancy + Perceptions.Corruption+ +Log.GDP:Social.support+Log.GDP:Freedom.Choices +(Log.GDP+Freedom.Choices|Region), data = d[[1]])
summary(c3_rc1)
check_collinearity(c3_rc1)
anova(p3_rc1,p3_rc)
anova(c3_rc1,c3_rc)

p3_rc2<-polr(formula = happinesslevel ~ Log.GDP + Region + Freedom.Choices + 
               Generosity + Social.support + Life.Expectancy + Perceptions.Corruption+
               Region:Freedom.Choices + Log.GDP:Freedpm.Choices + Log.GDP:Region , data = d[[1]])
summary(p3_rc2)
check_collinearity(p3_rc2)

##gdp:le makes the vcov undefined,corr bw fc and all an issue, gdp:ss and gdp:fc a problem
c3_rc2<-clmm(happinesslevel ~ Log.GDP + Region + Freedom.Choices + 
               Generosity + Social.support + Life.Expectancy + Perceptions.Corruption+ Log.GDP:Freedom.Choices +(Log.GDP+Freedom.Choices|Region), data = d[[1]])
summary(c3_rc2)
check_collinearity(c3_rc2)
anova(p3_rc1,p3_rc2)
anova(c3_rc1,c3_rc2)
##############################################################

ip1<-polr(happinesslevel ~ Log.GDP + Generosity + Social.support + Life.Expectancy+Freedom.Choices+Perceptions.Corruption+(Log.GDP+Freedom.Choices+Perceptions.Corruption)*Region, data = d[[1]])
ic1<-clmm(happinesslevel ~ Log.GDP + Generosity + Social.support + Life.Expectancy+Freedom.Choices+Perceptions.Corruption+(Log.GDP+Freedom.Choices+Perceptions.Corruption|Region), data = d[[1]])
summary(ip1)
check_collinearity(ip1)
summary(ic1)
check_collinearity(ic1)

ip2<-polr(happinesslevel ~ Log.GDP + Generosity + Social.support + Life.Expectancy+Freedom.Choices+Perceptions.Corruption+(Log.GDP+Freedom.Choices)*Region, data = d[[1]])
ic2<-clmm(happinesslevel ~ Log.GDP + Generosity + Social.support + Life.Expectancy+Freedom.Choices+Perceptions.Corruption+(Log.GDP+Freedom.Choices|Region), data = d[[1]])
summary(ip2)
check_collinearity(ip2)
summary(ic2)
check_collinearity(ic2)
anova(ip1,ip2)
anova(ic1,ic2) ###0.09265 .

ip3<-polr(happinesslevel ~ Log.GDP + Generosity + Social.support + Life.Expectancy+Freedom.Choices+Perceptions.Corruption+(Freedom.Choices+Perceptions.Corruption)*Region, data = d[[1]])
ic3<-clmm(happinesslevel ~ Log.GDP + Generosity + Social.support + Life.Expectancy+Freedom.Choices+Perceptions.Corruption+(Freedom.Choices+Perceptions.Corruption|Region), data = d[[1]])
summary(ip3)
check_collinearity(ip3)
summary(ic3)
check_collinearity(ic3)
anova(ip1,ip3)
anova(ic1,ic3)#### 0.03464 *


ip4<-polr(happinesslevel ~ Log.GDP + Generosity + Social.support + Life.Expectancy+Freedom.Choices+Perceptions.Corruption+(Log.GDP+Perceptions.Corruption)*Region, data = d[[1]])
ic4<-clmm(happinesslevel ~ Log.GDP + Generosity + Social.support + Life.Expectancy+Freedom.Choices+Perceptions.Corruption+(Log.GDP+Perceptions.Corruption|Region), data = d[[1]])
summary(ip4)
check_collinearity(ip4)
summary(ic4)
check_collinearity(ic4)
anova(ip1,ip4)
anova(ic1,ic4) ### 4.569e-06 ***

#For random slopes, gdp+ fc are the best if two are two to be retained. Signi anova +good vif

#####################################Adding interactions

iip1<-polr(happinesslevel ~ Log.GDP + Generosity + Social.support + Life.Expectancy+Freedom.Choices+Perceptions.Corruption+Social.support:Life.Expectancy+(Log.GDP+Freedom.Choices+Perceptions.Corruption)*Region, data = d[[1]])
iic1<-clmm(happinesslevel ~ Log.GDP + Generosity + Social.support + Life.Expectancy+Freedom.Choices+Perceptions.Corruption+Social.support:Life.Expectancy+(Log.GDP+Freedom.Choices+Perceptions.Corruption|Region), data = d[[1]])
summary(iip1)
check_collinearity(iip1)
summary(iic1)
check_collinearity(iic1)
anova(iip1,ip1)
anova(iic1,ic1)
#gdp:ss 4.33e-11 ***, vif high, corr might be a prob
#gdp:fc  0.928, vif high, corr ok-bad
#le:ss   1.082e-14 ***,vif high, corr ok-bad
#gdp:le  8.382e-09 ***, vif high, corr-bad

iip2<-polr(happinesslevel ~ Log.GDP + Generosity + Social.support + Life.Expectancy+Freedom.Choices+Perceptions.Corruption+Social.support:Life.Expectancy+(Log.GDP+Freedom.Choices)*Region, data = d[[1]])
iic2<-clmm(happinesslevel ~ Log.GDP + Generosity + Social.support + Life.Expectancy+Freedom.Choices+Perceptions.Corruption+Social.support:Life.Expectancy+(Log.GDP+Freedom.Choices|Region), data = d[[1]])
summary(iip2)
check_collinearity(iip2)
summary(iic2)
check_collinearity(iic2)
anova(iip2,ip2)
anova(iic2,ic2)
#gdp:ss 3.104e-10 ***, vif in control, corr better than all slopes
#gdp:fc  exp, high vif, high corr
#le:ss  1.23e-13 ***, vif in check, corr okay
#gdp:le 6.558e-07 ***, vif in check, corr okay
#fc:ss 0.01238 *, high vif, corr for gdp bad, ss+le doesn't perform as bad

###Expanding model above by adding all sig var
iip3<-polr(happinesslevel ~ Log.GDP + Generosity + Social.support + Life.Expectancy+Freedom.Choices+Perceptions.Corruption+Social.support:Life.Expectancy+Log.GDP:Life.Expectancy+Social.support:Log.GDP+ (Log.GDP+Freedom.Choices)*Region, data = d[[1]])
iic3<-clmm(happinesslevel ~ Log.GDP + Generosity + Social.support + Life.Expectancy+Freedom.Choices+Perceptions.Corruption+Social.support:Life.Expectancy++Log.GDP:Life.Expectancy+Social.support:Log.GDP+(Log.GDP+Freedom.Choices|Region), data = d[[1]])
summary(iip3)
check_collinearity(iip3)
summary(iic3)
check_collinearity(iic3)
#slightly high vif, corr okay


###############vif is okay with it
iip4<-polr(happinesslevel ~ Log.GDP + Generosity + Social.support + Life.Expectancy+Freedom.Choices+Perceptions.Corruption+Log.GDP:Life.Expectancy+Social.support:Log.GDP+ (Log.GDP+Freedom.Choices)*Region, data = d[[1]])
iic4<-clmm(happinesslevel ~ Log.GDP + Generosity + Social.support + Life.Expectancy+Freedom.Choices+Perceptions.Corruption+Log.GDP:Life.Expectancy+Social.support:Log.GDP+(Log.GDP+Freedom.Choices|Region), data = d[[1]])
summary(iip4)
check_collinearity(iip4)
summary(iic4)
check_collinearity(iic4)
anova(iip3,iip4)
anova(iic3,iic4) #if okay, ss le is a must

iip5<-polr(happinesslevel ~ Log.GDP + Generosity + Social.support + Life.Expectancy+Freedom.Choices+Perceptions.Corruption+Social.support:Life.Expectancy+Log.GDP:Life.Expectancy+ (Log.GDP+Freedom.Choices)*Region, data = d[[1]])
iic5<-clmm(happinesslevel ~ Log.GDP + Generosity + Social.support + Life.Expectancy+Freedom.Choices+Perceptions.Corruption+Social.support:Life.Expectancy+Log.GDP:Life.Expectancy+(Log.GDP+Freedom.Choices|Region), data = d[[1]])
summary(iip5)
check_collinearity(iip5)
summary(iic5)
check_collinearity(iic5)
anova(iip3,iip5)
anova(iic3,iic5) #vif is an issue slight

iip6<-polr(happinesslevel ~ Log.GDP + Generosity + Social.support + Life.Expectancy+Freedom.Choices+Perceptions.Corruption+Social.support:Life.Expectancy+Social.support:Log.GDP+ (Log.GDP+Freedom.Choices)*Region, data = d[[1]])
iic6<-clmm(happinesslevel ~ Log.GDP + Generosity + Social.support + Life.Expectancy+Freedom.Choices+Perceptions.Corruption+Social.support:Life.Expectancy+Social.support:Log.GDP+(Log.GDP+Freedom.Choices|Region), data = d[[1]])
summary(iip6)
check_collinearity(iip6)
summary(iic6)
check_collinearity(iic6)
anova(iip3,iip6)
anova(iic3,iic6) #vif is an issue, otherwise same, corr okay


anova(iip2,iip4)
anova(iic2,iic4) #ss:le=le:gdp+gdp:ss
AIC(iic2) #ss:le has lower aic.
AIC(iic4)


iip7<-polr(happinesslevel ~ Log.GDP + Generosity + Social.support + Life.Expectancy+Freedom.Choices+Perceptions.Corruption+Log.GDP:Life.Expectancy+Social.support:Log.GDP+Social.support:Freedom.Choices+ (Log.GDP+Freedom.Choices)*Region, data = d[[1]])
iic7<-clmm(happinesslevel ~ Log.GDP + Generosity + Social.support + Life.Expectancy+Freedom.Choices+Perceptions.Corruption+Log.GDP:Life.Expectancy+Social.support:Log.GDP+Social.support:Freedom.Choices+(Log.GDP+Freedom.Choices|Region), data = d[[1]])
summary(iip7)
check_collinearity(iip7)
summary(iic7)
check_collinearity(iic7)
anova(iic7,iic4) #not sig

iip8<-polr(happinesslevel ~ Log.GDP + Generosity + Social.support + Life.Expectancy+Freedom.Choices+Perceptions.Corruption+Life.Expectancy:Social.support+Social.support:Freedom.Choices+ (Log.GDP+Freedom.Choices)*Region, data = d[[1]])
iic8<-clmm(happinesslevel ~ Log.GDP + Generosity + Social.support + Life.Expectancy+Freedom.Choices+Perceptions.Corruption+Life.Expectancy:Social.support+Social.support:Freedom.Choices+(Log.GDP+Freedom.Choices|Region), data = d[[1]])
summary(iip8)
check_collinearity(iip8)
summary(iic8)
check_collinearity(iic8)
anova(iic8,iic2) #not sig

iipp<-polr(happinesslevel ~ Log.GDP + Generosity + Social.support + Life.Expectancy+Freedom.Choices+Social.support:Life.Expectancy+(Log.GDP+Freedom.Choices)*Region, data = d[[1]])
iicp<-clmm(happinesslevel ~ Log.GDP + Generosity + Social.support + Life.Expectancy+Freedom.Choices+Social.support:Life.Expectancy+(Log.GDP+Freedom.Choices|Region), data = d[[1]])
summary(iipp)
check_collinearity(iipp)
summary(iicp)
check_collinearity(iicp)
anova(iipp,iip2)
anova(iicp,iic2)

iip21<-polr(happinesslevel ~ Log.GDP + Generosity + Social.support + Life.Expectancy+Freedom.Choices+Perceptions.Corruption+Social.support:Life.Expectancy+ Generosity:Social.support+(Log.GDP+Freedom.Choices)*Region, data = d[[1]])
iic21<-clmm(happinesslevel ~ Log.GDP + Generosity + Social.support + Life.Expectancy+Freedom.Choices+Perceptions.Corruption+Social.support:Life.Expectancy+Generosity:Social.support+(Log.GDP+Freedom.Choices|Region), data = d[[1]])
summary(iip21)
check_collinearity(iip21)
summary(iic21)
check_collinearity(iic21)
anova(iip21,iip2)
anova(iic21,iic2) #gen:ss 0.00425 **, slightly high vif, corr a little bad

model_imp<-clmm(happinesslevel ~ Log.GDP + Generosity + Social.support + Life.Expectancy+Freedom.Choices+ Log.GDP:Life.Expectancy+ Log.GDP:Social.support+Perceptions.Corruption+Generosity:Social.support+(Log.GDP+Freedom.Choices|Region), data = d[[1]])
summary(model_imp)
AIC(model_imp)
check_collinearity(model_imp) #great vif, aic 1941, pc not sig, corr good


###################The best model tested by all parameters#########
model_impp<-clmm(happinesslevel ~ Log.GDP + Generosity + Social.support + Life.Expectancy+Freedom.Choices+ Log.GDP:Life.Expectancy+ Log.GDP:Social.support+Generosity:Social.support+(Log.GDP+Freedom.Choices|Region), data = d[[1]])
summary(model_impp)
AIC(model_impp)
check_collinearity(model_impp)
anova(model_imp,model_impp)


model_imp2<-clmm(happinesslevel ~ Log.GDP + Generosity + Social.support + Life.Expectancy+Freedom.Choices+ Life.Expectancy:Social.support+Perceptions.Corruption+Generosity:Social.support+(Log.GDP+Freedom.Choices|Region), data = d[[1]])
summary(model_imp2)
AIC(model_imp2)
check_collinearity(model_imp2) #slight off vif, pc sig, corr okay, aic 1932
