#Crop Loss Models
library(arm)
library(AICcmodavg)
library(MuMIn)
hwc_mod<-read.csv("model_tadoba.csv", header=TRUE)

#subsetting the data for running crop loss models
crmod1<-hwc_mod[c(1:402, 404:nrow(hwc_mod)),c(1:ncol(hwc_mod))] 

#Testing for correlation between predictor variables 
corrcr<-crmod1[c(2,11,12,13,15,21,31:37)]
corrcr1<-cor(corrcr[,unlist(lapply(corrcr, is.numeric))], 
               use="pairwise.complete.obs", method="pearson") 
View(corrcr1)

#Models for crop loss 
CRHH<-list()
CRHH[[ 1]]<-standardize(glm(crhh~land+dp+dw+fc3km+elev+hht+crmgen,crmod1, family=binomial))
CRHH[[ 2]]<-standardize(glm(crhh~land+dp+fc3km+elev+hht+crfenc+crntwh+crgdan+crlgtn+crscrd+crbush,crmod1,family=binomial))
CRHH[[ 3]]<-standardize(glm(crhh~land+dp+fc3km+elev,crmod1,family=binomial))
CRHH[[ 4]]<-standardize(glm(crhh~land+hht+crmgen,crmod1,family=binomial))
CRHH[[ 5]]<-standardize(glm(crhh~land+dp+fc3km+elev+hht,crmod1, family=binomial))

names(CRHH)<-c(
  "land.parkdistance.water distance.forestcover.elevation.hhsize.cropmit",
  "land.parkdistance.forestcover.elevation.hhsize.fence.nightwatch.guardanimals.lighting.scaredevices.clearbush",
  "land.parkdistance.forestcover.elevation",
  "land.hhsize.cropmit",
  "land.parkdistance.forestcover.elevation.hhsize")

crhh<-aictab(CRHH) 
View(crhh)

#Predicted probability per household
predcr1 <- predict(CRHH[[1]], newdata = NULL, type = "response", se.fit = TRUE)
ppcr1 <- predcr1$fit
se1 <- predcr1$se.fit

#Average predicted probability
ppcr1aic <- ppcr1*0.97 #AICcWtof model1 = 0.97
se1aic <- se1*0.97

summary(ppcr1aic) #average estimated probability
summary(se1aic) #standard error


##################################################################
####Livestock Depredation Models
#subsetting the data for running livestock depredation models
lpmod1<-hwc_mod[c(1:402, 404:nrow(hwc_mod)),c(1:ncol(hwc_mod))] 

#Testing for correlation between predictor variables 
corrlp<-lpmod1[c(11,12,13,15,16,21,38:44,123)]
corrlp1<-cor(corrlp[,unlist(lapply(corrlp, is.numeric))], 
             use="pairwise.complete.obs", method="pearson") 
View(corrlp1)

#Models for livestock depredation 
LPHH<-list()
LPHH[[ 1]]<-standardize(glm(lphh~lvstkown+dp+dw+fc3km+elev+hht+lpmgen,lpmod1, family=binomial))
LPHH[[ 2]]<-standardize(glm(lphh~cattleown+dp+dw+fc3km+elev+hht+lpmgen,lpmod1,family=binomial))
LPHH[[ 3]]<-standardize(glm(lphh~lvstkown+dp+dw+fc3km+elev,lpmod1,family=binomial))
LPHH[[ 4]]<-standardize(glm(lphh~lvstkown+hht+lpmgen,lpmod1,family=binomial))
LPHH[[ 5]]<-standardize(glm(lphh~lvstkown+dp+fc3km+hht+lpmgen,lpmod1,family=binomial))

names(LPHH)<-c(
  "totallivestock.parkdistance.water distance.forestcover.elevation.hhsize.livesmit",
  "cattle.parkdistance.water distance.forestcover.elevation.hhsize.livesmit",
  "totallivestock.parkdistance.water distance.forestcover.elevation.",
  "totallivestock.hhsize.livesmit",
  "totallivestock.parkdistance.forestcover.hhsize.livesmit")

lphh<-aictab(LPHH)
View(lphh)

#Model averaging 
lphh.avg <- list()
lphh.avg[[1]] <- LPHH[[1]]
lphh.avg[[2]] <- LPHH[[2]]
names(lphh.avg) <- c("lvstkown+dp+dw+fc3km+elev+hht+lpmgen",
                     "cattleown+dp+dw+fc3km+elev+hht+lpmgen")

#Intercept 
mod_int<-(-1.58*.77+-1.58*.22/(.77+.22))
mod_int

#Standard error 
mod_se<-(0.14*.77+0.14*.22)/(.77+.22)
mod_se

lvstkownavg <- modavg(cand.set=lphh.avg, parm = "z.lvstkown")
cattleownavg <- modavg(lphh.avg, parm = "z.cattleown")
dpavg <- modavg(lphh.avg, parm = "z.dp")
dwavg <- modavg(lphh.avg, parm = "z.dw")
fc3kmavg <- modavg(lphh.avg, parm = "z.fc3km")
elevavg <- modavg(lphh.avg, parm = "z.elev")
hhtavg <- modavg(lphh.avg, parm = "z.hht")
lpmgenavg <- modavg(lphh.avg, parm = "z.lpmgen")


#Predicted probability per household
pred1 <- predict(LPHH[[1]], newdata = NULL, type = "response", se.fit = TRUE)
pp1 <- pred1$fit
se1 <- pred1$se.fit

pred2 <- predict(LPHH[[2]], newdata = NULL, type = "response", se.fit = TRUE)
pp2 <- pred2$fit
se2 <- pred2$se.fit

#Average predicted probability
pp1aic <- pp1*0.77 #AICcWtof model1 = 0.77
pp2aic <- pp2*0.22 #AICcWtof model2 = 0.22

se1aic <- se1*0.77
se2aic <- se2*0.22

ppmodavg <- pp1aic + pp2aic
semodavg <- se1aic + se2aic 
lpmod1_avg <- as.data.frame(cbind(ppmodavg, semodavg))

summary(lpmod1_avg$ppmodavg) #average estimated probability
summary(lpmod1_avg$semodavg) #standard error


##################################################################
####Compensation Models
#subsetting the data for running compensation models
compmod1<-hwc_mod[c(1:402, 404:nrow(hwc_mod)),c(1:ncol(hwc_mod))]

#Total number of literate individuals
compmod1$lit<-rowSums(compmod1[,c("less8","totwelve","abvtwelve")])
compmod1$edutotal<-rowSums(compmod1[,c("Il","less8","totwelve","abvtwelve")])
compmod1$pctlit<-(compmod1$lit/compmod1$edutotal)*100

#Testing for correlation between predictor variables 
corrcomp<-compmod1[c(4,11,98:105,135,139)]
corrcomp1<-cor(corrcomp[,unlist(lapply(corrcomp, is.numeric))], 
               use="pairwise.complete.obs", method="pearson") 
View(corrcomp1)

#Models for livestock depredation 
COMPHH<-list()
COMPHH[[ 1]]<-standardize(glm(comprec~anymitiguse+dp+conflyear+rptgovcr+rptgovlp,compmod1, family=binomial))
COMPHH[[ 2]]<-standardize(glm(comprec~anymitiguse+dp+conflyear,compmod1, family=binomial))
COMPHH[[ 3]]<-standardize(glm(comprec~anymitiguse+dp+conflyear+pctlit,compmod1, family=binomial))
COMPHH[[ 4]]<-standardize(glm(comprec~anymitiguse+dp+rptgovcr+crpig+crchtl+crnlgi,compmod1, family=binomial))
COMPHH[[ 5]]<-standardize(glm(comprec~anymitiguse+dp+rptgovlp+lplprd,compmod1, family=binomial))
COMPHH[[ 6]]<-standardize(glm(comprec~anymitiguse+dp+lptgr+lplprd,compmod1, family=binomial))

names(COMPHH)<-c(
  "mitiguse.parkdistance.yearsofconflict.crgovtrpt.lpgovtrpt",
  "mitiguse.parkdistance.yearsofconflict",
  "mitiguse.parkdistance.yearsofconflict.percentage literate",
  "mitiguse.parkdistance.crgovtrpt.pig.chital.nilgai",
  "mitiguse.parkdistance.lpgovtrpt.leopard",
  "mitiguse.parkdistance.tiger.leopard")

comphh<-aictab(COMPHH)
View(comphh)

#Predicted probability per household
predcomp1 <- predict(COMPHH[[1]], newdata = NULL, type = "response", se.fit = TRUE)
ppcomp1 <- predcomp1$fit
secomp1 <- predcomp1$se.fit

#Average predicted probability
ppcomp1aic <- ppcomp1*0.99 #AICcWtof model1 = 0.99
secomp1aic <- se1*0.99

summary(ppcomp1aic) #average estimated probability
summary(secomp1aic) #standard error


##################################################################
####Perception Models
#subsetting the data for running perception models
perpmod1<-hwc_mod[c(1:402, 404:nrow(hwc_mod)),c(1:ncol(hwc_mod))]

#Total number of literate individuals
perpmod1$lit<-rowSums(perpmod1[,c("less8","totwelve","abvtwelve")])
perpmod1$edutotal<-rowSums(perpmod1[,c("Il","less8","totwelve","abvtwelve")])
perpmod1$pctlit<-(perpmod1$lit/perpmod1$edutotal)*100

#Testing for correlation between predictor variables 
corrperp<-perpmod1[c(2,11,23,24,133,135,136,139)]
corrperp1<-cor(corrperp[,unlist(lapply(corrperp, is.numeric))], 
               use="pairwise.complete.obs", method="pearson") 
View(corrperp1)

#Models for perception 
PERPHH<-list()
PERPHH[[ 1]]<-standardize(glm(imppa~land+dp+gender+age+pctlit,perpmod1, family=binomial))
PERPHH[[ 2]]<-standardize(glm(imppa~land+dp+conflyear+obtainres+fdaccess,perpmod1, family=binomial))
PERPHH[[ 3]]<-standardize(glm(imppa~land+dp+conflyear+obtainres,perpmod1, family=binomial))
PERPHH[[ 4]]<-standardize(glm(imppa~land+dp+obtainres,perpmod1, family=binomial))
PERPHH[[ 5]]<-standardize(glm(imppa~conflyear+fdaccess,perpmod1, family=binomial))

names(PERPHH)<-c(
  "land.park distance.gender.age.percentage literate.years of conflict.forest dependence",
  "land.park distance.years of conflict.forest dependence.fd affected access",
  "land.park distance.years of conflict.forest dependence",
  "land.park distance.forest dependence",
  "years of conflict.fd affected access")

perphh<-aictab(PERPHH)
View(perphh)

#Model Averaging 
perphh.avg <- list()
perphh.avg[[1]] <- PERPHH[[2]]
perphh.avg[[2]] <- PERPHH[[5]]
names(perphh.avg) <- c("land+dp+conflyear+obtainres+fdaccess",
                       "conflyear+fdaccess")

#Intercept 
perpmod_int<-(0.90*.86+0.97*.14/(.86+.14))
perpmod_int

#Standard error 
perpmod_se<-(0.12*.86+0.12*.14)/(.86+.14)
perpmod_se

landavg <- modavg(perphh.avg, parm = "z.land")
dpavg <- modavg(perphh.avg, parm = "z.dp")
conflyearavg <- modavg(perphh.avg, parm = "z.conflyear")
obtainresavg <- modavg(perphh.avg, parm = "z.obtainres")
fdaccessavg <- modavg(perphh.avg, parm = "z.fdaccess")
