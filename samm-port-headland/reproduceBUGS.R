# TODO: Add comment
# 
# Author: lsalas
###############################################################################


library(plyr); library(lme4);library(ggplot2); library(mfp)

## read the data
k<-read.csv("C:/Users/lsalas/Desktop/Greg/Sensors/K.csv")
yobs<-read.csv("C:/Users/lsalas/Desktop/Greg/Sensors/y.csv")

## We have an observed response for each sensor at each time stamp
## The issue here is that we can regress each sensor against each of the 11 sources, thus obtaining 11 slopes.
## These slopes represent the contribution of each source to the reading in the sensor.
## But since we have 12 sensors, then we'll end up with 110 slopes. (11-1 sources x 12-1 sensors)
## Should there exist a constraint for the slopes of all sources such that they should be the same regardless of the sensor?
## Probably not. Sensors may not all be equidistant to the sources.
## But we DO want the covariance...
## The goal is to determine if one of these sources is polluting more than usual
## Thus a spike in one sensor from one source must also be evident in other sensors (all or some, let's discuss this with the team)

## We will explore 3 basic options to answer the question:
## 1) Let's say no to constraints on slopes, and fit the 132 slopes (12 models burning 11 d.f. each)
##		This is equivalent to a model with no covariance, but spikes can be detected post hoc 
##		with a review of the predictions of source contributions
## 2) Then let's say yes and fit a hierarchical model, with sensor as random effect (1 model burning 14 d.f.)
##		This model makes sensor a nuisance variable, such that the mean value of the slope of source for any sensor must conform to some
##		distribution of values. Because sensor is a random effect, the source is co-varying among all sensors.
## 3) Finally, let's say yes and fit a model with a fixed effect of sensor (1 model burning 11 + 121 + 11 d.f., or 143 d.f.) 
##		This is probably the best approach, in that the source slope would be modified by a sensor effect, and an interaction of sensor and source
##		Makes most sense because there is a general source effect, then there is an effect of where the sensor is in respect to the source (the covariance),
##		and finally there is also possibly a basic difference among sensors

########
## After e-mail exchange with Greg, he seemed to indicate that all we needed to do was a simple regression
## So, we call that option #4
## 4) Do a simple regression

## We'll do the above with the data as-is. However, Tasko said that the source slopes cannot be negative, so we must log source data too
## Then we'll repeat by using the log of yobs, and scaling the values in K

####################################################
## Formatting the data to fit our needs
snames<-names(yobs)[2:13]
yobsl<-reshape(yobs, idvar = "tstamp", varying = list(2:13), v.names = "yobs", direction = "long")
row.names(yobsl)<-NULL
sensornames<-data.frame(time=1:12, sensor=snames)
yobsl<-merge(yobsl,sensornames,by="time",all.x=T)
yobsl<-yobsl[,which(names(yobsl) != "time")]

df<-merge(k,yobsl,by=c("tstamp","sensor"), all.x=T)

##
dfs<-df
dfs[,3:13]<-scale(dfs[,3:13])
library(bestNormalize)
q<-orderNorm(dfs$yobs)
dfs$yobs_on<-q$x.t
ggplot(dfs,aes(y=yobs_on,x=reclaimers)) + geom_point() + geom_smooth(span=0.95,se=F)


## But slopes cannot be negative, so applying log to covariates
for(vv in names(df)[3:13]){
	df[,vv]<-ifelse(df[,vv]==0,-9.5,log(df[,vv]))
}

## This function makes it easier and clearer what's happening in approach #1 if not versed in regression analysis
## Later we may code this all using matrix algebra entirely
fitSingleModels<-function(dat,snames,resvar="yobs"){
	#fit a model for each sensor, retrieve coefs and SEs into 2 data.frames
	#collapse into a single data.frame
	fml<-paste0(resvar," ~ ",paste(names(k)[3:13], collapse=" + "))
	mdls<-llply(.data=snames, .fun=function(ss,dat){
				sdf<-subset(dat, sensor==ss)
				resmdl<-lm(fml,data=sdf)
				return(resmdl)
			}, dat=dat) #returns the fitted models, one per sensor
	resdf<-data.frame()
	for(m in 1:NROW(mdls)){
		smdl<-summary(mdls[[m]])
		cdf<-as.data.frame(smdl$coefficients);
		cdf$slope_SE<-paste0(round(cdf[,1],2)," (",round(cdf[,2],2),")")
		wdf<-as.data.frame(t(cdf$slope_SE));names(wdf)<-as.character(row.names(cdf))
		wdf$sensor<-snames[m]
		wdf$adjR2<-smdl$adj.r.squared
		wdf<-wdf[,c(13,14,1:12)]
		resdf<-rbind(resdf,wdf)
	}
	pdf<-ldply(.data=1:NROW(mdls),.fun=function(ss,mdls,dat,snames){
				yobs<-subset(dat, sensor==snames[ss])[,resvar]
				cmdl<-mdls[[ss]];yest=as.numeric(predict(cmdl))
				tpdf<-data.frame(yobs=yobs,yest=yest);tpdf$sensor<-snames[ss]
				return(tpdf)
			},mdls=mdls,dat=dat,snames=snames)
	
	return(list(models=mdls,results=resdf,pdf=pdf))
}

getmode <- function(v) {
	uniqv <- unique(v)
	uniqv[which.max(tabulate(match(v, uniqv)))]
}


####################################################
## Tasko's approach: yobs not logged, slopes must be > 0, and no intercept
tfml<-as.formula(paste0("yobs ~ ",paste(names(k)[3:13], collapse=" + "), " - 1"))
mdlt<-lm(tfml,data=df)
tmres<-summary(mdlt)

#reclaimers has a negative slope
modevect<-apply(df[,3:13],2,getmode)
newdf<-as.data.frame(matrix(sapply(modevect,FUN=function(x,i){return(rep(x,times=i))},i=nrow(df)),ncol=11))
names(newdf)<-names(df[3:13])
newdf$reclaimers<-df$reclaimers
newdf$predicted<-predict(mdlt,newdata=newdf)

for(nn in names(df[3:13])){
	tdf<-data.frame(yobs=df$yobs)
	tdf$Var<-df[,nn]
	tdf$roundVar<-round(tdf$Var)
	pdf<-aggregate(yobs~roundVar,tdf,max)
	p<-ggplot(tdf,aes(x=Var,y=yobs)) + geom_point() + labs(x=nn,y="yobs")
	plg<-ggplot(pdf,aes(x=roundVar,y=log(yobs))) + geom_point() + geom_smooth(method="loess",span= 0.9, se=F) + labs(x=nn,y="Log(yobs)")
	dev.new();print(p);dev.new();print(plg)
}


pdf<-data.frame(yobs=df$yobs,yest=predict(mdlt),sensor=df$sensor)
ggplot(data=pdf,aes(x=yobs,y=yest)) + geom_point(aes(color=sensor)) + geom_abline(slope=1,intercept=0,color="black")
ggplot(data=pdf,aes(x=log(yobs),y=log(yest))) + geom_point(aes(color=sensor)) + geom_abline(slope=1,intercept=0,color="black")

library(car)
outlierTest(mdlt)
qqPlot(mdlt, main="QQ Plot") 
leveragePlots(mdlt)
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(mdlt)

####################################################
## Approach #1
res1<-fitSingleModels(dat=df,snames=snames)
res1$results

pdf<-res1$pdf
ggplot(data=pdf,aes(x=yobs,y=yest)) + geom_point(aes(color=sensor)) + geom_abline(slope=1,intercept=0,color="black")
ggplot(data=pdf,aes(x=log(yobs),y=log(yest))) + geom_point(aes(color=sensor)) + geom_abline(slope=1,intercept=0,color="black")


####################################################
## Approach #2
refml<-as.formula(paste0("yobs ~ ",paste(names(k)[3:13], collapse=" + "), "+ (1|sensor)"))
mdl2<-lmer(refml,data=df)
reres<-summary(mdl2)
reres$coefficients
reres$AICtab
reres$logLik

pdf<-data.frame(yobs=df$yobs,yest=predict(mdl2),sensor=df$sensor)
ggplot(data=pdf,aes(x=yobs,y=yest)) + geom_point(aes(color=sensor)) + geom_abline(slope=1,intercept=0,color="black")
ggplot(data=pdf,aes(x=log(yobs),y=log(yest))) + geom_point(aes(color=sensor)) + geom_abline(slope=1,intercept=0,color="black")


####################################################
## Approach #3
fefml<-as.formula(paste0("yobs ~ ",paste(paste0("sensor*",names(k)[3:13]),collapse=" + ")))
mdl3<-lm(fefml,data=df)
(smdl3<-summary(mdl3))
as.data.frame(smdl3$coefficients)[1:12,1:2]

pdf<-data.frame(yobs=df$yobs,yest=predict(mdl3),sensor=df$sensor)
ggplot(data=pdf,aes(x=yobs,y=yest)) + geom_point(aes(color=sensor)) + geom_abline(slope=1,intercept=0,color="black")
ggplot(data=pdf,aes(x=log(yobs),y=log(yest))) + geom_point(aes(color=sensor)) + geom_abline(slope=1,intercept=0,color="black")

####################################################
## Approach #4
smfml<-as.formula(paste0("yobs ~ ",paste(names(k)[3:13], collapse=" + "), " + sensor"))
mdl4<-lm(smfml,data=df)
smres<-summary(mdl4)
smres$coefficients

pdf<-data.frame(yobs=df$yobs,yest=predict(mdl4),sensor=df$sensor)
ggplot(data=pdf,aes(x=yobs,y=yest)) + geom_point(aes(color=sensor)) + geom_abline(slope=1,intercept=0,color="black")
ggplot(data=pdf,aes(x=log(yobs),y=log(yest))) + geom_point(aes(color=sensor)) + geom_abline(slope=1,intercept=0,color="black")


#################################################################################################################################################################
## Only approach #2, using random effects of sensor, seemed to have a proper normal residual distribution.
## Let's scale the data and use log(yobs) as the response

####################################################
## Preliminaries
df$logYobs<-log(df$yobs)
dfs<-df[,c("tstamp","sensor","logYobs")]
dfc<-scale(df[,names(k)[3:13]])
dfs<-cbind(dfs,dfc)

####################################################
## Approach #1 + log + scale
res1s<-fitSingleModels(dat=dfs,snames=snames,resvar="logYobs")
res1s$results

pdf<-res1s$pdf
ggplot(data=pdf,aes(x=yobs,y=yest)) + geom_point(aes(color=sensor)) + geom_abline(slope=1,intercept=0,color="black")


####################################################
## Approach #2 + log + scale
refml<-as.formula(paste0("logYobs ~ ",paste(names(k)[3:13], collapse=" + "), "+ (1|sensor)"))
mdl2s<-lmer(refml,data=dfs)
reress<-summary(mdl2s)
reress$coefficients
reress$AICtab
reress$logLik

pdf<-data.frame(logYobs=dfs$logYobs,yest=predict(mdl2s),sensor=dfs$sensor)
ggplot(data=pdf,aes(x=logYobs,y=yest)) + geom_point(aes(color=sensor)) + geom_abline(slope=1,intercept=0,color="black")


####################################################
## Approach #3 + log + scale
fefml<-as.formula(paste0("logYobs ~ ",paste(paste0("sensor*",names(k)[3:13]),collapse=" + ")))
mdl3s<-lm(fefml,data=dfs)
(smdl3s<-summary(mdl3s))
as.data.frame(smdl3s$coefficients)[1:12,1:2]

pdf<-data.frame(logYobs=dfs$logYobs,yest=predict(mdl3s),sensor=dfs$sensor)
ggplot(data=pdf,aes(x=logYobs,y=yest)) + geom_point(aes(color=sensor)) + geom_abline(slope=1,intercept=0,color="black")
ggplot(data=pdf,aes(x=logYobs,y=yest)) + geom_point(aes(color=sensor)) + geom_abline(slope=1,intercept=0,color="black") + geom_smooth(method="lm", formula=y~x,color="blue",se=F)


####################################################
## Approach #4 + log + scale
smfml<-as.formula(paste0("logYobs ~ ",paste(names(k)[3:13], collapse=" + ")))
mdl4s<-lm(smfml,data=dfs)
smress<-summary(mdl4s)
smress$coefficients
exp(smress$coefficients[1:12])

pdf<-data.frame(logYobs=dfs$logYobs,yest=predict(mdl4s),sensor=dfs$sensor)
ggplot(data=pdf,aes(x=logYobs,y=yest)) + geom_point(aes(color=sensor)) + geom_abline(slope=1,intercept=0,color="black")



