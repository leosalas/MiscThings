# TODO: Add comment
# 
# Author: lsalas
###############################################################################


## The point of this file is to determine the number of days in which all, all but one, all but two, etc., locations had outlier values of residuals of predicted O3

libs<-c("mgcv","XLConnect","plyr","ggplot2")
lapply(libs,require,character.only = TRUE)

pthToFile<-"C:/Users/lsalas/Desktop/Greg/JaffeReport/"

get95Quant<-function(vect){
	mv<-mean(vect,na.rm=T);sdv<-sd(vect,na.rm=T)
	q95<-qnorm(0.95,mv,sdv)
	return(q95)
}

getQuantiles<-function(dat){
	df<-subset(dat,TrCrSm==0)
	manQ<-get95Quant(df$manres)
	aldQ<-get95Quant(df$aldres)
	nwhQ<-get95Quant(df$nwhres)
	lanQ<-get95Quant(df$lanres)
	bayQ<-get95Quant(df$bayres)
	prkQ<-get95Quant(df$parkrres)
	cntQ<-get95Quant(df$clires)
	drpQ<-get95Quant(df$deeres)
	seaQ<-get95Quant(df$seares)
	hoeQ<-get95Quant(df$nehres)
	q95v<-c(manQ,aldQ,nwhQ,lanQ,bayQ,prkQ,cntQ,drpQ,seaQ,hoeQ)
	return(q95v)
}

dat<-try(readWorksheetFromFile(paste0(pthToFile,"Houston_2008_2018_GAMS.xlsx"),sheet="Data"))
## WARNINGS: 
# Many cells have values that are lookups elsewhere. Look at cells T1955-T1976
nrow(dat) #=2354
dat$QUAD<-as.factor(dat$QUAD)	## This is a by variable for the smoothingspline for Distance - see formula below - so must be a factor

## Get the quantile limits:
vect95<-getQuantiles(dat)

## Construct a data.frame that includes: date, numAbove95, numMeasured, allButN
res<-ldply(.data=format(dat$Date,"%Y-%m-%d"),.fun=function(dd,dat,vect95){
			evv<-subset(dat,format(Date,"%Y-%m-%d")==dd,select=c("manres","aldres","nwhres","lanres","bayres","parkrres","clires","deeres","seares","nehres","TrCrSm"))
			smk<-evv[1,11]
			tdf<-data.frame(resval=as.numeric(evv[1,1:10]),qval=vect95)
			tdf$evalout<-ifelse(is.na(tdf$resval),NA,ifelse(tdf$resval>tdf$qval,1,0))
			outdf<-data.frame(datum=dd,smoke=smk,numOut=sum(tdf$evalout,na.rm=T),numMeasured=sum(!is.na(tdf$evalout)))
			outdf$allButN<-outdf$numMeasured-outdf$numOut
			return(outdf)
		},dat=dat,vect95=vect95)

## Some dates had no measurements across sites, so...
res<-subset(res,numMeasured>0)
res$NumOutliers<-ifelse(res$allButN==0,"All",paste("All but",res$allButN))
res$DayType<-ifelse(res$smoke==0,"Training",ifelse(res$smoke==1,"Test","Smoke day"))

pdf<-subset(res,allButN<6)

p<-ggplot(data=pdf,aes(x=NumOutliers)) + geom_histogram(aes(fill=DayType),position="dodge",stat="count") +
		labs(title="Number of sites with >95% quantile residuals of predicted MDA8 O3 values",x="Number of sites",y="Count",fill="Day type") +
		theme_bw()


pdf$DayType2<-ifelse(pdf$DayType=="Smoke day","Smoke day","Non-smoke day")
qdf<-aggregate(datum~NumOutliers+DayType2,data=pdf,NROW)
tt<-reshape(qdf,idvar="NumOutliers",timevar="DayType2",direction="wide")
names(tt)<-c("NumOutliers","NonSmoke","Smoke")
tt$proportionSmokeDays<-tt$Smoke/(tt$NonSmoke+tt$smoke)


