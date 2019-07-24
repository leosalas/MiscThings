# TODO: Add comment
# 
# Author: lsalas
###############################################################################


libs<-c("mgcv","XLConnect","plyr","ggplot2")
lapply(libs,require,character.only = TRUE)

pthToFile<-"C:/Users/lsalas/Desktop/Greg/JaffeReport/"

getEmpQuants<-function(pdval,pdf){
	
	if(!is.na(pdval)){
		empQ<-round(sum(pdf$resVal<pdval)*100/nrow(pdf),1)
	}else{
		empQ<-NA
	}
	
	return(empQ)
}

getDistQuants<-function(pdval,pdf){
	if(!is.na(pdval)){
		mv<-mean(pdf$resVal,na.rm=T);sdv<-sd(pdf$resVal,na.rm=T)
		distQ<-round(pnorm(pdval,mean=mv,sd=sdv)*100,1)
	}else{
		distQ<-NA
	}
	return(distQ)
}

makeTargetDayPlots<-function(dat,gmdl,site){
	
	dat$predVal<-predict(gmdl,newdata=dat)
	pdf<-subset(dat,TrCrSm==0 & !is.na(predVal),select=c(site,"predVal"))
	names(pdf)<-gsub(site,"Site",names(pdf))
	pdf<-subset(pdf,!is.na(Site))
	pdf$resVal<-pdf$Site-pdf$predVal
	plm<-lm(Site~predVal,data=pdf);mcoef<-round(coef(plm),3);prsq<-round(summary(plm)$r.sq,3)
	pres<-predict(plm,interval="prediction")
	pdf<-cbind(pdf,pres)
	pdays<-subset(dat,format(Date,"%Y-%m-%d") %in% c("2018-07-26","2018-07-27","2018-08-23","2018-08-24"),select=c(site,"predVal","Date"))
	names(pdays)<-gsub(site,"Site",names(pdays))
	pdays$resVal<-pdays$Site-pdays$predVal
	
	pmdl<-ggplot(data=pdf,aes(x=predVal,y=Site)) + geom_point(color="blue") + 
			geom_line(aes(y=upr),color="red") + geom_line(aes(y=lwr),color="red") +
			geom_smooth(method="lm",formula=y~x,se=FALSE,color="black") + theme_bw() +
			annotate("text", x=0, y=120, label=paste0("y=",mcoef[2],"+",mcoef[1],"; R2=",prsq),hjust=0) +
			labs(x="GAM Predicted MDA8 ppb",y="Observed MDA8 ppb") +
			geom_point(data=pdays,aes(x=predVal,y=Site,color=as.character(Date)),shape=17,size=3) +
			theme(legend.position="none")
	
	presmdl<-ggplot(data=pdf,aes(x=resVal)) + geom_histogram() +
			geom_vline(data=pdays,aes(xintercept=resVal,color=as.character(Date)),size=1.2) +
			theme_bw() + labs(x="GAM Residuals",y="Frequency") +
			theme(legend.position="none")

	
	#empirical quantiles:
	pdays$empQuants<-sapply(pdays$resVal,FUN=getEmpQuants,pdf=pdf)
	#normal-dist quantiles:
	pdays$distQuants<-sapply(pdays$resVal,getDistQuants,pdf=pdf)
	pdays$Site<-site
	
	reslst<-list(pmdl,presmdl,pdays)
	return(reslst)
}

plotProfileEffects<-function(dat,gmdl,site){
	
	dat$predVal<-predict(gmdl,newdata=dat)
	pdf<-subset(dat,TrCrSm %in% c(0,2) & !is.na(predVal),select=c(site,"predVal","TrCrSm","PM4"))
	names(pdf)<-gsub(site,"Site",names(pdf)); pdf<-subset(pdf,!is.na(Site))
	pdf$resVal<-pdf$Site-pdf$predVal
	p1<-ggplot(data=subset(pdf,TrCrSm==0),aes(x=PM4,y=predVal)) + geom_point() +
			geom_point(data=subset(pdf,TrCrSm==2),aes(x=PM4,y=predVal),color="red") +
			labs(x="PM2.5",y="Predicted O3")
	p2<-ggplot(data=subset(pdf,TrCrSm==0),aes(x=PM4,y=resVal)) + geom_point() +
			geom_point(data=subset(pdf,TrCrSm==2),aes(x=PM4,y=resVal),color="red") +
			labs(x="PM2.5",y="Residuals")
	
	resplts<-list(p1,p2)
	return(resplts)
	
}
###
## Load the data
dat<-try(readWorksheetFromFile(paste0(pthToFile,"Houston_2008_2018_GAMS.xlsx"),sheet="Data"))
#if(inherits(dat,"try-error")){
#	stop("Can't read the data")
#}else{
	## WARNINGS: 
	# Many cells have values that are lookups elsewhere. Look at cells T1955-T1976
	nrow(dat) #=2354
	dat$QUAD<-as.factor(dat$QUAD)	## This is a by variable for the smoothingspline for Distance - see formula below - so must be a factor
	
	#Uninformed variables:
	#DOY is day of the year,DOW is day of the week
	#HE is Houston East

	##GAM formulas
	fmanv<-as.formula(paste0('Manvel~s(Distance,by=QUAD)+s(TrajDir,bs="cr",k=10)+s(LGANO2F,bs="cr",k=5)+s(DOW,bs="cr",k=5)+s(NO2,bs="cr",k=10)+',
					's(TMIN,bs="cr",k=10)+s(Y,bs="cr",k=10)+s(DOY,bs="cr",k=10)+s(Cape7, bs="cr",k=10)+s(MLMR19, bs="cr",k=10)+s(TH7,bs="cr",k=10)+',
					's(T1km7,bs="cr",k=10)+s(AIRS700DSF,bs="cr",k=10)+s(TMAX,bs="cr",k=10)'))
	
	fald<-as.formula(paste0('Aldine~s(Distance,by=QUAD)+s(TrajDir,bs="cr",k=10)+s(LGANO2F,bs="cr",k=5)+s(DOW,bs="cr",k=5)+s(NO2,bs="cr",k=10)+',
					's(TMIN,bs="cr",k=10)+s(Y,bs="cr",k=10)+s(DOY,bs="cr",k=10)+s(Cape7, bs="cr",k=10)+s(MLMR19, bs="cr",k=10)+s(TH7,bs="cr",k=10)+',
					's(T1km7,bs="cr",k=10)+s(AIRS700DSF,bs="cr",k=10)+s(TMAX,bs="cr",k=10)'))
	
	fnwh<-as.formula(paste0('NWHarris~s(Distance,by=QUAD)+s(TrajDir,bs="cr",k=10)+s(LGANO2F,bs="cr",k=5)+s(DOW,bs="cr",k=5)+s(NO2,bs="cr",k=10)+',
					's(TMIN,bs="cr",k=10)+s(Y,bs="cr",k=10)+s(DOY,bs="cr",k=10)+s(Cape7, bs="cr",k=10)+s(MLMR19, bs="cr",k=10)+s(TH7,bs="cr",k=10)+',
					's(T1km7,bs="cr",k=10)+s(AIRS700DSF,bs="cr",k=10)+s(TMAX,bs="cr",k=10)'))
	
	flng<-as.formula(paste0('Lang~s(Distance,by=QUAD)+s(TrajDir,bs="cr",k=10)+s(LGANO2F,bs="cr",k=5)+s(DOW,bs="cr",k=5)+s(NO2,bs="cr",k=10)+',
					's(TMIN,bs="cr",k=10)+s(Y,bs="cr",k=10)+s(DOY,bs="cr",k=10)+s(Cape7, bs="cr",k=10)+s(MLMR19, bs="cr",k=10)+s(TH7,bs="cr",k=10)+',
					's(T1km7,bs="cr",k=10)+s(AIRS700DSF,bs="cr",k=10)+s(TMAX,bs="cr",k=10)'))
	
	fbay<-as.formula(paste0('Bayland~s(Distance,by=QUAD)+s(TrajDir,bs="cr",k=10)+s(LGANO2F,bs="cr",k=5)+s(DOW,bs="cr",k=5)+s(NO2,bs="cr",k=10)+',
					's(TMIN,bs="cr",k=10)+s(Y,bs="cr",k=10)+s(DOY,bs="cr",k=10)+s(Cape7, bs="cr",k=10)+s(MLMR19, bs="cr",k=10)+s(TH7,bs="cr",k=10)+',
					's(T1km7,bs="cr",k=10)+s(AIRS700DSF,bs="cr",k=10)+s(TMAX,bs="cr",k=10)'))
	
	fprk<-as.formula(paste0('Parkplace~s(Distance,by=QUAD)+s(TrajDir,bs="cr",k=10)+s(LGANO2F,bs="cr",k=5)+s(DOW,bs="cr",k=5)+s(NO2,bs="cr",k=10)+',
					's(TMIN,bs="cr",k=10)+s(Y,bs="cr",k=10)+s(DOY,bs="cr",k=10)+s(Cape7, bs="cr",k=10)+s(MLMR19, bs="cr",k=10)+s(TH7,bs="cr",k=10)+',
					's(T1km7,bs="cr",k=10)+s(AIRS700DSF,bs="cr",k=10)+s(TMAX,bs="cr",k=10)'))
	
	fclnt<-as.formula(paste0('Clinton~s(Distance,by=QUAD)+s(TrajDir,bs="cr",k=10)+s(LGANO2F,bs="cr",k=5)+s(DOW,bs="cr",k=5)+s(NO2,bs="cr",k=10)+',
					's(TMIN,bs="cr",k=10)+s(Y,bs="cr",k=10)+s(DOY,bs="cr",k=10)+s(Cape7, bs="cr",k=10)+s(MLMR19, bs="cr",k=10)+s(TH7,bs="cr",k=10)+',
					's(T1km7,bs="cr",k=10)+s(AIRS700DSF,bs="cr",k=10)+s(TMAX,bs="cr",k=10)'))
	
	fdeer<-as.formula(paste0('Deerpark~s(Distance,by=QUAD)+s(TrajDir,bs="cr",k=10)+s(LGANO2F,bs="cr",k=5)+s(DOW,bs="cr",k=5)+s(NO2,bs="cr",k=10)+',
					's(TMIN,bs="cr",k=10)+s(Y,bs="cr",k=10)+s(DOY,bs="cr",k=10)+s(Cape7, bs="cr",k=10)+s(MLMR19, bs="cr",k=10)+s(TH7,bs="cr",k=10)+',
					's(T1km7,bs="cr",k=10)+s(AIRS700DSF,bs="cr",k=10)+s(TMAX,bs="cr",k=10)'))
	
	fsea<-as.formula(paste0('Seabrook~s(Distance,by=QUAD)+s(TrajDir,bs="cr",k=10)+s(LGANO2F,bs="cr",k=5)+s(DOW,bs="cr",k=5)+s(NO2,bs="cr",k=10)+',
					's(TMIN,bs="cr",k=10)+s(Y,bs="cr",k=10)+s(DOY,bs="cr",k=10)+s(Cape7, bs="cr",k=10)+s(MLMR19, bs="cr",k=10)+s(TH7,bs="cr",k=10)+',
					's(T1km7,bs="cr",k=10)+s(AIRS700DSF,bs="cr",k=10)+s(TMAX,bs="cr",k=10)'))
	
	fhoe<-as.formula(paste0('HE~s(Distance,by=QUAD)+s(TrajDir,bs="cr",k=10)+s(LGANO2F,bs="cr",k=5)+s(DOW,bs="cr",k=5)+s(NO2,bs="cr",k=10)+',
					's(TMIN,bs="cr",k=10)+s(Y,bs="cr",k=10)+s(DOY,bs="cr",k=10)+s(Cape7, bs="cr",k=10)+s(MLMR19, bs="cr",k=10)+s(TH7,bs="cr",k=10)+',
					's(T1km7,bs="cr",k=10)+s(AIRS700DSF,bs="cr",k=10)+s(TMAX,bs="cr",k=10)'))
	
	
	
	###################################################################################################################################################
	
	## Correlational evidence
	mda8Clinton<-subset(dat,Clinton>70)
	pClinton<-ggplot(data=mda8Clinton,aes(x=PM4)) + #geom_rect(xmin=60,xmax=70,ymin=-0.05,ymax=0.055,fill="gray") +
			geom_histogram(fill="light blue",binwidth=2.5) + theme_bw() +
			labs(x="PM2.5",y="Frequency",title="Clinton") + #adding exceptional days per Table 2 of the report
			geom_vline(xintercept=12.6,color="red") +
			geom_vline(xintercept=19.2,color="red") +
			geom_vline(xintercept=22.9,color="red") +
			geom_vline(xintercept=18.8,color="red")
	
	mda8Aldine<-subset(dat,Aldine>70)
	pAldine<-ggplot(data=mda8Aldine,aes(x=PM4)) + #geom_rect(xmin=60,xmax=70,ymin=-0.05,ymax=0.055,fill="gray") +
			geom_histogram(fill="light blue",binwidth=2.5) + theme_bw() +
			labs(x="PM2.5",y="Frequency",title="Aldine") + #adding exceptional days per Table 2 of the report
			geom_vline(xintercept=15.3,color="red") +
			geom_vline(xintercept=19.5,color="red")
	
	mda8Deer<-subset(dat,Deerpark>70)
	pDeer<-ggplot(data=mda8Deer,aes(x=PM4)) + #geom_rect(xmin=60,xmax=70,ymin=-0.05,ymax=0.055,fill="gray") +
			geom_histogram(fill="light blue",binwidth=2.5) + theme_bw() +
			labs(x="PM2.5",y="Frequency",title="Deer Park") + #adding exceptional days per Table 2 of the report
			geom_vline(xintercept=18.6,color="red") +
			geom_vline(xintercept=18.0,color="red") +
			geom_vline(xintercept=15.4,color="red") +
			geom_vline(xintercept=13.7,color="red")

	mda8HE<-subset(dat,HE>70)
	pHE<-ggplot(data=mda8HE,aes(x=PM4)) + #geom_rect(xmin=60,xmax=70,ymin=-0.05,ymax=0.055,fill="gray") +
			geom_histogram(fill="light blue",binwidth=2.5) + theme_bw() +
			labs(x="PM2.5",y="Frequency",title="Houston East") + #adding exceptional days per Table 2 of the report
			geom_vline(xintercept=17.6,color="red") +
			geom_vline(xintercept=19.6,color="red") +
			geom_vline(xintercept=19.5,color="red") +
			geom_vline(xintercept=20.8,color="red")
	
	boxp<-data.frame(site=c(rep("Clinton",times=nrow(mda8Clinton)),rep("Aldine",times=nrow(mda8Aldine)),
					rep("Deer Park",times=nrow(mda8Deer)),rep("Houston E.",times=nrow(mda8HE))),
			MDA8=c(mda8Clinton$Clinton,mda8Aldine$Aldine,mda8Deer$Deerpark,mda8HE$HE))
	excepdays=data.frame(x=c(1,1,1,2,2,2,2,3,3,3,3,4,4,4,4),y=c(68,99,60,75,77,91,109,64,71,85,91,67,71,86,94),
			quant=c("outlier","outlier","outlier","inner 95%","inner 95%","inner 95%","outlier",
					"outlier","inner 95%","inner 95%","inner 95%","outlier","inner 95%","inner 95%","inner 95%"))
	#read more about box and whiskers plots here: https://stat.ethz.ch/R-manual/R-patched/library/grDevices/html/boxplot.stats.html
	p<-ggplot(data=boxp,aes(x=site,y=MDA8)) + geom_boxplot(fill="light blue") + theme_bw() +
			geom_point(data=excepdays,aes(x=x,y=y,color=quant),size=2) +
			scale_color_manual(values=c("black","red")) + labs(x="Site",y="O3 (ppb)",color="Quantile")
	
	
	
	###################################################################################################################################################
	## GAM evidence 
	
	## Per instructions, use the training set, they call it "dat3"
	dat3<-subset(dat,TrCrSm==0)
	nrow(dat3) # =2034
	
	## Train the model	
	gmanv<-gam(formula=fmanv,data=dat3,na.action=na.exclude)
	gald<-gam(formula=fald,data=dat3,na.action=na.exclude)
	gnwh<-gam(formula=fnwh,data=dat3,na.action=na.exclude)
	glng<-gam(formula=flng,data=dat3,na.action=na.exclude)
	gbay<-gam(formula=fbay,data=dat3,na.action=na.exclude)
	gprk<-gam(formula=fprk,data=dat3,na.action=na.exclude)
	gclnt<-gam(formula=fclnt,data=dat3,na.action=na.exclude)
	gdeer<-gam(formula=fdeer,data=dat3,na.action=na.exclude)
	gsea<-gam(formula=fsea,data=dat3,na.action=na.exclude)
	ghoe<-gam(formula=fhoe,data=dat3,na.action=na.exclude)
	
	rsqdf<-data.frame(model=c("Manvel","Aldine","NW Harris","Lang","Bayland","Parkplace","Clinton","Deerpark","Seabrook","Houston East"),
			rsq=c(summary(gmanv)$r.sq,summary(gald)$r.sq,summary(gnwh)$r.sq,summary(glng)$r.sq,summary(gbay)$r.sq,
					summary(gprk)$r.sq,summary(gclnt)$r.sq,summary(gdeer)$r.sq,summary(gsea)$r.sq,summary(ghoe)$r.sq))
	print(rsqdf)
	
	#predict to the test set and confirm results?
	## MANVEL
	dat$predManvel<-predict(gmanv,newdata=dat)
	
	mnvdf<-subset(dat,TrCrSm==0 & !is.na(predManvel) & !is.na(Manvel),select=c("Manvel","predManvel","PM4"))
	mnvlm<-lm(Manvel~predManvel,data=mnvdf);mnvcoef<-round(coef(mnvlm),3);mnvrsq<-round(summary(mnvlm)$r.sq,3)
	mnvres<-predict(mnvlm,interval="prediction")
	mnvdf<-cbind(mnvdf,mnvres)
	
	pmanv<-ggplot(data=mnvdf,aes(x=predManvel,y=Manvel)) + geom_point(color="blue") + 
			geom_line(aes(y=upr),color="red") + geom_line(aes(y=lwr),color="red") +
			geom_smooth(method="lm",formula=y~x,se=FALSE,color="black") + theme_bw() +
			geom_point(data=subset(dat,TrCrSm==2),aes(x=predManvel,y=Manvel),color="red",shape=17,size=2) +
			annotate("text", x=0, y=120, label=paste0("y=",mnvcoef[2],"+",mnvcoef[1],"; R2=",mnvrsq),hjust=0) +
			labs(title="Manvel Croix", x="GAM Predicted MDA8 ppb",y="Observed MDA8 ppb")
	
	## ALDINE
	dat$predAldine<-predict(gald,newdata=dat)
	alddf<-subset(dat,TrCrSm %in% c(0,2) & !is.na(predAldine) & !is.na(Aldine),select=c("Aldine","predAldine","TrCrSm"))
	alddf$resAldine<-alddf$Aldine-alddf$predAldine
	aldlm<-lm(Aldine~predAldine,data=alddf);aldcoef<-round(coef(aldlm),3);aldrsq<-round(summary(aldlm)$r.sq,3)
	aldres<-predict(aldlm,interval="prediction")
	alddf<-cbind(alddf,aldres)
	
	pald<-ggplot(data=alddf,aes(x=predAldine,y=Aldine)) + geom_point(color="blue") + 
			geom_line(aes(y=upr),color="red") + geom_line(aes(y=lwr),color="red") +
			geom_smooth(method="lm",formula=y~x,se=FALSE,color="black") + theme_bw() +
			geom_point(data=subset(dat,TrCrSm==2),aes(x=predAldine,y=Aldine),color="red",shape=17,size=2) +
			annotate("text", x=0, y=120, label=paste0("y=",aldcoef[2],"+",aldcoef[1],"; R2=",aldrsq),hjust=0) +
			labs(title="Aldine", x="GAM Predicted MDA8 ppb",y="Observed MDA8 ppb")
	
	presald<-ggplot(data=subset(alddf,TrCrSm==0),aes(x=predAldine,y=resAldine)) + geom_point(shape=1,size=2) +
			geom_point(data=subset(alddf,TrCrSm==2),aes(x=predAldine,y=resAldine),shape=19,size=2,color="red") +
			theme_bw() + labs(title="Aldine",x="GAM Predicted MDA8 ppb",y="GAM Residuals ppb")
	
	
	## Anomalous day in 2012 
	w<-subset(dat,Manvel>100 & TrCrSm==2,select=c("Manvel","Aldine","NWHarris","Lang","Bayland","Parkplace","Clinton","Deerpark","Seabrook","HE"))
	t(w)
	
	
	### Estimate the effect of smoke days
	dat$smoke<-ifelse(dat$TrCrSm==2,"Smoke","Normal")
	
	fmanv2<-as.formula(paste0('Manvel~s(Distance,by=QUAD)+s(TrajDir,bs="cr",k=10)+s(LGANO2F,bs="cr",k=5)+s(DOW,bs="cr",k=5)+s(NO2,bs="cr",k=10)+',
					's(TMIN,bs="cr",k=10)+s(Y,bs="cr",k=10)+s(DOY,bs="cr",k=10)+s(Cape7, bs="cr",k=10)+s(MLMR19, bs="cr",k=10)+s(TH7,bs="cr",k=10)+',
					's(T1km7,bs="cr",k=10)+s(AIRS700DSF,bs="cr",k=10)+s(TMAX,bs="cr",k=10)+smoke'))
	gmanv<-gam(formula=fmanv,data=dat,na.action=na.exclude)
	gmanv2<-gam(formula=fmanv2,data=dat,na.action=na.exclude)
	
	anova.gam(gmanv2,gmanv,test="F")
	
	### Profile the effect of PM2.5
	## Manvel
	pmeff<-plotProfileEffects(dat=dat,gmdl=gmanv,site="Manvel")
	p1<-pmeff[[1]];p2<-pmeff[[2]]
		
	## Aldine
	pmeff<-plotProfileEffects(dat=dat,gmdl=gald,site="Aldine")
	p1<-pmeff[[1]];p2<-pmeff[[2]]
	
	## NW Harris
	pmeff<-plotProfileEffects(dat=dat,gmdl=gnwh,site="NWHarris")
	p1<-pmeff[[1]];p2<-pmeff[[2]]
	
	## Lang
	pmeff<-plotProfileEffects(dat=dat,gmdl=glng,site="Lang")
	p1<-pmeff[[1]];p2<-pmeff[[2]]
	
	## Bayland
	pmeff<-plotProfileEffects(dat=dat,gmdl=gbay,site="Bayland")
	p1<-pmeff[[1]];p2<-pmeff[[2]]
	
	## Park Place
	pmeff<-plotProfileEffects(dat=dat,gmdl=gprk,site="Parkplace")
	p1<-pmeff[[1]];p2<-pmeff[[2]]
	
	## Clinton
	pmeff<-plotProfileEffects(dat=dat,gmdl=gclnt,site="Clinton")
	p1<-pmeff[[1]];p2<-pmeff[[2]]
	
	## Deer Park
	pmeff<-plotProfileEffects(dat=dat,gmdl=gdeer,site="Deerpark")
	p1<-pmeff[[1]];p2<-pmeff[[2]]
	
	## Seabrook
	pmeff<-plotProfileEffects(dat=dat,gmdl=gsea,site="Seabrook")
	p1<-pmeff[[1]];p2<-pmeff[[2]]
	
	## Houston East
	pmeff<-plotProfileEffects(dat=dat,gmdl=ghoe,site="HE")
	p1<-pmeff[[1]];p2<-pmeff[[2]]
	
	###################################################################################################################
	###Evaluating the four days of interest: 7/26, 7/27, 8/23 and 8/24/2018 - MUST EXCLUDE FIRST
	dat3<-subset(dat,TrCrSm==0)
	nrow(dat3) # =2034
	dat3<-subset(dat3,!format(Date,"%Y-%m-%d") %in% c("2018-07-26","2018-07-27","2018-08-23","2018-08-24"))
	nrow(dat3) # =2034
	
	gmanv<-gam(formula=fmanv,data=dat3,na.action=na.exclude)
	gald<-gam(formula=fald,data=dat3,na.action=na.exclude)
	gnwh<-gam(formula=fnwh,data=dat3,na.action=na.exclude)
	glng<-gam(formula=flng,data=dat3,na.action=na.exclude)
	gbay<-gam(formula=fbay,data=dat3,na.action=na.exclude)
	gprk<-gam(formula=fprk,data=dat3,na.action=na.exclude)
	gclnt<-gam(formula=fclnt,data=dat3,na.action=na.exclude)
	gdeer<-gam(formula=fdeer,data=dat3,na.action=na.exclude)
	gsea<-gam(formula=fsea,data=dat3,na.action=na.exclude)
	ghoe<-gam(formula=fhoe,data=dat3,na.action=na.exclude)
	
	##################
	## Results of outlier analysis of 4 smoke days in 2018
	quantdf<-data.frame()
	plots<-makeTargetDayPlots(dat=dat,gmdl=gmanv,site="Manvel")
	p1<-plots[[1]]; p2<-plots[[2]]
	tqdf<-plots[[3]][,c("Site","Date","empQuants","distQuants")]
	quantdf<-rbind(quantdf,tqdf)
	
	plots<-makeTargetDayPlots(dat=dat,gmdl=gald,site="Aldine")
	p1<-plots[[1]]; p2<-plots[[2]]
	tqdf<-plots[[3]][,c("Site","Date","empQuants","distQuants")]
	quantdf<-rbind(quantdf,tqdf)
	
	plots<-makeTargetDayPlots(dat=dat,gmdl=gnwh,site="NWHarris")
	p1<-plots[[1]]; p2<-plots[[2]]
	tqdf<-plots[[3]][,c("Site","Date","empQuants","distQuants")]
	quantdf<-rbind(quantdf,tqdf)
	
	plots<-makeTargetDayPlots(dat=dat,gmdl=glng,site="Lang")
	p1<-plots[[1]]; p2<-plots[[2]]
	tqdf<-plots[[3]][,c("Site","Date","empQuants","distQuants")]
	quantdf<-rbind(quantdf,tqdf)
	
	plots<-makeTargetDayPlots(dat=dat,gmdl=gbay,site="Bayland")
	p1<-plots[[1]]; p2<-plots[[2]]
	tqdf<-plots[[3]][,c("Site","Date","empQuants","distQuants")]
	quantdf<-rbind(quantdf,tqdf)
	
	plots<-makeTargetDayPlots(dat=dat,gmdl=gprk,site="Parkplace")
	p1<-plots[[1]]; p2<-plots[[2]]
	tqdf<-plots[[3]][,c("Site","Date","empQuants","distQuants")]
	quantdf<-rbind(quantdf,tqdf)
	
	plots<-makeTargetDayPlots(dat=dat,gmdl=gclnt,site="Clinton")
	p1<-plots[[1]]; p2<-plots[[2]]
	tqdf<-plots[[3]][,c("Site","Date","empQuants","distQuants")]
	quantdf<-rbind(quantdf,tqdf)
	
	plots<-makeTargetDayPlots(dat=dat,gmdl=gdeer,site="Deerpark")
	p1<-plots[[1]]; p2<-plots[[2]]
	tqdf<-plots[[3]][,c("Site","Date","empQuants","distQuants")]
	quantdf<-rbind(quantdf,tqdf)
	
	plots<-makeTargetDayPlots(dat=dat,gmdl=gsea,site="Seabrook")
	p1<-plots[[1]]; p2<-plots[[2]]
	tqdf<-plots[[3]][,c("Site","Date","empQuants","distQuants")]
	quantdf<-rbind(quantdf,tqdf)
	
	plots<-makeTargetDayPlots(dat=dat,gmdl=ghoe,site="HE")
	p1<-plots[[1]]; p2<-plots[[2]]
	tqdf<-plots[[3]][,c("Site","Date","empQuants","distQuants")]
	quantdf<-rbind(quantdf,tqdf)
	
	quantable<-reshape(quantdf[,c("Site","Date","Quantiles")],idvar="Site",timevar="Date",direction="wide")
	write.csv(quantable,file="c:/users/lsalas/desktop/quantable.csv")
	
	
	###################################################################################################
	## CLINTON - reviewing bias inmodel predictions 
	fclnt<-as.formula(paste0('Clinton~s(Distance,by=QUAD)+s(TrajDir,bs="cr",k=10)+s(LGANO2F,bs="cr",k=5)+s(DOW,bs="cr",k=5)+s(NO2,bs="cr",k=10)+',
					's(TMIN,bs="cr",k=10)+s(Y,bs="cr",k=10)+s(DOY,bs="cr",k=10)+s(Cape7, bs="cr",k=10)+s(MLMR19, bs="cr",k=10)+s(TH7,bs="cr",k=10)+',
					's(T1km7,bs="cr",k=10)+s(AIRS700DSF,bs="cr",k=10)+s(TMAX,bs="cr",k=10)'))
	
	dat3<-subset(dat,TrCrSm==0)		#this includes the 4 smoke days of 2018
	nrow(dat3) # =2034
	gclnt<-gam(formula=fclnt,data=dat3,na.action=na.exclude)
	dat$predClinton<-predict(gclnt,newdata=dat)
	clntdf<-subset(dat,TrCrSm==0 & !is.na(predClinton) & !is.na(Clinton),select=c("Clinton","predClinton","TrCrSm"))
	clntdf$resClinton<-clntdf$Clinton-clntdf$predClinton
	
	presclnt_WS<-ggplot(data=clntdf,aes(x=resClinton)) + geom_histogram() + geom_vline(xintercept=0,color="white")
	
	#removing the smoke days for 2018... 
	dat3<-subset(dat,TrCrSm==0 & !format(Date,"%Y-%m-%d") %in% c("2018-07-26","2018-07-27","2018-08-23","2018-08-24"))
	nrow(dat3) # =2034
	gclnt<-gam(formula=fclnt,data=dat3,na.action=na.exclude)
	dat$predClinton<-predict(gclnt,newdata=dat)
	clntdf<-subset(dat,TrCrSm==0 & !is.na(predClinton) & !is.na(Clinton),select=c("Clinton","predClinton","TrCrSm"))
	clntdf$resClinton<-clntdf$Clinton-clntdf$predClinton
	
	presclnt_NS<-ggplot(data=clntdf,aes(x=resClinton)) + geom_histogram() + geom_vline(xintercept=0,color="white")
	
	
	
	
	#########################
	## How are data dfferent from what is said in the report?
	dat$diffPred<-dat$predManvel-dat$manp
	#here is how much off we are
	datM<-subset(dat,!is.na(Manvel))
	print(paste("Mean squared difference:",sqrt(mean(datM$diffPred^2,na.rm=T))))
	print(paste("Maximum difference:",sqrt(max(datM$diffPred^2,na.rm=T))))
	print(paste("Our model predicts over Jaffe's by a maximum of:",max(datM$diffPred,na.rm=T)))
	print(paste("our model predicts under Jaffe's by a maximum of:",min(datM$diffPred,na.rm=T)))
	
#}