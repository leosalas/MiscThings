# TODO: Add comment
# 
# Author: lsalas
###############################################################################


# Read the data
data<-read.csv("C:/Users/lsalas/git/samm-port-hedland/output/paired_concentrations.csv", stringsAsFactors=F) 
train<-subset(data,category=="yobs")
ltrain<-reshape(train[,-1],idvar="tstamp",varying = list(2:16),v.names = "observed",times=names(train)[3:17],direction="long",new.row.names = NULL)
row.names(ltrain)<-NULL; names(ltrain)<-gsub("time","site",names(ltrain))

pred<-subset(data,category=="yest")
lpred<-reshape(pred[,-1],idvar="tstamp",varying = list(2:16),v.names = "predicted",times=names(train)[3:17],direction="long",new.row.names = NULL)
row.names(lpred)<-NULL; names(lpred)<-gsub("time","site",names(lpred))

pdf<-merge(ltrain,lpred,by=c("tstamp","site"))
library(ggplot2)
(p1<-ggplot(data=pdf,aes(x=observed,y=predicted)) + geom_point(aes(color=site)) + 
			geom_abline(slope=1) +
			theme(legend.position="none"))

# remove the big outlier - why is that an outlier Is this a good means to detect an emission issue?
(p2<-ggplot(data=subset(pdf,observed<1000),aes(x=log(observed),y=log(predicted))) + geom_point(aes(color=site)) +
			geom_abline(slope=1) +
			theme(legend.position="none"))


# NOTE: obviously, we wanted predictions to line up on the 1:1 line. That is not the case. Why not?

# Let's look for patterns in outliers
# First we calculate a standard difference, the chi-squared distance between observed and predicted
pdf$chisqdiff<-sqrt(((pdf$observed-pdf$predicted)^2)/pdf$observed)
pdf$timedat<-as.POSIXct(pdf$tstamp)
pdf$year<-format(pdf$timedat,"%Y")
pdf$month<-format(pdf$timedat,"%m")
pdf$day<-format(pdf$timedat,"%d")
pdf$hour<-format(pdf$timedat,"%H")


(p3<-ggplot(data=subset(pdf,observed<1000),aes(x=chisqdiff)) + geom_density(aes(color=site,fill=site)) + 
			labs(y="Frequency",x="Chi-sq difference") +
			coord_flip() + facet_wrap(~site,ncol=5))

(p4<-ggplot(data=subset(pdf,observed<1000),aes(x=timedat,y=chisqdiff)) + geom_point(aes(color=site)) +
			labs(x="Date-time", y="Chi-squared difference"))
pdfmx<-aggregate(chisqdiff~timedat,subset(pdf,observed<1000),max);names(pdfmx)<-gsub("chisqdiff","diffmx",names(pdfmx))
pdf<-merge(pdf,pdfmx,by="timedat",all.x=T)
pdf<-pdf[order(pdf$timedat),]
(p4a<-ggplot(data=subset(pdf,observed<1000),aes(x=timedat,y=chisqdiff)) + geom_point(aes(color=site)) +
			geom_line(aes(y=diffmx)))

(p5<-ggplot(data=pdfmx,aes(aes(x=timedat,y=chisqdiff))) + geom_line())

(p5<-ggplot(data=subset(pdf,observed<1000),aes(x=year,y=chisqdiff)) + geom_boxplot() + geom_jitter(aes(color=site)))


