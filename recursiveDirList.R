# TODO: Add comment
# 
# Author: lsalas
###############################################################################


## This code list the directory contents, recursively.
library(plyr)
bb<-list.dirs("V:",full.names=TRUE,recursive=TRUE)
outfile<-"C:/Users/Public/Documents/V_dircontents.csv"
colns<-data.frame("filenam", "folder", "fileSize","prettySize","lastModified","whenCreated","isExe")
write.table(colns, file=outfile, sep=",", append=TRUE, quote=FALSE,	col.names=FALSE, row.names=FALSE)
tm<-Sys.time()
dircontents<-l_ply(bb,function(dd,outfile){
			fnn<-list.files(dd,all.files=TRUE,include.dirs=FALSE,no..=TRUE,recursive = FALSE,pattern="\\.")
			if(length(fnn)>0){
				tdf<-data.frame(filenam=fnn,folder=rep(dd,times=length(fnn)))
				tdf$fileSize<-file.size(paste0(tdf$folder,"/",tdf$filenam))
				tdf$prettySize<-as.character(sapply(tdf$fileSize,function(ss){
									if(!is.na(ss)){
										sv<-utils:::format.object_size(ss,"auto")
									}else(sv<-"NA");return(sv)}))
				tdf$lastModified<-file.mtime(paste0(tdf$folder,"/",tdf$filenam))
				tdf$whenCreated<-file.info(paste0(tdf$folder,"/",tdf$filenam))$ctime
				tdf$isExe<-file.info(paste0(tdf$folder,"/",tdf$filenam))$exe
				write.table(tdf, file=outfile, sep=",",	append=TRUE, quote=FALSE,
						col.names=FALSE, row.names=FALSE)
			}
		},outfile=outfile)
Sys.time()-tm
dirIndex<-data.frame(Directory=bb,dirIndex=1:length(bb))
write.csv(dirIndex,file="C:/Users/Public/Documents/V_dirIndex.csv")



## This code will tile the image and write into small tiles ONLY if there is data in the tile
libs<-c("plyr","dplyr","rgdal","raster","ggplot2","RStoolbox","gdalUtils")
suppressPackageStartupMessages(lapply(libs, require, character.only = TRUE))
r<-stack("c:/users/lsalas/downloads/croz_20191202_east_v7.tif")
savepth<-"c:/users/lsalas/downloads/tiles/"
er<-extent(r)
tzpx<-512	# Tile size in pixels on x
tzpy<-256	# on y
psizen<-"512x256"
length_x<-er[2]-er[1]
length_y<-er[2]-er[1]
rezx<-length_x/ncol(r)
rezy<-length_y/nrow(r)
tzx<-8.27  #tzpx*rezx
tzy<-4.13  #tzpy*rezy - if not flooring, I get 259 pixels
stepx<-tzx/2  # Overlapping tiles in half-width and height
stepy<-tzy/2  # Overlapping tiles in half-width and height
ne<-floor((er[2]-er[1])/tzx);nn<-floor((er[4]-er[3])/tzy)
makeTiles<-TRUE # Set to true if you want the tiles made, else you get a data.frame with the list of tiles so you can adjust tile size.
cf <- coord_fixed(); cf$default <- TRUE
tm<-Sys.time()
tilesdf<-ldply(0:ne,function(tx,tzx,tzy,stepx,stepy,er,nn,r,savepth,makeTiles,cf,psizen){
	mnx<-(tx*tzx)+er[1];mxx<-mnx+tzx
	mnsx<-(tx*tzx)+er[1]+stepx;mxsx<-mnsx+tzx
	londf<-ldply(0:nn,function(ty,tzy,stepy,er,r,savepth,tx,mnx,mxx,mnsx,mxsx,makeTiles,cf,psizen){   #can parallelize here .parallel=T but need snow and declare cores
				mny<-(ty*tzy)+er[3];mxy<-mny+tzy
				mnsy<-(ty*tzy)+er[3]+stepy;mxsy<-mnsy+tzy
				nmx<-paste0("x",tx);nmy<-paste0("y",ty)
				nmt<-paste0(nmx,"_",nmy,"_",psizen); nmts<-paste0(nmt,"S")
				tdf<-data.frame(name=c(nmt,nmts),xmin=c(mnx,mnsx),xmax=c(mxx,mxsx),ymin=c(mny,mnsy),ymax=c(mxy,mxsy))
				if(makeTiles){
					tcr<-crop(r,extent(c(mnx,mxx,mny,mxy))); tcrs<-crop(r,extent(c(mnsx,mxsx,mnsy,mxsy)))
					if(!identical(tcr@data@min[1:3],tcr@data@max[1:3])){
						writeRaster(tcr,filename=paste0(savepth,nmt,".tif"),format="GTiff",overwrite=TRUE)
						jpeg(filename = paste0(savepth,nmt,".jpg"),	width = 512, height = 256, units = "px", quality = 100)
							plotRGB(tcr)
						dev.off()
					}
					if(!identical(tcrs@data@min[1:3],tcrs@data@max[1:3])){
						writeRaster(tcrs,filename=paste0(savepth,nmts,".tif"),format="GTiff",overwrite=TRUE)
						jpeg(filename = paste0(savepth,nmts,".jpg"),	width = 512, height = 256, units = "px", quality = 100)
							plotRGB(tcrs)
						dev.off()
					}
				}
				return(tdf)
			},tzy=tzy,stepy=stepy,er=er,r=r,savepth=savepth,tx=tx,mnx=mnx,mxx=mxx,mnsx=mnsx,mxsx=mxsx,makeTiles=makeTiles,cf=cf,psizen=psizen)
	return(londf)
},tzx=tzx,tzy=tzy,stepx=stepx,stepy=stepy,er=er,nn=nn,r=r,savepth=savepth,makeTiles=makeTiles,cf=cf,psizen=psizen)
nrow(tilesdf)
Sys.time()-tm

#plot a tile
vti <- ggRGB(ti,r=1,g=2,b=3) + theme_bw() + coord_quickmap()

