# TODO: Add comment
# 
# Author: lsalas
###############################################################################


libs<-c("RMySQL","plyr","jsonlite")
lapply(libs, require, character.only = TRUE)

source("c:/users/lsalas/git/MiscThings/dataRequests/dataRequests_utils.R")

# 0) Define a protocol type: pointcount, areasearch
protype<-"pointcount"

# 1) create a connection to dwdb.ravian_wh
dwcon<-createConn()

# 2) get list of projects
projList<-getProjectsList(con=dwcon,protoType=protype)	#defaults to pointcount

# 3) for each project, get metadata
projMeta<-getProjectMetadata(con=dwcon,protoType=protype,projectList=projList$ProjectCode)

# 4) Close connection to dwdb and open connection to txdb.prbodb
dbDisconnect(dwcon)
tdcon<-createConn(host="txdb.pointblue.org",dbname="prbodb",username="lsalas",password="&pBtt2G.")

# 4) for each project, get the project leader info
projPL<-getProjectleaders(con=tdcon,projectList=projList$ProjectCode)
dbDisconnect(tdcon)

# 5) merge and save for export
projects<-merge(projList,projMeta,by="ProjectCode",all.x=T)
projects<-merge(projects,projPL,by="ProjectCode",all.x=T)
filen<-paste0("request_",protype,"_",format(Sys.time(),"%Y%m%d_%H%M"))
save(projects,file=paste0("c:/users/lsalas/desktop/dataRequests/",filen,".RData"))
write.csv(projects,file=paste0("c:/users/lsalas/desktop/dataRequests/",filen,".csv"))
