# TODO: Add comment
# 
# Author: lsalas
###############################################################################

#############
# REQUIRES: RMySQL, plyr, jsonlite


# Create a connection to a MySQL database
# host is the address to the database host
createConn<-function(host="dwdb.pointblue.org",dbname="ravian_wh",username="ravian",password="zakufeme"){
	con<-dbConnect(drv=MySQL(),dbname=dbname,username=username,password=password,host=host,port=3306)
	return(con)
}

# Get dataframe of projects at some sharing level and the number of records in each, from the warehouse database
# con is a valid connection - created with createConn
# table is the name of the table to query, with explicit sharing level 
getProjectsList<-function(con,protoType="pointcount"){
	projNames3<-dbGetQuery(conn=con,paste0("Select ProjectCode,count(*) as count3 from ravian",protoType,"level3_v1 group by ProjectCode Order by count3 desc"))
	projNames5<-dbGetQuery(conn=con,paste0("Select ProjectCode,count(*) as count5 from ravian",protoType,"level5_v1 group by ProjectCode Order by count5 desc"))
	projNames<-merge(projNames3,projNames5, by="ProjectCode",all.x=TRUE)
	projNames$count5<-ifelse(is.na(projNames$count5),0,projNames$count5)
	projNames$mustAsk<-ifelse(projNames$count5 < projNames$count3,TRUE,FALSE)
	return(projNames)
}

#This function returns a distance definition: exact, 3+ bins, 1-2 bins, no distance
getDistanceDef<-function(pp){
	DB3<-c("FRPC_Holmes_5m","FRPC_ODFWDet","FRPC_WDFW_Detailed","GLIMN_FR50","GLIMN_100m","FR10","FR50_00","MN_BBA","SEOW","m25_50M+",
			"m25_50_100M","m25_50_100_150M","m25_50_100M+","m50_100M+","m25_50M+","m10_25_50M","DISTANCE_ESTIMATION")
	DB12<-c("FR","m50_100M","m100M","FRPC_WDFW_General")
	if(grepl("VRPC",pp)){
		distdef<-"Exact"
	}else if(grepl("VCP",pp)){
		distdef<-"Exact/3+bins"
	}else if(TRUE %in% sapply(DB3,function(bb){grepl(bb,pp)})){
		distdef<-"3+bins"
	}else if(TRUE %in% sapply(DB12,function(bb){grepl(bb,pp)})){
		distdef<-"1_2bins"
	}else if(grepl("Unltd",pp)){
		distdef<-"None"
	}else{
		spl1<-strsplit(pp,"M")
		if(NROW(spl1[[1]])==2){
			spl2<-strsplit(spl1[[1]][1],"m")
			if(NROW(spl2[[1]])==2){
				dinf<-spl2[[1]][2]
				nbins<-NROW(as.numeric(gregexpr("_",dinf)[[1]][1]))+1
				if(nbins<3){
					distdef<-"1_2bins"
				}else if(nbins>=3){
					distdef<-"3+bins"
				}
			}else{
				distdef<-"Unknown"
			}
		}else{
			distdef<-"Unknown"
		}
		
	}
	return(distdef)
}

#This functions returns a time definition: the numbers and definitions of time bins (e.g., 1TB:5, or 2TB:3_5, or 3TB:3_5_10)
getTimeDef<-function(pp){
	if(pp=="VCP" | pp=="VCP200_ASSOC" | grepl("3_5",pp) | grepl("5M_2TB",toupper(pp)) | pp=="KBO_FRPC_1"){
		timedef<-"2TB 5min 3_5"
	}else if(pp=="VCP300_BBWO_PLAYBACK"){
		timedef<-"1TB 6min"
	}else if(grepl("VCP",pp) | grepl("5MIN_1TB",toupper(pp)) | grepl("5m_1TB",pp) | pp=="FR50"){
		timedef<-"1TB 5min"
	}else if(grepl("3_5_10",pp)){
		timedef<-"3TB 10min 3_5_10"
	}else if(grepl("MN_BBA",pp)){
		timedef<-"9TB 10min 2_3-10"
	}else if(grepl("0_to_9",pp)){
		timedef<-"10TB 10min 1-10"
	}else if(grepl("7M_3TB",toupper(pp)) | grepl("7min_3TB",pp)){
		timedef<-"3TB 7min 3_5_7"
	}else if(grepl("5m",pp) | grepl("FR50_IO",pp) | grepl("FR50_00",pp) | grepl("FR10_25_50",pp) | pp=="SEOW_Survey_1-c"){
		timedef<-"1TB 5min"
	}else if(grepl("10m",pp) | grepl("FR50_T10",pp) | grepl("10min_1TB",pp)){
		timedef<-"1TB 10min"
	}else if(pp=="3m100M"){
		timedef<-"1TB 3min"
	}else if(grepl("VRPC",pp)){
		if(grepl("General_3TB",pp) | grepl("_VRPC_2",pp)){
			timedef<-"2TB 5min 3_5"
		}else if(grepl("INBM",pp) | grepl("KBO_VRPC_4",pp)){
			timedef<-"10TB 10min 1-10"
		}else if(grepl("_VRPC_1",pp)){
			timedef<-"1TB 5min"
		}else if(grepl("BBS_1",pp)){
			timedef<-"1TB 3min"
		}else if(grepl("BBS_2",pp)){
			timedef<-"1TB 1min"
		}else if(grepl("KBO_VRPC_3",pp)){
			timedef<-"5TB 5min 1-5"
		}else if(grepl("RSL_VRPC_3",pp)){
			timedef<-"2TB 10min 5_10"
		}else if(grepl("RSL_VRPC_4",pp)){
			timedef<-"3TB 10min 3_5_10"
		}else if(grepl("RSL_VRPC_5",pp)){
			timedef<-"2TB 8min 4_8"
		}else{
			timedef<-"Unknown"
		}
	}else if(grepl("FRPC",pp)){
		if(grepl("5min_1TB",pp) | grepl("Holmes_5m",pp)){
			timedef<-"1TB 5min"
		}else if(grepl("5min_2TB",pp) | grepl("5m_2tb",pp) | pp=="RSL_FRPC_1"){
			timedef<-"2TB 3_5min"
		}else if(grepl("BES_8min",pp) | grepl("8min_3TB",pp)){
			timedef<-"3TB 3_5_8min"
		}else if(grepl("8min_2TB",pp)){
			timedef<-"2TB 5_8min"
		}else if(grepl("10min_3TB",pp)){
			timedef<-"3TB 3_5_10min"
		}else if(grepl("6min_1TB",pp)){
			timedef<-"1TB 6min"
		}else if(grepl("6min_3TB",pp)){
			timedef<-"3TB 3_5_6min"
		}else{
			timedef<-"Unknown"
		}
	}else if(grepl("FR",pp)){
		if(grepl("FR50_T10",pp)){
			timedef<-"1TB 10min"
		}else if(grepl("FR50_T35",pp)){
			timedef<-"2TB 3_5min"
		}else{
			timedef<-"Unknown"
		}
	}else{
		timedef<-"Unknown"
	}
	return(timedef)
}

# For a particular project, find the survey events at Level3 that do not exist at Level5
# con is a valid connection - created with createConn
# table is the name of the table to query, with explicit sharing level 
# proj is the ProjectCode whose records we are interested in
# knownGUIDs is the list of record GUIDs exposed at level5, presumably from a fetch using the getData function
getLevel3Uniques<-function(con,table,proj,knownGUIDs){
	l3recs<-dbGetQuery(conn=con,paste("Select GlobalUniqueIdentifier as guid from",table,"where ProjectCode = '",proj,"'"))
	l3recs<-subset(l3recs,!l3recs %in% knownGUIDS)
	return(l3recs)
}

# dispatch function to calculate the various metrics describing a dataset
# searches at level 3, since anything level 5 can be accessed via the DataCatalog and downloader tool
getProjectMetadata<-function(con,protoType="pointcount",projectList){
	## General info
	metadf<-ldply(projectList,function(pp,con,protoType){
				#numRecs comes from getprojectsList
				#numSpecies
				qry<-paste0("select distinct SpeciesCode from ravian",protoType,"level3_v1 where ProjectCode = '",pp,"'")
				spp<-dbGetQuery(conn=con,qry)
				projSpecies<-as.character(toJSON(spp$SpeciesCode))
				
				#protocols
				qry<-paste0("select distinct ProtocolCode from ravian",protoType,"level3_v1 where ProjectCode = '",pp,"'")
				pcd<-dbGetQuery(conn=con,qry)
				didef<-sapply(pcd$ProtocolCode,function(pc){
							dd<-getDistanceDef(pc)
							return(dd)
						})
				pcd$distDef<-didef
				tmdef<-sapply(pcd$ProtocolCode,function(pc){
							tt<-getTimeDef(pc)
							return(tt)
						})
				pcd$timeDef<-tmdef
				pcd$pURL<-paste0("https://data.pointblue.org/science/biologists/php/protocolsearch.php?protocol=",pcd$ProtocolCode)
				projProtocols<-as.character(toJSON(pcd))
				
				#visits per year
				qry<-paste0("select max(Visit) as maxVisit from ravian",protoType,"level3_v1 where ProjectCode = '",pp,"'")
				vst<-dbGetQuery(conn=con,qry)
				projVisits<-vst$maxVisit
				
				#yearspan
				qry<-paste0("select min(yearCollected) as minYear,max(yearCollected) as maxYear from ravian",protoType,"level3_v1 where ProjectCode = '",pp,"'")
				ysp<-dbGetQuery(conn=con,qry)
				projYearspan<-paste0(ysp$minYear,"-",ysp$maxYear)
				
				#number of survey locations
				if(protoType=="pointcount"){
					qry<-paste0("select count(distinct SamplingUnitId) as numPoints, count(distinct ParentSamplingUnitId) as numTransects, count(distinct StudyArea) as numAreas from ravian",protoType,"level3_v1 where ProjectCode = '",pp,"'")
					nsp<-dbGetQuery(conn=con,qry)
					projLocations<-as.character(toJSON(nsp))
				}else if(protoType=="areasearch"){
					qry<-paste0("select count(distinct SamplingUnitId) as numPlots, count(distinct StudyArea) as numAreas from ravian",protoType,"level3_v1 where ProjectCode = '",pp,"'")
					nsp<-dbGetQuery(conn=con,qry)
					projLocations<-as.character(toJSON(nsp))
				}else{}
				
				#extent, if available
				qry<-paste0("select min(DecimalLongitude) as minLon, max(DecimalLongitude) as maxLon, min(DecimalLatitude) as minLat, max(DecimalLatitude) as maxLat from ravian",protoType,"level3_v1 where ProjectCode = '",pp,"'")
				asp<-dbGetQuery(conn=con,qry)
				if(!is.null(asp$minLon) & !is.null(asp$maxLon) & !is.null(asp$minLat) & !is.null(asp$maxLat)){
					projGeoSpan<-as.character(toJSON(asp))
				}else{
					projGeoSpan<-"Unknown"
				}
				tdf<-data.frame(ProjectCode=pp,Species=projSpecies,Protocols=projProtocols,maxVisitsPerYear=projVisits,Yearspan=projYearspan,NumLocations=projLocations,GeoExtent=projGeoSpan)
				return(tdf)
			},con=con,protoType=protoType)
	
	return(metadf)
}

# function to retrieve Project leader info for each dataset
# con is a valid connection to txdb.prbodb
getProjectleaders<-function(con,projectList){
	#Project Leader info
	pleaddf<-ldply(projectList,function(pp,con){
				qry<-paste0("select t1.ResearcherId, t2.ResearcherFirstName, t2.ResearcherLastName, t2.EmailAddress from projectresearcher as t1 ",
						"inner join researcher as t2 on (t1.ResearcherId = t2.ResearcherId) where t1.projectId = '",pp,"' and t1.ProjectLeaderInd = 1 and t2.DoNotContactInd = 0")
				plv<-dbGetQuery(conn=con,qry)
				plv<-plv[order(plv$ResearcherId),]
				plv<-plv[c(1:3),]
				plv$Researcher<-paste(plv$ResearcherFirstName,plv$ResearcherLastName)
				plv<-plv[,c("Researcher","EmailAddress")]
				projPL<-as.character(toJSON(plv))
				tdf<-data.frame(ProjectCode=pp,ProjectLeads=projPL)
				return(tdf)
			},con=con)
	return(pleaddf)
}
