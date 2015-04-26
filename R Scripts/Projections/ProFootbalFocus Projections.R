###########################
# File: ProFootballFocus Projections.R
# Description: Downloads Fantasy Football Projections from profootballfocus.com
# Date: 4/14/2015
# Author: datalarge
# Notes:
# To do:
###########################

#Load libraries
library("XML")
library("stringr")
library("ggplot2")
library("plyr")
library("data.table")
library("Rglpk")
library("jsonlite")

#Functions
source(paste(getwd(),"/R Scripts/Functions/Functions.R", sep=""))
source(paste(getwd(),"/R Scripts/Functions/League Settings.R", sep=""))

#Projection Info
suffix <- "pff"

#Download fantasy football projections from cbssports.com
pff_baseurl <- "http://www.profootballfocus.com/toolkit/data/1/ROS/"
pff_pos <- c("QB","RB","WR","TE","K","DST")
pff_source <- c("pff")
pff_urls <- paste0(pff_baseurl, pff_pos)

#get the names for the data by position
headerNames <- list()
headerNames[["QB"]] <- c("rank","name","team","opp","gms","passComp","passAtt","passYds","passTds","passInt","passSack"
                         ,"rushAtt","rushYds","rushTd"
                         ,"fumbles","fumblesLost","twoPts","points","$")
headerNames[["RB"]] <- c("rank","name","team","opp","gms","rushAtt","rushYds","rushTds","targets","rec","recYds","recTds","returnYds","returnTds","fumbles","fl","twoPts","points","$")
headerNames[["WR"]] <- c("rank","name","team","opp","gms","targets","rec","recYds","recTds","rushAtt","rushYds","rushTds","returnYds","returnTds","fumbles","fl","twoPts","points","$")
headerNames[["TE"]] <- c("rank","name","team","opp","gms","targets","rec","recYds","recTds","returnYds","returnTds","fumbles","fl","twoPts","points","$")
headerNames[["K"]] <- c("rank","name","team","opp","gms","fg019","fg2029","fg3039","fg4049","fg50","xp","points","$")
headerNames[["DST"]] <- c("rank","name","opp","dstSack","dstSafety","dstInt","dstFumlForce","dstFumRec","dstTd","0","1_6","7_13","14_20","21_27","28_34","35_","points","$")
headerNames[["DL"]] <- c("rank","name","team","opp","gms","pos","idpSolo","idpAst","idpSack","idpSafety","idpTfl","idpInt","idpPD","idpFumlForce","idpFumlRec","idpTD","points","$")
headerNames[["LB"]] <- headerNames[["DL"]]
headerNames[["DB"]] <- headerNames[["DL"]]


#get the data for each position
pff_json = list()
index = 0

dir.create(file.path(getwd(), "/Data/temp/"), showWarnings = FALSE)

for (pos in pff_pos)
{
  index=index+1
  tempfile = paste0(getwd(), "/Data/temp/","pff_", pos, ".rtemp")
  tempurl = paste0(pff_baseurl, pos)
  
  if(file.exists(tempfile) && ((Sys.time()-(86400)) < file.info(tempfile)$mtime)){
    #get the cache if less than a day old
    pff_json[[pos]] <- read.table(tempfile, sep=",", header=TRUE)
  }else{
    #read the data from json
    #tempJson <- fromJSON(readLines(tempurl)[1])
    tempJson <- fromJSON(tempurl)
    
    tempJson <- tempJson$aaData
        
    #add data to the cache
    write.table(tempJson, file=tempfile, sep=",")    
    
    #populate the data
    pff_json[[pos]] <- read.table(tempfile, sep=",", header=TRUE)
  }
  
  #set column names
  colnames(pff_json[[pos]]) <- headerNames[[pos]]
  
  #add any additional info
  curlen <- length(pff_json[[pos]][,1])
  pff_json[[pos]]$pos <- rep(as.factor(pos), curlen)
  pff_json[[pos]]$sourceName <- rep(suffix, curlen)
  pff_json[[pos]]$positionRank <- rank(-pff_json[[pos]]$points, ties.method = "min")
  
}


#Merge
projections_pff <- rbind.fill(pff_json)

#Fix Kicker Projections
kColFix <- c("fg019","fg2029","fg3039","fg4049","fg50","xp")
for(fixcol in kColFix)
{
  tmpdf <- data.frame(do.call('rbind', strsplit(as.character(projections_pff[[fixcol]]),'/',fixed=TRUE)))
  projections_pff[[fixcol]] <- as.numeric(as.character(tmpdf[[1]]))
}

#Fix Team Names
projections_pff[["team"]] <- as.character(projections_pff[["team"]])
projections_pff[projections_pff$pos %in% c("DST"),"team"] <- 
  cleanTeamAbbreviations(convertTeamAbbreviation(as.character(projections_pff[projections_pff$pos %in% c("DST"),"name"])))
projections_pff[["team"]] <- cleanTeamAbbreviations(projections_pff[["team"]])

#Fix Player Names
projections_pff[["name"]] <- as.character(projections_pff[["name"]])
projections_pff[projections_pff$pos %in% c("DST"),"name"] <- convertTeamName(projections_pff[projections_pff$pos %in% c("DST"),"team"])
projections_pff$name_pff <- projections_pff$name
projections_pff$name <- nameMerge(projections_pff$name)

#check if there are any duplicates
duplicateCases_pff <- projections_pff[duplicated(projections_pff$name)]$name

#Calculate Overall Rank
projections_pff$overallRank <- rank(-projections_pff$points, ties.method = "min")

#Order variables in data set
allVars <- c(prefix, paste(sourceSpecific, suffix, sep="_"), varNames)
keepVars <- allVars[allVars %in% names(projections_pff)]
projections_pff <- projections_pff[,keepVars]

#Order players by overall rank
projections_pff <- projections_pff[order(projections_pff$overallRank),]

#Density Plot
ggplot(projections_pff, aes(x=points)) + geom_density(fill="red", alpha=.3) + xlab("Player's Projected Points") + ggtitle("Density Plot of PFF Projected Points")
ggsave(paste(getwd(),"/Figures/PFF projections.jpg", sep=""), width=10, height=10)
#supressResult <- dev.off()  #TODO: figure out why erroring in debug mode

#Add suffix to variable names prior to saving
colnames(projections_pff)[6:dim(projections_pff)[[2]]] <- paste(colnames(projections_pff)[6:dim(projections_pff)[[2]]],suffix,sep="_")

#Save file
save(projections_pff, file = paste0(getwd(), "/Data/PFF-Projections.RData"))
write.csv(projections_pff, file = paste0(getwd(), "/Data/PFF-Projections.csv"), row.names=FALSE)

save(projections_pff, file = paste0(getwd(), "/Data/Historical Projections/PFF-Projections-", season, ".RData"))
write.csv(projections_pff, file = paste0(getwd(), "/Data/Historical Projections/PFF-Projections-", season, ".csv"), row.names=FALSE)
