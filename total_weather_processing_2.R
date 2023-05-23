rm(list=ls())
library("xlsx")
library(dplyr)

setwd("~/Documents/")

###functions
filtering=function(x,y,z){
paste(x,y,x,z,sep="")
}

meandata=function(x,y,e,f,g,l,k,z){
paste(x,y,l,k,x,e,x,f,x,g,x,z,sep="")
}

collnames=function(x,y,z){
paste(y,x,z,sep="")
}

bindrowsfun=function(x,y,z){
paste(x,y,z,sep="")
}

########Georgia########
##totalweatherdata-gofrom1/10adegreetodegree
##thenfindaverageofalldataforthatcity
GAtotal<-read.csv("GA_data_total_weather.csv")

##makeprcp,tmax,tmin,andtavgnumeric

GAtotal$prcp<-as.numeric(GAtotal$prcp)
GAtotal$tmax<-as.numeric(GAtotal$tmax)
GAtotal$tmin<-as.numeric(GAtotal$tmin)
GAtotal$tavg<-as.numeric(GAtotal$tavg)

##multiplyby0.1ordivideby10tomake1degree

GAtotal$prcp<-GAtotal$prcp/10
GAtotal$tmax<-GAtotal$tmax/10
GAtotal$tmin<-GAtotal$tmin/10
GAtotal$tavg<-GAtotal$tavg/10

#pulldata
id2<-unique(GAtotal$id2)
cat(filtering(id2,y="<-filter(GAtotal,id2=='",z="')"),sep="\n")

athens2<-filter(GAtotal,id2=='athens2')
rome1<-filter(GAtotal,id2=='rome1')
milledgeville<-filter(GAtotal,id2=='milledgeville')
rome2<-filter(GAtotal,id2=='rome2')
darien1<-filter(GAtotal,id2=='darien1')
darien3<-filter(GAtotal,id2=='darien3')
darien2<-filter(GAtotal,id2=='darien2')
statesboro2<-filter(GAtotal,id2=='statesboro2')
statesboro1<-filter(GAtotal,id2=='statesboro1')
athens1<-filter(GAtotal,id2=='athens1')
gray<-filter(GAtotal,id2=='gray')
quitman1<-filter(GAtotal,id2=='quitman1')
quitman3<-filter(GAtotal,id2=='quitman3')
quitman2<-filter(GAtotal,id2=='quitman2')
dalton<-filter(GAtotal,id2=='dalton')
cochran1<-filter(GAtotal,id2=='cochran1')
colquit<-filter(GAtotal,id2=='colquit')
bowman<-filter(GAtotal,id2=='bowman')
macon<-filter(GAtotal,id2=='macon')
madison<-filter(GAtotal,id2=='madison')
cochran2<-filter(GAtotal,id2=='cochran2')
cochran3<-filter(GAtotal,id2=='cochran3')
newton<-filter(GAtotal,id2=='newton')


###findthemeanforeach
lennumber=(1:23)
df<-list(id2,lennumber)

lapply(df,cat(meandata(x=id2,y="_data<-data.frame((id2[",l=lennumber,k="]),(mean(",e="$prcp,na.rm=TRUE)),(mean(",f="$tmax,na.rm=TRUE)),
(mean(",g="$tmin,na.rm=TRUE)),(mean(",z="$tavg,na.rm=TRUE)))"),sep="\n"))

athens2_data<-data.frame((id2[1]),(mean(athens2$prcp,na.rm=TRUE)),(mean(athens2$tmax,na.rm=TRUE)),
(mean(athens2$tmin,na.rm=TRUE)),(mean(athens2$tavg,na.rm=TRUE)))
rome1_data<-data.frame((id2[2]),(mean(rome1$prcp,na.rm=TRUE)),(mean(rome1$tmax,na.rm=TRUE)),
(mean(rome1$tmin,na.rm=TRUE)),(mean(rome1$tavg,na.rm=TRUE)))
milledgeville_data<-data.frame((id2[3]),(mean(milledgeville$prcp,na.rm=TRUE)),(mean(milledgeville$tmax,na.rm=TRUE)),
(mean(milledgeville$tmin,na.rm=TRUE)),(mean(milledgeville$tavg,na.rm=TRUE)))
rome2_data<-data.frame((id2[4]),(mean(rome2$prcp,na.rm=TRUE)),(mean(rome2$tmax,na.rm=TRUE)),
(mean(rome2$tmin,na.rm=TRUE)),(mean(rome2$tavg,na.rm=TRUE)))
darien1_data<-data.frame((id2[5]),(mean(darien1$prcp,na.rm=TRUE)),(mean(darien1$tmax,na.rm=TRUE)),
(mean(darien1$tmin,na.rm=TRUE)),(mean(darien1$tavg,na.rm=TRUE)))
darien3_data<-data.frame((id2[6]),(mean(darien3$prcp,na.rm=TRUE)),(mean(darien3$tmax,na.rm=TRUE)),
(mean(darien3$tmin,na.rm=TRUE)),(mean(darien3$tavg,na.rm=TRUE)))
darien2_data<-data.frame((id2[7]),(mean(darien2$prcp,na.rm=TRUE)),(mean(darien2$tmax,na.rm=TRUE)),
(mean(darien2$tmin,na.rm=TRUE)),(mean(darien2$tavg,na.rm=TRUE)))
statesboro2_data<-data.frame((id2[8]),(mean(statesboro2$prcp,na.rm=TRUE)),(mean(statesboro2$tmax,na.rm=TRUE)),
(mean(statesboro2$tmin,na.rm=TRUE)),(mean(statesboro2$tavg,na.rm=TRUE)))
statesboro1_data<-data.frame((id2[9]),(mean(statesboro1$prcp,na.rm=TRUE)),(mean(statesboro1$tmax,na.rm=TRUE)),
(mean(statesboro1$tmin,na.rm=TRUE)),(mean(statesboro1$tavg,na.rm=TRUE)))
athens1_data<-data.frame((id2[10]),(mean(athens1$prcp,na.rm=TRUE)),(mean(athens1$tmax,na.rm=TRUE)),
(mean(athens1$tmin,na.rm=TRUE)),(mean(athens1$tavg,na.rm=TRUE)))
gray_data<-data.frame((id2[11]),(mean(gray$prcp,na.rm=TRUE)),(mean(gray$tmax,na.rm=TRUE)),
(mean(gray$tmin,na.rm=TRUE)),(mean(gray$tavg,na.rm=TRUE)))
quitman1_data<-data.frame((id2[12]),(mean(quitman1$prcp,na.rm=TRUE)),(mean(quitman1$tmax,na.rm=TRUE)),
(mean(quitman1$tmin,na.rm=TRUE)),(mean(quitman1$tavg,na.rm=TRUE)))
quitman3_data<-data.frame((id2[13]),(mean(quitman3$prcp,na.rm=TRUE)),(mean(quitman3$tmax,na.rm=TRUE)),
(mean(quitman3$tmin,na.rm=TRUE)),(mean(quitman3$tavg,na.rm=TRUE)))
quitman2_data<-data.frame((id2[14]),(mean(quitman2$prcp,na.rm=TRUE)),(mean(quitman2$tmax,na.rm=TRUE)),
(mean(quitman2$tmin,na.rm=TRUE)),(mean(quitman2$tavg,na.rm=TRUE)))
dalton_data<-data.frame((id2[15]),(mean(dalton$prcp,na.rm=TRUE)),(mean(dalton$tmax,na.rm=TRUE)),
(mean(dalton$tmin,na.rm=TRUE)),(mean(dalton$tavg,na.rm=TRUE)))
cochran1_data<-data.frame((id2[16]),(mean(cochran1$prcp,na.rm=TRUE)),(mean(cochran1$tmax,na.rm=TRUE)),
(mean(cochran1$tmin,na.rm=TRUE)),(mean(cochran1$tavg,na.rm=TRUE)))
colquit_data<-data.frame((id2[17]),(mean(colquit$prcp,na.rm=TRUE)),(mean(colquit$tmax,na.rm=TRUE)),
(mean(colquit$tmin,na.rm=TRUE)),(mean(colquit$tavg,na.rm=TRUE)))
bowman_data<-data.frame((id2[18]),(mean(bowman$prcp,na.rm=TRUE)),(mean(bowman$tmax,na.rm=TRUE)),
(mean(bowman$tmin,na.rm=TRUE)),(mean(bowman$tavg,na.rm=TRUE)))
macon_data<-data.frame((id2[19]),(mean(macon$prcp,na.rm=TRUE)),(mean(macon$tmax,na.rm=TRUE)),
(mean(macon$tmin,na.rm=TRUE)),(mean(macon$tavg,na.rm=TRUE)))
madison_data<-data.frame((id2[20]),(mean(madison$prcp,na.rm=TRUE)),(mean(madison$tmax,na.rm=TRUE)),
(mean(madison$tmin,na.rm=TRUE)),(mean(madison$tavg,na.rm=TRUE)))
cochran2_data<-data.frame((id2[21]),(mean(cochran2$prcp,na.rm=TRUE)),(mean(cochran2$tmax,na.rm=TRUE)),
(mean(cochran2$tmin,na.rm=TRUE)),(mean(cochran2$tavg,na.rm=TRUE)))
cochran3_data<-data.frame((id2[22]),(mean(cochran3$prcp,na.rm=TRUE)),(mean(cochran3$tmax,na.rm=TRUE)),
(mean(cochran3$tmin,na.rm=TRUE)),(mean(cochran3$tavg,na.rm=TRUE)))
newton_data<-data.frame((id2[23]),(mean(newton$prcp,na.rm=TRUE)),(mean(newton$tmax,na.rm=TRUE)),
(mean(newton$tmin,na.rm=TRUE)),(mean(newton$tavg,na.rm=TRUE)))


#columnnames
cat(collnames(unique(id2),y="colnames(",z="_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')"),sep="\n")

colnames(athens2_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(rome1_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(milledgeville_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(rome2_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(darien1_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(darien3_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(darien2_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(statesboro2_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(statesboro1_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(athens1_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(gray_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(quitman1_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(quitman3_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(quitman2_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(dalton_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(cochran1_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(colquit_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(bowman_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(macon_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(madison_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(cochran2_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(cochran3_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(newton_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')

##pasteintobind_rows()
cat(bindrowsfun(unique(id2),y="_data",z=","),sep="")
GAtotal_obs<-bind_rows(athens2_data,rome1_data,milledgeville_data,rome2_data,darien1_data,darien3_data,darien2_data,statesboro2_data,statesboro1_data,athens1_data,gray_data,quitman1_data,quitman3_data,quitman2_data,dalton_data,cochran1_data,colquit_data,bowman_data,macon_data,madison_data,cochran2_data,cochran3_data,newton_data)

###savedataasxlsxfile
write.xlsx(GAtotal_obs,file="GAtotal_obs.xlsx",sheetName="Sheet1",
col.names=TRUE,row.names=TRUE,append=FALSE)

#########Illinois########

##totalweatherdata-gofrom1/10adegreetodegree
##thenfindaverageofalldataforthatcity
ILtotal<-read.csv("IL_data_total_weather.csv")

##makeprcp,tmax,tmin,andtavgnumeric

ILtotal$prcp<-as.numeric(ILtotal$prcp)
ILtotal$tmax<-as.numeric(ILtotal$tmax)
ILtotal$tmin<-as.numeric(ILtotal$tmin)
ILtotal$tavg<-as.numeric(ILtotal$tavg)

##multiplyby0.1ordivideby10tomake1degree

ILtotal$prcp<-ILtotal$prcp/10
ILtotal$tmax<-ILtotal$tmax/10
ILtotal$tmin<-ILtotal$tmin/10
ILtotal$tavg<-ILtotal$tavg/10

#pulldata
id2<-unique(ILtotal$id2)
cat(filtering(id2,y="<-filter(ILtotal,id2=='",z="')"),sep="\n")

charleston2<-filter(ILtotal,id2=='charleston2')
chicago2<-filter(ILtotal,id2=='chicago2')
charleston1<-filter(ILtotal,id2=='charleston1')
champaign<-filter(ILtotal,id2=='champaign')
chicago1<-filter(ILtotal,id2=='chicago1')
naperville2<-filter(ILtotal,id2=='naperville2')
joilet<-filter(ILtotal,id2=='joilet')
momence<-filter(ILtotal,id2=='momence')
lacon<-filter(ILtotal,id2=='lacon')
naperville1<-filter(ILtotal,id2=='naperville1')
fairbury<-filter(ILtotal,id2=='fairbury')


###findthemeanforeach
lennumber=(1:11)
df<-liIL(id2,lennumber)

lapply(df,cat(meandata(x=id2,y="_data<-data.frame((id2[",l=lennumber,k="]),(mean(",e="$prcp,na.rm=TRUE)),(mean(",f="$tmax,na.rm=TRUE)),
(mean(",g="$tmin,na.rm=TRUE)),(mean(",z="$tavg,na.rm=TRUE)))"),sep="\n"))

charleston2_data<-data.frame((id2[1]),(mean(charleston2$prcp,na.rm=TRUE)),(mean(charleston2$tmax,na.rm=TRUE)),
(mean(charleston2$tmin,na.rm=TRUE)),(mean(charleston2$tavg,na.rm=TRUE)))
chicago2_data<-data.frame((id2[2]),(mean(chicago2$prcp,na.rm=TRUE)),(mean(chicago2$tmax,na.rm=TRUE)),
(mean(chicago2$tmin,na.rm=TRUE)),(mean(chicago2$tavg,na.rm=TRUE)))
charleston1_data<-data.frame((id2[3]),(mean(charleston1$prcp,na.rm=TRUE)),(mean(charleston1$tmax,na.rm=TRUE)),
(mean(charleston1$tmin,na.rm=TRUE)),(mean(charleston1$tavg,na.rm=TRUE)))
champaign_data<-data.frame((id2[4]),(mean(champaign$prcp,na.rm=TRUE)),(mean(champaign$tmax,na.rm=TRUE)),
(mean(champaign$tmin,na.rm=TRUE)),(mean(champaign$tavg,na.rm=TRUE)))
chicago1_data<-data.frame((id2[5]),(mean(chicago1$prcp,na.rm=TRUE)),(mean(chicago1$tmax,na.rm=TRUE)),
(mean(chicago1$tmin,na.rm=TRUE)),(mean(chicago1$tavg,na.rm=TRUE)))
naperville2_data<-data.frame((id2[6]),(mean(naperville2$prcp,na.rm=TRUE)),(mean(naperville2$tmax,na.rm=TRUE)),
(mean(naperville2$tmin,na.rm=TRUE)),(mean(naperville2$tavg,na.rm=TRUE)))
joilet_data<-data.frame((id2[7]),(mean(joilet$prcp,na.rm=TRUE)),(mean(joilet$tmax,na.rm=TRUE)),
(mean(joilet$tmin,na.rm=TRUE)),(mean(joilet$tavg,na.rm=TRUE)))
momence_data<-data.frame((id2[8]),(mean(momence$prcp,na.rm=TRUE)),(mean(momence$tmax,na.rm=TRUE)),
(mean(momence$tmin,na.rm=TRUE)),(mean(momence$tavg,na.rm=TRUE)))
lacon_data<-data.frame((id2[9]),(mean(lacon$prcp,na.rm=TRUE)),(mean(lacon$tmax,na.rm=TRUE)),
(mean(lacon$tmin,na.rm=TRUE)),(mean(lacon$tavg,na.rm=TRUE)))
naperville1_data<-data.frame((id2[10]),(mean(naperville1$prcp,na.rm=TRUE)),(mean(naperville1$tmax,na.rm=TRUE)),
(mean(naperville1$tmin,na.rm=TRUE)),(mean(naperville1$tavg,na.rm=TRUE)))
fairbury_data<-data.frame((id2[11]),(mean(fairbury$prcp,na.rm=TRUE)),(mean(fairbury$tmax,na.rm=TRUE)),
(mean(fairbury$tmin,na.rm=TRUE)),(mean(fairbury$tavg,na.rm=TRUE)))

#columnnames
cat(collnames(unique(id2),y="colnames(",z="_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')"),sep="\n")

colnames(charleston2_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(chicago2_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(charleston1_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(champaign_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(chicago1_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(naperville2_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(joilet_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(momence_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(lacon_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(naperville1_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(fairbury_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')

##paILeintobind_rows()
cat(bindrowsfun(unique(id2),y="_data",z=","),sep="")
ILtotal_obs<-bind_rows(charleston2_data,chicago2_data,charleston1_data,champaign_data,chicago1_data,naperville2_data,joilet_data,momence_data,lacon_data,naperville1_data,fairbury_data)

###savedataasxlsxfile
write.xlsx(ILtotal_obs,file="ILtotal_obs.xlsx",sheetName="Sheet1",
col.names=TRUE,row.names=TRUE,append=FALSE)



#######Indiana########

##totalweatherdata-gofrom1/10adegreetodegree
##thenfindaverageofalldataforthatcity
INtotal<-read.csv("IN_data_total_weather.csv")

##makeprcp,tmax,tmin,andtavgnumeric

INtotal$prcp<-as.numeric(INtotal$prcp)
INtotal$tmax<-as.numeric(INtotal$tmax)
INtotal$tmin<-as.numeric(INtotal$tmin)
INtotal$tavg<-as.numeric(INtotal$tavg)

##multiplyby0.1ordivideby10tomake1degree

INtotal$prcp<-INtotal$prcp/10
INtotal$tmax<-INtotal$tmax/10
INtotal$tmin<-INtotal$tmin/10
INtotal$tavg<-INtotal$tavg/10

#pulldata
id2<-unique(INtotal$id2)
cat(filtering(id2,y="<-filter(INtotal,id2=='",z="')"),sep="\n")

bloomingdale<-filter(INtotal,id2=='bloomingdale')
warsaw<-filter(INtotal,id2=='warsaw')
bedford2<-filter(INtotal,id2=='bedford2')
southport1<-filter(INtotal,id2=='southport1')
southport2<-filter(INtotal,id2=='southport2')
huntington1<-filter(INtotal,id2=='huntington1')
evansville<-filter(INtotal,id2=='evansville')
allencounty<-filter(INtotal,id2=='allencounty')
huntington2<-filter(INtotal,id2=='huntington2')
bedford1<-filter(INtotal,id2=='bedford1')
bloomington<-filter(INtotal,id2=='bloomington')
decatur1<-filter(INtotal,id2=='decatur1')
huntington3<-filter(INtotal,id2=='huntington3')
richmond<-filter(INtotal,id2=='richmond')
sulivan<-filter(INtotal,id2=='sulivan')
rushville<-filter(INtotal,id2=='rushville')
rockville<-filter(INtotal,id2=='rockville')
mountvernon<-filter(INtotal,id2=='mountvernon')

###findthemeanforeach
lennumber=(1:18)
df<-list(id2,lennumber)

lapply(df,cat(meandata(x=id2,y="_data<-data.frame((id2[",l=lennumber,k="]),(mean(",e="$prcp,na.rm=TRUE)),(mean(",f="$tmax,na.rm=TRUE)),
(mean(",g="$tmin,na.rm=TRUE)),(mean(",z="$tavg,na.rm=TRUE)))"),sep="\n"))

bloomingdale_data<-data.frame((id2[1]),(mean(bloomingdale$prcp,na.rm=TRUE)),(mean(bloomingdale$tmax,na.rm=TRUE)),
(mean(bloomingdale$tmin,na.rm=TRUE)),(mean(bloomingdale$tavg,na.rm=TRUE)))
warsaw_data<-data.frame((id2[2]),(mean(warsaw$prcp,na.rm=TRUE)),(mean(warsaw$tmax,na.rm=TRUE)),
(mean(warsaw$tmin,na.rm=TRUE)),(mean(warsaw$tavg,na.rm=TRUE)))
bedford2_data<-data.frame((id2[3]),(mean(bedford2$prcp,na.rm=TRUE)),(mean(bedford2$tmax,na.rm=TRUE)),
(mean(bedford2$tmin,na.rm=TRUE)),(mean(bedford2$tavg,na.rm=TRUE)))
southport1_data<-data.frame((id2[4]),(mean(southport1$prcp,na.rm=TRUE)),(mean(southport1$tmax,na.rm=TRUE)),
(mean(southport1$tmin,na.rm=TRUE)),(mean(southport1$tavg,na.rm=TRUE)))
southport2_data<-data.frame((id2[5]),(mean(southport2$prcp,na.rm=TRUE)),(mean(southport2$tmax,na.rm=TRUE)),
(mean(southport2$tmin,na.rm=TRUE)),(mean(southport2$tavg,na.rm=TRUE)))
huntington1_data<-data.frame((id2[6]),(mean(huntington1$prcp,na.rm=TRUE)),(mean(huntington1$tmax,na.rm=TRUE)),
(mean(huntington1$tmin,na.rm=TRUE)),(mean(huntington1$tavg,na.rm=TRUE)))
evansville_data<-data.frame((id2[7]),(mean(evansville$prcp,na.rm=TRUE)),(mean(evansville$tmax,na.rm=TRUE)),
(mean(evansville$tmin,na.rm=TRUE)),(mean(evansville$tavg,na.rm=TRUE)))
allencounty_data<-data.frame((id2[8]),(mean(allencounty$prcp,na.rm=TRUE)),(mean(allencounty$tmax,na.rm=TRUE)),
(mean(allencounty$tmin,na.rm=TRUE)),(mean(allencounty$tavg,na.rm=TRUE)))
huntington2_data<-data.frame((id2[9]),(mean(huntington2$prcp,na.rm=TRUE)),(mean(huntington2$tmax,na.rm=TRUE)),
(mean(huntington2$tmin,na.rm=TRUE)),(mean(huntington2$tavg,na.rm=TRUE)))
bedford1_data<-data.frame((id2[10]),(mean(bedford1$prcp,na.rm=TRUE)),(mean(bedford1$tmax,na.rm=TRUE)),
(mean(bedford1$tmin,na.rm=TRUE)),(mean(bedford1$tavg,na.rm=TRUE)))
bloomington_data<-data.frame((id2[11]),(mean(bloomington$prcp,na.rm=TRUE)),(mean(bloomington$tmax,na.rm=TRUE)),
(mean(bloomington$tmin,na.rm=TRUE)),(mean(bloomington$tavg,na.rm=TRUE)))
decatur1_data<-data.frame((id2[12]),(mean(decatur1$prcp,na.rm=TRUE)),(mean(decatur1$tmax,na.rm=TRUE)),
(mean(decatur1$tmin,na.rm=TRUE)),(mean(decatur1$tavg,na.rm=TRUE)))
huntington3_data<-data.frame((id2[13]),(mean(huntington3$prcp,na.rm=TRUE)),(mean(huntington3$tmax,na.rm=TRUE)),
(mean(huntington3$tmin,na.rm=TRUE)),(mean(huntington3$tavg,na.rm=TRUE)))
richmond_data<-data.frame((id2[14]),(mean(richmond$prcp,na.rm=TRUE)),(mean(richmond$tmax,na.rm=TRUE)),
(mean(richmond$tmin,na.rm=TRUE)),(mean(richmond$tavg,na.rm=TRUE)))
sulivan_data<-data.frame((id2[15]),(mean(sulivan$prcp,na.rm=TRUE)),(mean(sulivan$tmax,na.rm=TRUE)),
(mean(sulivan$tmin,na.rm=TRUE)),(mean(sulivan$tavg,na.rm=TRUE)))
rushville_data<-data.frame((id2[16]),(mean(rushville$prcp,na.rm=TRUE)),(mean(rushville$tmax,na.rm=TRUE)),
(mean(rushville$tmin,na.rm=TRUE)),(mean(rushville$tavg,na.rm=TRUE)))
rockville_data<-data.frame((id2[17]),(mean(rockville$prcp,na.rm=TRUE)),(mean(rockville$tmax,na.rm=TRUE)),
(mean(rockville$tmin,na.rm=TRUE)),(mean(rockville$tavg,na.rm=TRUE)))
mountvernon_data<-data.frame((id2[18]),(mean(mountvernon$prcp,na.rm=TRUE)),(mean(mountvernon$tmax,na.rm=TRUE)),
(mean(mountvernon$tmin,na.rm=TRUE)),(mean(mountvernon$tavg,na.rm=TRUE)))


#columnnames
cat(collnames(unique(id2),y="colnames(",z="_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')"),sep="\n")

colnames(bloomingdale_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(warsaw_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(bedford2_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(southport1_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(southport2_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(huntington1_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(evansville_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(allencounty_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(huntington2_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(bedford1_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(bloomington_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(decatur1_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(huntington3_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(richmond_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(sulivan_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(rushville_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(rockville_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(mountvernon_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')

##paINeintobind_rows()
cat(bindrowsfun(unique(id2),y="_data",z=","),sep="")
INtotal_obs<-bind_rows(bloomingdale_data,warsaw_data,bedford2_data,southport1_data,southport2_data,huntington1_data,evansville_data,allencounty_data,huntington2_data,bedford1_data,bloomington_data,decatur1_data,huntington3_data,richmond_data,sulivan_data,rushville_data,rockville_data,mountvernon_data)



###savedataasxlsxfile
write.xlsx(INtotal_obs,file="INtotal_obs.xlsx",sheetName="Sheet1",
col.names=TRUE,row.names=TRUE,append=FALSE)


#######wisconsin########

##totalweatherdata-gofrom1/10adegreetodegree
##thenfindaverageofalldataforthatcity
WItotal<-read.csv("wisconsin_data_total_weather.csv")

##makeprcp,tmax,tmin,andtavgnumeric

WItotal$prcp<-as.numeric(WItotal$prcp)
WItotal$tmax<-as.numeric(WItotal$tmax)
WItotal$tmin<-as.numeric(WItotal$tmin)
WItotal$tavg<-as.numeric(WItotal$tavg)

##multiplyby0.1ordivideby10tomake1degree

WItotal$prcp<-WItotal$prcp/10
WItotal$tmax<-WItotal$tmax/10
WItotal$tmin<-WItotal$tmin/10
WItotal$tavg<-WItotal$tavg/10

#pulldata
id2<-unique(WItotal$id2)
cat(filtering(id2,y="<-filter(WItotal,id2=='",z="')"),sep="\n")

beloit<-filter(WItotal,id2=='beloit')
saukcity2<-filter(WItotal,id2=='saukcity2')
elkhom<-filter(WItotal,id2=='elkhom')
spirit<-filter(WItotal,id2=='spirit')
medford<-filter(WItotal,id2=='medford')
madison7<-filter(WItotal,id2=='madison7')
newlondon2<-filter(WItotal,id2=='newlondon2')
viroqua<-filter(WItotal,id2=='viroqua')
utica<-filter(WItotal,id2=='utica')
dodgeville2<-filter(WItotal,id2=='dodgeville2')
cornell<-filter(WItotal,id2=='cornell')
waukesha<-filter(WItotal,id2=='waukesha')
sturgeonbay<-filter(WItotal,id2=='sturgeonbay')
saukcity1<-filter(WItotal,id2=='saukcity1')
union<-filter(WItotal,id2=='union')
wausau<-filter(WItotal,id2=='wausau')
tworivers<-filter(WItotal,id2=='tworivers')
albany<-filter(WItotal,id2=='albany')
oshkosh<-filter(WItotal,id2=='oshkosh')
waupan<-filter(WItotal,id2=='waupan')
ithica<-filter(WItotal,id2=='ithica')
wentbend<-filter(WItotal,id2=='wentbend')
dodgeville1<-filter(WItotal,id2=='dodgeville1')
riverfalls<-filter(WItotal,id2=='riverfalls')
lacrosse<-filter(WItotal,id2=='lacrosse')
boscobel<-filter(WItotal,id2=='boscobel')
newlondon1<-filter(WItotal,id2=='newlondon1')
madison1<-filter(WItotal,id2=='madison1')
madison2<-filter(WItotal,id2=='madison2')
stevespoint<-filter(WItotal,id2=='stevespoint')
jefferson<-filter(WItotal,id2=='jefferson')
wetboro<-filter(WItotal,id2=='wetboro')
verona<-filter(WItotal,id2=='verona')


###findthemeanforeach
lennumber=(1:33)
df<-list(id2,lennumber)

lapply(df,cat(meandata(x=id2,y="_data<-data.frame((id2[",l=lennumber,k="]),(mean(",e="$prcp,na.rm=TRUE)),(mean(",f="$tmax,na.rm=TRUE)),
(mean(",g="$tmin,na.rm=TRUE)),(mean(",z="$tavg,na.rm=TRUE)))"),sep="\n"))

beloit_data<-data.frame((id2[1]),(mean(beloit$prcp,na.rm=TRUE)),(mean(beloit$tmax,na.rm=TRUE)),
(mean(beloit$tmin,na.rm=TRUE)),(mean(beloit$tavg,na.rm=TRUE)))
saukcity2_data<-data.frame((id2[2]),(mean(saukcity2$prcp,na.rm=TRUE)),(mean(saukcity2$tmax,na.rm=TRUE)),
(mean(saukcity2$tmin,na.rm=TRUE)),(mean(saukcity2$tavg,na.rm=TRUE)))
elkhom_data<-data.frame((id2[3]),(mean(elkhom$prcp,na.rm=TRUE)),(mean(elkhom$tmax,na.rm=TRUE)),
(mean(elkhom$tmin,na.rm=TRUE)),(mean(elkhom$tavg,na.rm=TRUE)))
spirit_data<-data.frame((id2[4]),(mean(spirit$prcp,na.rm=TRUE)),(mean(spirit$tmax,na.rm=TRUE)),
(mean(spirit$tmin,na.rm=TRUE)),(mean(spirit$tavg,na.rm=TRUE)))
medford_data<-data.frame((id2[5]),(mean(medford$prcp,na.rm=TRUE)),(mean(medford$tmax,na.rm=TRUE)),
(mean(medford$tmin,na.rm=TRUE)),(mean(medford$tavg,na.rm=TRUE)))
madison7_data<-data.frame((id2[6]),(mean(madison7$prcp,na.rm=TRUE)),(mean(madison7$tmax,na.rm=TRUE)),
(mean(madison7$tmin,na.rm=TRUE)),(mean(madison7$tavg,na.rm=TRUE)))
newlondon2_data<-data.frame((id2[7]),(mean(newlondon2$prcp,na.rm=TRUE)),(mean(newlondon2$tmax,na.rm=TRUE)),
(mean(newlondon2$tmin,na.rm=TRUE)),(mean(newlondon2$tavg,na.rm=TRUE)))
viroqua_data<-data.frame((id2[8]),(mean(viroqua$prcp,na.rm=TRUE)),(mean(viroqua$tmax,na.rm=TRUE)),
(mean(viroqua$tmin,na.rm=TRUE)),(mean(viroqua$tavg,na.rm=TRUE)))
utica_data<-data.frame((id2[9]),(mean(utica$prcp,na.rm=TRUE)),(mean(utica$tmax,na.rm=TRUE)),
(mean(utica$tmin,na.rm=TRUE)),(mean(utica$tavg,na.rm=TRUE)))
dodgeville2_data<-data.frame((id2[10]),(mean(dodgeville2$prcp,na.rm=TRUE)),(mean(dodgeville2$tmax,na.rm=TRUE)),
(mean(dodgeville2$tmin,na.rm=TRUE)),(mean(dodgeville2$tavg,na.rm=TRUE)))
cornell_data<-data.frame((id2[11]),(mean(cornell$prcp,na.rm=TRUE)),(mean(cornell$tmax,na.rm=TRUE)),
(mean(cornell$tmin,na.rm=TRUE)),(mean(cornell$tavg,na.rm=TRUE)))
waukesha_data<-data.frame((id2[12]),(mean(waukesha$prcp,na.rm=TRUE)),(mean(waukesha$tmax,na.rm=TRUE)),
(mean(waukesha$tmin,na.rm=TRUE)),(mean(waukesha$tavg,na.rm=TRUE)))
sturgeonbay_data<-data.frame((id2[13]),(mean(sturgeonbay$prcp,na.rm=TRUE)),(mean(sturgeonbay$tmax,na.rm=TRUE)),
(mean(sturgeonbay$tmin,na.rm=TRUE)),(mean(sturgeonbay$tavg,na.rm=TRUE)))
saukcity1_data<-data.frame((id2[14]),(mean(saukcity1$prcp,na.rm=TRUE)),(mean(saukcity1$tmax,na.rm=TRUE)),
(mean(saukcity1$tmin,na.rm=TRUE)),(mean(saukcity1$tavg,na.rm=TRUE)))
union_data<-data.frame((id2[15]),(mean(union$prcp,na.rm=TRUE)),(mean(union$tmax,na.rm=TRUE)),
(mean(union$tmin,na.rm=TRUE)),(mean(union$tavg,na.rm=TRUE)))
wausau_data<-data.frame((id2[16]),(mean(wausau$prcp,na.rm=TRUE)),(mean(wausau$tmax,na.rm=TRUE)),
(mean(wausau$tmin,na.rm=TRUE)),(mean(wausau$tavg,na.rm=TRUE)))
tworivers_data<-data.frame((id2[17]),(mean(tworivers$prcp,na.rm=TRUE)),(mean(tworivers$tmax,na.rm=TRUE)),
(mean(tworivers$tmin,na.rm=TRUE)),(mean(tworivers$tavg,na.rm=TRUE)))
albany_data<-data.frame((id2[18]),(mean(albany$prcp,na.rm=TRUE)),(mean(albany$tmax,na.rm=TRUE)),
(mean(albany$tmin,na.rm=TRUE)),(mean(albany$tavg,na.rm=TRUE)))
oshkosh_data<-data.frame((id2[19]),(mean(oshkosh$prcp,na.rm=TRUE)),(mean(oshkosh$tmax,na.rm=TRUE)),
(mean(oshkosh$tmin,na.rm=TRUE)),(mean(oshkosh$tavg,na.rm=TRUE)))
waupan_data<-data.frame((id2[20]),(mean(waupan$prcp,na.rm=TRUE)),(mean(waupan$tmax,na.rm=TRUE)),
(mean(waupan$tmin,na.rm=TRUE)),(mean(waupan$tavg,na.rm=TRUE)))
ithica_data<-data.frame((id2[21]),(mean(ithica$prcp,na.rm=TRUE)),(mean(ithica$tmax,na.rm=TRUE)),
(mean(ithica$tmin,na.rm=TRUE)),(mean(ithica$tavg,na.rm=TRUE)))
wentbend_data<-data.frame((id2[22]),(mean(wentbend$prcp,na.rm=TRUE)),(mean(wentbend$tmax,na.rm=TRUE)),
(mean(wentbend$tmin,na.rm=TRUE)),(mean(wentbend$tavg,na.rm=TRUE)))
dodgeville1_data<-data.frame((id2[23]),(mean(dodgeville1$prcp,na.rm=TRUE)),(mean(dodgeville1$tmax,na.rm=TRUE)),
(mean(dodgeville1$tmin,na.rm=TRUE)),(mean(dodgeville1$tavg,na.rm=TRUE)))
riverfalls_data<-data.frame((id2[24]),(mean(riverfalls$prcp,na.rm=TRUE)),(mean(riverfalls$tmax,na.rm=TRUE)),
(mean(riverfalls$tmin,na.rm=TRUE)),(mean(riverfalls$tavg,na.rm=TRUE)))
lacrosse_data<-data.frame((id2[25]),(mean(lacrosse$prcp,na.rm=TRUE)),(mean(lacrosse$tmax,na.rm=TRUE)),
(mean(lacrosse$tmin,na.rm=TRUE)),(mean(lacrosse$tavg,na.rm=TRUE)))
boscobel_data<-data.frame((id2[26]),(mean(boscobel$prcp,na.rm=TRUE)),(mean(boscobel$tmax,na.rm=TRUE)),
(mean(boscobel$tmin,na.rm=TRUE)),(mean(boscobel$tavg,na.rm=TRUE)))
newlondon1_data<-data.frame((id2[27]),(mean(newlondon1$prcp,na.rm=TRUE)),(mean(newlondon1$tmax,na.rm=TRUE)),
(mean(newlondon1$tmin,na.rm=TRUE)),(mean(newlondon1$tavg,na.rm=TRUE)))
madison1_data<-data.frame((id2[28]),(mean(madison1$prcp,na.rm=TRUE)),(mean(madison1$tmax,na.rm=TRUE)),
(mean(madison1$tmin,na.rm=TRUE)),(mean(madison1$tavg,na.rm=TRUE)))
madison2_data<-data.frame((id2[29]),(mean(madison2$prcp,na.rm=TRUE)),(mean(madison2$tmax,na.rm=TRUE)),
(mean(madison2$tmin,na.rm=TRUE)),(mean(madison2$tavg,na.rm=TRUE)))
stevespoint_data<-data.frame((id2[30]),(mean(stevespoint$prcp,na.rm=TRUE)),(mean(stevespoint$tmax,na.rm=TRUE)),
(mean(stevespoint$tmin,na.rm=TRUE)),(mean(stevespoint$tavg,na.rm=TRUE)))
jefferson_data<-data.frame((id2[31]),(mean(jefferson$prcp,na.rm=TRUE)),(mean(jefferson$tmax,na.rm=TRUE)),
(mean(jefferson$tmin,na.rm=TRUE)),(mean(jefferson$tavg,na.rm=TRUE)))
wetboro_data<-data.frame((id2[32]),(mean(wetboro$prcp,na.rm=TRUE)),(mean(wetboro$tmax,na.rm=TRUE)),
(mean(wetboro$tmin,na.rm=TRUE)),(mean(wetboro$tavg,na.rm=TRUE)))
verona_data<-data.frame((id2[33]),(mean(verona$prcp,na.rm=TRUE)),(mean(verona$tmax,na.rm=TRUE)),
(mean(verona$tmin,na.rm=TRUE)),(mean(verona$tavg,na.rm=TRUE)))


#columnnames
cat(collnames(unique(id2),y="colnames(",z="_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')"),sep="\n")

colnames(beloit_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(saukcity2_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(elkhom_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(spirit_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(medford_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(madison7_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(newlondon2_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(viroqua_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(utica_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(dodgeville2_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(cornell_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(waukesha_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(sturgeonbay_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(saukcity1_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(union_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(wausau_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(tworivers_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(albany_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(oshkosh_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(waupan_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(ithica_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(wentbend_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(dodgeville1_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(riverfalls_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(lacrosse_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(boscobel_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(newlondon1_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(madison1_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(madison2_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(stevespoint_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(jefferson_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(wetboro_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(verona_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')


##pasteintobind_rows()
cat(bindrowsfun(unique(id2),y="_data",z=","),sep="")
WItotal_obs<-bind_rows(beloit_data,saukcity2_data,elkhom_data,spirit_data,medford_data,madison7_data,newlondon2_data,viroqua_data,utica_data,dodgeville2_data,cornell_data,waukesha_data,sturgeonbay_data,saukcity1_data,union_data,wausau_data,tworivers_data,albany_data,oshkosh_data,waupan_data,ithica_data,wentbend_data,dodgeville1_data,riverfalls_data,lacrosse_data,boscobel_data,newlondon1_data,madison1_data,madison2_data,stevespoint_data,jefferson_data,wetboro_data,verona_data)



###savedataasxlsxfile
write.xlsx(WItotal_obs,file="WItotal_obs.xlsx",sheetName="Sheet1",
col.names=TRUE,row.names=TRUE,append=FALSE)


#######Virginia#######

##totalweatherdata-gofrom1/10adegreetodegree
##thenfindaverageofalldataforthatcity
VItotal<-read.csv("virginia_data_total_weather.csv")

##makeprcp,tmax,tmin,andtavgnumeric

VItotal$prcp<-as.numeric(VItotal$prcp)
VItotal$tmax<-as.numeric(VItotal$tmax)
VItotal$tmin<-as.numeric(VItotal$tmin)
VItotal$tavg<-as.numeric(VItotal$tavg)

##multiplyby0.1ordivideby10tomake1degree

VItotal$prcp<-VItotal$prcp/10
VItotal$tmax<-VItotal$tmax/10
VItotal$tmin<-VItotal$tmin/10
VItotal$tavg<-VItotal$tavg/10

#pulldata
id2<-unique(VItotal$id2)
cat(filtering(id2,y="<-filter(VItotal,id2=='",z="')"),sep="\n")

hopewell2<-filter(VItotal,id2=='hopewell2')
richmond7<-filter(VItotal,id2=='richmond7')
richmond8<-filter(VItotal,id2=='richmond8')
richmond7.5<-filter(VItotal,id2=='richmond7.5')
richmond5<-filter(VItotal,id2=='richmond5')
middlesexcounty<-filter(VItotal,id2=='middlesexcounty')
richmond6<-filter(VItotal,id2=='richmond6')
exmore<-filter(VItotal,id2=='exmore')
pocomokecity<-filter(VItotal,id2=='pocomokecity')
williamsburg4<-filter(VItotal,id2=='williamsburg4')
franlkin<-filter(VItotal,id2=='franlkin')
alexandria1<-filter(VItotal,id2=='alexandria1')
richmond3<-filter(VItotal,id2=='richmond3')
norfolk8<-filter(VItotal,id2=='norfolk8')
virginiabeach2<-filter(VItotal,id2=='virginiabeach2')
williamsburg2<-filter(VItotal,id2=='williamsburg2')
norfolk3<-filter(VItotal,id2=='norfolk3')
fredericksburg1<-filter(VItotal,id2=='fredericksburg1')
leesburg<-filter(VItotal,id2=='leesburg')
norfolk4<-filter(VItotal,id2=='norfolk4')
norfolk5<-filter(VItotal,id2=='norfolk5')
richmond1<-filter(VItotal,id2=='richmond1')
richmond14<-filter(VItotal,id2=='richmond14')
reston<-filter(VItotal,id2=='reston')
charoltesville1<-filter(VItotal,id2=='charoltesville1')
richmond13<-filter(VItotal,id2=='richmond13')
charoltesville3<-filter(VItotal,id2=='charoltesville3')
winchester3<-filter(VItotal,id2=='winchester3')
alexandriacity<-filter(VItotal,id2=='alexandriacity')
richmond12<-filter(VItotal,id2=='richmond12')
norfolk1<-filter(VItotal,id2=='norfolk1')
norfolk2<-filter(VItotal,id2=='norfolk2')
williamsburg3<-filter(VItotal,id2=='williamsburg3')
richmond2<-filter(VItotal,id2=='richmond2')
harrisonburg9<-filter(VItotal,id2=='harrisonburg9')
harrisonburg6<-filter(VItotal,id2=='harrisonburg6')
virginiabeach1<-filter(VItotal,id2=='virginiabeach1')
norfolk6<-filter(VItotal,id2=='norfolk6')
richmond4<-filter(VItotal,id2=='richmond4')
richmond11<-filter(VItotal,id2=='richmond11')
hopewell1<-filter(VItotal,id2=='hopewell1')
fredericksburg3<-filter(VItotal,id2=='fredericksburg3')
charoltesville2<-filter(VItotal,id2=='charoltesville2')
radford2<-filter(VItotal,id2=='radford2')
staunton<-filter(VItotal,id2=='staunton')
emporia<-filter(VItotal,id2=='emporia')
suffolkcity1<-filter(VItotal,id2=='suffolkcity1')
newportnews<-filter(VItotal,id2=='newportnews')
lynchburg2<-filter(VItotal,id2=='lynchburg2')
richmond15<-filter(VItotal,id2=='richmond15')
roanokecounty<-filter(VItotal,id2=='roanokecounty')
harrisonburg4<-filter(VItotal,id2=='harrisonburg4')
winchester2<-filter(VItotal,id2=='winchester2')
radford1<-filter(VItotal,id2=='radford1')
harrisonburg2<-filter(VItotal,id2=='harrisonburg2')
lynchburg1<-filter(VItotal,id2=='lynchburg1')
lynchburg3<-filter(VItotal,id2=='lynchburg3')
harrisonburg5<-filter(VItotal,id2=='harrisonburg5')
harrisonburg8<-filter(VItotal,id2=='harrisonburg8')
harrisonburg3<-filter(VItotal,id2=='harrisonburg3')
fredericksburg2<-filter(VItotal,id2=='fredericksburg2')
williamsburg<-filter(VItotal,id2=='williamsburg')
harrisonburg1<-filter(VItotal,id2=='harrisonburg1')
harrisonburg7<-filter(VItotal,id2=='harrisonburg7')
norfolk7<-filter(VItotal,id2=='norfolk7')
suffolkcity2<-filter(VItotal,id2=='suffolkcity2')
accomac<-filter(VItotal,id2=='accomac')
lynchburg4<-filter(VItotal,id2=='lynchburg4')
winchester4<-filter(VItotal,id2=='winchester4')


###findthemeanforeach
lennumber=(1:69)
df<-list(id2,lennumber)

lapply(df,cat(meandata(x=id2,y="_data<-data.frame((id2[",l=lennumber,k="]),(mean(",e="$prcp,na.rm=TRUE)),(mean(",f="$tmax,na.rm=TRUE)),
(mean(",g="$tmin,na.rm=TRUE)),(mean(",z="$tavg,na.rm=TRUE)))"),sep="\n"))

hopewell2_data<-data.frame((id2[1]),(mean(hopewell2$prcp,na.rm=TRUE)),(mean(hopewell2$tmax,na.rm=TRUE)),
(mean(hopewell2$tmin,na.rm=TRUE)),(mean(hopewell2$tavg,na.rm=TRUE)))
richmond7_data<-data.frame((id2[2]),(mean(richmond7$prcp,na.rm=TRUE)),(mean(richmond7$tmax,na.rm=TRUE)),
(mean(richmond7$tmin,na.rm=TRUE)),(mean(richmond7$tavg,na.rm=TRUE)))
richmond8_data<-data.frame((id2[3]),(mean(richmond8$prcp,na.rm=TRUE)),(mean(richmond8$tmax,na.rm=TRUE)),
(mean(richmond8$tmin,na.rm=TRUE)),(mean(richmond8$tavg,na.rm=TRUE)))
richmond7.5_data<-data.frame((id2[4]),(mean(richmond7.5$prcp,na.rm=TRUE)),(mean(richmond7.5$tmax,na.rm=TRUE)),
(mean(richmond7.5$tmin,na.rm=TRUE)),(mean(richmond7.5$tavg,na.rm=TRUE)))
richmond5_data<-data.frame((id2[5]),(mean(richmond5$prcp,na.rm=TRUE)),(mean(richmond5$tmax,na.rm=TRUE)),
(mean(richmond5$tmin,na.rm=TRUE)),(mean(richmond5$tavg,na.rm=TRUE)))
middlesexcounty_data<-data.frame((id2[6]),(mean(middlesexcounty$prcp,na.rm=TRUE)),(mean(middlesexcounty$tmax,na.rm=TRUE)),
(mean(middlesexcounty$tmin,na.rm=TRUE)),(mean(middlesexcounty$tavg,na.rm=TRUE)))
richmond6_data<-data.frame((id2[7]),(mean(richmond6$prcp,na.rm=TRUE)),(mean(richmond6$tmax,na.rm=TRUE)),
(mean(richmond6$tmin,na.rm=TRUE)),(mean(richmond6$tavg,na.rm=TRUE)))
exmore_data<-data.frame((id2[8]),(mean(exmore$prcp,na.rm=TRUE)),(mean(exmore$tmax,na.rm=TRUE)),
(mean(exmore$tmin,na.rm=TRUE)),(mean(exmore$tavg,na.rm=TRUE)))
pocomokecity_data<-data.frame((id2[9]),(mean(pocomokecity$prcp,na.rm=TRUE)),(mean(pocomokecity$tmax,na.rm=TRUE)),
(mean(pocomokecity$tmin,na.rm=TRUE)),(mean(pocomokecity$tavg,na.rm=TRUE)))
williamsburg4_data<-data.frame((id2[10]),(mean(williamsburg4$prcp,na.rm=TRUE)),(mean(williamsburg4$tmax,na.rm=TRUE)),
(mean(williamsburg4$tmin,na.rm=TRUE)),(mean(williamsburg4$tavg,na.rm=TRUE)))
franlkin_data<-data.frame((id2[11]),(mean(franlkin$prcp,na.rm=TRUE)),(mean(franlkin$tmax,na.rm=TRUE)),
(mean(franlkin$tmin,na.rm=TRUE)),(mean(franlkin$tavg,na.rm=TRUE)))
alexandria1_data<-data.frame((id2[12]),(mean(alexandria1$prcp,na.rm=TRUE)),(mean(alexandria1$tmax,na.rm=TRUE)),
(mean(alexandria1$tmin,na.rm=TRUE)),(mean(alexandria1$tavg,na.rm=TRUE)))
richmond3_data<-data.frame((id2[13]),(mean(richmond3$prcp,na.rm=TRUE)),(mean(richmond3$tmax,na.rm=TRUE)),
(mean(richmond3$tmin,na.rm=TRUE)),(mean(richmond3$tavg,na.rm=TRUE)))
norfolk8_data<-data.frame((id2[14]),(mean(norfolk8$prcp,na.rm=TRUE)),(mean(norfolk8$tmax,na.rm=TRUE)),
(mean(norfolk8$tmin,na.rm=TRUE)),(mean(norfolk8$tavg,na.rm=TRUE)))
virginiabeach2_data<-data.frame((id2[15]),(mean(virginiabeach2$prcp,na.rm=TRUE)),(mean(virginiabeach2$tmax,na.rm=TRUE)),
(mean(virginiabeach2$tmin,na.rm=TRUE)),(mean(virginiabeach2$tavg,na.rm=TRUE)))
williamsburg2_data<-data.frame((id2[16]),(mean(williamsburg2$prcp,na.rm=TRUE)),(mean(williamsburg2$tmax,na.rm=TRUE)),
(mean(williamsburg2$tmin,na.rm=TRUE)),(mean(williamsburg2$tavg,na.rm=TRUE)))
norfolk3_data<-data.frame((id2[17]),(mean(norfolk3$prcp,na.rm=TRUE)),(mean(norfolk3$tmax,na.rm=TRUE)),
(mean(norfolk3$tmin,na.rm=TRUE)),(mean(norfolk3$tavg,na.rm=TRUE)))
fredericksburg1_data<-data.frame((id2[18]),(mean(fredericksburg1$prcp,na.rm=TRUE)),(mean(fredericksburg1$tmax,na.rm=TRUE)),
(mean(fredericksburg1$tmin,na.rm=TRUE)),(mean(fredericksburg1$tavg,na.rm=TRUE)))
leesburg_data<-data.frame((id2[19]),(mean(leesburg$prcp,na.rm=TRUE)),(mean(leesburg$tmax,na.rm=TRUE)),
(mean(leesburg$tmin,na.rm=TRUE)),(mean(leesburg$tavg,na.rm=TRUE)))
norfolk4_data<-data.frame((id2[20]),(mean(norfolk4$prcp,na.rm=TRUE)),(mean(norfolk4$tmax,na.rm=TRUE)),
(mean(norfolk4$tmin,na.rm=TRUE)),(mean(norfolk4$tavg,na.rm=TRUE)))
norfolk5_data<-data.frame((id2[21]),(mean(norfolk5$prcp,na.rm=TRUE)),(mean(norfolk5$tmax,na.rm=TRUE)),
(mean(norfolk5$tmin,na.rm=TRUE)),(mean(norfolk5$tavg,na.rm=TRUE)))
richmond1_data<-data.frame((id2[22]),(mean(richmond1$prcp,na.rm=TRUE)),(mean(richmond1$tmax,na.rm=TRUE)),
(mean(richmond1$tmin,na.rm=TRUE)),(mean(richmond1$tavg,na.rm=TRUE)))
richmond14_data<-data.frame((id2[23]),(mean(richmond14$prcp,na.rm=TRUE)),(mean(richmond14$tmax,na.rm=TRUE)),
(mean(richmond14$tmin,na.rm=TRUE)),(mean(richmond14$tavg,na.rm=TRUE)))
reston_data<-data.frame((id2[24]),(mean(reston$prcp,na.rm=TRUE)),(mean(reston$tmax,na.rm=TRUE)),
(mean(reston$tmin,na.rm=TRUE)),(mean(reston$tavg,na.rm=TRUE)))
charoltesville1_data<-data.frame((id2[25]),(mean(charoltesville1$prcp,na.rm=TRUE)),(mean(charoltesville1$tmax,na.rm=TRUE)),
(mean(charoltesville1$tmin,na.rm=TRUE)),(mean(charoltesville1$tavg,na.rm=TRUE)))
richmond13_data<-data.frame((id2[26]),(mean(richmond13$prcp,na.rm=TRUE)),(mean(richmond13$tmax,na.rm=TRUE)),
(mean(richmond13$tmin,na.rm=TRUE)),(mean(richmond13$tavg,na.rm=TRUE)))
charoltesville3_data<-data.frame((id2[27]),(mean(charoltesville3$prcp,na.rm=TRUE)),(mean(charoltesville3$tmax,na.rm=TRUE)),
(mean(charoltesville3$tmin,na.rm=TRUE)),(mean(charoltesville3$tavg,na.rm=TRUE)))
winchester3_data<-data.frame((id2[28]),(mean(winchester3$prcp,na.rm=TRUE)),(mean(winchester3$tmax,na.rm=TRUE)),
(mean(winchester3$tmin,na.rm=TRUE)),(mean(winchester3$tavg,na.rm=TRUE)))
alexandriacity_data<-data.frame((id2[29]),(mean(alexandriacity$prcp,na.rm=TRUE)),(mean(alexandriacity$tmax,na.rm=TRUE)),
(mean(alexandriacity$tmin,na.rm=TRUE)),(mean(alexandriacity$tavg,na.rm=TRUE)))
richmond12_data<-data.frame((id2[30]),(mean(richmond12$prcp,na.rm=TRUE)),(mean(richmond12$tmax,na.rm=TRUE)),
(mean(richmond12$tmin,na.rm=TRUE)),(mean(richmond12$tavg,na.rm=TRUE)))
norfolk1_data<-data.frame((id2[31]),(mean(norfolk1$prcp,na.rm=TRUE)),(mean(norfolk1$tmax,na.rm=TRUE)),
(mean(norfolk1$tmin,na.rm=TRUE)),(mean(norfolk1$tavg,na.rm=TRUE)))
norfolk2_data<-data.frame((id2[32]),(mean(norfolk2$prcp,na.rm=TRUE)),(mean(norfolk2$tmax,na.rm=TRUE)),
(mean(norfolk2$tmin,na.rm=TRUE)),(mean(norfolk2$tavg,na.rm=TRUE)))
williamsburg3_data<-data.frame((id2[33]),(mean(williamsburg3$prcp,na.rm=TRUE)),(mean(williamsburg3$tmax,na.rm=TRUE)),
(mean(williamsburg3$tmin,na.rm=TRUE)),(mean(williamsburg3$tavg,na.rm=TRUE)))
richmond2_data<-data.frame((id2[34]),(mean(richmond2$prcp,na.rm=TRUE)),(mean(richmond2$tmax,na.rm=TRUE)),
(mean(richmond2$tmin,na.rm=TRUE)),(mean(richmond2$tavg,na.rm=TRUE)))
harrisonburg9_data<-data.frame((id2[35]),(mean(harrisonburg9$prcp,na.rm=TRUE)),(mean(harrisonburg9$tmax,na.rm=TRUE)),
(mean(harrisonburg9$tmin,na.rm=TRUE)),(mean(harrisonburg9$tavg,na.rm=TRUE)))
harrisonburg6_data<-data.frame((id2[36]),(mean(harrisonburg6$prcp,na.rm=TRUE)),(mean(harrisonburg6$tmax,na.rm=TRUE)),
(mean(harrisonburg6$tmin,na.rm=TRUE)),(mean(harrisonburg6$tavg,na.rm=TRUE)))
virginiabeach1_data<-data.frame((id2[37]),(mean(virginiabeach1$prcp,na.rm=TRUE)),(mean(virginiabeach1$tmax,na.rm=TRUE)),
(mean(virginiabeach1$tmin,na.rm=TRUE)),(mean(virginiabeach1$tavg,na.rm=TRUE)))
norfolk6_data<-data.frame((id2[38]),(mean(norfolk6$prcp,na.rm=TRUE)),(mean(norfolk6$tmax,na.rm=TRUE)),
(mean(norfolk6$tmin,na.rm=TRUE)),(mean(norfolk6$tavg,na.rm=TRUE)))
richmond4_data<-data.frame((id2[39]),(mean(richmond4$prcp,na.rm=TRUE)),(mean(richmond4$tmax,na.rm=TRUE)),
(mean(richmond4$tmin,na.rm=TRUE)),(mean(richmond4$tavg,na.rm=TRUE)))
richmond11_data<-data.frame((id2[40]),(mean(richmond11$prcp,na.rm=TRUE)),(mean(richmond11$tmax,na.rm=TRUE)),
(mean(richmond11$tmin,na.rm=TRUE)),(mean(richmond11$tavg,na.rm=TRUE)))
hopewell1_data<-data.frame((id2[41]),(mean(hopewell1$prcp,na.rm=TRUE)),(mean(hopewell1$tmax,na.rm=TRUE)),
(mean(hopewell1$tmin,na.rm=TRUE)),(mean(hopewell1$tavg,na.rm=TRUE)))
fredericksburg3_data<-data.frame((id2[42]),(mean(fredericksburg3$prcp,na.rm=TRUE)),(mean(fredericksburg3$tmax,na.rm=TRUE)),
(mean(fredericksburg3$tmin,na.rm=TRUE)),(mean(fredericksburg3$tavg,na.rm=TRUE)))
charoltesville2_data<-data.frame((id2[43]),(mean(charoltesville2$prcp,na.rm=TRUE)),(mean(charoltesville2$tmax,na.rm=TRUE)),
(mean(charoltesville2$tmin,na.rm=TRUE)),(mean(charoltesville2$tavg,na.rm=TRUE)))
radford2_data<-data.frame((id2[44]),(mean(radford2$prcp,na.rm=TRUE)),(mean(radford2$tmax,na.rm=TRUE)),
(mean(radford2$tmin,na.rm=TRUE)),(mean(radford2$tavg,na.rm=TRUE)))
staunton_data<-data.frame((id2[45]),(mean(staunton$prcp,na.rm=TRUE)),(mean(staunton$tmax,na.rm=TRUE)),
(mean(staunton$tmin,na.rm=TRUE)),(mean(staunton$tavg,na.rm=TRUE)))
emporia_data<-data.frame((id2[46]),(mean(emporia$prcp,na.rm=TRUE)),(mean(emporia$tmax,na.rm=TRUE)),
(mean(emporia$tmin,na.rm=TRUE)),(mean(emporia$tavg,na.rm=TRUE)))
suffolkcity1_data<-data.frame((id2[47]),(mean(suffolkcity1$prcp,na.rm=TRUE)),(mean(suffolkcity1$tmax,na.rm=TRUE)),
(mean(suffolkcity1$tmin,na.rm=TRUE)),(mean(suffolkcity1$tavg,na.rm=TRUE)))
newportnews_data<-data.frame((id2[48]),(mean(newportnews$prcp,na.rm=TRUE)),(mean(newportnews$tmax,na.rm=TRUE)),
(mean(newportnews$tmin,na.rm=TRUE)),(mean(newportnews$tavg,na.rm=TRUE)))
lynchburg2_data<-data.frame((id2[49]),(mean(lynchburg2$prcp,na.rm=TRUE)),(mean(lynchburg2$tmax,na.rm=TRUE)),
(mean(lynchburg2$tmin,na.rm=TRUE)),(mean(lynchburg2$tavg,na.rm=TRUE)))
richmond15_data<-data.frame((id2[50]),(mean(richmond15$prcp,na.rm=TRUE)),(mean(richmond15$tmax,na.rm=TRUE)),
(mean(richmond15$tmin,na.rm=TRUE)),(mean(richmond15$tavg,na.rm=TRUE)))
roanokecounty_data<-data.frame((id2[51]),(mean(roanokecounty$prcp,na.rm=TRUE)),(mean(roanokecounty$tmax,na.rm=TRUE)),
(mean(roanokecounty$tmin,na.rm=TRUE)),(mean(roanokecounty$tavg,na.rm=TRUE)))
harrisonburg4_data<-data.frame((id2[52]),(mean(harrisonburg4$prcp,na.rm=TRUE)),(mean(harrisonburg4$tmax,na.rm=TRUE)),
(mean(harrisonburg4$tmin,na.rm=TRUE)),(mean(harrisonburg4$tavg,na.rm=TRUE)))
winchester2_data<-data.frame((id2[53]),(mean(winchester2$prcp,na.rm=TRUE)),(mean(winchester2$tmax,na.rm=TRUE)),
(mean(winchester2$tmin,na.rm=TRUE)),(mean(winchester2$tavg,na.rm=TRUE)))
radford1_data<-data.frame((id2[54]),(mean(radford1$prcp,na.rm=TRUE)),(mean(radford1$tmax,na.rm=TRUE)),
(mean(radford1$tmin,na.rm=TRUE)),(mean(radford1$tavg,na.rm=TRUE)))
harrisonburg2_data<-data.frame((id2[55]),(mean(harrisonburg2$prcp,na.rm=TRUE)),(mean(harrisonburg2$tmax,na.rm=TRUE)),
(mean(harrisonburg2$tmin,na.rm=TRUE)),(mean(harrisonburg2$tavg,na.rm=TRUE)))
lynchburg1_data<-data.frame((id2[56]),(mean(lynchburg1$prcp,na.rm=TRUE)),(mean(lynchburg1$tmax,na.rm=TRUE)),
(mean(lynchburg1$tmin,na.rm=TRUE)),(mean(lynchburg1$tavg,na.rm=TRUE)))
lynchburg3_data<-data.frame((id2[57]),(mean(lynchburg3$prcp,na.rm=TRUE)),(mean(lynchburg3$tmax,na.rm=TRUE)),
(mean(lynchburg3$tmin,na.rm=TRUE)),(mean(lynchburg3$tavg,na.rm=TRUE)))
harrisonburg5_data<-data.frame((id2[58]),(mean(harrisonburg5$prcp,na.rm=TRUE)),(mean(harrisonburg5$tmax,na.rm=TRUE)),
(mean(harrisonburg5$tmin,na.rm=TRUE)),(mean(harrisonburg5$tavg,na.rm=TRUE)))
harrisonburg8_data<-data.frame((id2[59]),(mean(harrisonburg8$prcp,na.rm=TRUE)),(mean(harrisonburg8$tmax,na.rm=TRUE)),
(mean(harrisonburg8$tmin,na.rm=TRUE)),(mean(harrisonburg8$tavg,na.rm=TRUE)))
harrisonburg3_data<-data.frame((id2[60]),(mean(harrisonburg3$prcp,na.rm=TRUE)),(mean(harrisonburg3$tmax,na.rm=TRUE)),
(mean(harrisonburg3$tmin,na.rm=TRUE)),(mean(harrisonburg3$tavg,na.rm=TRUE)))
fredericksburg2_data<-data.frame((id2[61]),(mean(fredericksburg2$prcp,na.rm=TRUE)),(mean(fredericksburg2$tmax,na.rm=TRUE)),
(mean(fredericksburg2$tmin,na.rm=TRUE)),(mean(fredericksburg2$tavg,na.rm=TRUE)))
williamsburg_data<-data.frame((id2[62]),(mean(williamsburg$prcp,na.rm=TRUE)),(mean(williamsburg$tmax,na.rm=TRUE)),
(mean(williamsburg$tmin,na.rm=TRUE)),(mean(williamsburg$tavg,na.rm=TRUE)))
harrisonburg1_data<-data.frame((id2[63]),(mean(harrisonburg1$prcp,na.rm=TRUE)),(mean(harrisonburg1$tmax,na.rm=TRUE)),
(mean(harrisonburg1$tmin,na.rm=TRUE)),(mean(harrisonburg1$tavg,na.rm=TRUE)))
harrisonburg7_data<-data.frame((id2[64]),(mean(harrisonburg7$prcp,na.rm=TRUE)),(mean(harrisonburg7$tmax,na.rm=TRUE)),
(mean(harrisonburg7$tmin,na.rm=TRUE)),(mean(harrisonburg7$tavg,na.rm=TRUE)))
norfolk7_data<-data.frame((id2[65]),(mean(norfolk7$prcp,na.rm=TRUE)),(mean(norfolk7$tmax,na.rm=TRUE)),
(mean(norfolk7$tmin,na.rm=TRUE)),(mean(norfolk7$tavg,na.rm=TRUE)))
suffolkcity2_data<-data.frame((id2[66]),(mean(suffolkcity2$prcp,na.rm=TRUE)),(mean(suffolkcity2$tmax,na.rm=TRUE)),
(mean(suffolkcity2$tmin,na.rm=TRUE)),(mean(suffolkcity2$tavg,na.rm=TRUE)))
accomac_data<-data.frame((id2[67]),(mean(accomac$prcp,na.rm=TRUE)),(mean(accomac$tmax,na.rm=TRUE)),
(mean(accomac$tmin,na.rm=TRUE)),(mean(accomac$tavg,na.rm=TRUE)))
lynchburg4_data<-data.frame((id2[68]),(mean(lynchburg4$prcp,na.rm=TRUE)),(mean(lynchburg4$tmax,na.rm=TRUE)),
(mean(lynchburg4$tmin,na.rm=TRUE)),(mean(lynchburg4$tavg,na.rm=TRUE)))
winchester4_data<-data.frame((id2[69]),(mean(winchester4$prcp,na.rm=TRUE)),(mean(winchester4$tmax,na.rm=TRUE)),
(mean(winchester4$tmin,na.rm=TRUE)),(mean(winchester4$tavg,na.rm=TRUE)))


#columnnames
cat(collnames(unique(id2),y="colnames(",z="_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')"),sep="\n")

colnames(hopewell2_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(richmond7_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(richmond8_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(richmond7.5_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(richmond5_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(middlesexcounty_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(richmond6_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(exmore_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(pocomokecity_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(williamsburg4_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(franlkin_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(alexandria1_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(richmond3_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(norfolk8_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(virginiabeach2_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(williamsburg2_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(norfolk3_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(fredericksburg1_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(leesburg_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(norfolk4_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(norfolk5_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(richmond1_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(richmond14_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(reston_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(charoltesville1_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(richmond13_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(charoltesville3_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(winchester3_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(alexandriacity_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(richmond12_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(norfolk1_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(norfolk2_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(williamsburg3_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(richmond2_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(harrisonburg9_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(harrisonburg6_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(virginiabeach1_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(norfolk6_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(richmond4_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(richmond11_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(hopewell1_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(fredericksburg3_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(charoltesville2_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(radford2_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(staunton_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(emporia_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(suffolkcity1_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(newportnews_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(lynchburg2_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(richmond15_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(roanokecounty_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(harrisonburg4_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(winchester2_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(radford1_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(harrisonburg2_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(lynchburg1_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(lynchburg3_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(harrisonburg5_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(harrisonburg8_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(harrisonburg3_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(fredericksburg2_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(williamsburg_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(harrisonburg1_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(harrisonburg7_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(norfolk7_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(suffolkcity2_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(accomac_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(lynchburg4_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(winchester4_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')

##pasteintobind_rows()
cat(bindrowsfun(unique(id2),y="_data",z=","),sep="")
VItotal_obs<-bind_rows(hopewell2_data,richmond7_data,richmond8_data,richmond7.5_data,richmond5_data,middlesexcounty_data,richmond6_data,exmore_data,pocomokecity_data,williamsburg4_data,franlkin_data,alexandria1_data,richmond3_data,norfolk8_data,virginiabeach2_data,williamsburg2_data,norfolk3_data,fredericksburg1_data,leesburg_data,norfolk4_data,norfolk5_data,richmond1_data,richmond14_data,reston_data,charoltesville1_data,richmond13_data,charoltesville3_data,winchester3_data,alexandriacity_data,richmond12_data,norfolk1_data,norfolk2_data,williamsburg3_data,richmond2_data,harrisonburg9_data,harrisonburg6_data,virginiabeach1_data,norfolk6_data,richmond4_data,richmond11_data,hopewell1_data,fredericksburg3_data,charoltesville2_data,radford2_data,staunton_data,emporia_data,suffolkcity1_data,newportnews_data,lynchburg2_data,richmond15_data,roanokecounty_data,harrisonburg4_data,winchester2_data,radford1_data,harrisonburg2_data,lynchburg1_data,lynchburg3_data,harrisonburg5_data,harrisonburg8_data,harrisonburg3_data,fredericksburg2_data,williamsburg_data,harrisonburg1_data,harrisonburg7_data,norfolk7_data,suffolkcity2_data,accomac_data,lynchburg4_data,winchester4_data)



###savedataasxlsxfile
write.xlsx(VItotal_obs,file="VItotal_obs.xlsx",sheetName="Sheet1",
col.names=TRUE,row.names=TRUE,append=FALSE)

#########Texas#########

##totalweatherdata-gofrom1/10adegreetodegree
##thenfindaverageofalldataforthatcity
TXtotal<-read.csv("texas_data_total_weather.csv")

##makeprcp,tmax,tmin,andtavgnumeric

TXtotal$prcp<-as.numeric(TXtotal$prcp)
TXtotal$tmax<-as.numeric(TXtotal$tmax)
TXtotal$tmin<-as.numeric(TXtotal$tmin)
TXtotal$tavg<-as.numeric(TXtotal$tavg)

##multiplyby0.1ordivideby10tomake1degree

TXtotal$prcp<-TXtotal$prcp/10
TXtotal$tmax<-TXtotal$tmax/10
TXtotal$tmin<-TXtotal$tmin/10
TXtotal$tavg<-TXtotal$tavg/10

#pulldata
id2<-unique(TXtotal$id2)
cat(filtering(id2,y="<-filter(TXtotal,id2=='",z="')"),sep="\n")

FortWorth1<-filter(TXtotal,id2=='FortWorth1')
SanAntonio1<-filter(TXtotal,id2=='SanAntonio1')
Gunter1<-filter(TXtotal,id2=='Gunter1')
Burkburnett<-filter(TXtotal,id2=='Burkburnett')
Dallas6<-filter(TXtotal,id2=='Dallas6')
Abilene5<-filter(TXtotal,id2=='Abilene5')
Rockwall<-filter(TXtotal,id2=='Rockwall')
Grapevine<-filter(TXtotal,id2=='Grapevine')
Hamilton<-filter(TXtotal,id2=='Hamilton')
Alpine2<-filter(TXtotal,id2=='Alpine2')
Austin7<-filter(TXtotal,id2=='Austin7')
Austin3<-filter(TXtotal,id2=='Austin3')
Austin2<-filter(TXtotal,id2=='Austin2')
Ganado<-filter(TXtotal,id2=='Ganado')
Austin6<-filter(TXtotal,id2=='Austin6')
Austin5<-filter(TXtotal,id2=='Austin5')
Houston1<-filter(TXtotal,id2=='Houston1')
Vernon<-filter(TXtotal,id2=='Vernon')
Saginaw<-filter(TXtotal,id2=='Saginaw')
Abilene2<-filter(TXtotal,id2=='Abilene2')
Brownwood<-filter(TXtotal,id2=='Brownwood')
Terrell<-filter(TXtotal,id2=='Terrell')
Abilene1<-filter(TXtotal,id2=='Abilene1')
Hondo1<-filter(TXtotal,id2=='Hondo1')
Denton<-filter(TXtotal,id2=='Denton')
Arlington1<-filter(TXtotal,id2=='Arlington1')
Arlington3<-filter(TXtotal,id2=='Arlington3')
Waxahachie<-filter(TXtotal,id2=='Waxahachie')
Arlington5<-filter(TXtotal,id2=='Arlington5')
Dallas1<-filter(TXtotal,id2=='Dallas1')
Dallas3<-filter(TXtotal,id2=='Dallas3')
FortWorth2<-filter(TXtotal,id2=='FortWorth2')
FortWorth11<-filter(TXtotal,id2=='FortWorth11')
FortWorth5<-filter(TXtotal,id2=='FortWorth5')
Arlington2<-filter(TXtotal,id2=='Arlington2')
Austin1<-filter(TXtotal,id2=='Austin1')
CollegeStation1<-filter(TXtotal,id2=='CollegeStation1')
CollegeStation4<-filter(TXtotal,id2=='CollegeStation4')
CollegeStation2<-filter(TXtotal,id2=='CollegeStation2')
Bryan3<-filter(TXtotal,id2=='Bryan3')
Bryan1<-filter(TXtotal,id2=='Bryan1')
Voca<-filter(TXtotal,id2=='Voca')
Fannin<-filter(TXtotal,id2=='Fannin')
FortWorth12<-filter(TXtotal,id2=='FortWorth12')
Waco1<-filter(TXtotal,id2=='Waco1')
Franklin2<-filter(TXtotal,id2=='Franklin2')
Franklin1<-filter(TXtotal,id2=='Franklin1')
Waco3<-filter(TXtotal,id2=='Waco3')
FortWorth10<-filter(TXtotal,id2=='FortWorth10')
Waco4<-filter(TXtotal,id2=='Waco4')
Waco5<-filter(TXtotal,id2=='Waco5')
Houston2<-filter(TXtotal,id2=='Houston2')
Abilene4<-filter(TXtotal,id2=='Abilene4')
FortWorth7<-filter(TXtotal,id2=='FortWorth7')
FortWorth4<-filter(TXtotal,id2=='FortWorth4')
Sonora1<-filter(TXtotal,id2=='Sonora1')
Fredericksburg1<-filter(TXtotal,id2=='Fredericksburg1')
Fredericksburg2<-filter(TXtotal,id2=='Fredericksburg2')
FortWorth9<-filter(TXtotal,id2=='FortWorth9')
Coleman1<-filter(TXtotal,id2=='Coleman1')
Mason<-filter(TXtotal,id2=='Mason')
Calallen<-filter(TXtotal,id2=='Calallen')
FortWorth8<-filter(TXtotal,id2=='FortWorth8')
Abilene3<-filter(TXtotal,id2=='Abilene3')
SanAngelo1<-filter(TXtotal,id2=='SanAngelo1')
Santo<-filter(TXtotal,id2=='Santo')
CollegeStation3<-filter(TXtotal,id2=='CollegeStation3')
CollegeStation5<-filter(TXtotal,id2=='CollegeStation5')
Georgetown<-filter(TXtotal,id2=='Georgetown')
SanAngelo2<-filter(TXtotal,id2=='SanAngelo2')
Alpine1<-filter(TXtotal,id2=='Alpine1')
Brady1<-filter(TXtotal,id2=='Brady1')
Eden<-filter(TXtotal,id2=='Eden')
Llano2<-filter(TXtotal,id2=='Llano2')
Llano1<-filter(TXtotal,id2=='Llano1')
Callahan<-filter(TXtotal,id2=='Callahan')


###findthemeanforeach
lennumber=(1:76)
df<-list(id2,lennumber)

lapply(df,cat(meandata(x=id2,y="_data<-data.frame((id2[",l=lennumber,k="]),(mean(",e="$prcp,na.rm=TRUE)),(mean(",f="$tmax,na.rm=TRUE)),
(mean(",g="$tmin,na.rm=TRUE)),(mean(",z="$tavg,na.rm=TRUE)))"),sep="\n"))

FortWorth1_data<-data.frame((id2[1]),(mean(FortWorth1$prcp,na.rm=TRUE)),(mean(FortWorth1$tmax,na.rm=TRUE)),
(mean(FortWorth1$tmin,na.rm=TRUE)),(mean(FortWorth1$tavg,na.rm=TRUE)))
SanAntonio1_data<-data.frame((id2[2]),(mean(SanAntonio1$prcp,na.rm=TRUE)),(mean(SanAntonio1$tmax,na.rm=TRUE)),
(mean(SanAntonio1$tmin,na.rm=TRUE)),(mean(SanAntonio1$tavg,na.rm=TRUE)))
Gunter1_data<-data.frame((id2[3]),(mean(Gunter1$prcp,na.rm=TRUE)),(mean(Gunter1$tmax,na.rm=TRUE)),
(mean(Gunter1$tmin,na.rm=TRUE)),(mean(Gunter1$tavg,na.rm=TRUE)))
Burkburnett_data<-data.frame((id2[4]),(mean(Burkburnett$prcp,na.rm=TRUE)),(mean(Burkburnett$tmax,na.rm=TRUE)),
(mean(Burkburnett$tmin,na.rm=TRUE)),(mean(Burkburnett$tavg,na.rm=TRUE)))
Dallas6_data<-data.frame((id2[5]),(mean(Dallas6$prcp,na.rm=TRUE)),(mean(Dallas6$tmax,na.rm=TRUE)),
(mean(Dallas6$tmin,na.rm=TRUE)),(mean(Dallas6$tavg,na.rm=TRUE)))
Abilene5_data<-data.frame((id2[6]),(mean(Abilene5$prcp,na.rm=TRUE)),(mean(Abilene5$tmax,na.rm=TRUE)),
(mean(Abilene5$tmin,na.rm=TRUE)),(mean(Abilene5$tavg,na.rm=TRUE)))
Rockwall_data<-data.frame((id2[7]),(mean(Rockwall$prcp,na.rm=TRUE)),(mean(Rockwall$tmax,na.rm=TRUE)),
(mean(Rockwall$tmin,na.rm=TRUE)),(mean(Rockwall$tavg,na.rm=TRUE)))
Grapevine_data<-data.frame((id2[8]),(mean(Grapevine$prcp,na.rm=TRUE)),(mean(Grapevine$tmax,na.rm=TRUE)),
(mean(Grapevine$tmin,na.rm=TRUE)),(mean(Grapevine$tavg,na.rm=TRUE)))
Hamilton_data<-data.frame((id2[9]),(mean(Hamilton$prcp,na.rm=TRUE)),(mean(Hamilton$tmax,na.rm=TRUE)),
(mean(Hamilton$tmin,na.rm=TRUE)),(mean(Hamilton$tavg,na.rm=TRUE)))
Alpine2_data<-data.frame((id2[10]),(mean(Alpine2$prcp,na.rm=TRUE)),(mean(Alpine2$tmax,na.rm=TRUE)),
(mean(Alpine2$tmin,na.rm=TRUE)),(mean(Alpine2$tavg,na.rm=TRUE)))
Austin7_data<-data.frame((id2[11]),(mean(Austin7$prcp,na.rm=TRUE)),(mean(Austin7$tmax,na.rm=TRUE)),
(mean(Austin7$tmin,na.rm=TRUE)),(mean(Austin7$tavg,na.rm=TRUE)))
Austin3_data<-data.frame((id2[12]),(mean(Austin3$prcp,na.rm=TRUE)),(mean(Austin3$tmax,na.rm=TRUE)),
(mean(Austin3$tmin,na.rm=TRUE)),(mean(Austin3$tavg,na.rm=TRUE)))
Austin2_data<-data.frame((id2[13]),(mean(Austin2$prcp,na.rm=TRUE)),(mean(Austin2$tmax,na.rm=TRUE)),
(mean(Austin2$tmin,na.rm=TRUE)),(mean(Austin2$tavg,na.rm=TRUE)))
Ganado_data<-data.frame((id2[14]),(mean(Ganado$prcp,na.rm=TRUE)),(mean(Ganado$tmax,na.rm=TRUE)),
(mean(Ganado$tmin,na.rm=TRUE)),(mean(Ganado$tavg,na.rm=TRUE)))
Austin6_data<-data.frame((id2[15]),(mean(Austin6$prcp,na.rm=TRUE)),(mean(Austin6$tmax,na.rm=TRUE)),
(mean(Austin6$tmin,na.rm=TRUE)),(mean(Austin6$tavg,na.rm=TRUE)))
Austin5_data<-data.frame((id2[16]),(mean(Austin5$prcp,na.rm=TRUE)),(mean(Austin5$tmax,na.rm=TRUE)),
(mean(Austin5$tmin,na.rm=TRUE)),(mean(Austin5$tavg,na.rm=TRUE)))
Houston1_data<-data.frame((id2[17]),(mean(Houston1$prcp,na.rm=TRUE)),(mean(Houston1$tmax,na.rm=TRUE)),
(mean(Houston1$tmin,na.rm=TRUE)),(mean(Houston1$tavg,na.rm=TRUE)))
Vernon_data<-data.frame((id2[18]),(mean(Vernon$prcp,na.rm=TRUE)),(mean(Vernon$tmax,na.rm=TRUE)),
(mean(Vernon$tmin,na.rm=TRUE)),(mean(Vernon$tavg,na.rm=TRUE)))
Saginaw_data<-data.frame((id2[19]),(mean(Saginaw$prcp,na.rm=TRUE)),(mean(Saginaw$tmax,na.rm=TRUE)),
(mean(Saginaw$tmin,na.rm=TRUE)),(mean(Saginaw$tavg,na.rm=TRUE)))
Abilene2_data<-data.frame((id2[20]),(mean(Abilene2$prcp,na.rm=TRUE)),(mean(Abilene2$tmax,na.rm=TRUE)),
(mean(Abilene2$tmin,na.rm=TRUE)),(mean(Abilene2$tavg,na.rm=TRUE)))
Brownwood_data<-data.frame((id2[21]),(mean(Brownwood$prcp,na.rm=TRUE)),(mean(Brownwood$tmax,na.rm=TRUE)),
(mean(Brownwood$tmin,na.rm=TRUE)),(mean(Brownwood$tavg,na.rm=TRUE)))
Terrell_data<-data.frame((id2[22]),(mean(Terrell$prcp,na.rm=TRUE)),(mean(Terrell$tmax,na.rm=TRUE)),
(mean(Terrell$tmin,na.rm=TRUE)),(mean(Terrell$tavg,na.rm=TRUE)))
Abilene1_data<-data.frame((id2[23]),(mean(Abilene1$prcp,na.rm=TRUE)),(mean(Abilene1$tmax,na.rm=TRUE)),
(mean(Abilene1$tmin,na.rm=TRUE)),(mean(Abilene1$tavg,na.rm=TRUE)))
Hondo1_data<-data.frame((id2[24]),(mean(Hondo1$prcp,na.rm=TRUE)),(mean(Hondo1$tmax,na.rm=TRUE)),
(mean(Hondo1$tmin,na.rm=TRUE)),(mean(Hondo1$tavg,na.rm=TRUE)))
Denton_data<-data.frame((id2[25]),(mean(Denton$prcp,na.rm=TRUE)),(mean(Denton$tmax,na.rm=TRUE)),
(mean(Denton$tmin,na.rm=TRUE)),(mean(Denton$tavg,na.rm=TRUE)))
Arlington1_data<-data.frame((id2[26]),(mean(Arlington1$prcp,na.rm=TRUE)),(mean(Arlington1$tmax,na.rm=TRUE)),
(mean(Arlington1$tmin,na.rm=TRUE)),(mean(Arlington1$tavg,na.rm=TRUE)))
Arlington3_data<-data.frame((id2[27]),(mean(Arlington3$prcp,na.rm=TRUE)),(mean(Arlington3$tmax,na.rm=TRUE)),
(mean(Arlington3$tmin,na.rm=TRUE)),(mean(Arlington3$tavg,na.rm=TRUE)))
Waxahachie_data<-data.frame((id2[28]),(mean(Waxahachie$prcp,na.rm=TRUE)),(mean(Waxahachie$tmax,na.rm=TRUE)),
(mean(Waxahachie$tmin,na.rm=TRUE)),(mean(Waxahachie$tavg,na.rm=TRUE)))
Arlington5_data<-data.frame((id2[29]),(mean(Arlington5$prcp,na.rm=TRUE)),(mean(Arlington5$tmax,na.rm=TRUE)),
(mean(Arlington5$tmin,na.rm=TRUE)),(mean(Arlington5$tavg,na.rm=TRUE)))
Dallas1_data<-data.frame((id2[30]),(mean(Dallas1$prcp,na.rm=TRUE)),(mean(Dallas1$tmax,na.rm=TRUE)),
(mean(Dallas1$tmin,na.rm=TRUE)),(mean(Dallas1$tavg,na.rm=TRUE)))
Dallas3_data<-data.frame((id2[31]),(mean(Dallas3$prcp,na.rm=TRUE)),(mean(Dallas3$tmax,na.rm=TRUE)),
(mean(Dallas3$tmin,na.rm=TRUE)),(mean(Dallas3$tavg,na.rm=TRUE)))
FortWorth2_data<-data.frame((id2[32]),(mean(FortWorth2$prcp,na.rm=TRUE)),(mean(FortWorth2$tmax,na.rm=TRUE)),
(mean(FortWorth2$tmin,na.rm=TRUE)),(mean(FortWorth2$tavg,na.rm=TRUE)))
FortWorth11_data<-data.frame((id2[33]),(mean(FortWorth11$prcp,na.rm=TRUE)),(mean(FortWorth11$tmax,na.rm=TRUE)),
(mean(FortWorth11$tmin,na.rm=TRUE)),(mean(FortWorth11$tavg,na.rm=TRUE)))
FortWorth5_data<-data.frame((id2[34]),(mean(FortWorth5$prcp,na.rm=TRUE)),(mean(FortWorth5$tmax,na.rm=TRUE)),
(mean(FortWorth5$tmin,na.rm=TRUE)),(mean(FortWorth5$tavg,na.rm=TRUE)))
Arlington2_data<-data.frame((id2[35]),(mean(Arlington2$prcp,na.rm=TRUE)),(mean(Arlington2$tmax,na.rm=TRUE)),
(mean(Arlington2$tmin,na.rm=TRUE)),(mean(Arlington2$tavg,na.rm=TRUE)))
Austin1_data<-data.frame((id2[36]),(mean(Austin1$prcp,na.rm=TRUE)),(mean(Austin1$tmax,na.rm=TRUE)),
(mean(Austin1$tmin,na.rm=TRUE)),(mean(Austin1$tavg,na.rm=TRUE)))
CollegeStation1_data<-data.frame((id2[37]),(mean(CollegeStation1$prcp,na.rm=TRUE)),(mean(CollegeStation1$tmax,na.rm=TRUE)),
(mean(CollegeStation1$tmin,na.rm=TRUE)),(mean(CollegeStation1$tavg,na.rm=TRUE)))
CollegeStation4_data<-data.frame((id2[38]),(mean(CollegeStation4$prcp,na.rm=TRUE)),(mean(CollegeStation4$tmax,na.rm=TRUE)),
(mean(CollegeStation4$tmin,na.rm=TRUE)),(mean(CollegeStation4$tavg,na.rm=TRUE)))
CollegeStation2_data<-data.frame((id2[39]),(mean(CollegeStation2$prcp,na.rm=TRUE)),(mean(CollegeStation2$tmax,na.rm=TRUE)),
(mean(CollegeStation2$tmin,na.rm=TRUE)),(mean(CollegeStation2$tavg,na.rm=TRUE)))
Bryan3_data<-data.frame((id2[40]),(mean(Bryan3$prcp,na.rm=TRUE)),(mean(Bryan3$tmax,na.rm=TRUE)),
(mean(Bryan3$tmin,na.rm=TRUE)),(mean(Bryan3$tavg,na.rm=TRUE)))
Bryan1_data<-data.frame((id2[41]),(mean(Bryan1$prcp,na.rm=TRUE)),(mean(Bryan1$tmax,na.rm=TRUE)),
(mean(Bryan1$tmin,na.rm=TRUE)),(mean(Bryan1$tavg,na.rm=TRUE)))
Voca_data<-data.frame((id2[42]),(mean(Voca$prcp,na.rm=TRUE)),(mean(Voca$tmax,na.rm=TRUE)),
(mean(Voca$tmin,na.rm=TRUE)),(mean(Voca$tavg,na.rm=TRUE)))
Fannin_data<-data.frame((id2[43]),(mean(Fannin$prcp,na.rm=TRUE)),(mean(Fannin$tmax,na.rm=TRUE)),
(mean(Fannin$tmin,na.rm=TRUE)),(mean(Fannin$tavg,na.rm=TRUE)))
FortWorth12_data<-data.frame((id2[44]),(mean(FortWorth12$prcp,na.rm=TRUE)),(mean(FortWorth12$tmax,na.rm=TRUE)),
(mean(FortWorth12$tmin,na.rm=TRUE)),(mean(FortWorth12$tavg,na.rm=TRUE)))
Waco1_data<-data.frame((id2[45]),(mean(Waco1$prcp,na.rm=TRUE)),(mean(Waco1$tmax,na.rm=TRUE)),
(mean(Waco1$tmin,na.rm=TRUE)),(mean(Waco1$tavg,na.rm=TRUE)))
Franklin2_data<-data.frame((id2[46]),(mean(Franklin2$prcp,na.rm=TRUE)),(mean(Franklin2$tmax,na.rm=TRUE)),
(mean(Franklin2$tmin,na.rm=TRUE)),(mean(Franklin2$tavg,na.rm=TRUE)))
Franklin1_data<-data.frame((id2[47]),(mean(Franklin1$prcp,na.rm=TRUE)),(mean(Franklin1$tmax,na.rm=TRUE)),
(mean(Franklin1$tmin,na.rm=TRUE)),(mean(Franklin1$tavg,na.rm=TRUE)))
Waco3_data<-data.frame((id2[48]),(mean(Waco3$prcp,na.rm=TRUE)),(mean(Waco3$tmax,na.rm=TRUE)),
(mean(Waco3$tmin,na.rm=TRUE)),(mean(Waco3$tavg,na.rm=TRUE)))
FortWorth10_data<-data.frame((id2[49]),(mean(FortWorth10$prcp,na.rm=TRUE)),(mean(FortWorth10$tmax,na.rm=TRUE)),
(mean(FortWorth10$tmin,na.rm=TRUE)),(mean(FortWorth10$tavg,na.rm=TRUE)))
Waco4_data<-data.frame((id2[50]),(mean(Waco4$prcp,na.rm=TRUE)),(mean(Waco4$tmax,na.rm=TRUE)),
(mean(Waco4$tmin,na.rm=TRUE)),(mean(Waco4$tavg,na.rm=TRUE)))
Waco5_data<-data.frame((id2[51]),(mean(Waco5$prcp,na.rm=TRUE)),(mean(Waco5$tmax,na.rm=TRUE)),
(mean(Waco5$tmin,na.rm=TRUE)),(mean(Waco5$tavg,na.rm=TRUE)))
Houston2_data<-data.frame((id2[52]),(mean(Houston2$prcp,na.rm=TRUE)),(mean(Houston2$tmax,na.rm=TRUE)),
(mean(Houston2$tmin,na.rm=TRUE)),(mean(Houston2$tavg,na.rm=TRUE)))
Abilene4_data<-data.frame((id2[53]),(mean(Abilene4$prcp,na.rm=TRUE)),(mean(Abilene4$tmax,na.rm=TRUE)),
(mean(Abilene4$tmin,na.rm=TRUE)),(mean(Abilene4$tavg,na.rm=TRUE)))
FortWorth7_data<-data.frame((id2[54]),(mean(FortWorth7$prcp,na.rm=TRUE)),(mean(FortWorth7$tmax,na.rm=TRUE)),
(mean(FortWorth7$tmin,na.rm=TRUE)),(mean(FortWorth7$tavg,na.rm=TRUE)))
FortWorth4_data<-data.frame((id2[55]),(mean(FortWorth4$prcp,na.rm=TRUE)),(mean(FortWorth4$tmax,na.rm=TRUE)),
(mean(FortWorth4$tmin,na.rm=TRUE)),(mean(FortWorth4$tavg,na.rm=TRUE)))
Sonora1_data<-data.frame((id2[56]),(mean(Sonora1$prcp,na.rm=TRUE)),(mean(Sonora1$tmax,na.rm=TRUE)),
(mean(Sonora1$tmin,na.rm=TRUE)),(mean(Sonora1$tavg,na.rm=TRUE)))
Fredericksburg1_data<-data.frame((id2[57]),(mean(Fredericksburg1$prcp,na.rm=TRUE)),(mean(Fredericksburg1$tmax,na.rm=TRUE)),
(mean(Fredericksburg1$tmin,na.rm=TRUE)),(mean(Fredericksburg1$tavg,na.rm=TRUE)))
Fredericksburg2_data<-data.frame((id2[58]),(mean(Fredericksburg2$prcp,na.rm=TRUE)),(mean(Fredericksburg2$tmax,na.rm=TRUE)),
(mean(Fredericksburg2$tmin,na.rm=TRUE)),(mean(Fredericksburg2$tavg,na.rm=TRUE)))
FortWorth9_data<-data.frame((id2[59]),(mean(FortWorth9$prcp,na.rm=TRUE)),(mean(FortWorth9$tmax,na.rm=TRUE)),
(mean(FortWorth9$tmin,na.rm=TRUE)),(mean(FortWorth9$tavg,na.rm=TRUE)))
Coleman1_data<-data.frame((id2[60]),(mean(Coleman1$prcp,na.rm=TRUE)),(mean(Coleman1$tmax,na.rm=TRUE)),
(mean(Coleman1$tmin,na.rm=TRUE)),(mean(Coleman1$tavg,na.rm=TRUE)))
Mason_data<-data.frame((id2[61]),(mean(Mason$prcp,na.rm=TRUE)),(mean(Mason$tmax,na.rm=TRUE)),
(mean(Mason$tmin,na.rm=TRUE)),(mean(Mason$tavg,na.rm=TRUE)))
Calallen_data<-data.frame((id2[62]),(mean(Calallen$prcp,na.rm=TRUE)),(mean(Calallen$tmax,na.rm=TRUE)),
(mean(Calallen$tmin,na.rm=TRUE)),(mean(Calallen$tavg,na.rm=TRUE)))
FortWorth8_data<-data.frame((id2[63]),(mean(FortWorth8$prcp,na.rm=TRUE)),(mean(FortWorth8$tmax,na.rm=TRUE)),
(mean(FortWorth8$tmin,na.rm=TRUE)),(mean(FortWorth8$tavg,na.rm=TRUE)))
Abilene3_data<-data.frame((id2[64]),(mean(Abilene3$prcp,na.rm=TRUE)),(mean(Abilene3$tmax,na.rm=TRUE)),
(mean(Abilene3$tmin,na.rm=TRUE)),(mean(Abilene3$tavg,na.rm=TRUE)))
SanAngelo1_data<-data.frame((id2[65]),(mean(SanAngelo1$prcp,na.rm=TRUE)),(mean(SanAngelo1$tmax,na.rm=TRUE)),
(mean(SanAngelo1$tmin,na.rm=TRUE)),(mean(SanAngelo1$tavg,na.rm=TRUE)))
Santo_data<-data.frame((id2[66]),(mean(Santo$prcp,na.rm=TRUE)),(mean(Santo$tmax,na.rm=TRUE)),
(mean(Santo$tmin,na.rm=TRUE)),(mean(Santo$tavg,na.rm=TRUE)))
CollegeStation3_data<-data.frame((id2[67]),(mean(CollegeStation3$prcp,na.rm=TRUE)),(mean(CollegeStation3$tmax,na.rm=TRUE)),
(mean(CollegeStation3$tmin,na.rm=TRUE)),(mean(CollegeStation3$tavg,na.rm=TRUE)))
CollegeStation5_data<-data.frame((id2[68]),(mean(CollegeStation5$prcp,na.rm=TRUE)),(mean(CollegeStation5$tmax,na.rm=TRUE)),
(mean(CollegeStation5$tmin,na.rm=TRUE)),(mean(CollegeStation5$tavg,na.rm=TRUE)))
Georgetown_data<-data.frame((id2[69]),(mean(Georgetown$prcp,na.rm=TRUE)),(mean(Georgetown$tmax,na.rm=TRUE)),
(mean(Georgetown$tmin,na.rm=TRUE)),(mean(Georgetown$tavg,na.rm=TRUE)))
SanAngelo2_data<-data.frame((id2[70]),(mean(SanAngelo2$prcp,na.rm=TRUE)),(mean(SanAngelo2$tmax,na.rm=TRUE)),
(mean(SanAngelo2$tmin,na.rm=TRUE)),(mean(SanAngelo2$tavg,na.rm=TRUE)))
Alpine1_data<-data.frame((id2[71]),(mean(Alpine1$prcp,na.rm=TRUE)),(mean(Alpine1$tmax,na.rm=TRUE)),
(mean(Alpine1$tmin,na.rm=TRUE)),(mean(Alpine1$tavg,na.rm=TRUE)))
Brady1_data<-data.frame((id2[72]),(mean(Brady1$prcp,na.rm=TRUE)),(mean(Brady1$tmax,na.rm=TRUE)),
(mean(Brady1$tmin,na.rm=TRUE)),(mean(Brady1$tavg,na.rm=TRUE)))
Eden_data<-data.frame((id2[73]),(mean(Eden$prcp,na.rm=TRUE)),(mean(Eden$tmax,na.rm=TRUE)),
(mean(Eden$tmin,na.rm=TRUE)),(mean(Eden$tavg,na.rm=TRUE)))
Llano2_data<-data.frame((id2[74]),(mean(Llano2$prcp,na.rm=TRUE)),(mean(Llano2$tmax,na.rm=TRUE)),
(mean(Llano2$tmin,na.rm=TRUE)),(mean(Llano2$tavg,na.rm=TRUE)))
Llano1_data<-data.frame((id2[75]),(mean(Llano1$prcp,na.rm=TRUE)),(mean(Llano1$tmax,na.rm=TRUE)),
(mean(Llano1$tmin,na.rm=TRUE)),(mean(Llano1$tavg,na.rm=TRUE)))
Callahan_data<-data.frame((id2[76]),(mean(Callahan$prcp,na.rm=TRUE)),(mean(Callahan$tmax,na.rm=TRUE)),
(mean(Callahan$tmin,na.rm=TRUE)),(mean(Callahan$tavg,na.rm=TRUE)))


#columnnames
cat(collnames(unique(id2),y="colnames(",z="_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')"),sep="\n")

colnames(FortWorth1_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(SanAntonio1_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(Gunter1_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(Burkburnett_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(Dallas6_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(Abilene5_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(Rockwall_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(Grapevine_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(Hamilton_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(Alpine2_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(Austin7_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(Austin3_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(Austin2_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(Ganado_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(Austin6_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(Austin5_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(Houston1_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(Vernon_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(Saginaw_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(Abilene2_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(Brownwood_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(Terrell_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(Abilene1_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(Hondo1_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(Denton_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(Arlington1_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(Arlington3_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(Waxahachie_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(Arlington5_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(Dallas1_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(Dallas3_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(FortWorth2_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(FortWorth11_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(FortWorth5_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(Arlington2_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(Austin1_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(CollegeStation1_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(CollegeStation4_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(CollegeStation2_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(Bryan3_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(Bryan1_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(Voca_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(Fannin_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(FortWorth12_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(Waco1_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(Franklin2_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(Franklin1_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(Waco3_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(FortWorth10_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(Waco4_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(Waco5_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(Houston2_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(Abilene4_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(FortWorth7_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(FortWorth4_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(Sonora1_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(Fredericksburg1_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(Fredericksburg2_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(FortWorth9_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(Coleman1_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(Mason_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(Calallen_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(FortWorth8_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(Abilene3_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(SanAngelo1_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(Santo_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(CollegeStation3_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(CollegeStation5_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(Georgetown_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(SanAngelo2_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(Alpine1_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(Brady1_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(Eden_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(Llano2_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(Llano1_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(Callahan_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')

##pasteintobind_rows()
cat(bindrowsfun(unique(id2),y="_data",z=","),sep="")
TXtotal_obs<-bind_rows(FortWorth1_data,SanAntonio1_data,Gunter1_data,Burkburnett_data,Dallas6_data,Abilene5_data,Rockwall_data,Grapevine_data,Hamilton_data,Alpine2_data,Austin7_data,Austin3_data,Austin2_data,Ganado_data,Austin6_data,Austin5_data,Houston1_data,Vernon_data,Saginaw_data,Abilene2_data,Brownwood_data,Terrell_data,Abilene1_data,Hondo1_data,Denton_data,Arlington1_data,Arlington3_data,Waxahachie_data,Arlington5_data,Dallas1_data,Dallas3_data,FortWorth2_data,FortWorth11_data,FortWorth5_data,Arlington2_data,Austin1_data,CollegeStation1_data,CollegeStation4_data,CollegeStation2_data,Bryan3_data,Bryan1_data,Voca_data,Fannin_data,FortWorth12_data,Waco1_data,Franklin2_data,Franklin1_data,Waco3_data,FortWorth10_data,Waco4_data,Waco5_data,Houston2_data,Abilene4_data,FortWorth7_data,FortWorth4_data,Sonora1_data,Fredericksburg1_data,Fredericksburg2_data,FortWorth9_data,Coleman1_data,Mason_data,Calallen_data,FortWorth8_data,Abilene3_data,SanAngelo1_data,Santo_data,CollegeStation3_data,CollegeStation5_data,Georgetown_data,SanAngelo2_data,Alpine1_data,Brady1_data,Eden_data,Llano2_data,Llano1_data,Callahan_data)



###savedataasxlsxfile
write.xlsx(TXtotal_obs,file="TXtotal_obs.xlsx",sheetName="Sheet1",
col.names=TRUE,row.names=TRUE,append=FALSE)


###########Michigan##########

MItotal<-read.csv("michigan_data_total_weather.csv")

##makeprcp,tmax,tmin,andtavgnumeric

MItotal$prcp<-as.numeric(MItotal$prcp)
MItotal$tmax<-as.numeric(MItotal$tmax)
MItotal$tmin<-as.numeric(MItotal$tmin)
MItotal$tavg<-as.numeric(MItotal$tavg)

##multiplyby0.1ordivideby10tomake1degree

MItotal$prcp<-MItotal$prcp/10
MItotal$tmax<-MItotal$tmax/10
MItotal$tmin<-MItotal$tmin/10
MItotal$tavg<-MItotal$tavg/10

#pulldata
id2<-unique(MItotal$id2)
cat(filtering(id2,y="<-filter(MItotal,id2 =='",z="')"),sep="\n")

Ypsilanti<-filter(MItotal,id2 =='Ypsilanti')
EastLansing<-filter(MItotal,id2 =='EastLansing')
Hudsonville1<-filter(MItotal,id2 =='Hudsonville1')
Richmond<-filter(MItotal,id2 =='Richmond')
BeaverIsland<-filter(MItotal,id2 =='BeaverIsland')
GrandRapids2<-filter(MItotal,id2 =='GrandRapids2')
Wyoming<-filter(MItotal,id2 =='Wyoming')
GrandRapids4<-filter(MItotal,id2 =='GrandRapids4')
Hudsonville2<-filter(MItotal,id2 =='Hudsonville2')
GrandRapids3<-filter(MItotal,id2 =='GrandRapids3')
Kalamazoo<-filter(MItotal,id2 =='Kalamazoo')
GrandRapids1<-filter(MItotal,id2 =='GrandRapids1')
Muskegon<-filter(MItotal,id2 =='Muskegon')
CharlevoixCounty<-filter(MItotal,id2 =='CharlevoixCounty')
TawasCity<-filter(MItotal,id2 =='TawasCity')
Springfield<-filter(MItotal,id2 =='Springfield')
Mt.Pleasant<-filter(MItotal,id2 =='Mt.Pleasant')
BigRapids1<-filter(MItotal,id2 =='BigRapids1')
BigRapids2<-filter(MItotal,id2 =='BigRapids2')
Hart<-filter(MItotal,id2 =='Hart')
Fremont<-filter(MItotal,id2 =='Fremont')
Otsego<-filter(MItotal,id2 =='Otsego')
Williamston<-filter(MItotal,id2 =='Williamston')


###findthemeanforeach
lennumber=(1:23)
df<-list(id2,lennumber)

lapply(df,cat(meandata(x=id2,y="_data<-data.frame((id2[",l=lennumber,k="]),(mean(",e="$prcp,na.rm=TRUE)),(mean(",f="$tmax,na.rm=TRUE)),
(mean(",g="$tmin,na.rm=TRUE)),(mean(",z="$tavg,na.rm=TRUE)))"),sep="\n"))

Ypsilanti_data<-data.frame((id2[1]),(mean(Ypsilanti$prcp,na.rm=TRUE)),(mean(Ypsilanti$tmax,na.rm=TRUE)),
                           (mean(Ypsilanti$tmin,na.rm=TRUE)),(mean(Ypsilanti$tavg,na.rm=TRUE)))
EastLansing_data<-data.frame((id2[2]),(mean(EastLansing$prcp,na.rm=TRUE)),(mean(EastLansing$tmax,na.rm=TRUE)),
                             (mean(EastLansing$tmin,na.rm=TRUE)),(mean(EastLansing$tavg,na.rm=TRUE)))
Hudsonville1_data<-data.frame((id2[3]),(mean(Hudsonville1$prcp,na.rm=TRUE)),(mean(Hudsonville1$tmax,na.rm=TRUE)),
                              (mean(Hudsonville1$tmin,na.rm=TRUE)),(mean(Hudsonville1$tavg,na.rm=TRUE)))
Richmond_data<-data.frame((id2[4]),(mean(Richmond$prcp,na.rm=TRUE)),(mean(Richmond$tmax,na.rm=TRUE)),
                          (mean(Richmond$tmin,na.rm=TRUE)),(mean(Richmond$tavg,na.rm=TRUE)))
BeaverIsland_data<-data.frame((id2[5]),(mean(BeaverIsland$prcp,na.rm=TRUE)),(mean(BeaverIsland$tmax,na.rm=TRUE)),
                              (mean(BeaverIsland$tmin,na.rm=TRUE)),(mean(BeaverIsland$tavg,na.rm=TRUE)))
GrandRapids2_data<-data.frame((id2[6]),(mean(GrandRapids2$prcp,na.rm=TRUE)),(mean(GrandRapids2$tmax,na.rm=TRUE)),
                              (mean(GrandRapids2$tmin,na.rm=TRUE)),(mean(GrandRapids2$tavg,na.rm=TRUE)))
Wyoming_data<-data.frame((id2[7]),(mean(Wyoming$prcp,na.rm=TRUE)),(mean(Wyoming$tmax,na.rm=TRUE)),
                         (mean(Wyoming$tmin,na.rm=TRUE)),(mean(Wyoming$tavg,na.rm=TRUE)))
GrandRapids4_data<-data.frame((id2[8]),(mean(GrandRapids4$prcp,na.rm=TRUE)),(mean(GrandRapids4$tmax,na.rm=TRUE)),
                              (mean(GrandRapids4$tmin,na.rm=TRUE)),(mean(GrandRapids4$tavg,na.rm=TRUE)))
Hudsonville2_data<-data.frame((id2[9]),(mean(Hudsonville2$prcp,na.rm=TRUE)),(mean(Hudsonville2$tmax,na.rm=TRUE)),
                              (mean(Hudsonville2$tmin,na.rm=TRUE)),(mean(Hudsonville2$tavg,na.rm=TRUE)))
GrandRapids3_data<-data.frame((id2[10]),(mean(GrandRapids3$prcp,na.rm=TRUE)),(mean(GrandRapids3$tmax,na.rm=TRUE)),
                              (mean(GrandRapids3$tmin,na.rm=TRUE)),(mean(GrandRapids3$tavg,na.rm=TRUE)))
Kalamazoo_data<-data.frame((id2[11]),(mean(Kalamazoo$prcp,na.rm=TRUE)),(mean(Kalamazoo$tmax,na.rm=TRUE)),
                           (mean(Kalamazoo$tmin,na.rm=TRUE)),(mean(Kalamazoo$tavg,na.rm=TRUE)))
GrandRapids1_data<-data.frame((id2[12]),(mean(GrandRapids1$prcp,na.rm=TRUE)),(mean(GrandRapids1$tmax,na.rm=TRUE)),
                              (mean(GrandRapids1$tmin,na.rm=TRUE)),(mean(GrandRapids1$tavg,na.rm=TRUE)))
Muskegon_data<-data.frame((id2[13]),(mean(Muskegon$prcp,na.rm=TRUE)),(mean(Muskegon$tmax,na.rm=TRUE)),
                          (mean(Muskegon$tmin,na.rm=TRUE)),(mean(Muskegon$tavg,na.rm=TRUE)))
CharlevoixCounty_data<-data.frame((id2[14]),(mean(CharlevoixCounty$prcp,na.rm=TRUE)),(mean(CharlevoixCounty$tmax,na.rm=TRUE)),
                                  (mean(CharlevoixCounty$tmin,na.rm=TRUE)),(mean(CharlevoixCounty$tavg,na.rm=TRUE)))
TawasCity_data<-data.frame((id2[15]),(mean(TawasCity$prcp,na.rm=TRUE)),(mean(TawasCity$tmax,na.rm=TRUE)),
                           (mean(TawasCity$tmin,na.rm=TRUE)),(mean(TawasCity$tavg,na.rm=TRUE)))
Springfield_data<-data.frame((id2[16]),(mean(Springfield$prcp,na.rm=TRUE)),(mean(Springfield$tmax,na.rm=TRUE)),
                             (mean(Springfield$tmin,na.rm=TRUE)),(mean(Springfield$tavg,na.rm=TRUE)))
Mt.Pleasant_data<-data.frame((id2[17]),(mean(Mt.Pleasant$prcp,na.rm=TRUE)),(mean(Mt.Pleasant$tmax,na.rm=TRUE)),
                             (mean(Mt.Pleasant$tmin,na.rm=TRUE)),(mean(Mt.Pleasant$tavg,na.rm=TRUE)))
BigRapids1_data<-data.frame((id2[18]),(mean(BigRapids1$prcp,na.rm=TRUE)),(mean(BigRapids1$tmax,na.rm=TRUE)),
                            (mean(BigRapids1$tmin,na.rm=TRUE)),(mean(BigRapids1$tavg,na.rm=TRUE)))
BigRapids2_data<-data.frame((id2[19]),(mean(BigRapids2$prcp,na.rm=TRUE)),(mean(BigRapids2$tmax,na.rm=TRUE)),
                            (mean(BigRapids2$tmin,na.rm=TRUE)),(mean(BigRapids2$tavg,na.rm=TRUE)))
Hart_data<-data.frame((id2[20]),(mean(Hart$prcp,na.rm=TRUE)),(mean(Hart$tmax,na.rm=TRUE)),
                      (mean(Hart$tmin,na.rm=TRUE)),(mean(Hart$tavg,na.rm=TRUE)))
Fremont_data<-data.frame((id2[21]),(mean(Fremont$prcp,na.rm=TRUE)),(mean(Fremont$tmax,na.rm=TRUE)),
                         (mean(Fremont$tmin,na.rm=TRUE)),(mean(Fremont$tavg,na.rm=TRUE)))
Otsego_data<-data.frame((id2[22]),(mean(Otsego$prcp,na.rm=TRUE)),(mean(Otsego$tmax,na.rm=TRUE)),
                        (mean(Otsego$tmin,na.rm=TRUE)),(mean(Otsego$tavg,na.rm=TRUE)))
Williamston_data<-data.frame((id2[23]),(mean(Williamston$prcp,na.rm=TRUE)),(mean(Williamston$tmax,na.rm=TRUE)),
                             (mean(Williamston$tmin,na.rm=TRUE)),(mean(Williamston$tavg,na.rm=TRUE)))

#columnnames
cat(collnames(unique(id2),y="colnames(",z="_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')"),sep="\n")

colnames(Ypsilanti_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(EastLansing_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(Hudsonville1_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(Richmond_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(BeaverIsland_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(GrandRapids2_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(Wyoming_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(GrandRapids4_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(Hudsonville2_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(GrandRapids3_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(Kalamazoo_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(GrandRapids1_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(Muskegon_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(CharlevoixCounty_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(TawasCity_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(Springfield_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(Mt.Pleasant_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(BigRapids1_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(BigRapids2_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(Hart_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(Fremont_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(Otsego_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')
colnames(Williamston_data)<-c('id','prcp_avg','tmax_avg','tmin_avg','tavg_avg')

##pasteintobind_rows()
cat(bindrowsfun(unique(id2),y="_data",z=","),sep="")
MItotal_obs<-bind_rows(Ypsilanti_data,EastLansing_data,Hudsonville1_data,Richmond_data,BeaverIsland_data,GrandRapids2_data,Wyoming_data,GrandRapids4_data,Hudsonville2_data,GrandRapids3_data,Kalamazoo_data,GrandRapids1_data,Muskegon_data,CharlevoixCounty_data,TawasCity_data,Springfield_data,Mt.Pleasant_data,BigRapids1_data,BigRapids2_data,Hart_data,Fremont_data,Otsego_data,Williamston_data)

###savedataasxlsxfile
write.xlsx(MItotal_obs,file="MItotal_obs.xlsx",sheetName="Sheet1",
col.names=TRUE,row.names=TRUE,append=FALSE)


########### Ohio ######

OHtotal <- read.csv("OH_data_total_weather.csv")

##make prcp, tmax, tmin, and tavg numeric

OHtotal$prcp <- as.numeric(OHtotal$prcp)
OHtotal$tmax <- as.numeric(OHtotal$tmax)
OHtotal$tmin <- as.numeric(OHtotal$tmin)
OHtotal$tavg <- as.numeric(OHtotal$tavg)

## multiply by 0.1 or divide by 10 to make 1 degree 

OHtotal$prcp <- OHtotal$prcp/10
OHtotal$tmax <- OHtotal$tmax/10
OHtotal$tmin <- OHtotal$tmin/10
OHtotal$tavg <- OHtotal$tavg/10

#pull data 
id2 <- unique(OHtotal$id2)
cat(filtering(id2, y =  "<- filter(OHtotal, id2 == '", z = "')"), sep = "\n")

coshocton<- filter(OHtotal, id2 == 'coshocton')
oxford<- filter(OHtotal, id2 == 'oxford')
stclairsville<- filter(OHtotal, id2 == 'stclairsville')
ironton<- filter(OHtotal, id2 == 'ironton')
hamilton1<- filter(OHtotal, id2 == 'hamilton1')
hamilton2<- filter(OHtotal, id2 == 'hamilton2')
lebanon<- filter(OHtotal, id2 == 'lebanon')
columbus<- filter(OHtotal, id2 == 'columbus')


### find the mean for each 
lennumber = (1:8)
df <- list(id2, lennumber)

lapply(df, cat(meandata(x = id2,  y = "_data <- data.frame((id2[", l= lennumber, k = "]), (mean(", e = "$prcp, na.rm=TRUE)), (mean(", f = "$tmax, na.rm=TRUE)), 
                              (mean(", g = "$tmin, na.rm=TRUE)), (mean(", z = "$tavg, na.rm=TRUE)))"), sep = "\n"))

coshocton_data <- data.frame((id2[1]), (mean(coshocton$prcp, na.rm=TRUE)), (mean(coshocton$tmax, na.rm=TRUE)), 
                             (mean(coshocton$tmin, na.rm=TRUE)), (mean(coshocton$tavg, na.rm=TRUE)))
oxford_data <- data.frame((id2[2]), (mean(oxford$prcp, na.rm=TRUE)), (mean(oxford$tmax, na.rm=TRUE)), 
                          (mean(oxford$tmin, na.rm=TRUE)), (mean(oxford$tavg, na.rm=TRUE)))
stclairsville_data <- data.frame((id2[3]), (mean(stclairsville$prcp, na.rm=TRUE)), (mean(stclairsville$tmax, na.rm=TRUE)), 
                                 (mean(stclairsville$tmin, na.rm=TRUE)), (mean(stclairsville$tavg, na.rm=TRUE)))
ironton_data <- data.frame((id2[4]), (mean(ironton$prcp, na.rm=TRUE)), (mean(ironton$tmax, na.rm=TRUE)), 
                           (mean(ironton$tmin, na.rm=TRUE)), (mean(ironton$tavg, na.rm=TRUE)))
hamilton1_data <- data.frame((id2[5]), (mean(hamilton1$prcp, na.rm=TRUE)), (mean(hamilton1$tmax, na.rm=TRUE)), 
                             (mean(hamilton1$tmin, na.rm=TRUE)), (mean(hamilton1$tavg, na.rm=TRUE)))
hamilton2_data <- data.frame((id2[6]), (mean(hamilton2$prcp, na.rm=TRUE)), (mean(hamilton2$tmax, na.rm=TRUE)), 
                             (mean(hamilton2$tmin, na.rm=TRUE)), (mean(hamilton2$tavg, na.rm=TRUE)))
lebanon_data <- data.frame((id2[7]), (mean(lebanon$prcp, na.rm=TRUE)), (mean(lebanon$tmax, na.rm=TRUE)), 
                           (mean(lebanon$tmin, na.rm=TRUE)), (mean(lebanon$tavg, na.rm=TRUE)))
columbus_data <- data.frame((id2[8]), (mean(columbus$prcp, na.rm=TRUE)), (mean(columbus$tmax, na.rm=TRUE)), 
                            (mean(columbus$tmin, na.rm=TRUE)), (mean(columbus$tavg, na.rm=TRUE)))


#column names 
cat(collnames(unique(id2), y = "colnames(", z = "_data) <- c('id', 'prcp_avg','tmax_avg', 'tmin_avg', 'tavg_avg')"), sep = "\n")

colnames(coshocton_data) <- c('id', 'prcp_avg','tmax_avg', 'tmin_avg', 'tavg_avg')
colnames(oxford_data) <- c('id', 'prcp_avg','tmax_avg', 'tmin_avg', 'tavg_avg')
colnames(stclairsville_data) <- c('id', 'prcp_avg','tmax_avg', 'tmin_avg', 'tavg_avg')
colnames(ironton_data) <- c('id', 'prcp_avg','tmax_avg', 'tmin_avg', 'tavg_avg')
colnames(hamilton1_data) <- c('id', 'prcp_avg','tmax_avg', 'tmin_avg', 'tavg_avg')
colnames(hamilton2_data) <- c('id', 'prcp_avg','tmax_avg', 'tmin_avg', 'tavg_avg')
colnames(lebanon_data) <- c('id', 'prcp_avg','tmax_avg', 'tmin_avg', 'tavg_avg')
colnames(columbus_data) <- c('id', 'prcp_avg','tmax_avg', 'tmin_avg', 'tavg_avg')

##paste into bind_rows()
cat(bindrowsfun(unique(id2), y = "_data", z = ","), sep = "")
OHtotal_obs <- bind_rows(coshocton_data,oxford_data,stclairsville_data,ironton_data,hamilton1_data,hamilton2_data,lebanon_data,columbus_data)



### save data as xlsx file 
write.xlsx(OHtotal_obs, file = "OHtotal_obs.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)

######### Oklahoma #########

OKtotal <- read.csv("OK_data_total_weather.csv")

##make prcp, tmax, tmin, and tavg numeric

OKtotal$prcp <- as.numeric(OKtotal$prcp)
OKtotal$tmax <- as.numeric(OKtotal$tmax)
OKtotal$tmin <- as.numeric(OKtotal$tmin)
OKtotal$tavg <- as.numeric(OKtotal$tavg)

## multiply by 0.1 or divide by 10 to make 1 degree 

OKtotal$prcp <- OKtotal$prcp/10
OKtotal$tmax <- OKtotal$tmax/10
OKtotal$tmin <- OKtotal$tmin/10
OKtotal$tavg <- OKtotal$tavg/10

#pull data 
id2 <- unique(OKtotal$id2)
cat(filtering(id2, y =  "<- filter(OKtotal, id2 == '", z = "')"), sep = "\n")

bristow<- filter(OKtotal, id2 == 'bristow')
sulphur<- filter(OKtotal, id2 == 'sulphur')
oklahomacity1<- filter(OKtotal, id2 == 'oklahomacity1')
stillwater<- filter(OKtotal, id2 == 'stillwater')
drippingsprings<- filter(OKtotal, id2 == 'drippingsprings')
summers<- filter(OKtotal, id2 == 'summers')
peggs<- filter(OKtotal, id2 == 'peggs')
riverbottom1<- filter(OKtotal, id2 == 'riverbottom1')
riverbottom2<- filter(OKtotal, id2 == 'riverbottom2')
shawnee<- filter(OKtotal, id2 == 'shawnee')
alva<- filter(OKtotal, id2 == 'alva')
oklahomacity2<- filter(OKtotal, id2 == 'oklahomacity2')
oklahomacity3<- filter(OKtotal, id2 == 'oklahomacity3')
oklahomacity4<- filter(OKtotal, id2 == 'oklahomacity4')
norman<- filter(OKtotal, id2 == 'norman')


### find the mean for each 
lennumber = (1:15)
df <- list(id2, lennumber)

lapply(df, cat(meandata(x = id2,  y = "_data <- data.frame((id2[", l= lennumber, k = "]), (mean(", e = "$prcp, na.rm=TRUE)), (mean(", f = "$tmax, na.rm=TRUE)), 
                              (mean(", g = "$tmin, na.rm=TRUE)), (mean(", z = "$tavg, na.rm=TRUE)))"), sep = "\n"))

bristow_data <- data.frame((id2[1]), (mean(bristow$prcp, na.rm=TRUE)), (mean(bristow$tmax, na.rm=TRUE)), 
                           (mean(bristow$tmin, na.rm=TRUE)), (mean(bristow$tavg, na.rm=TRUE)))
sulphur_data <- data.frame((id2[2]), (mean(sulphur$prcp, na.rm=TRUE)), (mean(sulphur$tmax, na.rm=TRUE)), 
                           (mean(sulphur$tmin, na.rm=TRUE)), (mean(sulphur$tavg, na.rm=TRUE)))
oklahomacity1_data <- data.frame((id2[3]), (mean(oklahomacity1$prcp, na.rm=TRUE)), (mean(oklahomacity1$tmax, na.rm=TRUE)), 
                                 (mean(oklahomacity1$tmin, na.rm=TRUE)), (mean(oklahomacity1$tavg, na.rm=TRUE)))
stillwater_data <- data.frame((id2[4]), (mean(stillwater$prcp, na.rm=TRUE)), (mean(stillwater$tmax, na.rm=TRUE)), 
                              (mean(stillwater$tmin, na.rm=TRUE)), (mean(stillwater$tavg, na.rm=TRUE)))
drippingsprings_data <- data.frame((id2[5]), (mean(drippingsprings$prcp, na.rm=TRUE)), (mean(drippingsprings$tmax, na.rm=TRUE)), 
                                   (mean(drippingsprings$tmin, na.rm=TRUE)), (mean(drippingsprings$tavg, na.rm=TRUE)))
summers_data <- data.frame((id2[6]), (mean(summers$prcp, na.rm=TRUE)), (mean(summers$tmax, na.rm=TRUE)), 
                           (mean(summers$tmin, na.rm=TRUE)), (mean(summers$tavg, na.rm=TRUE)))
peggs_data <- data.frame((id2[7]), (mean(peggs$prcp, na.rm=TRUE)), (mean(peggs$tmax, na.rm=TRUE)), 
                         (mean(peggs$tmin, na.rm=TRUE)), (mean(peggs$tavg, na.rm=TRUE)))
riverbottom1_data <- data.frame((id2[8]), (mean(riverbottom1$prcp, na.rm=TRUE)), (mean(riverbottom1$tmax, na.rm=TRUE)), 
                                (mean(riverbottom1$tmin, na.rm=TRUE)), (mean(riverbottom1$tavg, na.rm=TRUE)))
riverbottom2_data <- data.frame((id2[9]), (mean(riverbottom2$prcp, na.rm=TRUE)), (mean(riverbottom2$tmax, na.rm=TRUE)), 
                                (mean(riverbottom2$tmin, na.rm=TRUE)), (mean(riverbottom2$tavg, na.rm=TRUE)))
shawnee_data <- data.frame((id2[10]), (mean(shawnee$prcp, na.rm=TRUE)), (mean(shawnee$tmax, na.rm=TRUE)), 
                           (mean(shawnee$tmin, na.rm=TRUE)), (mean(shawnee$tavg, na.rm=TRUE)))
alva_data <- data.frame((id2[11]), (mean(alva$prcp, na.rm=TRUE)), (mean(alva$tmax, na.rm=TRUE)), 
                        (mean(alva$tmin, na.rm=TRUE)), (mean(alva$tavg, na.rm=TRUE)))
oklahomacity2_data <- data.frame((id2[12]), (mean(oklahomacity2$prcp, na.rm=TRUE)), (mean(oklahomacity2$tmax, na.rm=TRUE)), 
                                 (mean(oklahomacity2$tmin, na.rm=TRUE)), (mean(oklahomacity2$tavg, na.rm=TRUE)))
oklahomacity3_data <- data.frame((id2[13]), (mean(oklahomacity3$prcp, na.rm=TRUE)), (mean(oklahomacity3$tmax, na.rm=TRUE)), 
                                 (mean(oklahomacity3$tmin, na.rm=TRUE)), (mean(oklahomacity3$tavg, na.rm=TRUE)))
oklahomacity4_data <- data.frame((id2[14]), (mean(oklahomacity4$prcp, na.rm=TRUE)), (mean(oklahomacity4$tmax, na.rm=TRUE)), 
                                 (mean(oklahomacity4$tmin, na.rm=TRUE)), (mean(oklahomacity4$tavg, na.rm=TRUE)))
norman_data <- data.frame((id2[15]), (mean(norman$prcp, na.rm=TRUE)), (mean(norman$tmax, na.rm=TRUE)), 
                          (mean(norman$tmin, na.rm=TRUE)), (mean(norman$tavg, na.rm=TRUE)))


#column names 
cat(collnames(unique(id2), y = "colnames(", z = "_data) <- c('id', 'prcp_avg','tmax_avg', 'tmin_avg', 'tavg_avg')"), sep = "\n")

colnames(bristow_data) <- c('id', 'prcp_avg','tmax_avg', 'tmin_avg', 'tavg_avg')
colnames(sulphur_data) <- c('id', 'prcp_avg','tmax_avg', 'tmin_avg', 'tavg_avg')
colnames(oklahomacity1_data) <- c('id', 'prcp_avg','tmax_avg', 'tmin_avg', 'tavg_avg')
colnames(stillwater_data) <- c('id', 'prcp_avg','tmax_avg', 'tmin_avg', 'tavg_avg')
colnames(drippingsprings_data) <- c('id', 'prcp_avg','tmax_avg', 'tmin_avg', 'tavg_avg')
colnames(summers_data) <- c('id', 'prcp_avg','tmax_avg', 'tmin_avg', 'tavg_avg')
colnames(peggs_data) <- c('id', 'prcp_avg','tmax_avg', 'tmin_avg', 'tavg_avg')
colnames(riverbottom1_data) <- c('id', 'prcp_avg','tmax_avg', 'tmin_avg', 'tavg_avg')
colnames(riverbottom2_data) <- c('id', 'prcp_avg','tmax_avg', 'tmin_avg', 'tavg_avg')
colnames(shawnee_data) <- c('id', 'prcp_avg','tmax_avg', 'tmin_avg', 'tavg_avg')
colnames(alva_data) <- c('id', 'prcp_avg','tmax_avg', 'tmin_avg', 'tavg_avg')
colnames(oklahomacity2_data) <- c('id', 'prcp_avg','tmax_avg', 'tmin_avg', 'tavg_avg')
colnames(oklahomacity3_data) <- c('id', 'prcp_avg','tmax_avg', 'tmin_avg', 'tavg_avg')
colnames(oklahomacity4_data) <- c('id', 'prcp_avg','tmax_avg', 'tmin_avg', 'tavg_avg')
colnames(norman_data) <- c('id', 'prcp_avg','tmax_avg', 'tmin_avg', 'tavg_avg')

##paste into bind_rows()
cat(bindrowsfun(unique(id2), y = "_data", z = ","), sep = "")
OKtotal_obs <- bind_rows(bristow_data,sulphur_data,oklahomacity1_data,stillwater_data,drippingsprings_data,summers_data,peggs_data,riverbottom1_data,riverbottom2_data,shawnee_data,alva_data,oklahomacity2_data,oklahomacity3_data,oklahomacity4_data,norman_data)



### save data as xlsx file 
write.xlsx(OKtotal_obs, file = "OKtotal_obs.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)


########## Maryland ########

MYtotal <- read.csv("maryland_data_total_weather.csv")

##make prcp, tmax, tmin, and tavg numeric

MYtotal$prcp <- as.numeric(MYtotal$prcp)
MYtotal$tmax <- as.numeric(MYtotal$tmax)
MYtotal$tmin <- as.numeric(MYtotal$tmin)
MYtotal$tavg <- as.numeric(MYtotal$tavg)

## multiply by 0.1 or divide by 10 to make 1 degree 

MYtotal$prcp <- MYtotal$prcp/10
MYtotal$tmax <- MYtotal$tmax/10
MYtotal$tmin <- MYtotal$tmin/10
MYtotal$tavg <- MYtotal$tavg/10

#pull data 
id2 <- unique(MYtotal$id2)
cat(filtering(id2, y =  "<- filter(MYtotal, id2 == '", z = "')"), sep = "\n")


laplata<- filter(MYtotal, id2 == 'laplata')
kennedyville<- filter(MYtotal, id2 == 'kennedyville')
carney2<- filter(MYtotal, id2 == 'carney2')
carney3<- filter(MYtotal, id2 == 'carney3')
carney1<- filter(MYtotal, id2 == 'carney1')
cockeysville<- filter(MYtotal, id2 == 'cockeysville')
northbesthesda<- filter(MYtotal, id2 == 'northbesthesda')
thumont<- filter(MYtotal, id2 == 'thumont')
baltimorecounty<- filter(MYtotal, id2 == 'baltimorecounty')
timomium<- filter(MYtotal, id2 == 'timomium')
silverspring2<- filter(MYtotal, id2 == 'silverspring2')
owingmills<- filter(MYtotal, id2 == 'owingmills')
lilypons<- filter(MYtotal, id2 == 'lilypons')
townson2<- filter(MYtotal, id2 == 'townson2')
reistertown<- filter(MYtotal, id2 == 'reistertown')
townson1<- filter(MYtotal, id2 == 'townson1')
phoenix<- filter(MYtotal, id2 == 'phoenix')
townson3<- filter(MYtotal, id2 == 'townson3')
gaithersburg<- filter(MYtotal, id2 == 'gaithersburg')
accokeek<- filter(MYtotal, id2 == 'accokeek')
uppermarlboro<- filter(MYtotal, id2 == 'uppermarlboro')
collegepark<- filter(MYtotal, id2 == 'collegepark')
rosedale<- filter(MYtotal, id2 == 'rosedale')
preston1<- filter(MYtotal, id2 == 'preston1')
waddellscorner<- filter(MYtotal, id2 == 'waddellscorner')
eastnewmarket<- filter(MYtotal, id2 == 'eastnewmarket')
snowhill1<- filter(MYtotal, id2 == 'snowhill1')
snowhill2<- filter(MYtotal, id2 == 'snowhill2')
mandelasprings<- filter(MYtotal, id2 == 'mandelasprings')
easton<- filter(MYtotal, id2 == 'easton')
harmony<- filter(MYtotal, id2 == 'harmony')
newcomb<- filter(MYtotal, id2 == 'newcomb')
galestown<- filter(MYtotal, id2 == 'galestown')
preston2<- filter(MYtotal, id2 == 'preston2')

### find the mean for each 
lennumber = (1:34)
df <- list(id2, lennumber)

lapply(df, cat(meandata(x = id2,  y = "_data <- data.frame((id2[", l= lennumber, k = "]), (mean(", e = "$prcp, na.rm=TRUE)), (mean(", f = "$tmax, na.rm=TRUE)), 
                              (mean(", g = "$tmin, na.rm=TRUE)), (mean(", z = "$tavg, na.rm=TRUE)))"), sep = "\n"))

laplata_data <- data.frame((id2[1]), (mean(laplata$prcp, na.rm=TRUE)), (mean(laplata$tmax, na.rm=TRUE)), 
                           (mean(laplata$tmin, na.rm=TRUE)), (mean(laplata$tavg, na.rm=TRUE)))
kennedyville_data <- data.frame((id2[2]), (mean(kennedyville$prcp, na.rm=TRUE)), (mean(kennedyville$tmax, na.rm=TRUE)), 
                                (mean(kennedyville$tmin, na.rm=TRUE)), (mean(kennedyville$tavg, na.rm=TRUE)))
carney2_data <- data.frame((id2[3]), (mean(carney2$prcp, na.rm=TRUE)), (mean(carney2$tmax, na.rm=TRUE)), 
                           (mean(carney2$tmin, na.rm=TRUE)), (mean(carney2$tavg, na.rm=TRUE)))
carney3_data <- data.frame((id2[4]), (mean(carney3$prcp, na.rm=TRUE)), (mean(carney3$tmax, na.rm=TRUE)), 
                           (mean(carney3$tmin, na.rm=TRUE)), (mean(carney3$tavg, na.rm=TRUE)))
carney1_data <- data.frame((id2[5]), (mean(carney1$prcp, na.rm=TRUE)), (mean(carney1$tmax, na.rm=TRUE)), 
                           (mean(carney1$tmin, na.rm=TRUE)), (mean(carney1$tavg, na.rm=TRUE)))
cockeysville_data <- data.frame((id2[6]), (mean(cockeysville$prcp, na.rm=TRUE)), (mean(cockeysville$tmax, na.rm=TRUE)), 
                                (mean(cockeysville$tmin, na.rm=TRUE)), (mean(cockeysville$tavg, na.rm=TRUE)))
northbesthesda_data <- data.frame((id2[7]), (mean(northbesthesda$prcp, na.rm=TRUE)), (mean(northbesthesda$tmax, na.rm=TRUE)), 
                                  (mean(northbesthesda$tmin, na.rm=TRUE)), (mean(northbesthesda$tavg, na.rm=TRUE)))
thumont_data <- data.frame((id2[8]), (mean(thumont$prcp, na.rm=TRUE)), (mean(thumont$tmax, na.rm=TRUE)), 
                           (mean(thumont$tmin, na.rm=TRUE)), (mean(thumont$tavg, na.rm=TRUE)))
baltimorecounty_data <- data.frame((id2[9]), (mean(baltimorecounty$prcp, na.rm=TRUE)), (mean(baltimorecounty$tmax, na.rm=TRUE)), 
                                   (mean(baltimorecounty$tmin, na.rm=TRUE)), (mean(baltimorecounty$tavg, na.rm=TRUE)))
timomium_data <- data.frame((id2[10]), (mean(timomium$prcp, na.rm=TRUE)), (mean(timomium$tmax, na.rm=TRUE)), 
                            (mean(timomium$tmin, na.rm=TRUE)), (mean(timomium$tavg, na.rm=TRUE)))
silverspring2_data <- data.frame((id2[11]), (mean(silverspring2$prcp, na.rm=TRUE)), (mean(silverspring2$tmax, na.rm=TRUE)), 
                                 (mean(silverspring2$tmin, na.rm=TRUE)), (mean(silverspring2$tavg, na.rm=TRUE)))
owingmills_data <- data.frame((id2[12]), (mean(owingmills$prcp, na.rm=TRUE)), (mean(owingmills$tmax, na.rm=TRUE)), 
                              (mean(owingmills$tmin, na.rm=TRUE)), (mean(owingmills$tavg, na.rm=TRUE)))
lilypons_data <- data.frame((id2[13]), (mean(lilypons$prcp, na.rm=TRUE)), (mean(lilypons$tmax, na.rm=TRUE)), 
                            (mean(lilypons$tmin, na.rm=TRUE)), (mean(lilypons$tavg, na.rm=TRUE)))
townson2_data <- data.frame((id2[14]), (mean(townson2$prcp, na.rm=TRUE)), (mean(townson2$tmax, na.rm=TRUE)), 
                            (mean(townson2$tmin, na.rm=TRUE)), (mean(townson2$tavg, na.rm=TRUE)))
reistertown_data <- data.frame((id2[15]), (mean(reistertown$prcp, na.rm=TRUE)), (mean(reistertown$tmax, na.rm=TRUE)), 
                               (mean(reistertown$tmin, na.rm=TRUE)), (mean(reistertown$tavg, na.rm=TRUE)))
townson1_data <- data.frame((id2[16]), (mean(townson1$prcp, na.rm=TRUE)), (mean(townson1$tmax, na.rm=TRUE)), 
                            (mean(townson1$tmin, na.rm=TRUE)), (mean(townson1$tavg, na.rm=TRUE)))
phoenix_data <- data.frame((id2[17]), (mean(phoenix$prcp, na.rm=TRUE)), (mean(phoenix$tmax, na.rm=TRUE)), 
                           (mean(phoenix$tmin, na.rm=TRUE)), (mean(phoenix$tavg, na.rm=TRUE)))
townson3_data <- data.frame((id2[18]), (mean(townson3$prcp, na.rm=TRUE)), (mean(townson3$tmax, na.rm=TRUE)), 
                            (mean(townson3$tmin, na.rm=TRUE)), (mean(townson3$tavg, na.rm=TRUE)))
gaithersburg_data <- data.frame((id2[19]), (mean(gaithersburg$prcp, na.rm=TRUE)), (mean(gaithersburg$tmax, na.rm=TRUE)), 
                                (mean(gaithersburg$tmin, na.rm=TRUE)), (mean(gaithersburg$tavg, na.rm=TRUE)))
accokeek_data <- data.frame((id2[20]), (mean(accokeek$prcp, na.rm=TRUE)), (mean(accokeek$tmax, na.rm=TRUE)), 
                            (mean(accokeek$tmin, na.rm=TRUE)), (mean(accokeek$tavg, na.rm=TRUE)))
uppermarlboro_data <- data.frame((id2[21]), (mean(uppermarlboro$prcp, na.rm=TRUE)), (mean(uppermarlboro$tmax, na.rm=TRUE)), 
                                 (mean(uppermarlboro$tmin, na.rm=TRUE)), (mean(uppermarlboro$tavg, na.rm=TRUE)))
collegepark_data <- data.frame((id2[22]), (mean(collegepark$prcp, na.rm=TRUE)), (mean(collegepark$tmax, na.rm=TRUE)), 
                               (mean(collegepark$tmin, na.rm=TRUE)), (mean(collegepark$tavg, na.rm=TRUE)))
rosedale_data <- data.frame((id2[23]), (mean(rosedale$prcp, na.rm=TRUE)), (mean(rosedale$tmax, na.rm=TRUE)), 
                            (mean(rosedale$tmin, na.rm=TRUE)), (mean(rosedale$tavg, na.rm=TRUE)))
preston1_data <- data.frame((id2[24]), (mean(preston1$prcp, na.rm=TRUE)), (mean(preston1$tmax, na.rm=TRUE)), 
                            (mean(preston1$tmin, na.rm=TRUE)), (mean(preston1$tavg, na.rm=TRUE)))
waddellscorner_data <- data.frame((id2[25]), (mean(waddellscorner$prcp, na.rm=TRUE)), (mean(waddellscorner$tmax, na.rm=TRUE)), 
                                  (mean(waddellscorner$tmin, na.rm=TRUE)), (mean(waddellscorner$tavg, na.rm=TRUE)))
eastnewmarket_data <- data.frame((id2[26]), (mean(eastnewmarket$prcp, na.rm=TRUE)), (mean(eastnewmarket$tmax, na.rm=TRUE)), 
                                 (mean(eastnewmarket$tmin, na.rm=TRUE)), (mean(eastnewmarket$tavg, na.rm=TRUE)))
snowhill1_data <- data.frame((id2[27]), (mean(snowhill1$prcp, na.rm=TRUE)), (mean(snowhill1$tmax, na.rm=TRUE)), 
                             (mean(snowhill1$tmin, na.rm=TRUE)), (mean(snowhill1$tavg, na.rm=TRUE)))
snowhill2_data <- data.frame((id2[28]), (mean(snowhill2$prcp, na.rm=TRUE)), (mean(snowhill2$tmax, na.rm=TRUE)), 
                             (mean(snowhill2$tmin, na.rm=TRUE)), (mean(snowhill2$tavg, na.rm=TRUE)))
mandelasprings_data <- data.frame((id2[29]), (mean(mandelasprings$prcp, na.rm=TRUE)), (mean(mandelasprings$tmax, na.rm=TRUE)), 
                                  (mean(mandelasprings$tmin, na.rm=TRUE)), (mean(mandelasprings$tavg, na.rm=TRUE)))
easton_data <- data.frame((id2[30]), (mean(easton$prcp, na.rm=TRUE)), (mean(easton$tmax, na.rm=TRUE)), 
                          (mean(easton$tmin, na.rm=TRUE)), (mean(easton$tavg, na.rm=TRUE)))
harmony_data <- data.frame((id2[31]), (mean(harmony$prcp, na.rm=TRUE)), (mean(harmony$tmax, na.rm=TRUE)), 
                           (mean(harmony$tmin, na.rm=TRUE)), (mean(harmony$tavg, na.rm=TRUE)))
newcomb_data <- data.frame((id2[32]), (mean(newcomb$prcp, na.rm=TRUE)), (mean(newcomb$tmax, na.rm=TRUE)), 
                           (mean(newcomb$tmin, na.rm=TRUE)), (mean(newcomb$tavg, na.rm=TRUE)))
galestown_data <- data.frame((id2[33]), (mean(galestown$prcp, na.rm=TRUE)), (mean(galestown$tmax, na.rm=TRUE)), 
                             (mean(galestown$tmin, na.rm=TRUE)), (mean(galestown$tavg, na.rm=TRUE)))
preston2_data <- data.frame((id2[34]), (mean(preston2$prcp, na.rm=TRUE)), (mean(preston2$tmax, na.rm=TRUE)), 
                            (mean(preston2$tmin, na.rm=TRUE)), (mean(preston2$tavg, na.rm=TRUE)))


#column names 
cat(collnames(unique(id2), y = "colnames(", z = "_data) <- c('id', 'prcp_avg','tmax_avg', 'tmin_avg', 'tavg_avg')"), sep = "\n")

colnames(laplata_data) <- c('id', 'prcp_avg','tmax_avg', 'tmin_avg', 'tavg_avg')
colnames(kennedyville_data) <- c('id', 'prcp_avg','tmax_avg', 'tmin_avg', 'tavg_avg')
colnames(carney2_data) <- c('id', 'prcp_avg','tmax_avg', 'tmin_avg', 'tavg_avg')
colnames(carney3_data) <- c('id', 'prcp_avg','tmax_avg', 'tmin_avg', 'tavg_avg')
colnames(carney1_data) <- c('id', 'prcp_avg','tmax_avg', 'tmin_avg', 'tavg_avg')
colnames(cockeysville_data) <- c('id', 'prcp_avg','tmax_avg', 'tmin_avg', 'tavg_avg')
colnames(northbesthesda_data) <- c('id', 'prcp_avg','tmax_avg', 'tmin_avg', 'tavg_avg')
colnames(thumont_data) <- c('id', 'prcp_avg','tmax_avg', 'tmin_avg', 'tavg_avg')
colnames(baltimorecounty_data) <- c('id', 'prcp_avg','tmax_avg', 'tmin_avg', 'tavg_avg')
colnames(timomium_data) <- c('id', 'prcp_avg','tmax_avg', 'tmin_avg', 'tavg_avg')
colnames(silverspring2_data) <- c('id', 'prcp_avg','tmax_avg', 'tmin_avg', 'tavg_avg')
colnames(owingmills_data) <- c('id', 'prcp_avg','tmax_avg', 'tmin_avg', 'tavg_avg')
colnames(lilypons_data) <- c('id', 'prcp_avg','tmax_avg', 'tmin_avg', 'tavg_avg')
colnames(townson2_data) <- c('id', 'prcp_avg','tmax_avg', 'tmin_avg', 'tavg_avg')
colnames(reistertown_data) <- c('id', 'prcp_avg','tmax_avg', 'tmin_avg', 'tavg_avg')
colnames(townson1_data) <- c('id', 'prcp_avg','tmax_avg', 'tmin_avg', 'tavg_avg')
colnames(phoenix_data) <- c('id', 'prcp_avg','tmax_avg', 'tmin_avg', 'tavg_avg')
colnames(townson3_data) <- c('id', 'prcp_avg','tmax_avg', 'tmin_avg', 'tavg_avg')
colnames(gaithersburg_data) <- c('id', 'prcp_avg','tmax_avg', 'tmin_avg', 'tavg_avg')
colnames(accokeek_data) <- c('id', 'prcp_avg','tmax_avg', 'tmin_avg', 'tavg_avg')
colnames(uppermarlboro_data) <- c('id', 'prcp_avg','tmax_avg', 'tmin_avg', 'tavg_avg')
colnames(collegepark_data) <- c('id', 'prcp_avg','tmax_avg', 'tmin_avg', 'tavg_avg')
colnames(rosedale_data) <- c('id', 'prcp_avg','tmax_avg', 'tmin_avg', 'tavg_avg')
colnames(preston1_data) <- c('id', 'prcp_avg','tmax_avg', 'tmin_avg', 'tavg_avg')
colnames(waddellscorner_data) <- c('id', 'prcp_avg','tmax_avg', 'tmin_avg', 'tavg_avg')
colnames(eastnewmarket_data) <- c('id', 'prcp_avg','tmax_avg', 'tmin_avg', 'tavg_avg')
colnames(snowhill1_data) <- c('id', 'prcp_avg','tmax_avg', 'tmin_avg', 'tavg_avg')
colnames(snowhill2_data) <- c('id', 'prcp_avg','tmax_avg', 'tmin_avg', 'tavg_avg')
colnames(mandelasprings_data) <- c('id', 'prcp_avg','tmax_avg', 'tmin_avg', 'tavg_avg')
colnames(easton_data) <- c('id', 'prcp_avg','tmax_avg', 'tmin_avg', 'tavg_avg')
colnames(harmony_data) <- c('id', 'prcp_avg','tmax_avg', 'tmin_avg', 'tavg_avg')
colnames(newcomb_data) <- c('id', 'prcp_avg','tmax_avg', 'tmin_avg', 'tavg_avg')
colnames(galestown_data) <- c('id', 'prcp_avg','tmax_avg', 'tmin_avg', 'tavg_avg')
colnames(preston2_data) <- c('id', 'prcp_avg','tmax_avg', 'tmin_avg', 'tavg_avg')

##paste into bind_rows()
cat(bindrowsfun(unique(id2), y = "_data", z = ","), sep = "")
MYtotal_obs <- bind_rows(laplata_data,kennedyville_data,carney2_data,carney3_data,carney1_data,cockeysville_data,northbesthesda_data,thumont_data,baltimorecounty_data,timomium_data,silverspring2_data,owingmills_data,lilypons_data,townson2_data,reistertown_data,townson1_data,phoenix_data,townson3_data,gaithersburg_data,accokeek_data,uppermarlboro_data,collegepark_data,rosedale_data,preston1_data,waddellscorner_data,eastnewmarket_data,snowhill1_data,snowhill2_data,mandelasprings_data,easton_data,harmony_data,newcomb_data,galestown_data,preston2_data)



### save data as xlsx file 
write.xlsx(MYtotal_obs, file = "MYtotal_obs.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)

########### Utah Nevada #############

UNtotal <- read.csv("UN_data_total_weather.csv")

##make prcp, tmax, tmin, and tavg numeric

UNtotal$prcp <- as.numeric(UNtotal$prcp)
UNtotal$tmax <- as.numeric(UNtotal$tmax)
UNtotal$tmin <- as.numeric(UNtotal$tmin)
UNtotal$tavg <- as.numeric(UNtotal$tavg)

## multiply by 0.1 or divide by 10 to make 1 degree 

UNtotal$prcp <- UNtotal$prcp/10
UNtotal$tmax <- UNtotal$tmax/10
UNtotal$tmin <- UNtotal$tmin/10
UNtotal$tavg <- UNtotal$tavg/10

#pull data 
id2 <- unique(UNtotal$id2)
cat(filtering(id2, y =  "<- filter(UNtotal, id2 == '", z = "')"), sep = "\n")

springville2<- filter(UNtotal, id2 == 'springville2')
provo3<- filter(UNtotal, id2 == 'provo3')
welcome<- filter(UNtotal, id2 == 'welcome')
northlogan1<- filter(UNtotal, id2 == 'northlogan1')
provo2<- filter(UNtotal, id2 == 'provo2')
millcreek<- filter(UNtotal, id2 == 'millcreek')
cache<- filter(UNtotal, id2 == 'cache')
provo1<- filter(UNtotal, id2 == 'provo1')
fallon<- filter(UNtotal, id2 == 'fallon')
tooele<- filter(UNtotal, id2 == 'tooele')
moab<- filter(UNtotal, id2 == 'moab')
saltlakecity4<- filter(UNtotal, id2 == 'saltlakecity4')
herriman<- filter(UNtotal, id2 == 'herriman')
saltlakecity2<- filter(UNtotal, id2 == 'saltlakecity2')
eureka2<- filter(UNtotal, id2 == 'eureka2')
eureka3<- filter(UNtotal, id2 == 'eureka3')
northlogan2<- filter(UNtotal, id2 == 'northlogan2')
pleasantgrove<- filter(UNtotal, id2 == 'pleasantgrove')
eureka1<- filter(UNtotal, id2 == 'eureka1')
springville1<- filter(UNtotal, id2 == 'springville1')


### find the mean for each 
lennumber = (1:20)
df <- list(id2, lennumber)

lapply(df, cat(meandata(x = id2,  y = "_data <- data.frame((id2[", l= lennumber, k = "]), (mean(", e = "$prcp, na.rm=TRUE)), (mean(", f = "$tmax, na.rm=TRUE)), 
                              (mean(", g = "$tmin, na.rm=TRUE)), (mean(", z = "$tavg, na.rm=TRUE)))"), sep = "\n"))

springville2_data <- data.frame((id2[1]), (mean(springville2$prcp, na.rm=TRUE)), (mean(springville2$tmax, na.rm=TRUE)), 
                                (mean(springville2$tmin, na.rm=TRUE)), (mean(springville2$tavg, na.rm=TRUE)))
provo3_data <- data.frame((id2[2]), (mean(provo3$prcp, na.rm=TRUE)), (mean(provo3$tmax, na.rm=TRUE)), 
                          (mean(provo3$tmin, na.rm=TRUE)), (mean(provo3$tavg, na.rm=TRUE)))
welcome_data <- data.frame((id2[3]), (mean(welcome$prcp, na.rm=TRUE)), (mean(welcome$tmax, na.rm=TRUE)), 
                           (mean(welcome$tmin, na.rm=TRUE)), (mean(welcome$tavg, na.rm=TRUE)))
northlogan1_data <- data.frame((id2[4]), (mean(northlogan1$prcp, na.rm=TRUE)), (mean(northlogan1$tmax, na.rm=TRUE)), 
                               (mean(northlogan1$tmin, na.rm=TRUE)), (mean(northlogan1$tavg, na.rm=TRUE)))
provo2_data <- data.frame((id2[5]), (mean(provo2$prcp, na.rm=TRUE)), (mean(provo2$tmax, na.rm=TRUE)), 
                          (mean(provo2$tmin, na.rm=TRUE)), (mean(provo2$tavg, na.rm=TRUE)))
millcreek_data <- data.frame((id2[6]), (mean(millcreek$prcp, na.rm=TRUE)), (mean(millcreek$tmax, na.rm=TRUE)), 
                             (mean(millcreek$tmin, na.rm=TRUE)), (mean(millcreek$tavg, na.rm=TRUE)))
cache_data <- data.frame((id2[7]), (mean(cache$prcp, na.rm=TRUE)), (mean(cache$tmax, na.rm=TRUE)), 
                         (mean(cache$tmin, na.rm=TRUE)), (mean(cache$tavg, na.rm=TRUE)))
provo1_data <- data.frame((id2[8]), (mean(provo1$prcp, na.rm=TRUE)), (mean(provo1$tmax, na.rm=TRUE)), 
                          (mean(provo1$tmin, na.rm=TRUE)), (mean(provo1$tavg, na.rm=TRUE)))
fallon_data <- data.frame((id2[9]), (mean(fallon$prcp, na.rm=TRUE)), (mean(fallon$tmax, na.rm=TRUE)), 
                          (mean(fallon$tmin, na.rm=TRUE)), (mean(fallon$tavg, na.rm=TRUE)))
tooele_data <- data.frame((id2[10]), (mean(tooele$prcp, na.rm=TRUE)), (mean(tooele$tmax, na.rm=TRUE)), 
                          (mean(tooele$tmin, na.rm=TRUE)), (mean(tooele$tavg, na.rm=TRUE)))
moab_data <- data.frame((id2[11]), (mean(moab$prcp, na.rm=TRUE)), (mean(moab$tmax, na.rm=TRUE)), 
                        (mean(moab$tmin, na.rm=TRUE)), (mean(moab$tavg, na.rm=TRUE)))
saltlakecity4_data <- data.frame((id2[12]), (mean(saltlakecity4$prcp, na.rm=TRUE)), (mean(saltlakecity4$tmax, na.rm=TRUE)), 
                                 (mean(saltlakecity4$tmin, na.rm=TRUE)), (mean(saltlakecity4$tavg, na.rm=TRUE)))
herriman_data <- data.frame((id2[13]), (mean(herriman$prcp, na.rm=TRUE)), (mean(herriman$tmax, na.rm=TRUE)), 
                            (mean(herriman$tmin, na.rm=TRUE)), (mean(herriman$tavg, na.rm=TRUE)))
saltlakecity2_data <- data.frame((id2[14]), (mean(saltlakecity2$prcp, na.rm=TRUE)), (mean(saltlakecity2$tmax, na.rm=TRUE)), 
                                 (mean(saltlakecity2$tmin, na.rm=TRUE)), (mean(saltlakecity2$tavg, na.rm=TRUE)))
eureka2_data <- data.frame((id2[15]), (mean(eureka2$prcp, na.rm=TRUE)), (mean(eureka2$tmax, na.rm=TRUE)), 
                           (mean(eureka2$tmin, na.rm=TRUE)), (mean(eureka2$tavg, na.rm=TRUE)))
eureka3_data <- data.frame((id2[16]), (mean(eureka3$prcp, na.rm=TRUE)), (mean(eureka3$tmax, na.rm=TRUE)), 
                           (mean(eureka3$tmin, na.rm=TRUE)), (mean(eureka3$tavg, na.rm=TRUE)))
northlogan2_data <- data.frame((id2[17]), (mean(northlogan2$prcp, na.rm=TRUE)), (mean(northlogan2$tmax, na.rm=TRUE)), 
                               (mean(northlogan2$tmin, na.rm=TRUE)), (mean(northlogan2$tavg, na.rm=TRUE)))
pleasantgrove_data <- data.frame((id2[18]), (mean(pleasantgrove$prcp, na.rm=TRUE)), (mean(pleasantgrove$tmax, na.rm=TRUE)), 
                                 (mean(pleasantgrove$tmin, na.rm=TRUE)), (mean(pleasantgrove$tavg, na.rm=TRUE)))
eureka1_data <- data.frame((id2[19]), (mean(eureka1$prcp, na.rm=TRUE)), (mean(eureka1$tmax, na.rm=TRUE)), 
                           (mean(eureka1$tmin, na.rm=TRUE)), (mean(eureka1$tavg, na.rm=TRUE)))
springville1_data <- data.frame((id2[20]), (mean(springville1$prcp, na.rm=TRUE)), (mean(springville1$tmax, na.rm=TRUE)), 
                                (mean(springville1$tmin, na.rm=TRUE)), (mean(springville1$tavg, na.rm=TRUE)))


#column names 
cat(collnames(unique(id2), y = "colnames(", z = "_data) <- c('id', 'prcp_avg','tmax_avg', 'tmin_avg', 'tavg_avg')"), sep = "\n")

colnames(springville2_data) <- c('id', 'prcp_avg','tmax_avg', 'tmin_avg', 'tavg_avg')
colnames(provo3_data) <- c('id', 'prcp_avg','tmax_avg', 'tmin_avg', 'tavg_avg')
colnames(welcome_data) <- c('id', 'prcp_avg','tmax_avg', 'tmin_avg', 'tavg_avg')
colnames(northlogan1_data) <- c('id', 'prcp_avg','tmax_avg', 'tmin_avg', 'tavg_avg')
colnames(provo2_data) <- c('id', 'prcp_avg','tmax_avg', 'tmin_avg', 'tavg_avg')
colnames(millcreek_data) <- c('id', 'prcp_avg','tmax_avg', 'tmin_avg', 'tavg_avg')
colnames(cache_data) <- c('id', 'prcp_avg','tmax_avg', 'tmin_avg', 'tavg_avg')
colnames(provo1_data) <- c('id', 'prcp_avg','tmax_avg', 'tmin_avg', 'tavg_avg')
colnames(fallon_data) <- c('id', 'prcp_avg','tmax_avg', 'tmin_avg', 'tavg_avg')
colnames(tooele_data) <- c('id', 'prcp_avg','tmax_avg', 'tmin_avg', 'tavg_avg')
colnames(moab_data) <- c('id', 'prcp_avg','tmax_avg', 'tmin_avg', 'tavg_avg')
colnames(saltlakecity4_data) <- c('id', 'prcp_avg','tmax_avg', 'tmin_avg', 'tavg_avg')
colnames(herriman_data) <- c('id', 'prcp_avg','tmax_avg', 'tmin_avg', 'tavg_avg')
colnames(saltlakecity2_data) <- c('id', 'prcp_avg','tmax_avg', 'tmin_avg', 'tavg_avg')
colnames(eureka2_data) <- c('id', 'prcp_avg','tmax_avg', 'tmin_avg', 'tavg_avg')
colnames(eureka3_data) <- c('id', 'prcp_avg','tmax_avg', 'tmin_avg', 'tavg_avg')
colnames(northlogan2_data) <- c('id', 'prcp_avg','tmax_avg', 'tmin_avg', 'tavg_avg')
colnames(pleasantgrove_data) <- c('id', 'prcp_avg','tmax_avg', 'tmin_avg', 'tavg_avg')
colnames(eureka1_data) <- c('id', 'prcp_avg','tmax_avg', 'tmin_avg', 'tavg_avg')
colnames(springville1_data) <- c('id', 'prcp_avg','tmax_avg', 'tmin_avg', 'tavg_avg')

##paste into bind_rows()
cat(bindrowsfun(unique(id2), y = "_data", z = ","), sep = "")
UNtotal_obs <- bind_rows(springville2_data,provo3_data,welcome_data,northlogan1_data,provo2_data,millcreek_data,cache_data,provo1_data,fallon_data,tooele_data,moab_data,saltlakecity4_data,herriman_data,saltlakecity2_data,eureka2_data,eureka3_data,northlogan2_data,pleasantgrove_data,eureka1_data,springville1_data)



### save data as xlsx file 
write.xlsx(UNtotal_obs, file = "UNtotal_obs.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)


######### PNW ##########

PNWtotal <- read.csv("PNW_data_total_weather.csv")

##make prcp, tmax, tmin, and tavg numeric

PNWtotal$prcp <- as.numeric(PNWtotal$prcp)
PNWtotal$tmax <- as.numeric(PNWtotal$tmax)
PNWtotal$tmin <- as.numeric(PNWtotal$tmin)
PNWtotal$tavg <- as.numeric(PNWtotal$tavg)

## multiply by 0.1 or divide by 10 to make 1 degree 

PNWtotal$prcp <- PNWtotal$prcp/10
PNWtotal$tmax <- PNWtotal$tmax/10
PNWtotal$tmin <- PNWtotal$tmin/10
PNWtotal$tavg <- PNWtotal$tavg/10

#pull data 
id2 <- unique(PNWtotal$id2)
cat(filtering(id2, y =  "<- filter(PNWtotal, id2 == '", z = "')"), sep = "\n")

browkenbow<- filter(PNWtotal, id2 == 'browkenbow')
corvallis<- filter(PNWtotal, id2 == 'corvallis')
pocatello<- filter(PNWtotal, id2 == 'pocatello')
caldwell<- filter(PNWtotal, id2 == 'caldwell')
wallawalla<- filter(PNWtotal, id2 == 'wallawalla')
pullman<- filter(PNWtotal, id2 == 'pullman')
cheney<- filter(PNWtotal, id2 == 'cheney')
anacortes<- filter(PNWtotal, id2 == 'anacortes')
chadron5<- filter(PNWtotal, id2 == 'chadron5')
almotacreek<- filter(PNWtotal, id2 == 'almotacreek')
alliance<- filter(PNWtotal, id2 == 'alliance')
gering<- filter(PNWtotal, id2 == 'gering')
chadron4<- filter(PNWtotal, id2 == 'chadron4')
chadron2<- filter(PNWtotal, id2 == 'chadron2')
albin<- filter(PNWtotal, id2 == 'albin')
crawford<- filter(PNWtotal, id2 == 'crawford')
hastings<- filter(PNWtotal, id2 == 'hastings')
chadron1<- filter(PNWtotal, id2 == 'chadron1')
mullen1<- filter(PNWtotal, id2 == 'mullen1')
bannercounty<- filter(PNWtotal, id2 == 'bannercounty')
mullen2<- filter(PNWtotal, id2 == 'mullen2')
longpine<- filter(PNWtotal, id2 == 'longpine')
littlebluetownship<- filter(PNWtotal, id2 == 'littlebluetownship')
jerome<- filter(PNWtotal, id2 == 'jerome')
chadron3<- filter(PNWtotal, id2 == 'chadron3')
glasgow<- filter(PNWtotal, id2 == 'glasgow')

### find the mean for each 
lennumber = (1:26)
df <- list(id2, lennumber)

lapply(df, cat(meandata(x = id2,  y = "_data <- data.frame((id2[", l= lennumber, k = "]), (mean(", e = "$prcp, na.rm=TRUE)), (mean(", f = "$tmax, na.rm=TRUE)), 
                              (mean(", g = "$tmin, na.rm=TRUE)), (mean(", z = "$tavg, na.rm=TRUE)))"), sep = "\n"))

browkenbow_data <- data.frame((id2[1]), (mean(browkenbow$prcp, na.rm=TRUE)), (mean(browkenbow$tmax, na.rm=TRUE)), 
                              (mean(browkenbow$tmin, na.rm=TRUE)), (mean(browkenbow$tavg, na.rm=TRUE)))
corvallis_data <- data.frame((id2[2]), (mean(corvallis$prcp, na.rm=TRUE)), (mean(corvallis$tmax, na.rm=TRUE)), 
                             (mean(corvallis$tmin, na.rm=TRUE)), (mean(corvallis$tavg, na.rm=TRUE)))
pocatello_data <- data.frame((id2[3]), (mean(pocatello$prcp, na.rm=TRUE)), (mean(pocatello$tmax, na.rm=TRUE)), 
                             (mean(pocatello$tmin, na.rm=TRUE)), (mean(pocatello$tavg, na.rm=TRUE)))
caldwell_data <- data.frame((id2[4]), (mean(caldwell$prcp, na.rm=TRUE)), (mean(caldwell$tmax, na.rm=TRUE)), 
                            (mean(caldwell$tmin, na.rm=TRUE)), (mean(caldwell$tavg, na.rm=TRUE)))
wallawalla_data <- data.frame((id2[5]), (mean(wallawalla$prcp, na.rm=TRUE)), (mean(wallawalla$tmax, na.rm=TRUE)), 
                              (mean(wallawalla$tmin, na.rm=TRUE)), (mean(wallawalla$tavg, na.rm=TRUE)))
pullman_data <- data.frame((id2[6]), (mean(pullman$prcp, na.rm=TRUE)), (mean(pullman$tmax, na.rm=TRUE)), 
                           (mean(pullman$tmin, na.rm=TRUE)), (mean(pullman$tavg, na.rm=TRUE)))
cheney_data <- data.frame((id2[7]), (mean(cheney$prcp, na.rm=TRUE)), (mean(cheney$tmax, na.rm=TRUE)), 
                          (mean(cheney$tmin, na.rm=TRUE)), (mean(cheney$tavg, na.rm=TRUE)))
anacortes_data <- data.frame((id2[8]), (mean(anacortes$prcp, na.rm=TRUE)), (mean(anacortes$tmax, na.rm=TRUE)), 
                             (mean(anacortes$tmin, na.rm=TRUE)), (mean(anacortes$tavg, na.rm=TRUE)))
chadron5_data <- data.frame((id2[9]), (mean(chadron5$prcp, na.rm=TRUE)), (mean(chadron5$tmax, na.rm=TRUE)), 
                            (mean(chadron5$tmin, na.rm=TRUE)), (mean(chadron5$tavg, na.rm=TRUE)))
almotacreek_data <- data.frame((id2[10]), (mean(almotacreek$prcp, na.rm=TRUE)), (mean(almotacreek$tmax, na.rm=TRUE)), 
                               (mean(almotacreek$tmin, na.rm=TRUE)), (mean(almotacreek$tavg, na.rm=TRUE)))
alliance_data <- data.frame((id2[11]), (mean(alliance$prcp, na.rm=TRUE)), (mean(alliance$tmax, na.rm=TRUE)), 
                            (mean(alliance$tmin, na.rm=TRUE)), (mean(alliance$tavg, na.rm=TRUE)))
gering_data <- data.frame((id2[12]), (mean(gering$prcp, na.rm=TRUE)), (mean(gering$tmax, na.rm=TRUE)), 
                          (mean(gering$tmin, na.rm=TRUE)), (mean(gering$tavg, na.rm=TRUE)))
chadron4_data <- data.frame((id2[13]), (mean(chadron4$prcp, na.rm=TRUE)), (mean(chadron4$tmax, na.rm=TRUE)), 
                            (mean(chadron4$tmin, na.rm=TRUE)), (mean(chadron4$tavg, na.rm=TRUE)))
chadron2_data <- data.frame((id2[14]), (mean(chadron2$prcp, na.rm=TRUE)), (mean(chadron2$tmax, na.rm=TRUE)), 
                            (mean(chadron2$tmin, na.rm=TRUE)), (mean(chadron2$tavg, na.rm=TRUE)))
albin_data <- data.frame((id2[15]), (mean(albin$prcp, na.rm=TRUE)), (mean(albin$tmax, na.rm=TRUE)), 
                         (mean(albin$tmin, na.rm=TRUE)), (mean(albin$tavg, na.rm=TRUE)))
crawford_data <- data.frame((id2[16]), (mean(crawford$prcp, na.rm=TRUE)), (mean(crawford$tmax, na.rm=TRUE)), 
                            (mean(crawford$tmin, na.rm=TRUE)), (mean(crawford$tavg, na.rm=TRUE)))
hastings_data <- data.frame((id2[17]), (mean(hastings$prcp, na.rm=TRUE)), (mean(hastings$tmax, na.rm=TRUE)), 
                            (mean(hastings$tmin, na.rm=TRUE)), (mean(hastings$tavg, na.rm=TRUE)))
chadron1_data <- data.frame((id2[18]), (mean(chadron1$prcp, na.rm=TRUE)), (mean(chadron1$tmax, na.rm=TRUE)), 
                            (mean(chadron1$tmin, na.rm=TRUE)), (mean(chadron1$tavg, na.rm=TRUE)))
mullen1_data <- data.frame((id2[19]), (mean(mullen1$prcp, na.rm=TRUE)), (mean(mullen1$tmax, na.rm=TRUE)), 
                           (mean(mullen1$tmin, na.rm=TRUE)), (mean(mullen1$tavg, na.rm=TRUE)))
bannercounty_data <- data.frame((id2[20]), (mean(bannercounty$prcp, na.rm=TRUE)), (mean(bannercounty$tmax, na.rm=TRUE)), 
                                (mean(bannercounty$tmin, na.rm=TRUE)), (mean(bannercounty$tavg, na.rm=TRUE)))
mullen2_data <- data.frame((id2[21]), (mean(mullen2$prcp, na.rm=TRUE)), (mean(mullen2$tmax, na.rm=TRUE)), 
                           (mean(mullen2$tmin, na.rm=TRUE)), (mean(mullen2$tavg, na.rm=TRUE)))
longpine_data <- data.frame((id2[22]), (mean(longpine$prcp, na.rm=TRUE)), (mean(longpine$tmax, na.rm=TRUE)), 
                            (mean(longpine$tmin, na.rm=TRUE)), (mean(longpine$tavg, na.rm=TRUE)))
littlebluetownship_data <- data.frame((id2[23]), (mean(littlebluetownship$prcp, na.rm=TRUE)), (mean(littlebluetownship$tmax, na.rm=TRUE)), 
                                      (mean(littlebluetownship$tmin, na.rm=TRUE)), (mean(littlebluetownship$tavg, na.rm=TRUE)))
jerome_data <- data.frame((id2[24]), (mean(jerome$prcp, na.rm=TRUE)), (mean(jerome$tmax, na.rm=TRUE)), 
                          (mean(jerome$tmin, na.rm=TRUE)), (mean(jerome$tavg, na.rm=TRUE)))
chadron3_data <- data.frame((id2[25]), (mean(chadron3$prcp, na.rm=TRUE)), (mean(chadron3$tmax, na.rm=TRUE)), 
                            (mean(chadron3$tmin, na.rm=TRUE)), (mean(chadron3$tavg, na.rm=TRUE)))
glasgow_data <- data.frame((id2[26]), (mean(glasgow$prcp, na.rm=TRUE)), (mean(glasgow$tmax, na.rm=TRUE)), 
                           (mean(glasgow$tmin, na.rm=TRUE)), (mean(glasgow$tavg, na.rm=TRUE)))

#column names 
cat(collnames(unique(id2), y = "colnames(", z = "_data) <- c('id', 'prcp_avg','tmax_avg', 'tmin_avg', 'tavg_avg')"), sep = "\n")

colnames(browkenbow_data) <- c('id', 'prcp_avg','tmax_avg', 'tmin_avg', 'tavg_avg')
colnames(corvallis_data) <- c('id', 'prcp_avg','tmax_avg', 'tmin_avg', 'tavg_avg')
colnames(pocatello_data) <- c('id', 'prcp_avg','tmax_avg', 'tmin_avg', 'tavg_avg')
colnames(caldwell_data) <- c('id', 'prcp_avg','tmax_avg', 'tmin_avg', 'tavg_avg')
colnames(wallawalla_data) <- c('id', 'prcp_avg','tmax_avg', 'tmin_avg', 'tavg_avg')
colnames(pullman_data) <- c('id', 'prcp_avg','tmax_avg', 'tmin_avg', 'tavg_avg')
colnames(cheney_data) <- c('id', 'prcp_avg','tmax_avg', 'tmin_avg', 'tavg_avg')
colnames(anacortes_data) <- c('id', 'prcp_avg','tmax_avg', 'tmin_avg', 'tavg_avg')
colnames(chadron5_data) <- c('id', 'prcp_avg','tmax_avg', 'tmin_avg', 'tavg_avg')
colnames(almotacreek_data) <- c('id', 'prcp_avg','tmax_avg', 'tmin_avg', 'tavg_avg')
colnames(alliance_data) <- c('id', 'prcp_avg','tmax_avg', 'tmin_avg', 'tavg_avg')
colnames(gering_data) <- c('id', 'prcp_avg','tmax_avg', 'tmin_avg', 'tavg_avg')
colnames(chadron4_data) <- c('id', 'prcp_avg','tmax_avg', 'tmin_avg', 'tavg_avg')
colnames(chadron2_data) <- c('id', 'prcp_avg','tmax_avg', 'tmin_avg', 'tavg_avg')
colnames(albin_data) <- c('id', 'prcp_avg','tmax_avg', 'tmin_avg', 'tavg_avg')
colnames(crawford_data) <- c('id', 'prcp_avg','tmax_avg', 'tmin_avg', 'tavg_avg')
colnames(hastings_data) <- c('id', 'prcp_avg','tmax_avg', 'tmin_avg', 'tavg_avg')
colnames(chadron1_data) <- c('id', 'prcp_avg','tmax_avg', 'tmin_avg', 'tavg_avg')
colnames(mullen1_data) <- c('id', 'prcp_avg','tmax_avg', 'tmin_avg', 'tavg_avg')
colnames(bannercounty_data) <- c('id', 'prcp_avg','tmax_avg', 'tmin_avg', 'tavg_avg')
colnames(mullen2_data) <- c('id', 'prcp_avg','tmax_avg', 'tmin_avg', 'tavg_avg')
colnames(longpine_data) <- c('id', 'prcp_avg','tmax_avg', 'tmin_avg', 'tavg_avg')
colnames(littlebluetownship_data) <- c('id', 'prcp_avg','tmax_avg', 'tmin_avg', 'tavg_avg')
colnames(jerome_data) <- c('id', 'prcp_avg','tmax_avg', 'tmin_avg', 'tavg_avg')
colnames(chadron3_data) <- c('id', 'prcp_avg','tmax_avg', 'tmin_avg', 'tavg_avg')
colnames(glasgow_data) <- c('id', 'prcp_avg','tmax_avg', 'tmin_avg', 'tavg_avg')

##paste into bind_rows()
cat(bindrowsfun(unique(id2), y = "_data", z = ","), sep = "")
PNWtotal_obs <- bind_rows(browkenbow_data,corvallis_data,pocatello_data,caldwell_data,wallawalla_data,pullman_data,cheney_data,anacortes_data,chadron5_data,almotacreek_data,alliance_data,gering_data,chadron4_data,chadron2_data,albin_data,crawford_data,hastings_data,chadron1_data,mullen1_data,bannercounty_data,mullen2_data,longpine_data,littlebluetownship_data,jerome_data,chadron3_data,glasgow_data)

### save data as xlsx file 
write.xlsx(PNWtotal_obs, file = "PNWtotal_obs.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)















