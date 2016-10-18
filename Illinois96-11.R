#############################################################################
#############ILLINOIS 1996-2011 ANALYSIS with Daymet GDD data####################
#############################################################################

#load jagsUI library
library(jagsUI)
library(reshape)

#set working directory
setwd("C:/Users/Sarah/Desktop/Monarch Post-doc/IL 96-11")

#read in data 
mon = read.csv("SurveyData_IL96to11.csv", header=TRUE,sep=',',na.strings=T)
mon[1:10,]
names(mon)
str(mon)

#get rid of years outside 1996-2011 in survey data file
dim(mon) #9234,12
sort(unique(mon$Year))
mon<-mon[!mon$Year %in% c(1987,1988,1989,1990,1991,1992,1993,1994,1995,2012,2013),]
dim(mon) #7433, 12

#get rid of least surveyed sites
mon<-mon[!mon$SiteID %in% c(2,3,7,9,16,47,53,67,83,106,115,117,129,141,146,159,163,171,172,222,224,662,663,743,1001,1002,1237,1261,1273,1289,2220,12313,943,72,1272,1,4,24,28,58,145,177,209,1284,1303,2012,12318,49,93,116,149,175,186,612,1902,12316,50,71,87,443,1233,1982,1983,5,157,185,202,1042,1271,30,1410,1430,96,108,211,262,820,1542,60,78,134,167,183,661,1286,12314,39,442,77,251,945,174,189,207,1231,137,151,1792,12,125,1236,1262,1302,12312,12317,68,99,135,1183,1234,54,162,181,611,742,12315),]
dim(mon) #6506, 12

mon$PDSIwk <- as.numeric(as.character(mon$PDSIwk))
mon$GDDwk10NEW <- as.numeric(as.character(mon$GDDwk10NEW))
mon$GDDavgNEW <- as.numeric(as.character(mon$GDDavgNEW))
mon$GDDdiffNEW <- as.numeric(as.character(mon$GDDdiffNEW))
str(mon)

#Look at data (checking all years/weeks/sites present)
uyear = sort(unique(mon$Year))
#1996-2011
uweek= sort(unique(mon$Week))
#5-35
usite = sort(unique(mon$SiteID))
#total sites (246/133). Note numeric order!

#Reshape the count data.
#NOTE: This assumes columns called Year, Week, SiteID and SumofMonarch count
#Does not need to be in any specific order
junk.melt=melt(mon,id.var=c("Year", "Week", "SiteID"), measure.var="SumOfMonarch.count")
monarchs=cast(junk.melt, SiteID ~ Week ~ Year, sum)
#This organizes into tables that are sums of counts according to Week by SiteID for each year
#Checked and looks right

#need to put in NAs when site wasn't surveyed a given week of a given year (0 in Excel file)
#If surveyed in a given week, insert count (including 0)
#NOTE!! This assumes that sites are organized numerically across columns! Was necessary to edit Excel file
#Refer to monarchs data frame to make sure order of sites matches order shown in monarchs
dates=read.csv("Weeks_IL96to11.csv", header=T,sep=',',na.strings=T) 
dates=read.csv("Weeks_IL96to11_Pruned1.csv", header=T,sep=',',na.strings=T)
head(dates)
tail(dates)
names(dates)

#get rid of years outside 1996-2011 in weeks file
dim(dates) #837,248
sort(unique(dates$Year))
dates<-dates[!dates$Year %in% c(1987,1988,1989,1990,1991,1992,1993,1994,1995,2012,2013),]
dim(dates) #496, 248 (246 sites) (133 for pruned)
str(dates)

for (k in 1:length(uweek)){
  for (tt in 1:length(uyear)) {
    a=which(dates$Year==uyear[tt] & dates$Week==uweek[k]) 
    b= which(dates[a,]==0)
    bb=b-2
    monarchs[bb,k,tt]=NA
  }}
monarchs
#checked, looks right
#dim: 246 rows (sites; 133 pruned), 31 weeks, 16 years

#Include only spring/summer (e.g., weeks 5-28)
#Remove weeks 29-35 from the data

monarchs1=monarchs[,1:24,]
#keep weeks 5-28--now 246 rows/sites, 24 cols/weeks, 16 years

#####################
###Adding covariates
#####################

#WEEK
uweek1=uweek[1:24]
#standardize uweek and make quadratic term
mweek=mean(uweek1)
sdweek=sd(uweek1)
week1<-(uweek1-mweek)/sdweek
week2<-week1*week1

#YEAR
myear=mean(uyear)
sdyear=sd(uyear)
year1<-(uyear-myear)/sdyear
year2<-year1*year1

#LAT/LONG
#NOTE: We don't end up using lat/long in model
coord=read.csv("SiteEffects_IL96to11.csv", header=T,sep=',',na.strings=T)
#NOTE: we don't have to omit years outside 1996-2011 in this file because only site-specific
str(coord)
dim(coord)

#get rid of least surveyed sites
coord<-coord[!coord$SiteID %in% c(2,3,7,9,16,47,53,67,83,106,115,117,129,141,146,159,163,171,172,222,224,662,663,743,1001,1002,1237,1261,1273,1289,2220,12313,943,72,1272,1,4,24,28,58,145,177,209,1284,1303,2012,12318,49,93,116,149,175,186,612,1902,12316,50,71,87,443,1233,1982,1983,5,157,185,202,1042,1271,30,1410,1430,96,108,211,262,820,1542,60,78,134,167,183,661,1286,12314,39,442,77,251,945,174,189,207,1231,137,151,1792,12,125,1236,1262,1302,12312,12317,68,99,135,1183,1234,54,162,181,611,742,12315),]
dim(coord) #133, 5

#match siteID in this file with siteID in monarchs data to put lat/long with correct site
a=pmatch(dimnames(monarchs1)$SiteID,coord$SiteID)
lat=coord$Latitude
long=coord$Longitude

mlat=mean(lat)
mlong=mean(long)
sdlat=sd(lat)
sdlong=sd(long)

lat1=(lat-mlat)/sdlat
long1=(long-mlong)/sdlong 

#SURVEYING EFFORT
#NOTE: Effort is 0 when site wasn't surveyed; log of 0 in model matches to a count of NA so valid
junk.melt2=melt(mon,id.var=c("Year", "Week", "SiteID"), measure.var="SumOfTotalTime")
effort=cast(junk.melt2, SiteID ~ Week ~ Year, sum)
#this creates tables of week by site according to year with 'sum' being sum of total time
effort = effort[,1:24,] 
#this eliminates weeks 29-35 as above
effort=effort/60
#turns the variable into hours

#HABITAT COVARIATES
#Site effects--constant across years
#% open habitat (standardized)
mopen=mean(coord$X.Open)
sdopen=sd(coord$X.Open)
open1<-(coord$X.Open-mopen)/sdopen

#NOTE: ignore urban variable since not used in final model

#Avg GDD
mavggdd=mean(coord$avgGDD10.28NEW)
sdavggdd=sd(coord$avgGDD10.28NEW)
avggdd1<-(coord$avgGDD10.28NEW-mavggdd)/sdavggdd

#SITE BY YEAR effects
#NOTE: sites are in numerical order in this file
siteyear = read.csv("SiteYear_IL96to11.csv", header=T,sep=',',na.strings=T)

#get rid of years outside 1996-2011 in site year file
dim(siteyear) #5412, 4
sort(unique(siteyear$Year))
siteyear<-siteyear[!siteyear$Year %in% c(1992,1993,1994,1995,2012,2013),]
dim(siteyear) #3936, 4
sort(unique(siteyear$SiteID)) #246 sites (matches mon)
str(siteyear)
siteyear$AccGDD10.28NEW<-as.numeric(as.character(siteyear$AccGDD10.28NEW))

#get rid of least surveyed sites
siteyear<-siteyear[!siteyear$SiteID %in% c(2,3,7,9,16,47,53,67,83,106,115,117,129,141,146,159,163,171,172,222,224,662,663,743,1001,1002,1237,1261,1273,1289,2220,12313,943,72,1272,1,4,24,28,58,145,177,209,1284,1303,2012,12318,49,93,116,149,175,186,612,1902,12316,50,71,87,443,1233,1982,1983,5,157,185,202,1042,1271,30,1410,1430,96,108,211,262,820,1542,60,78,134,167,183,661,1286,12314,39,442,77,251,945,174,189,207,1231,137,151,1792,12,125,1236,1262,1302,12312,12317,68,99,135,1183,1234,54,162,181,611,742,12315),]
dim(siteyear) #2128, 4
sort(unique(siteyear$SiteID)) #133 sites pruned

#organize site by year
#report appropriate GDD and PDSI values according to siteID and year
sitegdd = matrix(0,nrow=length(usite), ncol=length(uyear))
sitepalm = matrix(0,nrow=length(usite), ncol=length(uyear))
for (tt in 1:length(uyear)) {
  for (j in 1:length(usite)) {
    a=which(siteyear$SiteID == usite[j] & siteyear$Year == uyear[tt])
    sitegdd[j,tt] = siteyear$AccGDD10.28NEW[a]  
    sitepalm[j,tt] = siteyear$AvgPDSI10.28[a] 
  }} 

#standardize site GDDs
msitegdd=mean(as.matrix(sitegdd))
sdsitegdd=sd(as.vector(sitegdd))
sitegdd1=as.matrix((sitegdd-msitegdd)/sdsitegdd)

#standardize site drought indeces
msitepalm=mean(as.matrix(sitepalm))
sdsitepalm=sd(as.vector(sitepalm))
sitepalm1=as.matrix((sitepalm-msitepalm)/sdsitepalm)

#YEAR effects (TX GDD and drought)
#NOTE: This file is 1996 (first row) to 2011 (bottom row)
yeareffects=read.csv("YearEffects_IL96to11.csv", header=T,sep=',',na.strings=T)
plot(yeareffects$SprGDD4to9, yeareffects$SprPrecipTX)

#get rid of years outside 1996-2011 in year file
dim(yeareffects) #27, 4
sort(unique(yeareffects$Year))
yeareffects<-yeareffects[!yeareffects$Year %in% c(1987,1988,1989,1990,1991,1992,1993,1994,1995,2012,2013),]
dim(yeareffects) #16, 4
str(yeareffects)
yeareffects$AvgofPDI4to9<-as.numeric(as.character(yeareffects$AvgofPDI4to9))

spGDD=yeareffects$SprGDD4to9
spPrec=yeareffects$SprPrecipTX

#standardize spring GDD
mspGDD=mean(as.matrix(spGDD))
sdspGDD=sd(as.vector(spGDD))
spGDD1=as.vector((spGDD-mspGDD)/sdspGDD)

#standardize spring Precip
mspPrec=mean(as.matrix(spPrec))
sdspPrec=sd(as.vector(spPrec))
spPrec1=as.vector((spPrec-mspPrec)/sdspPrec)

#SITE BY WEEK BY YEAR effects
#NOTE: survey file doesn't need to be in order by site for this loop since it reads by siteID
gdd=array(0, dim=dim(monarchs1))
gdddiff=array(0, dim=dim(monarchs1))
pdsi=array(0, dim=dim(monarchs1))
for (tt in 1:length(uyear)) {
  for (k in 1:length(uweek1)) { 
    for (j in 1:length(usite)) {
      a=which(mon$SiteID==usite[j] & mon$Year==uyear[tt] & mon$Week==uweek1[k])
      if (length(a)==1)   {
        gdd[j,k,tt]=mon$GDDwk10NEW[a]  
        gdddiff[j,k,tt]=mon$GDDdiffNEW[a]
        pdsi[j,k,tt]=mon$PDSIwk[a]  }
    }}   }
#checked, looks right

#standardize GDDdiff
mgdddiff=mean(gdddiff[,6:24,], na.rm=T)
sdgdddiff=sd(as.vector(gdddiff[,6:24,]), na.rm=T)
gdddiff1<-(gdddiff[,6:24,]-mgdddiff)/sdgdddiff     

#standardize drought index
mpdsi=mean(pdsi[,6:24,], na.rm=T)
sdpdsi=sd(as.vector(pdsi[,6:24,]), na.rm=T)
pdsi1<-(pdsi[,6:24,]-mpdsi)/sdpdsi 

#use weeks 10-28 (e.g. summer weeks)
uweek2=uweek1[6:24]
week11<-(uweek2-mean(uweek2))/sd(uweek2)

#PREP JAGSUI DATA
bugsdata<-list(uyear=length(uyear), usite=length(usite), uweek=length(uweek2),
               y=monarchs1[,6:24,], week1=week11, effort=effort[,6:24,], 
               gdddiff1=gdddiff1, sitepalm1=sitepalm1,
               spGDD1=spGDD1, open1=open1, spPrec1=spPrec1, 
               sitepalm2=sitepalm1*sitepalm1, spGDD2=spGDD1*spGDD1,
               spPrec2=spPrec1*spPrec1, avggdd1=avggdd1, avggdd2=avggdd1*avggdd1) 

#inits for NEG BINOM model
inits<-function(){
  list(rprec=rgamma(1,1))
}

parameters<-c('a1', 'a2', 'a3', 'a4', 'a5', 'a6', 'a7', 'a8', 'a9', 
              'a10', 'a11', 'a12', 'a13', 'a14', 'a15', 'a16', 'a17','fit','fit.new')

###################################################################
##RUN BUGS MODEL IN JAGSUI
###################################################################
#final run: chains=3, adapt=100, iterations=4000, burn=1000, thin=3
out.pruned.final<-jags(data = bugsdata,
                     inits = inits,
                     parameters.to.save = parameters,
                     model.file = 'model_IL96to11_NB_ResidBP.txt',
                     n.chains = 3,
                     n.adapt = 100,
                     n.iter = 4000,
                     n.burnin = 1000,
                     n.thin = 3,
                     #parallel=TRUE
)
print(out.pruned.final,digits=4)

###########Evaluation of fit####################################################
mean(out.pruned.final$sims.list$fit.new>out.pruned.final$sims.list$fit) #this should be near 0.5
mean(out.pruned.final$mean$fit)/mean(out.pruned.final$mean$fit.new) #this should be near 1
pp.check(out.pruned.final,actual="fit",new="fit.new")
whiskerplot(out.pruned.final,parameters=c("a1","a2","a3","a4","a5","a6","a7","a8","a9","a10","a11","a12","a13","a14","a15","a16","a17"))
#out.pruned.final: 0.7 BPV

########SUMMARY STATS##########################################################
site.max<-apply(monarchs1[,6:24,],1,max,na.rm=TRUE)
table(site.max)
plot(table(site.max))
table(site.max>0)
#observed occupancy per site: 100%

site.week.max<-apply(monarchs1[,6:24,],c(1,2),max,na.rm=TRUE)
table(site.week.max)
plot(table(site.week.max))
table(site.week.max>0)
#observed occupancy per site per week: 1862/(1862+2812): 60%
site.week.max[site.week.max==-Inf]<-0
obs.siteweek.sum<-apply(site.week.max,2,sum,na.rm=TRUE)
plot(obs.siteweek.sum,type="b")
#clear trend of increasing abundance as weeks progress

####################################################################
####Replace certain years with NA to determine predictive ability of model
####################################################################
monarchs2=monarchs1
monarchs2[,,11:16]<-NA

monarchs3=monarchs1
monarchs3[,,13:16]<-NA

monarchs4=monarchs1
monarchs4[,,15:16]<-NA

monarchs5=monarchs1
monarchs5[,,9:16]<-NA

#PREP WINBUGS DATA
#change years in yy according to those to omit
#change y dataset to monarchs2, monarchs3, etc.
#yy=(monarchs1[,24,15:16])
bugsdata<-list(uyear=length(uyear), usite=length(usite), uweek=length(uweek2),
               yy=(monarchs1[,24,15:16]), 
               y=monarchs4[,6:24,],
               week1=week11, effort=effort[,6:24,], 
               gdddiff1=gdddiff1, sitepalm1=sitepalm1,
               spGDD1=spGDD1, open1=open1, spPrec1=spPrec1, 
               sitepalm2=sitepalm1*sitepalm1, spGDD2=spGDD1*spGDD1,
               spPrec2=spPrec1*spPrec1, avggdd1=avggdd1, avggdd2=avggdd1*avggdd1) 

#inits for NEG BINOM model
inits<-function(){
  list(rprec=rgamma(1,1))
}

#add new parameters to monitor
parameters<-c('a1', 'a2', 'a3', 'a4', 'a5', 'a6', 'a7', 'a8', 'a9', 
              'a10', 'a11', 'a12', 'a13', 'a14', 'a15', 'a16', 'a17','fit','fit.new','fit.year','fit.new.year','fit1','fit.new1')

###################################################################
##RUN BUGS MODEL
###################################################################

#NOTE: change model input file according to years to omit!!
#NOTE: change object name
#run: 3 chains, 100 adapt, 4000 iterations, 1000 burnin, 3 thinning
out.predict4<-jags(data = bugsdata,
                   inits = inits,
                   parameters.to.save = parameters,
                   model.file = 'IL Yr Subsetting Predict4.txt',
                   n.chains = 3,
                   n.adapt = 100,
                   n.iter = 4000,
                   n.burnin = 1000,
                   n.thin = 3,
                   #parallel=TRUE
)

mean(out.predict2$sims.list$fit.new>out.predict2$sims.list$fit) #0.42 (0.22)
mean(out.predict2$sims.list$fit.new1>out.predict2$sims.list$fit1) #0.40 (0.20)
mean(out.predict2$sims.list$fit.new.year[,6]>out.predict2$sims.list$fit.year[,6])
#0.68 (2006), 0.11 (2007), 0.88 (2008), 0 (2009), 0.06 (2010), 0.14 (2011)

mean(out.predict3$sims.list$fit.new>out.predict3$sims.list$fit) #0.56 (0.16)
mean(out.predict3$sims.list$fit.new1>out.predict3$sims.list$fit1) #0.42
mean(out.predict3$sims.list$fit.new.year[,4]>out.predict3$sims.list$fit.year[,4])
#0.90 (2008), 0 (2009), 0.02 (2010), 0.10 (2011)

mean(out.predict4$sims.list$fit.new>out.predict4$sims.list$fit) #0.6 (0.2)
mean(out.predict4$sims.list$fit.new1>out.predict4$sims.list$fit1) #0.3 (0.2)
mean(out.predict4$sims.list$fit.new.year[,2]>out.predict4$sims.list$fit.year[,2])
#0.20 (2010), 0.10 (2011)

mean(out.predict5$sims.list$fit.new>out.predict5$sims.list$fit) #0.32
mean(out.predict5$sims.list$fit.new1>out.predict5$sims.list$fit1) #0.77
mean(out.predict5$sims.list$fit.new.year[,8]>out.predict5$sims.list$fit.year[,8])
#1.0 (2004), 0.17 (2005), 0.64 (2006), 0.03 (2007), 0.88 (2008),0.30 (2009), 0.13 (2010),0.14 (2011)
