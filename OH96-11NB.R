#############################################################################
#############OHIO 1996-2011 ANALYSIS with Daymet GDD data####################
#############################################################################

#load libraries
library(jagsUI)
library(reshape)

#set working directory
setwd("C:/Users/Sarah/Desktop/Monarch Post-doc/OH 96-11/NB model")

#read in data 
mon = read.csv("SurveyData_OH96to11.csv", header=TRUE,sep=',',na.strings=T)
mon[1:10,]
names(mon)
str(mon)

#Post-hoc year subsetting
dim(mon) #13039, 12
sort(unique(mon$Year))
mon<-mon[!mon$Year %in% c(2008,2009,2010,2011),]
dim(mon) #8879, 12

#Look at data (checking all years/weeks/sites present)
uyear = sort(unique(mon$Year))
#1996-2011
uweek= sort(unique(mon$Week))
#5-35
usite = sort(unique(mon$SiteID))
#total sites (116)

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
dates=read.csv("weeks_OH96to11.csv", header=T,sep=',',na.strings=T)
dates=read.csv("weeks_OH96to11_Yr2.csv", header=T,sep=',',na.strings=T) #change with year subsets
dates=read.csv("weeks_OH96to11_Yr3.csv", header=T,sep=',',na.strings=T) #change with year subsets
head(dates)
tail(dates)
names(dates)

#post-hoc year subsetting
dim(dates) #496, 93
sort(unique(dates$Year))
dates<-dates[!dates$Year %in% c(2008,2009,2010,2011),]
dim(dates) #372, 91
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

#Include only spring/summer (e.g., weeks 5-28)
#Remove weeks 29-35 from the data

monarchs1=monarchs[,1:24,]
#keep weeks 5-28

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
coord=read.csv("siteEffects_OH96to11.csv", header=T,sep=',',na.strings=T)

#get rid of sites not surveyed in Year1 to use correct standardization
#change with year subsets
sort(unique(coord$SiteID))
coord<-coord[!coord$SiteID %in% c(111,112,113,114,115,117,118,119,120,122,123,124,125,126,127,128,129,132,133,134,135,136,138,144,146),]
dim(coord) #91, 5

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
siteyear = read.csv("SiteYear_OH96to11.csv", header=T,sep=',',na.strings=T)

#post-hoc year subsetting
dim(siteyear) #1856, 4
sort(unique(siteyear$Year))
siteyear<-siteyear[!siteyear$Year %in% c(2008,2009,2010,2011),]
dim(siteyear) #1392, 4

#get rid of sites not surveyed in Year1 to use correct standardization
sort(unique(siteyear$SiteID))
siteyear<-siteyear[!siteyear$SiteID %in% c(111,112,113,114,115,117,118,119,120,122,123,124,125,126,127,128,129,132,133,134,135,136,138,144,146),]

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
yeareffects=read.csv("YearEffects_OH96to11.csv", header=T,sep=',',na.strings=T)
plot(yeareffects$SprGDD4to9, yeareffects$SprPrecipTX)

#post-hoc year subsetting
dim(yeareffects) #16, 4
sort(unique(yeareffects$Year))
yeareffects<-yeareffects[!yeareffects$Year %in% c(2008,2009,2010,2011),]
dim(yeareffects) #12,4

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

#standardize GDDdiff
#NOTE: adjust weeks below as needed
mgdddiff=mean(gdddiff[,6:24,], na.rm=T)
sdgdddiff=sd(as.vector(gdddiff[,6:24,]), na.rm=T)
gdddiff1<-(gdddiff[,6:24,]-mgdddiff)/sdgdddiff     

#standardize drought index
#NOTE: adjust weeks below as needed
mpdsi=mean(pdsi[,6:24,], na.rm=T)
sdpdsi=sd(as.vector(pdsi[,6:24,]), na.rm=T)
pdsi1<-(pdsi[,6:24,]-mpdsi)/sdpdsi 

#use summer weeks
#NOTE: adjust weeks below as needed
uweek2=uweek1[6:24]
week11<-(uweek2-mean(uweek2))/sd(uweek2)

#PREP WINBUGS DATA
#NOTE:  Added year1 for zero-inflated component. Adjust weeks as needed
bugsdata<-list(uyear=length(uyear), usite=length(usite), uweek=length(uweek2),
               y=monarchs1[,6:24,], week1=week11, effort=effort[,6:24,], 
               gdddiff1=gdddiff1, sitepalm1=sitepalm1, #year1=year1,
               spGDD1=spGDD1, open1=open1, spPrec1=spPrec1, 
               sitepalm2=sitepalm1*sitepalm1, spGDD2=spGDD1*spGDD1,
               spPrec2=spPrec1*spPrec1, avggdd1=avggdd1, avggdd2=avggdd1*avggdd1) 

#inits for NEG BINOM model
inits<-function(){
  list(rprec=rgamma(1,1))
}

#inits for POISSON model
#inits<-function(){
#list(a1=rnorm(1),a2=rnorm(1),a3=rnorm(1),a4=rnorm(1), 
#a5=rnorm(1),a6=rnorm(1),a7=rnorm(1),a8=rnorm(1),a9=rnorm(1), 
#a10=rnorm(1),a11=rnorm(1),a12=rnorm(1),a13=rnorm(1),a14=rnorm(1), 
#a15=rnorm(1),a16=rnorm(1),a17=rnorm(1))
#}

#inits for ZERO-INFLATED models
z.new=array(1,dim=dim(monarchs1[,6:24,]))
inits<-function(){
list(z=z.new)#,zz.new=z.new)
}

#note: add b0,b1,b2 if using zero-inflated models
parameters<-c('a1', 'a2', 'a3', 'a4', 'a5', 'a6', 'a7', 'a8', 'a9', 
              'a10', 'a11', 'a12', 'a13', 'a14', 'a15', 'a16', 'a17')#,'fit','fit.new')

###################################################################
##RUN BUGS MODEL
###################################################################

library(jagsUI)
#this MCMC input yields Rhat values of 1.01 or less
out.jui3.final<-jags(data = bugsdata,
              inits = inits,
              parameters.to.save = parameters,
              model.file = 'model_OH96to11_NB1_NewVarBP2.txt',
              n.chains = 3,
              n.adapt = 100,
              n.iter = 4000,
              n.burnin = 1000,
              n.thin = 3,
              #parallel=TRUE
)

print(out.jui3.final,digits=4)

#post-hoc year subsetting run (lengthened since less data). year1=1996-2003. year2=1996-2005. year3=1996-2007
out.year3subset<-jags(data = bugsdata,
                     inits = inits,
                     parameters.to.save = parameters,
                     model.file = 'model_OH96to11_NB1.txt',
                     n.chains = 3,
                     n.adapt = 100,
                     n.iter = 5000,
                     n.burnin = 2000,
                     n.thin = 3,
                     #parallel=TRUE
)
whiskerplot(out.year3subset,parameters=c("a1","a2","a3","a4","a5","a6","a7","a8","a9","a10","a11","a12","a13","a14","a15","a16","a17"))

library(R2WinBUGS)
#bugs code
out<-bugs(bugsdata, inits, parameters, "model_OH96to11_NB1_NewVarBP2.txt", debug=TRUE,
               n.chains=2, n.iter=100, n.burnin=30, n.thin=2)
#R2jags code
jout<-jags(bugsdata,inits, parameters,"model_OH96to11_NB1_NewVarBP2.txt",
                n.chains=2, n.iter=100, n.burnin=30, n.thin=2)
#n.chains=3,n.thin=4,n.iter=25000,n.burnin=10000)

###########Evaluation of fit####################################################
mean(out.jui3.final$sims.list$fit.new>out.jui3.final$sims.list$fit) #this should be near 0.5
mean(out.jui3.final$mean$fit)/mean(out.jui3.final$mean$fit.new) #this should be near 1
pp.check(out.jui3.final,actual="fit",new="fit.new")
whiskerplot(out.jui3.final,parameters=c("a1","a2","a3","a4","a5","a6","a7","a8","a9","a10","a11","a12","a13","a14","a15","a16","a17"))
#CONCLUSION: STAY WITH 6:24 weeks!
#250 iterations per chain: NB1_ZuurBP_FINAL BPV: 0.46/0.53 (jui3cc)
#NB1_ZuurBP_FINAL2 (jui3d) BPV: 0.25/0.75 with 250 iterations
#***NewVar2 (jui3f) BPV: 0.51/0.49 with 250 iterations; with 1000 its (jui3 final): 0.5/0.5