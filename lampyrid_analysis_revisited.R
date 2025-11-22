# lampyrid analysis
#bring data in from figshare
lampyrid<-read.csv(file="data/LTER_lampyrid_data_20042025.csv",
                   header=T)

#we're going to be looking at the responses of lampyrids to environmental conditions
#this means making some choices about when to stat counting
#what day of the year should we start the analysis on? 
#giving it a start day of Mar 1
start<-60

#coding the variable like this makes it easy to re-run the code with different start dates
#to see what the effect of the start date has on our conclusions. this type of testing is 
#often referred to as 'sensitivity analysis'- ie seeing how sensitive your conclusions are to
#your  assumptions, guesses or starting points.

#clean data
#fix dates, make them ISO'ed
library(dplyr)
library(lubridate)
library(ISOweek)
lampyrid$newdate<-mdy(lampyrid$DATE)
#extract year
lampyrid$year<-year(lampyrid$newdate)
#extract day of year. DOY is very useful for a phenology type analyses
#because you don't have to deal with day-of-month numbers starting over 
#in the middle of a phenological event.
lampyrid$DOY<-yday(lampyrid$newdate)
#use ISO week, so we start counting on Monday, not Jan 1, COOL! Our sampling usually 
#takes place Wed-Friday, so if we use week of year stating on Jan 1, there is a good chance that
#samples taken within a sampling week would get grouped incorrectly when we go to do the analysis.
lampyrid$week<-isoweek(lampyrid$newdate)

#let's look for the data problems we found we used OpenRefine and see if
#we can impliment our cleaning operations here- that way we have a complete
#record of EVERYTHING that happened to these data. Recall there were issues 
#with TREAT_DESC
#let's look at these columns individually and fix errors as we find them
#and we should also check for weirdness in our numeric values

summary(lampyrid)
#looks like there's one missing data point (NA) for adults. Let's ditch
#it so it doesn't cause any problems in subsequent analyses
lampyrid<-na.omit(lampyrid)
summary(lampyrid)

#looks good. Okay, TREAT_DESC:

summary(as.factor(lampyrid$TREAT_DESC))
#wow, we've got some spelling errors. Let's clean that up

lampyrid$TREAT_DESC<-gsub("Early succesional community", "Early successional", lampyrid$TREAT_DESC)
lampyrid$TREAT_DESC<-gsub("Early sucessional community", "Early successional", lampyrid$TREAT_DESC)
lampyrid$TREAT_DESC<-gsub("Early Successional Community", "Early successional", lampyrid$TREAT_DESC)
lampyrid$TREAT_DESC<-gsub("Early successional community", "Early successional", lampyrid$TREAT_DESC)
lampyrid$TREAT_DESC<-gsub("Succesional", "Successional", lampyrid$TREAT_DESC)
lampyrid$TREAT_DESC<-gsub("Sucessional", "Successional", lampyrid$TREAT_DESC)
lampyrid$TREAT_DESC<-gsub("poplar trees", "Poplar trees", lampyrid$TREAT_DESC)
#also shorten biologically based (organic) and conventional till for plotting purposes 
lampyrid$TREAT_DESC<-gsub("Biologically based \\(organic\\)", "Organic", lampyrid$TREAT_DESC)
lampyrid$TREAT_DESC<-gsub("Conventional till", "Conventional", lampyrid$TREAT_DESC)

#alfalfa plots T5 were switched(ha!) to switchgrass in 2017, so we're just going to call that treatment Forage
lampyrid$TREAT_DESC<-gsub("Alfalfa", "Forage", lampyrid$TREAT_DESC)
lampyrid$TREAT_DESC<-gsub("Switchgrass", "Forage", lampyrid$TREAT_DESC)

#also convert this column to factor (gsub sometimes turns it into character type)
lampyrid$TREAT_DESC<-as.factor(lampyrid$TREAT_DESC)
summary(lampyrid$TREAT_DESC)

#do the same for HABITAT
summary(as.factor(lampyrid$HABITAT))
#checks out. Let's make sure R is seeing it as a factor, and also rep and station while we're at it

lampyrid$HABITAT<-as.factor(lampyrid$HABITAT)
lampyrid$REPLICATE<-as.factor(lampyrid$REPLICATE)
lampyrid$STATION<-as.factor(lampyrid$STATION)

#one more check to see if the data looks clean
summary(lampyrid)

#new! we're adding a variable to our dataset that lets us group the early study data

#and the new survey data into 2 groups for comparative analysis
#also a new variable to group data into 4 year chunks

lampyrid$study<-as.factor(ifelse(lampyrid$year<=2015, "Hermann","New"))


lampyrid$timechunk<-ifelse(lampyrid$year<=2007, "first", 
                           ifelse(lampyrid$year>=2008&lampyrid$year<=2011, "second",
                                  ifelse(lampyrid$year>=2012&lampyrid$year<=2015, "third",
                                         ifelse((lampyrid$year>=2016&lampyrid$year<=2019), "fourth",
                                                ifelse((lampyrid$year>=2020&lampyrid$year<=2023), "fifth",
                                                       ifelse(lampyrid$year>=2024, "sixth", NA)
                                                )))))

#give these categories an order because they're alphabetical by default
lampyrid$timechunk <- factor(lampyrid$timechunk,
                           levels = c("first", "second", "third", "fourth", "fifth", "sixth"),
                           ordered = TRUE)
lampyrid$timechunk<-as.factor(lampyrid$timechunk)
#also do TREAT_DESC while we're here
lampyrid$TREAT_DESC <- factor(lampyrid$TREAT_DESC,
                             levels = c("Conventional", "No till", "Reduced input", "Organic", "Forage", 
                                        "Poplar trees", "Early successional", "Coniferous", "Deciduous", "Successional"),
                             ordered = TRUE)


#so we have a small issue with these data. The counts will be strongly zero-biased because we 
# give each subsample its own observation. When it comes to modelling and plotting, we're going to
#want to have the subsamples combined (summed), but because sometimes we lost traps (weather, accidental loss)
#not all plots will have the same number of subsamples
#we will process our data set so that we've got our subsamples combined by plot date etc and create a vector with counts
library(reshape2)
#tell R where the data is by melting it, assigning IDs to the columns
lampyrid1<-melt(lampyrid, id=c("DATE","TREAT_DESC","HABITAT","REPLICATE","STATION","newdate", "year", "DOY", "week", "timechunk", "study"))
#cast the data to count up the fireflies
lampyrid2<-dcast(lampyrid1, year+DOY+week+TREAT_DESC+HABITAT+REPLICATE+timechunk+study~., sum)
#cast the data to count the traps
lampyrid3<-dcast(lampyrid1, year+DOY+week+TREAT_DESC+HABITAT+REPLICATE+timechunk+study~., length)

#let's rename these new vectors within the data frame
names(lampyrid2)[9]<-"ADULTS"
names(lampyrid3)[9]<-"TRAPS"

#rename the data frame and combine the number of traps we counted into it from lampyrid3
lampyrid<-lampyrid2
lampyrid$TRAPS<-lampyrid3$TRAPS





#download weather data from KBS weather station
weather<-read.table(file="http://lter.kbs.msu.edu/datatables/7.csv",
                    header=T, sep=",", na.strings="", comment.char = '#')

#extract day of year, so we have a continuous variable running for each year.
#since we're in a temperate northern climate, this is convenient- not too 
#much insect action happening at the december-january transition, so we 
#can use the yearly break as a blocking variable for rowing season.
#it's convenient living where we do! 

weather$DOY<-yday(weather$date)
weather$week<-isoweek(weather$date)
#do a few simple plots to make sure the data makes sense -this is
#a good way to check that the importation was successful

plot(weather$DOY, weather$air_temp_mean)
plot(weather$DOY, weather$precipitation)

#let's cut out the data from before 2004 so we can process the weather data more quickly.
weather<-subset(weather, weather$year>=2004 & weather$year<=2025)


#lets also get rid of the variables we don't need:
weather$flag_precip<-NULL
weather$flag_air_temp_mean<-NULL
weather$flag_air_temp_max<-NULL
weather$flag_air_temp_min<-NULL

#data after Aug 20, 2025 doesn't include daily max mins so let's cut that out
weather<-subset(weather, weather$date<="2025-09-29")

#also, these data are sorted in descending order. It's easier to think of this 
#stuff in ascending order, so let's sort the data by year and DOY

weather<-weather[order(weather$year, weather$DOY),]

#Let's examine the data to see how complete it is
summary(weather)

#let's pre-process these weather data so we get rid of missing values
# we can write a function to do this for us.
#if missing data is rare, it is probably safe to assume that missing
#temperatures are similar to the weather on the day before or after.
#for the sake of simplicity, let's replace a missing value with the 
#value for that variable for the day before

#first, define the function

replace.missing<-function(vec){
  #create a vector to put our new values into
  New = c()
  for (i in 1:(length(vec))){
    if (is.na(vec[i])){
      vec[i]<-mean(c(vec[i-1], vec[i+1]), na.rm=TRUE)
      #if the data is missing, sub in the value from the measurement before averaged with the value after
      
    } else{
      #if the value is not missing, just pass it through to the result vector
      vec[i]<-vec[i]
    }
    New=c(New, vec[i])
  }
  if (any(is.na(New))){
    replace.missing(New)
  }
  return(New)
}
#now let's use our replace missing function to gap fill our weather data
weather$temp_mean_cleaned<-replace.missing(weather$air_temp_mean)
weather$temp_min_cleaned<-replace.missing(weather$air_temp_min)
weather$temp_max_cleaned<-replace.missing(weather$air_temp_max)

# calculate the degree day accumulation for the first half of the day dd1,
#assuming a sine wave structure of temperature over the day
#use a development threshold of 10C, well, because it's a nice number
#to work with
#we'll use the model presented in Allen 1976 which uses daily max and min temperatures
#and assumes temperature follows a sine wave

allen<-function(maxi, mini, thresh){
  #if threshold is not given, assume it's 10 Celcius
  if(missing(thresh)) {
    thresh<-10
  } else {
    thresh<-thresh
  }
  dd1<-c()
  dd2<-c()
  for (i in 1:(length(maxi)-1)){ #have to fudge it so it doesn't calculate a dd on the last day of the time series
    if (maxi[i]>= thresh & mini[i]<thresh) {
      #first half of day
      #amplitude of temperature difference
      alpha1<-(maxi[i]-mini[i])/2
      #average temperature
      avg1<-(maxi[i]+mini[i])/2
      #theta is time point when temperature crosses the threshold
      #assuming temperature is roughly following the sine curve
      theta1<-asin((thresh-avg1)/alpha1)
      #use these to calculate degree day accumulation over first half of day
      dd1.T<-(1/(2*pi))*((avg1-thresh)*(pi/2 - theta1)+alpha1*cos(theta1))
      dd1<-c(dd1, dd1.T)
      #second half of day
      #two possible cases, min temperature on day i+1 could be below thereshold or above
      #for below threshold:
      if (mini[i+1]<thresh){
        #amplitude of temperature difference
        alpha2<-(maxi[i]-mini[i+1])/2
        #average temperature
        avg2<-(maxi[i]+mini[i+1])/2
        #theta is time point when temperature crosses the threshold
        #assuming temperature is roughly following the sine curve
        theta2<-asin((thresh-avg2)/alpha2)
        #use these to calculate degree day accumulation over first half of day
        dd2.T<-(1/(2*pi))*((avg2-thresh)*(pi/2 - theta2)+alpha2*cos(theta2))
        dd2<-c(dd2, dd2.T)
      } else { #for above threshold
        #second half of day
        avg2<-(maxi[i]+mini[i+1])/2
        dd2.T<-(avg2-thresh)/2
        dd2<-c(dd2, dd2.T)
      }
      
    } else if (mini[i]>=thresh){
      #first half of day
      avg1<-(maxi[i]+mini[i])/2
      dd1.T<-(avg1-thresh)/2
      dd1<-c(dd1, dd1.T)
      #second half of day, as above, two possible cases
      if (mini[i+1]>=thresh){
        avg2<-(maxi[i]+mini[i+1])/2
        dd2.T<-(avg2-thresh)/2
        dd2<-c(dd2, dd2.T)
      } else{
        #amplitude of temperature difference
        alpha2<-(maxi[i]-mini[i+1])/2
        #average temperature
        avg2<-(maxi[i]+mini[i+1])/2
        #theta is time point when temperature crosses the threshold
        #assuming temperature is roughly following the sine curve
        theta2<-asin((thresh-avg2)/alpha2)
        #use these to calculate degree day accumulation over first half of day
        dd2.T<-(1/(2*pi))*((avg2-thresh)*(pi/2 - theta2)+alpha2*cos(theta2))
        dd2<-c(dd2, dd2.T)
      }
      
    }
    else  {
      #if temperature doesn't get over threshold, no degree days accumulated
      #first half of day
      dd1<-c(dd1, 0)
      #second half of day
      dd2<-c(dd2, 0)
    }
    #total accumulation over the day is just first half of day plus second
    
  }
  result<-c((dd1+dd2),0)
  return(result)
  
}


#do some checks to make sure the function is working properly

weather$dd<-allen(weather$temp_max_cleaned, weather$temp_min_cleaned, 10)



#plot to make sure nothing weird is happening- look for more degree days midyear,
#and NO negative values. Looks like we're WINNING!
plot(weather$DOY, weather$dd)

#now write a new function to calculate accumulated degree days


accum.allen<-function(maxi, mini, thresh, DOY, startday){
  #if startday is not given, assume it's day 1
  if(missing(startday)) {
    startday<-1
  } else {
    startday<-startday
  }
  dd<-allen(maxi, mini, thresh)
  dd.accum<-c()
  for (i in 1:length(dd)){
    #hmm, need a way to sum up over the year, starting anew for each year.
    #this should do it
    if (DOY[i]==1){
      dd.accum.day=0
    }
    #the accumulation on day i is the degree day accumulation before
    #plus the dd accumulated on that day
    dd.accum.day<-dd.accum.day+dd[i]
    
    #but if the degdays are accumulating before the startday, we want to forget them
    if (DOY[i]<startday){
      dd.accum.day=0
    }
    #add that day's accumulation to the vector
    dd.accum<-c(dd.accum, dd.accum.day)
  }
  return (dd.accum)
}

#same sort of checks. Run the function for our data
start<-1
weather$dd.accum<-accum.allen(weather$temp_max_cleaned, weather$temp_min_cleaned, 10, weather$DOY, start)
#and plot that thing to look for problems:
plot(weather$DOY, weather$dd.accum)
#looks good! victory!!!

#we have good reason to think precipitation may also be important 
#let's use the functions developed for the previous lampyrid analysis to aggregate some precipitation metrics

accum.precip<-function (precip, week){
  precip.acc<-c()
  counter<-week[1]
  accumulation<-0
  for (i in 1:length(precip)){
    if(week[i]==counter){
      accumulation<-accumulation + precip[i]
    }else{
      counter<-week[i]
      accumulation<-precip[i]
    }
    precip.acc<-c(precip.acc, accumulation)
  }
  return(precip.acc)
}

#run the precipitation accumulation function
weather$prec.accum<-accum.precip(weather$precipitation, weather$week)


#looks good! now let's count rainy days
#this is a simple thing, doesn't really need a function to encode for it, but what the heck
#might as well be consistent with how we've handled processing other weather data
#encoding rain days as 0/1 will allow us to simply sum up the number of rainy days for whatever time 
#period we like

rainy.days<-function (precip, week){
  rainy.days<-c()
  for (i in 1:length(precip)){
    if(precip[i]>0){
      raindays<-1
    }else{
      raindays<-0
    }
    rainy.days<-c(rainy.days, raindays)
  }
  return(rainy.days)
}

#and now the rain day counter
weather$rain.days<-rainy.days(weather$precipitation, weather$week)

#finally, we need to be able to compute the accumulated precipitation over the season from a given timepoint
#another function? I think SO! base this one on the degree day accumulation function 


accum.precip.time<-function(precip, DOY, startday){
  #if startday is not given, assume it's day 1
  if(missing(startday)) {
    startday<-1
  } else {
    startday<-startday
  }
  prec.accum<-c()
  for (i in 1:length(DOY)){
    #hmm, need a way to sum up over the year, starting anew for each year.
    #this should do it
    if (DOY[i]==1){
      prec.accum.day=0
    }
    #the accumulation on day i is the precip accumulation before
    #plus the precip accumulated on that day
    prec.accum.day<-prec.accum.day+precip[i]
    
    #but if the precip is accumulating before the startday, we want to forget them
    if (DOY[i]<startday){
      prec.accum.day=0
    }
    #add that day's accumulation to the vector
    prec.accum<-c(prec.accum, prec.accum.day)
  }
  return (prec.accum)
}

weather$prec.accum.0<-accum.precip.time(weather$precipitation, weather$DOY, start)
#and plot that thing to look for problems:
plot(weather$DOY, weather$prec.accum.0)

#now let's put together a weekly 'weather report'

weather1<-group_by(weather, year, week)

weather_weekly<-dplyr::summarize(weather1,
                                 mean.prec=mean(precipitation),
                                 rain.days=sum(rain.days),
                                 weekly.precip=max(prec.accum),
                                 yearly.precip.accum=max(prec.accum.0),
                                 max.rainfall=max(precipitation),
                                 mean.temp=mean(temp_mean_cleaned),
                                 min.temp=min(temp_min_cleaned),
                                 max.temp=max(temp_max_cleaned),
                                 weekly.dd=max(dd),
                                 dd.accum=max(dd.accum),
)



#so, now we have two datasets that both have information we need in them.
#let's put it all together in one frame


lampyrid.weather<-merge(lampyrid, weather_weekly, by=c("year","week"), all.x=TRUE)

#let's take a look at our data now and see what patterns we can see

library(ggplot2)

#create a palate based on colour brewer. We want to use 'Spectral' for year data
#we're going to need to adjust given our number of years
#just extract the hex from colorbrewer, and find an additional shade that works on one of the ends

pal<-c('#f46d43', '#74add1')
pal1<-c('#9e0142','#fdae61','#ffffbf','#66c2a5','#3288bd','#5e4fa2')

#plot raw 
lampyrid.doy<-ggplot(lampyrid.weather, aes(DOY, ADULTS, fill=as.factor(study)))+
  scale_fill_manual(values=pal)+
  geom_point(colour="black", pch=21, size=4)+
  theme_bw(base_size = 20)+
  facet_wrap(~study)+
  guides(fill=FALSE)+
  xlab("Day")+
  ylab("# Adults captured")
lampyrid.doy

#plot raw 
lampyrid.doy.timechunk<-ggplot(lampyrid.weather, aes(DOY, ADULTS, fill=as.factor(timechunk)))+
  scale_fill_manual(values=pal1)+
  geom_point(colour="black", pch=21, size=4)+
  theme_bw(base_size = 20)+
  facet_wrap(~timechunk)+
  guides(fill=FALSE)+
  xlab("Day")+
  ylab("# Adults captured")
lampyrid.doy.timechunk


#save to pdf
#pdf("lampyriddoy.pdf", height=6, width=8)
#lampyrid.doy
#dev.off()

#plot by sample week
lampyrid.week<-ggplot(lampyrid.weather, aes(week, ADULTS, fill=factor(study)))+
  scale_fill_manual(values=pal)+
  geom_point(colour="black", pch=21, size=4)+
  theme_bw(base_size = 20)+
  facet_wrap(~study)+
  guides(fill=FALSE)+
  xlab("Week")+
  ylab("# Adults captured")
lampyrid.week

lampyrid.week.timechunk<-ggplot(lampyrid.weather, aes(week, ADULTS, fill=factor(timechunk)))+
  scale_fill_manual(values=pal1)+
  geom_point(colour="black", pch=21, size=4)+
  theme_bw(base_size = 20)+
  facet_wrap(~timechunk)+
  guides(fill=FALSE)+
  xlab("Week")+
  ylab("# Adults captured")
lampyrid.week.timechunk

#save to pdf
#pdf("lampyridweek.pdf", height=6, width=8)
#lampyrid.week
#dev.off()

# we're interested in looking at more general trends. We'll need to produce 
#summary data to do this


captures.by.year <- lampyrid.weather %>%
  group_by(year, timechunk, study) %>%
  summarise(
    total = sum(ADULTS, na.rm = TRUE),
    traps = sum(TRAPS, na.rm = TRUE),
    avg   = sum(ADULTS, na.rm = TRUE) / sum(TRAPS, na.rm = TRUE),
    ddacc = max(dd.accum, na.rm = TRUE),
    .groups = "drop"
  )

captures.by.week.year <- lampyrid.weather %>%
  group_by(year,timechunk, study, week) %>%
  summarise(
    total     = sum(ADULTS, na.rm = TRUE),
    traps     = sum(TRAPS, na.rm = TRUE),
    avg       = sum(ADULTS, na.rm = TRUE) / sum(TRAPS, na.rm = TRUE),
    ddacc     = max(dd.accum, na.rm = TRUE),
    rain.days = max(rain.days, na.rm = TRUE),
    .groups = "drop"
  )


#look at captures by week, over the growing season, by year
lampyrid.summary.week<-ggplot(captures.by.week.year, aes(week, avg, 
                                                         fill=as.factor(study), color=as.factor(study)))+
  scale_fill_manual(values=pal)+
  scale_color_manual(values=pal)+
  geom_point(colour="black", pch=21, size=4)+
  geom_smooth(se=FALSE, show.legend = FALSE)+
  theme_bw(base_size = 20)+
  guides(fill=guide_legend(title="Study"))+
  theme(legend.key=element_blank())+
  xlab("\nWeek")+
  ylab("Adults per trap\n")

lampyrid.summary.week



lampyrid.summary.week.timechunk<-ggplot(captures.by.week.year, aes(week, avg, 
                                                         fill=as.factor(timechunk), color=as.factor(timechunk)))+
  scale_fill_manual(values=pal1)+
  scale_color_manual(values=pal1)+
  geom_point(colour="black", pch=21, size=4)+
  geom_smooth(se=FALSE, show.legend = FALSE)+
    theme_bw(base_size = 20)+
  guides(fill=guide_legend(title="Time block"))+
  theme(legend.key=element_blank())+
  xlab("\nWeek")+
  ylab("Adults per trap\n")

lampyrid.summary.week.timechunk


#save to pdf
#pdf("lampyridsummaryweek.pdf", height=6, width=8)
#lampyrid.summary.week
#dev.off()

#look at captures by degree day accumulation to see if our activity pattern is clearer

lampyrid.summary.ddacc<-ggplot(captures.by.week.year, aes(ddacc, avg, 
                                                          fill=as.factor(study), color=as.factor(study)))+
  scale_fill_manual(values=pal)+
  scale_color_manual(values=pal)+
  geom_point(colour="black", pch=21, size=4)+
  geom_smooth( se=FALSE,  show.legend = FALSE)+
  theme_bw(base_size = 20)+
  guides(fill=guide_legend(title="Study"))+
  theme(legend.key=element_blank())+
  xlab("\nDegree day accumulation")+
  ylab("Adults per trap\n")

lampyrid.summary.ddacc


lampyrid.summary.ddacc.timechunk<-ggplot(captures.by.week.year, aes(ddacc, avg, 
                                                          fill=as.factor(timechunk), color=as.factor(timechunk)))+
  scale_fill_manual(values=pal1)+
  scale_color_manual(values=pal1)+
  geom_point(colour="black", pch=21, size=4)+
  geom_smooth( se=FALSE,  show.legend = FALSE)+
  theme_bw(base_size = 20)+
  guides(fill=guide_legend(title="Time block"))+
  theme(legend.key=element_blank())+
  xlab("\nDegree day accumulation")+
  ylab("Adults per trap\n")

lampyrid.summary.ddacc.timechunk


#save to pdf
#pdf("lampyridsummaryddacc.pdf", height=6, width=8)
#lampyrid.summary.ddacc
#dev.off()

#we want to stack these figures together because they are a driect comparison of the predictivity of these two factors
#since this is a ggplot, we'll need to use arrangegrob. we can alter the panels before feeding them to arrangegrob
#to remove redundant information and to add labels
library(gridExtra)


#remove legend from panel A, add label
lampyrid.summary.week1<-lampyrid.summary.week+guides(fill=FALSE)+
  annotate("text", x=20, y=4.2, label="A", size=14)
#remove Y axis title from panel B, add label
lampyrid.summary.ddacc1<-lampyrid.summary.ddacc+ylab(NULL)+
  annotate("text", x=255, y=4.2, label="B", size=14)
#stack it together
grid.arrange(arrangeGrob(lampyrid.summary.week1, lampyrid.summary.ddacc1, ncol=2, widths=c(0.49, 0.62)))


#save to pdf
pdf("figure4.pdf", height=6, width=10)
grid.arrange(arrangeGrob(lampyrid.summary.week1, lampyrid.summary.ddacc1, ncol=2, widths=c(0.49, 0.62)))
dev.off()


#remove legend from panel A, add label
lampyrid.summary.week1.tc<-lampyrid.summary.week.timechunk+guides(fill=FALSE)+
  annotate("text", x=20, y=4.2, label="A", size=14)
#remove Y axis title from panel B, add label
lampyrid.summary.ddacc1.tc<-lampyrid.summary.ddacc.timechunk+ylab(NULL)+
  annotate("text", x=255, y=4.2, label="B", size=14)
#stack it together
grid.arrange(arrangeGrob(lampyrid.summary.week1.tc, lampyrid.summary.ddacc1.tc, ncol=2, widths=c(0.49, 0.62)))


#save to pdf
pdf("figure4tc.pdf", height=6, width=10)
grid.arrange(arrangeGrob(lampyrid.summary.week1.tc, lampyrid.summary.ddacc1.tc, ncol=2, widths=c(0.49, 0.62)))
dev.off()


#we want to look at captures by treatment 
#when we look at it by plant community (habitat), things get a little wackier because of the three year crop rotation. 
#It looks like we get very good beahvior of the loess when we use TREAT_DESC

captures.by.treatment <- lampyrid.weather %>%
  group_by(year, timechunk, study, TREAT_DESC) %>%
  summarise(
    total = sum(ADULTS, na.rm = TRUE),
    traps = sum(TRAPS, na.rm = TRUE),
    avg   = sum(ADULTS, na.rm = TRUE) / sum(TRAPS, na.rm = TRUE),
    .groups = "drop"
  )

# let's look at captures by treatment in the broadest sense first

treatment.boxplot <- ggplot(captures.by.treatment,
                            aes(x = factor(TREAT_DESC), y = avg, fill = factor(study))) +
  geom_boxplot(colour = "black", position = position_dodge(width = 0.8)) +
  theme_bw(base_size = 20) +
  xlab("\nTreatment") +
  ylab("Adults per trap\n") +
  theme(axis.text.x = element_text(angle = 90))+
  guides(fill=guide_legend(title="Study"))

treatment.boxplot

treatment.boxplot.tc <- ggplot(captures.by.treatment,
                            aes(x = factor(TREAT_DESC), y = avg, fill = factor(timechunk))) +
  geom_boxplot(colour = "black", position = position_dodge(width = 0.8)) +
  facet_wrap(~timechunk, ncol = 1) +
  scale_fill_manual(values=pal1)+
  theme_bw(base_size = 20) +
  xlab("\nTreatment") +
  ylab("Adults per trap\n") +
  theme(axis.text.x = element_text(angle = 90))+
  guides(fill=guide_legend(title="Time block"))
  
treatment.boxplot.tc

#save to pdf
pdf("figure1.pdf", height=5, width=10)
treatment.boxplot
dev.off()

pdf("figure1tc.pdf", height=20, width=10)
treatment.boxplot.tc
dev.off()

#looks to me like we are most likely to capture fireflies in annual herbaceous crops with the least soil disturbance
#forage, and no till. Hmm.


#and now we look at captures by treatment over the years

lampyrid.summary.treatment<-ggplot(captures.by.treatment, aes(year, avg, 
                                                              fill=as.factor(TREAT_DESC)))+
  #scale_fill_brewer(palette="Set3")+
  geom_point(colour="black", pch=21, size=4)+
  geom_smooth(aes(year, avg, fill=NULL), colour="black", se=FALSE, method="gam")+
  theme_bw(base_size = 20)+
  guides(fill=guide_legend(title="Treatment"))+
  theme(legend.key=element_blank())+
  xlab("\nYear")+
  ylab("Adults per trap\n")
  
lampyrid.summary.treatment

#save to pdf
pdf("figure2.pdf", height=6, width=8)
lampyrid.summary.treatment
dev.off()

#an interesting population cycling pattern emerges, but it doesn't look like there's major changes of crop use
#At least not at the yearly resolution
#we can investigate this futher with a multivariate analysis later
#regardless of how we plot it, we see an interesting pattern in the population variation- basically, a 6-7 year oscillation.
#so the question is, is there and obvious environmental cause?

#we want to look at captures by treatment relative to degree day accumulation too- are peaks earlier or later by crop? 

captures.by.treatment.dd <- lampyrid.weather %>%
  group_by(year, timechunk, study, week, TREAT_DESC) %>%
  summarise(
    total = sum(ADULTS, na.rm = TRUE),
    traps = sum(TRAPS, na.rm = TRUE),
    avg   = sum(ADULTS, na.rm = TRUE) / sum(TRAPS, na.rm = TRUE),
    ddacc = max(dd.accum, na.rm = TRUE),
    .groups = "drop"
  )


lampyrid.summary.treatment.dd<-ggplot(captures.by.treatment.dd, aes(ddacc, avg, 
                                                                    fill=as.factor(TREAT_DESC)))+
  #scale_fill_brewer(palette="Set3")+
  geom_point(colour="black", pch=21, size=4)+
  geom_smooth(colour="black", se=FALSE)+
  theme_bw(base_size = 20)+
  guides(fill=guide_legend(title="Treatment"))+
  theme(legend.key=element_blank())+
  xlab("\nDegree day accumulation")+
  ylab("Adults per trap\n")
lampyrid.summary.treatment.dd

#save to pdf
#pdf("lampyridsummarytreatmentdd.pdf", height=6, width=8)
#lampyrid.summary.treatment.dd
#dev.off()

#it looks like peaks by degree day accumulation is roughly synced by crop. We'll need to quantify how crop 
#use varies between crops but it looks like these factors do not interact with time. Good! makes our analysis
#more strightforward

#Let's see if there's anyting obvious in the weather data that explains the population cycling over time 
#that we saw above

#compute yearly weather summary from weather data (do't want this calulation to be affectred by length of sampling season)
weather.by.year <- weather1 %>%
  group_by(year) %>%
  summarise(
    precip    = sum(prec.accum, na.rm = TRUE),
    rain.days = sum(rain.days, na.rm = TRUE),
    ddacc     = max(dd.accum, na.rm = TRUE),
    .groups = "drop"
  )
#plot degree day accumulations by year, see if that explains it

ddacc.summary.year<-ggplot(weather.by.year, aes(x=as.factor(year), y=ddacc, fill=as.factor(year)))+
  #scale_fill_manual(values=pal)+
  geom_bar(stat="identity", colour="black")+
  theme_bw(base_size = 20)+
  guides(fill=FALSE)+
  ylab("\nDegree day accumulation\n")+
  xlab("\nYear\n")+
  theme(axis.text.x=element_text(angle=90))

ddacc.summary.year

#save to pdf
#pdf("ddaccsummaryyear.pdf", height=6, width=8)
#ddacc.summary.year
#dev.off()

#what about amount of precipitation? say number of rainy days
rainday.summary.year<-ggplot(weather.by.year, aes(x=as.factor(year), y=rain.days, fill=as.factor(year)))+
  #scale_fill_manual(values=pal)+
  geom_bar(stat="identity", colour="black")+
  theme_bw(base_size = 20)+
  guides(fill=FALSE)+
  ylab("\nNumberof rainy days\n")+
  xlab("\nYear\n")+
  theme(axis.text.x=element_text(angle=90))

rainday.summary.year

#save to pdf
#pdf("raindaysummaryyear.pdf", height=6, width=8)
#rainday.summary.year
#dev.off()

#and total precipitation
precip.summary.year<-ggplot(weather.by.year, aes(x=as.factor(year), y=precip, fill=as.factor(year)))+
  #scale_fill_manual(values=pal)+
  geom_bar(stat="identity", colour="black")+
  theme_bw(base_size = 20)+
  guides(fill=FALSE)+
  ylab("\nTotal precipitation (mm)\n")+
  xlab("\nYear\n")+
  theme(axis.text.x=element_text(angle=90))

precip.summary.year

#save to pdf
#pdf("precipsummaryyear.pdf", height=6, width=8)
#precip.summary.year
#dev.off()


#is there a relationship between rain and degree day accumulation? 
plot(weather.by.year$precip,weather.by.year$ddacc)
#not much, though there are a few hot-dry and a few cold-wet years
#I don't think we need to go down this rabbit hole for the present analysis
 



#multivariate analysis. So we want to see if the habitat use patterns of the lampyrids have
#changed, both within season and through the years
#to do this, we'll need to reshape the data into two different matrices where we have 
#abundance of fireflies by TREAT_DESC at yearly and weekly resolutions- a cros-tab,
#wide format data. 

#start by building the matrices
#we can use our previously melted data fram 'lampyrid1' and cast it as needed
#because of unequal numbers of reps between forest and main sites, but same number of subsamples 
#per rep, we'll treat subsamples as rep for this analysis and pool by rep instead

#cast at the yearly resolution first
landscape.year<-dcast(lampyrid1, year+timechunk+study+STATION~TREAT_DESC, sum)
landscape.week<-dcast(lampyrid1, year+timechunk+study+week+STATION~TREAT_DESC, sum)

#there are some weeks where zero fireflies were captured. We need to remove these 
#weeks from the matrix before we can continue-

landscape.week$sums<-rowSums(landscape.week[6:15])
landscape.week<-landscape.week[which(landscape.week$sums>0),]
landscape.week$sums<-NULL

#now we need to create 'environmental' matricies- corresponding environmental 
#variables that may offer explanations about what is going on when we run our 
#multivariate analysis
#we already computed 'weather.by.year' but will need to also compute the same for 
#our weekly analysis
weather.by.week <- weather1 %>%
  group_by(year, week) %>%
  summarise(
    precip    = max(prec.accum, na.rm = TRUE),
    rain.days = sum(rain.days, na.rm = TRUE),
    ddacc     = max(dd.accum, na.rm = TRUE),
    precip.0  = max(prec.accum.0, na.rm = TRUE),
    .groups = "drop"
  )

#now create the environmental matrix, preserving order from the community matricies by
#creating them from the community matrix

env.landscape.year<-landscape.year[,1:4]
env.landscape.week<-landscape.week[,1:5]

#we now need to pull our weather summary data into these matrices
env.landscape.year<-merge(env.landscape.year, weather.by.year, by=c("year"), all.x=TRUE)
env.landscape.week<-merge(env.landscape.week, weather.by.week, by=c("year", "week"), all.x=TRUE)

#finally strip out the env data
landscape.year<-landscape.year[,5:14]
landscape.week<-landscape.week[,6:15]

#Ok! data is ready for some NMDSing! WOOO
library(vegan)

ord.year<-metaMDS(landscape.year, autotransform=TRUE)
ord.year


#environmental fit- are any environmental factors driving habitat use patterns? looks like rainy days
#are the only significant factor

#fit.year<-envfit(ord.year~rain.days, env.landscape.year, perm=999)
#summary(fit.year)
#fit.year

#so, MetaMDS assumes the x axis of our matrix is species and y is sites. We are
#screwing with this by instead looking at sites over samples for one species. So when I call "sites"
#here I'm actually calling sampling times. Just thought you should know

par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
plot(ord.year, disp='sites', type='n')
with(env.landscape.year, points(ord.year, display = "sites", col = "black", pch = 21, bg = pal[as.factor(study)], cex=1.5))
ordilabel(ord.year, display="species", cex=0.75, col="black")
with(env.landscape.year, legend("right", legend = levels(as.factor(study)),
                                bty = "n", col = "black", pch = 21, pt.bg = pal, 
                                cex=1, pt.cex=1.5, inset=c(-0.2, 0), title="Study"))


par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
plot(ord.year, disp='sites', type='n')
with(env.landscape.year, points(ord.year, display = "sites", col = "black", pch = 21, bg = pal1[as.factor(timechunk)], cex=1.5))
ordilabel(ord.year, display="species", cex=0.75, col="black")
with(env.landscape.year, legend("right", legend = levels(as.factor(timechunk)),
                                bty = "n", col = "black", pch = 21, pt.bg = pal1, 
                                cex=1, pt.cex=1.5, inset=c(-0.2, 0), title="Time block"))


#save to pdf
#pdf("NMDShabitatuseyear.pdf", height=6, width=8)
#par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
#plot(ord.year, disp='sites', type='n')
#with(env.landscape.year, points(ord.year, display = "sites", col = "black", pch = 21, bg = pal[as.factor(year)], cex=1.5))
#plot(fit.year, col="red")
#ordilabel(ord.year, display="species", cex=0.75, col="black")
#with(env.landscape.year, legend("right", legend = levels(as.factor(year)),
#                                bty = "n", col = "black", pch = 21, pt.bg = pal, 
#                                cex=1, pt.cex=1.5, inset=c(-0.2, 0), title="Year"))

#dev.off()

#repeat with week?

#ord.week<-metaMDS(landscape.week, autotransform=TRUE)
#ord.week

##week and degree day accumulation are the only factors significantly associated with habitat use at the weekly resolution
#fit.week<-envfit(ord.week~week+ddacc, data=env.landscape.week, perm=999)
#summary(fit.week)
#fit.week

#par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
#plot(ord.week, disp='sites', type='n')
#with(env.landscape.week, points(ord.week, display = "sites", col = "black", pch = 21, bg = pal[as.factor(year)], cex=0.8))
#plot(fit.week, col="red")
#ordilabel(ord.week, display="species", cex=0.75, col="black")
#with(env.landscape.week, legend("right", legend = levels(as.factor(year)),
#                                bty = "n", col = "black", pch = 21, pt.bg = pal, 
#                                cex=1, pt.cex=1.5, inset=c(-0.2, 0), title="Year"))

#save to pdf
#pdf("NMDShabitatuseweek.pdf", height=6, width=8)
#par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
#plot(ord.week, disp='sites', type='n')
#with(env.landscape.week, points(ord.week, display = "sites", col = "black", pch = 21, bg = pal[as.factor(year)], cex=0.8))
#plot(fit.week, col="red")
#ordilabel(ord.week, display="species", cex=0.75, col="black")
#with(env.landscape.week, legend("right", legend = levels(as.factor(year)),
#                                bty = "n", col = "black", pch = 21, pt.bg = pal, 
#                               cex=1, pt.cex=1.5, inset=c(-0.2, 0), title="Year"))
#dev.off()

#plot two plots together 
pdf("figure3.pdf", height=8, width=8)
par(mfrow=c(2,1), mar=c(4.1, 4.8, 1.5, 8.1),xpd=TRUE) 

plot(ord.year, disp='sites', type='n')
with(env.landscape.year, points(ord.year, display = "sites", col = "black", pch = 21, bg = pal[as.factor(study)], cex=1.5))
ordilabel(ord.year, display="species", cex=0.75, col="black")
with(env.landscape.year, legend("topright", legend = levels(as.factor(study)),
                                bty = "n", col = "black", pch = 21, pt.bg = pal, 
                                cex=1, pt.cex=1.5, inset=c(-0.2, 0), title="Study"))
text(-1,0.23, "A", cex=2)

plot(ord.year, disp='sites', type='n')
with(env.landscape.year, points(ord.year, display = "sites", col = "black", pch = 21, bg = pal1[as.factor(timechunk)], cex=1.5))
ordilabel(ord.year, display="species", cex=0.75, col="black")
with(env.landscape.year, legend("topright", legend = levels(as.factor(timechunk)),
                                bty = "n", col = "black", pch = 21, pt.bg = pal1, 
                                cex=1, pt.cex=1.5, inset=c(-0.2, 0), title="Time block"))
text(-1,0.23, "B", cex=2)
dev.off()

#finally, let's do some generalized linear modelling to see what's important and if we can explain what's going on
#we've clearly got a quadratic resonse to degree day accumulation, and since we're dealing with count data, we should model 
#it using a poisson structure (or negative binomial if we've got a high residual deviance)
#we'll use the MASS package

library(MASS)
#create a squared term so we can build a model with a quadratic in it
lampyrid.weather$dd.accum2<-(lampyrid.weather$dd.accum)^2




#After some initial fiddling, we find out that rain.days is a better predictor than precipitation accumulation, and given that these are 
#seriously autocorrelated, let's just use rain days
#we know TREAT_DESC is probably not important in interacting with dd.acc as we did not observe major tends by treatment when we looked at 
#trends in captures by degree day accumulation by  treatment so we won't look for interactions
#finally, because of convergence problems using glm.nb, we determined theta (dispersion parameter) iteratively
#using glm with a negative binomial family instead. Less elegant and more labour intensive- but really brought residual deviance and AIC
#values down, indicating a much better fit

lam_model<-glm(ADULTS~dd.accum+dd.accum2*(as.factor(year))+TREAT_DESC, 
               offset=TRAPS, data=lampyrid.weather, family=negative.binomial(0.6))
summary(lam_model)


#Let's just do a quick look to see how our model predictions look
x<-(1:length(lampyrid.weather$DOY))
lampyrid.weather$predicted<-(exp(predict(lam_model,lampyrid.weather)))

plot(x, lampyrid.weather$predicted, ylim=c(0, 100))
plot(x, lampyrid.weather$ADULTS, ylim=c(0, 100))

#let's reshape these data and make a nice plot to show how well the model fits peaks

model.performance<-as.data.frame(cbind(x,lampyrid.weather$predicted,lampyrid.weather$ADULTS))
names(model.performance)[1]<-"number"
names(model.performance)[2]<-"Predicted"
names(model.performance)[3]<-"Observed"

model.performance.1<-melt(model.performance, id="number")

#now we can do a two faceted plot to show this

model.plot<-ggplot(model.performance.1, aes(number, value, fill=as.factor(variable)))+
  #scale_fill_manual(values=pal)+
  geom_point(colour="black", pch=21, size=2)+
  theme_bw(base_size = 20)+
  ylim(0,50)+
  facet_wrap(~variable, ncol=1)+
  guides(fill=FALSE)+
  xlab("\nObservation number")+
  ylab("# Adults captured\n")
model.plot

#save to pdf
pdf("figure5.pdf", height=6, width=8)
model.plot
dev.off()


#Let's see how well the model works when we look at data with a lower resolution 
#(to damp out a bit of sampling variability + make it comparable to our smoothed plots from before)

lampyrid.weather.summary <- lampyrid.weather %>%
  group_by(year, week) %>%
  summarise(
    ADULTS    = sum(ADULTS, na.rm = TRUE),
    TRAPS     = sum(TRAPS, na.rm = TRUE),
    predicted = sum(predicted, na.rm = TRUE),
    avg       = sum(ADULTS, na.rm = TRUE) / sum(TRAPS, na.rm = TRUE),
    avgpred   = sum(predicted, na.rm = TRUE) / sum(TRAPS, na.rm = TRUE),
    dd.accum  = max(dd.accum, na.rm = TRUE),
    rain.days = max(rain.days, na.rm = TRUE),
    .groups = "drop"
  )

lampyrid.summary.ddacc.PRED<-ggplot(lampyrid.weather.summary, aes(dd.accum, avg, 
                                                     fill=factor(year)))+
  
  #scale_fill_manual(values=pal)+
  geom_smooth(aes(dd.accum, avgpred), color="black", se=FALSE)+
  geom_point(colour="black", pch=21, size=4)+
  theme_bw(base_size = 20)+
  guides(fill=guide_legend(title="Year"))+
  theme(legend.key=element_blank())+
  xlab("\nDegree day accumulation")+
  ylab("Adults per trap\n")

lampyrid.summary.ddacc.PRED

#save to pdf
#pdf("modelddsmoothwithpredicted.pdf", height=6, width=8)
#lampyrid.summary.ddacc.PRED
#dev.off()

#Cool! So now we want to see how the peak is varying by year, and see if there are any environmental parameters that explain it
#we first need to extract the coefficients from the lam_model

coef<-as.data.frame(summary(lam_model)$coefficients)
#get rid of those pesky t and P statistics
coef<-coef[,1:2]



ddcoef<-coef$Estimate[2]
dd2coef<-coef$Estimate[3]
ddcoef.err<-coef$"Std. Error"[2]
dd2coef.err<-coef$"Std. Error"[3]

#create a vector of years
year<-(2004:2025)

#create vector of coefficients
#remember 2004 is the 'intercept' vector so it's unmodified, we'll give it a year 
#modifier and error of zero

yearcoef<-c(0, coef$Estimate[34:54])
yearcoef.err<-c(0, coef$"Std. Error"[34:54])

#create a new data frame to integrate the coeficients with the year vector
peaks<-as.data.frame(cbind(year, yearcoef, yearcoef.err))

#peak will occur at -ddcoeficient/(2(dd2coeficient+year coeficient))
peaks$peak<- -ddcoef/(2*(dd2coef+yearcoef))

#peak error calculated using the general error propagation formula
#this will be a bit inelegant, but I calculated the partial derrivatives 
#relative to each variable myself!
peaks$peak.err<-sqrt((2*(dd2coef+yearcoef))^(-2) *ddcoef.err^2+
                       (ddcoef/(2*(dd2coef+yearcoef))^2)^2*(dd2coef.err^2+yearcoef.err^2))

#let's visualize this!

peaks.year<-ggplot(peaks, aes(x=as.factor(year), y=peak, fill=as.factor(year)))+
  #scale_fill_manual(values=pal)+
  geom_bar(stat="identity", colour="black")+
  geom_errorbar(aes(ymin=peak-peak.err, ymax=peak+peak.err))+
  theme_bw(base_size = 20)+
  guides(fill=FALSE)+
  ylab("\nDD at peak emergence\n")+
  xlab("\nYear\n")+
  theme(axis.text.x=element_text(angle=90))
peaks.year

#save to pdf
pdf("figure6.pdf", height=6, width=8)
peaks.year
dev.off()

#ok, now let's figure out which week each peak occurred in
weeks<-c()
for (i in 1:length(peaks$year)){
  #set an arbitrariliy high 'last week' dd caccumulation so the first condition is never
  #met in the first iteration for each year
  ddlastweek<-10000
    for(j in 1:length(weather.by.week$year)){
      if ((peaks$year[i]==weather.by.week$year[j])&
          (peaks$peak[i]>ddlastweek)&
          (peaks$peak[i]<weather.by.week$ddacc[j])){
        week<-weather.by.week$week[j]
        weeks<-c(weeks, week)
        break
      }
      else{
        ddlastweek<-weather.by.week$ddacc[j]
      }
    }
}
#put it into our peak object
peaks$week<-weeks

#this allows us to merge in other relevant data with our peak dataset
peaks<-merge(peaks, captures.by.year, by=c("year"), all.x=TRUE)
peaks$ddacc<-NULL
peaks<-merge(peaks, weather.by.week, by=c("year", "week"), all.x=TRUE)


dd.vs.precip<-ggplot(peaks, aes(precip.0, peak))+
  #scale_fill_manual(values=pal)+
  geom_smooth(method="lm", formula=y~poly(x,2), se=FALSE, color="black")+
  geom_errorbar(aes(ymin=peak-peak.err, ymax=peak+peak.err))+
  geom_point(aes(fill=as.factor(year)), pch=21, color="black", size=4)+
  theme_bw(base_size = 20)+
  guides(fill=guide_legend(title="Year"))+
  theme(legend.key=element_blank())+
  xlab("\nPrecipitation accumulation (mm)")+
  ylab("DD at peak emergence\n")

dd.vs.precip  

#save to pdf
pdf("figure7.pdf", height=6, width=8)
dd.vs.precip
dev.off()


peaks$precip.02<-peaks$precip.0^2

env.test<-glm(peak~precip.0+precip.02, data=peaks, family="gaussian")
summary(env.test)

######################
# Begin GAM phenology analysis

#while we're at this, let's make some yearly summary data that will allow us to
#characterize weather by year. Since it looks like seasonality plays a role in within-year 
#partitioning (spoilers!) let's get some accumulations at key points in the season- let's do
#week 25, 30, and 35 and get dd accum, precip accum for each year

keypoints<-c(20, 25, 30, 35)

weather_keypoints<-weather_weekly[which(weather_weekly$week  %in% keypoints),]

#cull out the non-accumulated data

weather_keypoints1<-weather_keypoints[,c(1:2, 6, 12)]

#now we need to recast each of the response columns as their own unique responses by week
#dd accum
library(reshape2)
dd.year<-dcast(weather_keypoints1, year~week,
               value.var ="dd.accum",  sum)
colnames(dd.year)<-c("year", "dd20", "dd25", "dd30", "dd35")
#create metrics for DIFFERENCE from last time point too
dd.year$dd25.dif<-dd.year$dd25-dd.year$dd20
dd.year$dd30.dif<-dd.year$dd30-dd.year$dd25
dd.year$dd35.dif<-dd.year$dd35-dd.year$dd30

#precip
precip.year<-dcast(weather_keypoints1, year~week,
                   value.var ="yearly.precip.accum",  sum)

colnames(precip.year)<-c("year", "precip20", "precip25", "precip30", "precip35")

#create metrics for DIFFERENCE from last time point too
precip.year$precip25.dif<-precip.year$precip25-precip.year$precip20
precip.year$precip30.dif<-precip.year$precip30-precip.year$precip25
precip.year$precip35.dif<-precip.year$precip35-precip.year$precip30




# let's rough in our gam models. Just like with the multivariate analysis, we'll look at
#two different scales- within year dynamics and between year dynamics
library(mgcv)
library(visreg)
library(ggpubr)
library(Hmisc)
library(cowplot)

#pearson correlation of environmental parameters

round(cor(lampyrid.weather[11:22], method="pearson"), digits=2)
#start withe the drivers of within-year variation




##################### Lampy gam


#by study
gam_lampy<-gam(ADULTS~s(dd.accum, sp=1, by= study)+
                 s(week, sp=1, by=study)+
                    s(weekly.precip, sp=1, by=study)+
                    s(max.temp, sp=1, by=study)+
                    s(min.temp, sp=1, by=study)+ 
                    TREAT_DESC*study+
                    #s(year, sp=1)+
                    offset(log(TRAPS)), method="REML", data=lampyrid.weather, family="quasipoisson")
summary(gam_lampy)
anova(gam_lampy) #significance of parametric terms


# #check concurvity
# concurvity(gam_lampy)
# #looks fine, sweet!
# gam.check(gam_lampy)


withinyear.dd.lampy<-visreg(gam_lampy, "dd.accum", "study", partial=F, rug=FALSE, 
                            overlay=T, scale="response", gg=TRUE)+
  scale_colour_manual(values = pal) +
  labs(x="Degree day accumulation", y="")+
  theme_classic()+ theme(legend.position = "none")+
  coord_cartesian(xlim = c(250, 1200), ylim=c(0, 20))

withinyear.dd.lampy

withinyear.week.lampy<-visreg(gam_lampy, "week", "study", partial=F, rug=FALSE, 
                            overlay=T, scale="response", gg=TRUE)+
  scale_colour_manual(values = pal) +
  labs(x="Week of year", y="")+
  theme_classic()+ theme(legend.position = "none")+
  coord_cartesian(xlim = c(22, 35), ylim=c(0, 20))

withinyear.week.lampy

withinyear.maxt.lampy<-visreg(gam_lampy, "max.temp", "study", partial=F, rug=FALSE, 
                              overlay=T, scale="response", gg=TRUE)+
  scale_colour_manual(values = pal) +
  labs(x="Maximum temperature", y="")+
  theme_classic()+ theme(legend.position = "none")+
  coord_cartesian(xlim = c(25, 35), ylim=c(0, 20))

withinyear.maxt.lampy

withinyear.mint.lampy<-visreg(gam_lampy, "min.temp", "study", partial=F, rug=FALSE, 
                              overlay=T, scale="response", gg=TRUE)+
  scale_colour_manual(values = pal) +
  labs(x="Minimum temperature", y="")+
  theme_classic()+ theme(legend.position = "none")+
  coord_cartesian(xlim = c(11, 19), ylim=c(0, 20))

withinyear.mint.lampy

withinyear.precip.lampy<-visreg(gam_lampy, "weekly.precip", "study", partial=F, rug=FALSE, 
                              overlay=T, scale="response", gg=TRUE)+
  scale_colour_manual(values = pal) +
  labs(x="Weekly precipitation", y="")+
  theme_classic()+ theme(legend.position = "none")+
  coord_cartesian(xlim = c(0, 30), ylim=c(0, 20))

withinyear.precip.lampy

withinyear.habitat.lampy<-visreg(gam_lampy, "TREAT_DESC", "study", partial=F, rug=FALSE, 
                                overlay=T, scale="response", gg=TRUE)+
  scale_colour_manual(values = pal) +
  labs(x="\nTreatment", y="")+
  theme_classic()+ theme(axis.text.x = element_text(angle = 90), legend.position = "none")+
  coord_cartesian(ylim=c(0, 20))


withinyear.habitat.lampy

#create a legend to pull
withinyear.dd.lampy.leg <- withinyear.dd.lampy +
  theme(legend.position = "right")+
  guides(fill = guide_legend(title = "Time block"),
         color = guide_legend(title = "Time block"))
legend_lampy <- get_legend(withinyear.dd.lampy.leg)


#plot the withinyear model all together:

withinyear.modelplot.lampy<-plot_grid(withinyear.dd.lampy,withinyear.week.lampy,  
                                      withinyear.mint.lampy, withinyear.maxt.lampy, 
                                      withinyear.precip.lampy, withinyear.habitat.lampy,
                                      ncol=1, rel_heights = c(1, 1, 1, 1, 1, 2), labels=c('A', 'B', 'C', 'D', 'E', 'F'), align="v")
withinyear.modelplot.lampy

#create overall y axis label
partresid<-text_grob(paste("        Partial residual captures"), color="black", size=12, rot=90)


#now replot with grob label
withinyear.plot.lampy<-plot_grid(partresid, withinyear.modelplot.lampy, ncol=2, rel_widths = c(1,11))

withinyear.plot.lampy


final_plot <- plot_grid(withinyear.plot.lampy,
                          legend_lampy,  ncol = 2,
                          rel_widths = c(12, 4))

final_plot


pdf("figurewithinyeargamlampybystudy.pdf", height=10, width=6)
final_plot
dev.off()
###

#Repeat the gam model with timechunk instead of study




#by timechunk
gam_lampy.t<-gam(ADULTS ~
    s(dd.accum, by=factor(timechunk, ordered=FALSE), sp=1)+
    s(week, by=factor(timechunk, ordered=FALSE), sp=1) +
    s(weekly.precip, by=factor(timechunk, ordered=FALSE), sp=1, k=4) +
    s(max.temp, by=factor(timechunk, ordered=FALSE), sp=1, k=4) +
    s(min.temp, by=factor(timechunk, ordered=FALSE), sp=1, k=4) +
    TREAT_DESC * factor(timechunk, ordered=FALSE) +
    offset(log(TRAPS)),
  method = "REML",
  data = lampyrid.weather,
  family = quasipoisson
)



summary(gam_lampy.t)
anova(gam_lampy.t) #significance of parametric terms



# #check concurvity
# concurvity(gam_lampy.t)
# #looks fine, sweet!
# gam.check(gam_lampy.t)


withinyear.dd.lampy.t<-visreg(gam_lampy.t, "dd.accum", "timechunk", partial=F, rug=FALSE,
                              overlay=T, scale="response", gg=TRUE)+
  scale_colour_manual(values = pal1) +
  scale_fill_manual(values = alpha(pal1,0.2)) +
  labs(x="Degree day accumulation", y="")+
  theme_classic()+ theme(legend.position = "none")+
  coord_cartesian(xlim = c(250, 1200), ylim=c(0, 20))

withinyear.dd.lampy.t

withinyear.week.lampy.t<-visreg(gam_lampy.t, "week", "timechunk", partial=F, rug=FALSE, 
                                overlay=T, scale="response", gg=TRUE)+
  scale_colour_manual(values = pal1) +
  scale_fill_manual(values = alpha(pal1,0.2)) +
  labs(x="Week of year", y="")+
  theme_classic()+ theme(legend.position = "none")+
  coord_cartesian(xlim = c(22, 35), ylim=c(0, 20))

withinyear.week.lampy.t

withinyear.maxt.lampy.t<-visreg(gam_lampy.t, "max.temp", "timechunk", partial=F, rug=FALSE,
                                overlay=T, scale="response", gg=TRUE)+
  scale_colour_manual(values = pal1) +
  scale_fill_manual(values = alpha(pal1,0.2)) +
  labs(x="Maximum temperature", y="")+
  theme_classic()+ theme(legend.position = "none")+
  coord_cartesian(xlim = c(25, 35), ylim=c(0, 20))

withinyear.maxt.lampy.t

withinyear.mint.lampy.t<-visreg(gam_lampy.t, "min.temp", "timechunk", partial=F, rug=FALSE,
                                overlay=T, scale="response", gg=TRUE)+
  scale_colour_manual(values = pal1) +
  scale_fill_manual(values = alpha(pal1,0.2)) +
  labs(x="Minimum temperature", y="")+
  theme_classic()+ theme(legend.position = "none")+
  coord_cartesian(xlim = c(11, 19), ylim=c(0, 20))

withinyear.mint.lampy.t

withinyear.precip.lampy.t<-visreg(gam_lampy.t, "weekly.precip", "timechunk", partial=F, rug=FALSE, 
                                  overlay=T, scale="response", gg=TRUE)+
  scale_colour_manual(values = pal1) +
  scale_fill_manual(values = alpha(pal1,0.2)) +
  labs(x="Weekly precipitation", y="")+
  theme_classic()+ theme(legend.position = "none")+
  coord_cartesian(xlim = c(0, 30), ylim=c(0, 20))

withinyear.precip.lampy.t

withinyear.habitat.lampy.t<-visreg(gam_lampy.t, "TREAT_DESC", "timechunk", partial=F, rug=FALSE,
                                   overlay=T, scale="response", gg=TRUE)+
  scale_colour_manual(values = pal1) +
  scale_fill_manual(values = alpha(pal1,0.2)) +
  labs(x="\nTreatment", y="")+
  theme_classic()+ theme(axis.text.x = element_text(angle = 90), legend.position = "none")+
  coord_cartesian(ylim=c(0, 20))


withinyear.habitat.lampy.t

#create a legend to pull
withinyear.dd.lampy.t.leg <- withinyear.dd.lampy.t +
  theme(legend.position = "right")+
  guides(fill = guide_legend(title = "Time block"),
         color = guide_legend(title = "Time block"))
legend_lampyt <- get_legend(withinyear.dd.lampy.t.leg)


#plot the withinyear.t model all together:

withinyear.modelplot.lampy.t<-plot_grid(withinyear.dd.lampy.t,withinyear.week.lampy.t,  
                                        withinyear.mint.lampy.t, withinyear.maxt.lampy.t, 
                                        withinyear.precip.lampy.t, withinyear.habitat.lampy.t,
                                        ncol=1, rel_heights = c(1, 1, 1, 1, 1, 2), labels=c('A', 'B', 'C', 'D', 'E', 'F'), align="v")
withinyear.modelplot.lampy.t

#create overall y axis label
partresid<-text_grob(paste("        Partial residual captures"), color="black", size=12, rot=90)


#now replot with grob label
withinyear.plot.lampy.t<-plot_grid(partresid, withinyear.modelplot.lampy.t, ncol=2, rel_widths = c(1,11))

final_plot.t <- plot_grid(withinyear.plot.lampy.t,
  legend_lampyt,  ncol = 2,
  rel_widths = c(12, 4))

final_plot.t



pdf("figurewithinyeargamlampybytimechunk.pdf", height=10, width=6)
final_plot.t
dev.off()


#we'll want to extract the data associated with activity peaks

#ok, I think we found the method we should use! here's the tutorial:
# https://fromthebottomoftheheap.net/2014/05/15/identifying-periods-of-change-with-gams/

#first we create a new dataframe that we can use our model to predict the values for optima
#we use good guesses at values for other optima to create conditions where species is reasonably abundant for modelled parameter 

#create data for lampy, holding everything constant but degree days
newData.lampy.dd <- with(lampyrid.weather,
                         data.frame(dd.accum = seq(250, 1500, length = 300),#use natural range of data
                                    TRAPS=5,
                                    week=28,
                                    weekly.precip=15, # not really important
                                    max.temp=31, #maxes near 31
                                    min.temp=14, #maxes near 12
                                    study="Hermann",
                                    TREAT_DESC="Forage")) #most abundant in both time periods

#make the same frame but for 1 more degday
newData.lampy.1.dd<- with(lampyrid.weather,
                          data.frame(dd.accum = seq(251, 1501, length = 300), #use natural range of data
                                     TRAPS=5, 
                                     week=28,
                                     weekly.precip=15, # not really important
                                     max.temp=31, #maxes near 31
                                     min.temp=14, #maxes near 12
                                     study="Hermann",
                                     TREAT_DESC="Forage")) #most abundant in both time periods




# Predictions
pred0 <- predict(gam_lampy, newData.lampy.dd, type = "link")
pred1 <- predict(gam_lampy, newData.lampy.1.dd, type = "link")

# Build a clean dataframe
dd.lampy.der <- data.frame(
  dd.accum = newData.lampy.dd$dd.accum,
  pred0 = pred0,
  pred1 = pred1
)

# Numerical derivative
dd.lampy.der$slope <- (dd.lampy.der$pred1 - dd.lampy.der$pred0)

# Peak = point where slope switches from + to -
peak_row <- dd.lampy.der[which.max(dd.lampy.der$pred0), ]
peak_row

#    dd.accum    pred0    pred1         slope
# 130 789.2977 2.939264 2.939143 -0.0001209834

#same for new data

#create data for lampy, holding everything constant but degree days
newData.lampy.dd <- with(lampyrid.weather,
                         data.frame(dd.accum = seq(250, 1500, length = 300),#use natural range of data
                                    TRAPS=5,
                                    week=28,
                                    weekly.precip=15, # not really important
                                    max.temp=31, #maxes near 31
                                    min.temp=14, #maxes near 12
                                    study="New",
                                    TREAT_DESC="Forage")) #most abundant in both time periods

#make the same frame but for 1 more degday
newData.lampy.1.dd<- with(lampyrid.weather,
                          data.frame(dd.accum = seq(251, 1501, length = 300), #use natural range of data
                                     TRAPS=5, 
                                     week=28,
                                     weekly.precip=15, # not really important
                                     max.temp=31, #maxes near 31
                                     min.temp=14, #maxes near 12
                                     study="New",
                                     TREAT_DESC="Forage")) #most abundant in both time periods




# Predictions
pred0 <- predict(gam_lampy, newData.lampy.dd, type = "link")
pred1 <- predict(gam_lampy, newData.lampy.1.dd, type = "link")

# Build a clean dataframe
dd.lampy.der <- data.frame(
  dd.accum = newData.lampy.dd$dd.accum,
  pred0 = pred0,
  pred1 = pred1
)

# Numerical derivative
dd.lampy.der$slope <- (dd.lampy.der$pred1 - dd.lampy.der$pred0)

# Peak = point where slope switches from + to -
peak_row <- dd.lampy.der[which.max(dd.lampy.der$pred0), ]
peak_row


#goes to heck - peak is at the poorly fit end
#dd.accum    pred0    pred1      slope
#300     1500 5.380359 5.387464 0.00710511

#do it for week

# Sequence of week values to explore
week_seq <- seq(18, 35, length = 300)

# Create new data frame holding everything constant except week
newData.lampy.week <- data.frame(
  dd.accum = 758,            # fixed degree days
  TRAPS = 5,
  week = week_seq,
  weekly.precip = 15,
  max.temp = 31,
  min.temp = 14,
  study = "Hermann",
  TREAT_DESC = "Forage"
)

# Create same frame with week + 1 for numerical derivative
newData.lampy.week.1 <- newData.lampy.week
newData.lampy.week.1$week <- newData.lampy.week.1$week + 1/300  # small increment for derivative

# Predict on the link scale
pred0 <- predict(gam_lampy, newData.lampy.week, type = "link")
pred1 <- predict(gam_lampy, newData.lampy.week.1, type = "link")

# Build clean dataframe
week.lampy.der <- data.frame(
  week = week_seq,
  pred0 = pred0,
  pred1 = pred1
)

# Numerical derivative
week.lampy.der$slope <- week.lampy.der$pred1 - week.lampy.der$pred0

# Peak = point where slope switches from + to -
peak_row <- week.lampy.der[which.max(week.lampy.der$pred0), ]
peak_row

#  week    pred0    pred1         slope
#1   18 3.993228 3.993201 -2.677338e-05

# Sequence of week values to explore
week_seq <- seq(18, 35, length = 300)

# Create new data frame holding everything constant except week
newData.lampy.week <- data.frame(
  dd.accum = 758,            # fixed degree days
  TRAPS = 5,
  week = week_seq,
  weekly.precip = 15,
  max.temp = 31,
  min.temp = 14,
  study = "New",
  TREAT_DESC = "Forage"
)

# Create same frame with week + 1 for numerical derivative
newData.lampy.week.1 <- newData.lampy.week
newData.lampy.week.1$week <- newData.lampy.week.1$week + 1/300  # small increment for derivative

# Predict on the link scale
pred0 <- predict(gam_lampy, newData.lampy.week, type = "link")
pred1 <- predict(gam_lampy, newData.lampy.week.1, type = "link")

# Build clean dataframe
week.lampy.der <- data.frame(
  week = week_seq,
  pred0 = pred0,
  pred1 = pred1
)

# Numerical derivative
week.lampy.der$slope <- week.lampy.der$pred1 - week.lampy.der$pred0

# Peak = point where slope switches from + to -
peak_row <- week.lampy.der[which.max(week.lampy.der$pred0), ]
peak_row

#week    pred0    pred1         slope
#173 27.77926 2.200434 2.200429 -5.036234e-06

# Sequence of min.temp values to explore
mint_seq <- seq(0, 20, length = 300)

# Create new data frame holding everything constant except min.temp
newData.lampy.mint <- data.frame(
  dd.accum = 758,          # fixed degree days
  TRAPS = 5,
  week = 28,               # fixed week
  weekly.precip = 15,
  max.temp = 31,
  min.temp = mint_seq,     # varying min.temp
  study = "Hermann",
  TREAT_DESC = "Forage"
)

# Create same frame with min.temp + small increment for numerical derivative
newData.lampy.mint.1 <- newData.lampy.mint
newData.lampy.mint.1$min.temp <- newData.lampy.mint.1$min.temp + 1/300  # small increment

# Predict on the link scale
pred0 <- predict(gam_lampy, newData.lampy.mint, type = "link")
pred1 <- predict(gam_lampy, newData.lampy.mint.1, type = "link")

# Build clean dataframe
mint.lampy.der <- data.frame(
  min.temp = mint_seq,
  pred0 = pred0,
  pred1 = pred1
)

# Numerical derivative
mint.lampy.der$slope <- mint.lampy.der$pred1 - mint.lampy.der$pred0

# Peak = point where slope switches from + to -
peak_row <- mint.lampy.der[which.max(mint.lampy.der$pred0), ]
peak_row

#  min.temp   pred0    pred1         slope
#217 14.44816 2.93519 2.935164 -2.514417e-05


# Sequence of min.temp values to explore
mint_seq <- seq(0, 20, length = 300)

# Create new data frame holding everything constant except min.temp
newData.lampy.mint <- data.frame(
  dd.accum = 758,          # fixed degree days
  TRAPS = 5,
  week = 28,               # fixed week
  weekly.precip = 15,
  max.temp = 31,
  min.temp = mint_seq,     # varying min.temp
  study = "New",
  TREAT_DESC = "Forage"
)

# Create same frame with min.temp + small increment for numerical derivative
newData.lampy.mint.1 <- newData.lampy.mint
newData.lampy.mint.1$min.temp <- newData.lampy.mint.1$min.temp + 1/300  # small increment

# Predict on the link scale
pred0 <- predict(gam_lampy, newData.lampy.mint, type = "link")
pred1 <- predict(gam_lampy, newData.lampy.mint.1, type = "link")

# Build clean dataframe
mint.lampy.der <- data.frame(
  min.temp = mint_seq,
  pred0 = pred0,
  pred1 = pred1
)

# Numerical derivative
mint.lampy.der$slope <- mint.lampy.der$pred1 - mint.lampy.der$pred0

# Peak = point where slope switches from + to -
peak_row <- mint.lampy.der[which.max(mint.lampy.der$pred0), ]
peak_row

#min.temp    pred0   pred1       slope
#300       20 4.112051 4.11425 0.002199302

# Sequence of max.temp values to explore
maxt_seq <- seq(25, 35, length = 300)

# Create new data frame holding everything constant except max.temp
newData.lampy.maxt <- data.frame(
  dd.accum = 758,
  TRAPS = 5,
  week = 28,
  weekly.precip = 15,
  max.temp = maxt_seq,    # varying max.temp
  min.temp = 14,
  study = "Hermann",
  TREAT_DESC = "Forage"
)

# Create same frame with max.temp + small increment for numerical derivative
newData.lampy.maxt.1 <- newData.lampy.maxt
newData.lampy.maxt.1$max.temp <- newData.lampy.maxt.1$max.temp + 1/300  # small increment

# Predict on the link scale
pred0 <- predict(gam_lampy, newData.lampy.maxt, type = "link")
pred1 <- predict(gam_lampy, newData.lampy.maxt.1, type = "link")

# Build clean dataframe
maxt.lampy.der <- data.frame(
  max.temp = maxt_seq,
  pred0 = pred0,
  pred1 = pred1
)

# Numerical derivative
maxt.lampy.der$slope <- maxt.lampy.der$pred1 - maxt.lampy.der$pred0

# Peak = point where slope switches from + to -
peak_row <- maxt.lampy.der[which.max(maxt.lampy.der$pred0), ]
peak_row


 #  max.temp    pred0    pred1        slope
#235 32.82609 3.167134 3.167145 1.088545e-05

# Sequence of max.temp values to explore
maxt_seq <- seq(25, 35, length = 300)

# Create new data frame holding everything constant except max.temp
newData.lampy.maxt <- data.frame(
  dd.accum = 758,
  TRAPS = 5,
  week = 28,
  weekly.precip = 15,
  max.temp = maxt_seq,    # varying max.temp
  min.temp = 14,
  study = "New",
  TREAT_DESC = "Forage"
)

# Create same frame with max.temp + small increment for numerical derivative
newData.lampy.maxt.1 <- newData.lampy.maxt
newData.lampy.maxt.1$max.temp <- newData.lampy.maxt.1$max.temp + 1/300  # small increment

# Predict on the link scale
pred0 <- predict(gam_lampy, newData.lampy.maxt, type = "link")
pred1 <- predict(gam_lampy, newData.lampy.maxt.1, type = "link")

# Build clean dataframe
maxt.lampy.der <- data.frame(
  max.temp = maxt_seq,
  pred0 = pred0,
  pred1 = pred1
)

# Numerical derivative
maxt.lampy.der$slope <- maxt.lampy.der$pred1 - maxt.lampy.der$pred0

# Peak = point where slope switches from + to -
peak_row <- maxt.lampy.der[which.max(maxt.lampy.der$pred0), ]
peak_row

#max.temp    pred0    pred1        slope
#188 31.25418 2.201306 2.201307 1.360731e-06

# Sequence of weekly.precip values
precip_seq <- seq(0, 20, length = 300)

# Create new data frame holding everything constant except weekly.precip
newData.lampy.precip <- data.frame(
  dd.accum = 758,
  TRAPS = 5,
  week = 28,
  weekly.precip = precip_seq,    # varying precipitation
  max.temp = 31,
  min.temp = 14,
  study = "Hermann",
  TREAT_DESC = "Forage"
)

# Create same frame with small increment for numerical derivative
newData.lampy.precip.1 <- newData.lampy.precip
newData.lampy.precip.1$weekly.precip <- newData.lampy.precip.1$weekly.precip + 1/300  # small increment

# Predict on the link scale
pred0 <- predict(gam_lampy, newData.lampy.precip, type = "link")
pred1 <- predict(gam_lampy, newData.lampy.precip.1, type = "link")

# Build clean dataframe
precip.lampy.der <- data.frame(
  weekly.precip = precip_seq,
  pred0 = pred0,
  pred1 = pred1
)

# Numerical derivative
precip.lampy.der$slope <- precip.lampy.der$pred1 - precip.lampy.der$pred0

# Peak = point where slope switches from + to -
peak_row <- precip.lampy.der[which.max(precip.lampy.der$pred0), ]
peak_row

# weekly.precip    pred0   pred1        slope
#1     0 3.133279 3.13321 -6.90108e-05


# Sequence of weekly.precip values
precip_seq <- seq(0, 20, length = 300)

# Create new data frame holding everything constant except weekly.precip
newData.lampy.precip <- data.frame(
  dd.accum = 758,
  TRAPS = 5,
  week = 28,
  weekly.precip = precip_seq,    # varying precipitation
  max.temp = 31,
  min.temp = 14,
  study = "New",
  TREAT_DESC = "Forage"
)

# Create same frame with small increment for numerical derivative
newData.lampy.precip.1 <- newData.lampy.precip
newData.lampy.precip.1$weekly.precip <- newData.lampy.precip.1$weekly.precip + 1/300  # small increment

# Predict on the link scale
pred0 <- predict(gam_lampy, newData.lampy.precip, type = "link")
pred1 <- predict(gam_lampy, newData.lampy.precip.1, type = "link")

# Build clean dataframe
precip.lampy.der <- data.frame(
  weekly.precip = precip_seq,
  pred0 = pred0,
  pred1 = pred1
)

# Numerical derivative
precip.lampy.der$slope <- precip.lampy.der$pred1 - precip.lampy.der$pred0

# Peak = point where slope switches from + to -
peak_row <- precip.lampy.der[which.max(precip.lampy.der$pred0), ]
peak_row

#  weekly.precip    pred0    pred1         slope
#70      4.615385 2.279095 2.279095 -4.809917e-08
