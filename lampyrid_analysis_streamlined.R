# lampyrid analysis
#bring data in 
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
lampyrid1<-melt(lampyrid, id=c("DATE","TREAT_DESC","HABITAT","REPLICATE","STATION","newdate", "year", "DOY", "week",  "study"))
#cast the data to count up the fireflies
lampyrid2<-dcast(lampyrid1, year+DOY+week+TREAT_DESC+HABITAT+REPLICATE+study~., sum)
#cast the data to count the traps
lampyrid3<-dcast(lampyrid1, year+DOY+week+TREAT_DESC+HABITAT+REPLICATE+study~., length)

#let's rename these new vectors within the data frame
names(lampyrid2)[8]<-"ADULTS"
names(lampyrid3)[8]<-"TRAPS"

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

#create a palate based on colour brewer. 

pal<-c('#f46d43', '#74add1')


# we're interested in looking at more general trends. We'll need to produce 
#summary data to do this


captures.by.year <- lampyrid.weather %>%
  group_by(year, study) %>%
  summarise(
    total = sum(ADULTS, na.rm = TRUE),
    traps = sum(TRAPS, na.rm = TRUE),
    avg   = sum(ADULTS, na.rm = TRUE) / sum(TRAPS, na.rm = TRUE),
    ddacc = max(dd.accum, na.rm = TRUE),
    .groups = "drop"
  )

captures.by.week.year <- lampyrid.weather %>%
  group_by(year, study, week) %>%
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
  geom_point(colour="black", pch=21, size=2)+
  geom_smooth(se=FALSE, show.legend = FALSE)+
  theme_bw(base_size = 15)+
  guides(fill=guide_legend(title="Study"))+
  theme(legend.key=element_blank())+
  xlab("\nWeek")+
  ylab("Adults per trap\n")

lampyrid.summary.week


#look at captures by degree day accumulation to see if our activity pattern is clearer

lampyrid.summary.ddacc<-ggplot(captures.by.week.year, aes(ddacc, avg, 
                                                          fill=as.factor(study), color=as.factor(study)))+
  scale_fill_manual(values=pal)+
  scale_color_manual(values=pal)+
  geom_point(colour="black", pch=21, size=2)+
  geom_smooth( se=FALSE,  show.legend = FALSE)+
  theme_bw(base_size = 15)+
  guides(fill=guide_legend(title="Study"))+
  theme(legend.key=element_blank())+
  xlab("\nDegree day accumulation")+
  ylab("Adults per trap\n")

lampyrid.summary.ddacc


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
pdf("FigureS1.pdf", height=6, width=10)
grid.arrange(arrangeGrob(lampyrid.summary.week1, lampyrid.summary.ddacc1, ncol=2, widths=c(0.49, 0.62)))
dev.off()



#we want to look at captures by treatment 
#when we look at it by plant community (habitat), things get a little wackier because of the three year crop rotation. 
#It looks like we get very good behavior of the loess when we use TREAT_DESC

captures.by.treatment <- lampyrid.weather %>%
  group_by(year,  study, TREAT_DESC) %>%
  summarise(
    total = sum(ADULTS, na.rm = TRUE),
    traps = sum(TRAPS, na.rm = TRUE),
    avg   = sum(ADULTS, na.rm = TRUE) / sum(TRAPS, na.rm = TRUE),
    .groups = "drop"
  )



#captures by treatment over the years

# first we estimate the slope (+% decline for each of the study periods:
hermann <- subset(captures.by.treatment, year <= 2015)
new <- subset(captures.by.treatment, year > 2015)

#fit SLRs

mod_hermann <- lm(avg ~ year, data = hermann)
mod_new <- lm(avg ~ year, data = new)
mod_all<-lm(avg ~ year, data = captures.by.treatment)
summary(mod_hermann)
summary(mod_new)
summary(mod_all)

#% change
start_val <- predict(mod_all, newdata = data.frame(year = min(captures.by.treatment$year)))
end_val   <- predict(mod_all, newdata = data.frame(year = max(captures.by.treatment$year)))

percent_change <- 100 * (end_val - start_val) / start_val
percent_change


lampyrid.summary.timeseries<-ggplot(captures.by.treatment, aes(year, avg))+
  geom_point(aes(fill=as.factor(study)), colour="black", pch=21, size=2, 
             position = position_jitter(width = 0.1, height = 0))+
  geom_smooth(method = "gam",
              colour = "grey70",
              linetype = "twodash",
              linewidth = 1,
              se = FALSE,
              show.legend = FALSE) +
  geom_smooth(method = "lm",
              colour = "black",
              linewidth = 1.5,
              se = FALSE,
              show.legend = FALSE)+
  scale_fill_manual(values = pal)+
  theme_classic(base_size = 13)+
  guides(fill=guide_legend(title="Study"))+
  theme(legend.key=element_blank())+
  xlab("\nYear")+
  ylab("Adults per trap\n")
  
lampyrid.summary.timeseries

#save to pdf
pdf("Figure3.pdf", height=5, width=7)
lampyrid.summary.timeseries
dev.off()

#an interesting population cycling pattern emerges 
#regardless of how we plot it, we see an interesting pattern in the population variation- basically, a 6-7 year oscillation.


#multivariate analysis. So we want to see if the habitat use patterns of the lampyrids have changes
#to do this, we'll need to reshape the data into two different matrices where we have 
#abundance of fireflies by TREAT_DESC at yearly resolutions- a cross-tab,
#wide format data. 

#start by building the matrices
#we can use our previously melted data from 'lampyrid1' and cast it as needed
#because of unequal numbers of reps between forest and main sites, but same number of subsamples 
#per rep, we'll treat subsamples as rep for this analysis and pool by rep instead

#cast at the yearly resolution 
landscape.year<-dcast(lampyrid1, year+study+STATION~TREAT_DESC, sum)

#now create the environmental matrix, preserving order from the community matrices by
#creating them from the community matrix

env.landscape.year<-landscape.year[,1:3]


#finally strip out the env data
landscape.year<-landscape.year[,4:13]

#Ok! data is ready for some NMDSing! WOOO
library(vegan)

ord.year<-metaMDS(landscape.year, autotransform=TRUE)
ord.year

permanova <- adonis2(
  landscape.year ~ study,
  data = env.landscape.year,
  method = "bray"   
)

permanova #yup they're different

#so, MetaMDS assumes the x axis of our matrix is species and y is sites. We are
#screwing with this by instead looking at sites over samples for one species. So when I call "sites"
#here I'm actually calling sampling times. 

par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
plot(ord.year, disp='sites', type='n')
ordiellipse(ord.year, groups = env.landscape.year$study,
                                                  kind = "sd",   # or "se"
                                                  conf = 0.90,
                                                  draw = "lines",
                                                  col = pal,
                                                  lwd = 2)
with(env.landscape.year, points(ord.year, display = "sites", col = "black", pch = 21, bg = pal[as.factor(study)], cex=1.5))
ordilabel(ord.year, display="species", cex=0.75, col="black")
with(env.landscape.year, legend("right", legend = levels(as.factor(study)),
                                bty = "n", col = "black", pch = 21, pt.bg = pal, 
                                cex=1, pt.cex=1.5, inset=c(-0.2, 0), title="Study"))

#veganize the plot
library(ggrepel)
# Site scores
sites <- as.data.frame(scores(ord.year, display = "sites"))
sites$study <- env.landscape.year$study

# Species scores
species <- as.data.frame(scores(ord.year, display = "species"))
species$label <- rownames(species)

lampy_ord<- ggplot() +
  # Ellipses (like ordiellipse)
  stat_ellipse(data = sites,
               aes(x = NMDS1, y = NMDS2, color = study),
               level = 0.90, linewidth = 1) +
  
  # Points (filled circles with black outline)
  geom_point(data = sites,
             aes(x = NMDS1, y = NMDS2, fill = study),
             shape = 21, color = "black", size = 3) +
  
  # Species labels
  geom_label_repel(data = species,
                   aes(x = NMDS1, y = NMDS2, label = label),
                   size = 3,
                   fill = "white",
                   color = "black",
                   label.size = 0,
                   box.padding = 0.2) +
  
  # Colors
  scale_fill_manual(values = pal) +
  scale_color_manual(values = pal) +
  
  # Theme
  theme_classic(base_size = 14) +
  theme(
    legend.position = "right",
    legend.title = element_text(face = "bold")
  ) +
  
  labs(x = "NMDS1", y = "NMDS2", fill = "Study", color = "Study")

lampy_ord

#plot  
pdf("Figure4.pdf", height=6, width=7)
lampy_ord

dev.off()



######################
# Begin GAM phenology analysis


# let's rough in our gam models.
library(mgcv)
library(visreg)
library(ggpubr)
library(Hmisc)
library(cowplot)

#pearson correlation of environmental parameters

round(cor(lampyrid.weather[10:19], method="pearson"), digits=2)
#start withe the drivers of within-year variation


##################### Lampy gam


#by study
gam_lampy <- gam(
  ADULTS ~ 
    s(week, by = study) +         
    s(dd.accum, by = study) +                
    s(year, by = study, bs = "ts", sp=1.3) +   # manage cyclicity but constrain amplitude so simple year effects do not dominate
    s(min.temp, by = study,  bs = "ts") +      # allow shrinkage if redundant
    s(max.temp, by = study,  bs = "ts") +      # allow shrinkage if redundant
    s(weekly.precip, by = study, bs = "ts") +  # allow shrinkage if redundant
    TREAT_DESC * study +
    offset(log(TRAPS)),
  method = "REML",
  family = "quasipoisson",
  data = lampyrid.weather,
  knots = list(week = c(1, 52))
)
summary(gam_lampy)
anova(gam_lampy) #significance of parametric terms

#check on how we're doing here- adjust k up for smooths as needed
gam.check(gam_lampy)


withinyear.dd.lampy<-visreg(gam_lampy, "dd.accum", "study", partial=F, rug=FALSE, 
                            overlay=T, scale="response", gg=TRUE)+
  scale_colour_manual(values = pal) +
  labs(x="Degree day accumulation", y="")+
  theme_classic()+ theme(legend.position = "none")+
  coord_cartesian(xlim = c(250, 1200), ylim=c(0, 30))+
  annotate(
    "text",
    x = -Inf, y = Inf,       # top-left corner
    label = "\U1F525",       # fire emoji
    hjust = -0.1, vjust = 1.3,
    size = 8, family='emoji'
  )

withinyear.dd.lampy

withinyear.week.lampy<-visreg(gam_lampy, "week", "study", partial=F, rug=FALSE, 
                            overlay=T, scale="response", gg=TRUE)+
  scale_colour_manual(values = pal) +
  labs(x="Week of year", y="")+
  theme_classic()+ theme(legend.position = "none")+
  coord_cartesian(xlim = c(24, 33), ylim=c(0, 30))+
  annotate(
              "text",
              x = -Inf, y = Inf,       # top-left corner
              label = "\u2600",       # 
              hjust = -0.1, vjust = 1.3,
              size = 8, family='emoji'
            )

withinyear.week.lampy

withinyear.maxt.lampy<-visreg(gam_lampy, "max.temp", "study", partial=F, rug=FALSE, 
                              overlay=T, scale="response", gg=TRUE)+
  scale_colour_manual(values = pal) +
  labs(x="Maximum temperature", y="")+
  theme_classic()+ theme(legend.position = "none")+
  coord_cartesian(xlim = c(25, 35), ylim=c(0, 30))+
  annotate(
    "text",
    x = -Inf, y = Inf,       # top-left corner
    label = "\U1F321",       # 
    hjust = -0.1, vjust = 1.3,
    size = 8, family='emoji'
  )
  

withinyear.maxt.lampy

withinyear.mint.lampy<-visreg(gam_lampy, "min.temp", "study", partial=F, rug=FALSE, 
                              overlay=T, scale="response", gg=TRUE)+
  scale_colour_manual(values = pal) +
  labs(x="Minimum temperature", y="")+
  theme_classic()+ theme(legend.position = "none")+
  coord_cartesian(xlim = c(10, 17), ylim=c(0, 30))+
  annotate(
    "text",
    x = -Inf, y = Inf,       # top-left corner
    label = "\U1F321",       # 
    hjust = -0.1, vjust = 1.3,
    size = 8, family='emoji'
  )

withinyear.mint.lampy

withinyear.precip.lampy<-visreg(gam_lampy, "weekly.precip", "study", partial=F, rug=FALSE, 
                              overlay=T, scale="response", gg=TRUE)+
  scale_colour_manual(values = pal) +
  labs(x="Weekly precipitation", y="")+
  theme_classic()+ theme(legend.position = "none")+
  coord_cartesian(xlim = c(0, 30), ylim=c(0, 30))+
  annotate(
  "text",
  x = -Inf, y = Inf,       # top-left corner
  label = "\U1F327",       # 
  hjust = -0.1, vjust = 1.3,
  size = 8, family='emoji'
)

withinyear.precip.lampy

#because the Herman and New data have different ranges, we need to snip off the wonky bits where there was no data to fit

vr <- visreg(gam_lampy, "year", "study",
             partial = FALSE,
             rug = FALSE,
             overlay = TRUE,
             scale = "response",
             gg = FALSE,
             plot=FALSE)

plot_data <- vr$fit
library(dplyr)

ranges <- lampyrid.weather %>%
  group_by(study) %>%
  summarise(
    min_year = min(year, na.rm = TRUE),
    max_year = max(year, na.rm = TRUE),
    .groups = "drop"
  )
plot_data_trimmed <- plot_data %>%
  left_join(ranges, by = "study") %>%
  filter(year >= min_year, year <= max_year)

withinyear.year.lampy <- ggplot(plot_data_trimmed,
                                aes(x = year, y = visregFit, colour = study)) +
  geom_ribbon(aes(ymin = visregLwr, ymax = visregUpr, fill = study),
              alpha = 0.4, colour = NA) +
  geom_line(linewidth=1) +
  scale_colour_manual(values = pal) +
  scale_fill_manual(values = pal) +
  labs(x = "Year", y = "") +
  theme_classic() +
  theme(legend.position = "none") +
  coord_cartesian(xlim = c(2004, 2025), ylim = c(0, 30))+
  annotate(
              "text",
              x = -Inf, y = Inf,       # top-left corner
              label = "\U1F4C5",       # 
              hjust = -0.1, vjust = 1.3,
              size = 8, family='emoji'
            )



withinyear.year.lampy

withinyear.habitat.lampy<-visreg(gam_lampy, "TREAT_DESC", "study", partial=F, rug=FALSE, 
                                overlay=T, scale="response", gg=TRUE)+
  scale_colour_manual(values = pal) +
  labs(x="\nTreatment", y="")+
  theme_classic()+ theme(axis.text.x = element_text(angle = 90), legend.position = "none")+
  coord_cartesian(ylim=c(0, 30))+
  annotate(
              "text",
              x = -Inf, y = Inf,       # top-left corner
              label = "\U1F33F",       # 
              hjust = -0.1, vjust = 1.3,
              size = 8, family='emoji'
            )



withinyear.habitat.lampy

#create a legend to pull
withinyear.dd.lampy.leg <- withinyear.dd.lampy +
  theme(legend.position = "right")+
  guides(fill = guide_legend(title = "Study"),
         color = guide_legend(title = "Study"))
legend_lampy <- get_legend(withinyear.dd.lampy.leg)


#plot the withinyear model all together:

withinyear.modelplot.lampy<-plot_grid(withinyear.dd.lampy,withinyear.week.lampy,  
                                      withinyear.mint.lampy, withinyear.maxt.lampy, 
                                      withinyear.precip.lampy, withinyear.year.lampy, withinyear.habitat.lampy,legend_lampy,
                                      ncol=2, rel_heights = c(1, 1, 1, 2), labels=c('A', 'B', 'C', 'D', 'E', 'F', 'G'), align="v")
withinyear.modelplot.lampy

#create overall y axis label
partresid<-text_grob(paste("                 Partial residuals of adult abundance"), color="black", size=12, rot=90)


#now replot with grob label
final_plot<-plot_grid(partresid, withinyear.modelplot.lampy, ncol=2, rel_widths = c(1,15))


final_plot


pdf("Figure2.pdf", height=8, width=8)
final_plot
dev.off()
###


##################
#ok, now we want to put our model coefficients into a nice graphical representation

library(dplyr)
library(ggplot2)

library(dplyr)
library(ggplot2)

# ---- 1. Smooth terms ----
gam_sum <- summary(gam_lampy)

smooth_df <- as.data.frame(gam_sum$s.table)
smooth_df$term <- rownames(smooth_df)

smooth_df_tidy <- smooth_df %>%
  mutate(
    study = sub(".*:study", "", term),
    variable = sub("s\\((.*)\\):.*", "\\1", term),
    F_value = .[[3]]  # third column is F
  ) %>%
  dplyr::select(variable, study, F_value)

# ---- 2. Parametric Habitat terms ----
# manually assign F-values from your anova output
param_df_tidy <- data.frame(
  variable = "Habitat",
  study = c("Hermann", "New"),
  F_value = c(54.33, 13.39)   # TREAT_DESC for Hermann, TREAT_DESC:study for New
)

# ---- 3. Combine smooth + parametric ----
combined_df <- bind_rows(
  smooth_df_tidy %>%
    mutate(variable_label = case_when(
      variable == "week" ~ "Week",
      variable == "dd.accum" ~ "Degree Days",
      variable == "year" ~ "Year",
      variable == "min.temp" ~ "Min Temp",
      variable == "max.temp" ~ "Max Temp",
      variable == "weekly.precip" ~ "Precipitation",
      TRUE ~ variable
    )),
  param_df_tidy %>%
    mutate(variable_label = "Habitat")
)

# ---- 4. Hard-coded variable order ----
variable_levels <- c("Degree Days", "Week", "Max Temp", "Min Temp", 
                     "Precipitation", "Year", "Habitat")
combined_df$variable_label <- factor(combined_df$variable_label, levels = variable_levels)

# ---- 5. Plot ----
library(dplyr)
library(ggplot2)

# Define Unicode icons per variable
variable_icons <- c(
  "Degree Days" ="\U1F525",       # fire
  "Week" =  "\u2600",            # sun
  "Max Temp" = "\U1F321",        # thermometer
  "Min Temp" = "\U1F321",        # thermometer
  "Precipitation" = "\U1F327",   # cloud with rain
  "Year" = "\U1F4C5",            # calendar
  "Habitat" = "\U1F33F"          # leaf
)

# Compute max F per variable for icon placement
icon_positions <- combined_df %>%
  group_by(variable_label) %>%
  summarise(
    y_pos = max(F_value) +5,
    icon = variable_icons[unique(variable_label)],
    .groups = "drop"
  )

# Plot bars with Unicode icons above each pair
F_barplot_unicode <- ggplot(combined_df, aes(x = variable_label, y = F_value, fill = study)) +
  geom_bar(stat = "identity",
           position = position_dodge(width = 0.6),
           colour = "black",
           width = 0.8) +
  geom_text(data = icon_positions, 
            aes(x = variable_label, y = y_pos, label = icon), 
            inherit.aes = FALSE, 
            size = 8, family = "emoji") +  # adjust size to taste
  labs(x = "Predictor", y = "F value", fill = "Study") +
  scale_fill_manual(values = pal) +
  theme_classic(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
    axis.title = element_text(face = "bold"),
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 12)
  )

F_barplot_unicode

pdf("Figure1.pdf", height=6, width=8)
F_barplot_unicode
dev.off()

##############################################################
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
                                    year=2012, #high year in Hermann
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
                                     year=2012, #high year in Hermann
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
#120 747.4916 2.561839 2.561639 -0.0001998607

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
                                    year=2017, #high year in New
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
                                     year=2017, #high year in New
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
#300     1500 8.614479 8.628227 0.01374787

#local peak at 730.7692

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
  year=2012, #high year in Hermann
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

#  multiple peaks, high error- nonsensical fit

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
  year=2017, #high year in New
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

#week    pred0    pred1        slope
#174 27.83612 2.783217 2.783249 3.226584e-05


##########code to compute similar pieces for other weather variables

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
