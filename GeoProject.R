library(BAStag)
library(TwGeos)
library(GeoLight)
library(grid)

d.lux<-readMTlux("./BD624_14Apr18_165147.lux")
d.lux<- subset(d.lux,select=c("Date","Light"))

d.lux$Light<-log(d.lux$Light)

d.lux <- subset(d.lux, Date > "2017-04-10" & Date < "2017-09-01")

# plot(d.lux$Date[1:2000], d.lux$Light[1:2000], type = "o", pch = 16, cex = 0.5)
# seed <- as.POSIXct(locator(n=1)$x, origin  = "1970-01-01", tz = "GMT")
# twl  <- findTwilights(d.lux, threshold=1, include = seed)
# 
# lightImage(d.lux, offset = 17, zlim = c(0, 12), dt = 120)
# tsimagePoints(twl$Twilight, offset = 12, pch = 16, cex = 0.5,
#               col = ifelse(twl$Rise, "dodgerblue", "firebrick"))
# 
# twl <- twilightEdit(twl, window = 4, outlier.mins = 45, stationary.mins = 25, plot = T)
# 
# twl <- twilightAdjust(twl, 5*60)

twl <- twilightCalc(datetime = d.lux$Date, light = d.lux$Light,
                    LightThreshold = log(1.5), ask = TRUE, preSelection = TRUE, 
                    nsee = 2000, allTwilights = FALSE, maxLight = T)
twl1 <- twl
#twl <- twl1
write.csv(twl, file = "~/Desktop/A2_twl_GeoL.csv", quote = FALSE, row.names = FALSE)


d.lux<-readMTlux("./BD624_14Apr18_165147driftadj.lux")
d.lux<- subset(d.lux,select=c("Date","Light"))

d.lux$Light<-log(d.lux$Light)

d.lux <- subset(d.lux, Date > "2017-04-10" & Date < "2017-09-01")

d.lux <- data.frame(datetime = format(d.lux$Date, format = "%Y-%m-%dT%H:%M:%S.000Z"),
                    light = d.lux$Light, twilight = 0, interp = FALSE, excluded = FALSE)

twl <- data.frame(datetime = format(twl$tFirst, format = "%Y-%m-%dT%H:%M:%S.000Z"), 
                  light = 2.5, twilight = twl$type, interp = TRUE, excluded = FALSE)

twl <- rbind(d.lux,twl)                        
twl <- twl[order(as.character(twl$datetime)),]

datetime <- strptime(twl$datetime, "%Y-%m-%dT%H:%M:%OSZ", "GMT")  #vector of datestamps in a format R can work with
twl <- twl[datetime < as.POSIXct("2017-08-01", "GMT") & datetime > as.POSIXct("2017-04-10", "GMT"),]    #Remove data logged after Apr 20, 2012

write.csv(twl, file = "~/Desktop/A2_twl_FlightR.csv", quote = FALSE, row.names = FALSE)

library(FLightR)
Proc.data<-get.tags.data("~/Desktop/A2_twl_FlightR.csv") #opens and formats data straight from TAGS formatted csv file

start=c(-77.938763,18.042684)  #tracking orgin, start location longitude and latitude

plot_slopes_by_location(Proc.data=Proc.data, location=start, ylim=c(-35,5))

#Use abline to visualize potential calibration periods
abline(v=as.POSIXct("2017-06-10")) # end of first calibration period

# Next create a data.frame with a separate line is designated for each calibration period. 
# The columns are: 
#     (1) start of the calibration period, 
#     (2) end of the calibration period, 
#     (3) longitude of the calibration location and, 
#     (4) latitude of the calibration location.

Calibration.periods<-data.frame(   #This will create two lines of data
  calibration.start=as.POSIXct(c("2017-04-15")),   
  calibration.stop=as.POSIXct(c("2017-04-20")),
  lon=-77.938763, lat=18.042684) #use c() also for the geographic coordinates, if you have more than one calibration location (e. g.,  lon=c(5.43, 6.00), lat=c(52.93,52.94))

#View results
Calibration.periods

#create a calibration object 
Calibration<-make.calibration(Proc.data, Calibration.periods)

Grid<-make.grid(left=-85, bottom=15, right=-75, top=50,
                distance.from.land.allowed.to.use=c(-Inf, 50),  #Use infinity to withold any restrictions on migration paths
                  distance.from.land.allowed.to.stay=c(-Inf, 0))

all.in<-make.prerun.object(Proc.data, Grid, start=c(-77.938763,18.042684), Calibration=Calibration)


nParticles=1e3     #just a quick trial
a= Sys.time()       #This lets you measure the analysis time

Result<-run.particle.filter(all.in, threads=-1,
                            nParticles=nParticles, known.last=FALSE,
                            precision.sd=25, check.outliers=T)
b= Sys.time()
b-a                 #how long did it take?



#PLOTTING
#Plot and save a simple map
map.FLightR.ggmap(Result, save.options = list(filename = "~/Desktop/FLightR.map3.pdf"))

#Plot lon lat graph
plot_lon_lat(Result)

#ESTIMATING ARRIVAL TIMES

#Choose a zone or portion of the grid that you want to focus on.
#For example, all longitudes greater than 2.
Index<-which(Result$Spatial$Grid[,2] > 18)

#Now you can calculte the probabilities that the bird was in 
#the indexed zone
Prob_in_Zone<-get.prob.of.being.inR(Result, Index)
plot(Prob_in_Zone)


# Plot utilization distribution

breeding = data.frame(start=as.POSIXct('2017-05-30'), stop=as.POSIXct('2017-06-30'))
plot_util_distr(Result, dates=breeding, percentiles=0.75)


stationary.migration.summary(Result, prob.cutoff = 0.1, min.stay=3)
