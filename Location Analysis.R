
install_github("eldarrak/FLightR")

library("TwGeos")
library(GeoLight)
library("FLightR")

################################# BD628 ###################################

d.lux<-readMTlux("./Desktop/BJ773.lux")
d.lux<- subset(d.lux,select=c("Date","Light"))
d.lux$Light<-log(d.lux$Light)

d.lux <- d.lux[16000:70000,]


lightImage(tagdata = d.lux, # light data
           offset = 18,     # adjusts the y-axis to put night (dark shades) in the middle
           zlim = c(0, 12), # y axis
           dt = 300)        # miniumn dark period


eseed <- as.POSIXct(locator(n=1)$x, origin  = "1970-01-01", tz = "GMT") # click at any time during the night

twl  <- findTwilights(tagdata = d.lux, 
                      threshold = 1.5, seed)

lightImage(tagdata = d.lux, 
           offset = 18, 
           zlim = c(0, 2))

tsimagePoints(twl$Twilight, 
              offset = 12, 
              pch = 16, 
              cex = 0.5,
              col = ifelse(twl$Rise, "dodgerblue", "firebrick"))

twl <- twilightEdit(twilights = twl, 
                    window = 4,           # two days before and two days after
                    outlier.mins = 20,    # difference in mins
                    stationary.mins = 25, # are the other surrounding twilights within 25 mins of one another
                    plot = TRUE)

twl <- subset(twl, Deleted == FALSE)

On_Bird <- as.POSIXct(c("2018-03-11", "2019-01-05"), tz = "GMT")

twl <- twl[twl$Twilight > On_Bird[1] & twl$Twilight < On_Bird[2],]

lightImage(d.lux,offset = 18, 
           zlim = c(0, 2),
           dt = 300)
tsimagePoints(twl$Twilight, offset = 19, pch = 16, cex = 0.5,
              col = ifelse(twl$Rise, "dodgerblue", "firebrick"))

abline(v  = On_Bird, col  ="orange", lwd = 3)

twl.lux <- subset(twl, Deleted == FALSE)


lux.tags<-BAStag2TAGS(raw = d.lux, 
                      twl = twl, 
                      threshold = 1.5)


write.csv(lux.tags,"lux.csv",quote=FALSE,row.names=FALSE)

Light.Data<-get.tags.data("lux.csv") 

CapLocs=c(-77.938763,18.042684) 

plot_slopes_by_location(Proc.data = Light.Data,location = CapLocs)

#Use abline to visualize potential calibration periods

# end of first calibration period
abline(v=as.POSIXct("2017-04-22"), col = "blue",lwd=2) 


Calibration.periods<-data.frame(   #This will create two lines of data
  calibration.start=as.POSIXct(c("2018-03-11")),   
  calibration.stop=as.POSIXct(c("2018-04-07")),
  lon=-77.938763, lat=18.042684) #use c() also for the geographic coordinates, if you have more than one calibration location (e. g.,  lon=c(5.43, 6.00), lat=c(52.93,52.94))

Calibration<-make.calibration(Proc.data = Light.Data, 
                              Calibration.periods = Calibration.periods,
                              model.ageing = FALSE,
                              plot.each = FALSE, 
                              plot.final = FALSE)

Grid<-make.grid(left=-85, bottom=15, right=-75, top=45,
                distance.from.land.allowed.to.use=c(-Inf, 100),  #Use infinity to withold any restrictions on migration paths
                distance.from.land.allowed.to.stay=c(-Inf, Inf))

all.in<-make.prerun.object(Light.Data, Grid, start=c(-77.938763,18.042684), Calibration=Calibration)


nParticles=1e4     #just a quick trial
a= Sys.time()       #This lets you measure the analysis time

Result<-run.particle.filter(all.in, threads=-1,
                            nParticles=nParticles, known.last=FALSE,
                            precision.sd=25, check.outliers=T)
b= Sys.time()
b-a                 #how long did it take?


#PLOTTING
#Plot and save a simple map
map.FLightR.ggmap(Result)

#Plot lon lat graph
plot_lon_lat(Result)


Plot a map with the most probable positions, i.e. combinations of the most probable latitude and longitude for each twilight:
  ```{r fig.width=9, fig.height=9}
library(grid)
try(map.FLightR.ggmap(Result, zoom=3))
```
Note, that we use `try` here to ease automated rendering. Just exclude it for your own runs. We also recommend a very nice feature of this function: `zoom='auto'` that will try finding optimal zoom for your output.


Plot space utilisation distribution for the wintering range:
  ```{r fig.width=9, fig.height=9, results=FALSE}
try(tmp<-plot_util_distr(Result, 
                         dates=data.frame(as.POSIXct('2018-03-11'), as.POSIXct('2018-04-07')),
                         add.scale.bar=TRUE, percentiles=0.5, zoom=7))

```

Note, that we use `try` here to ease automated rendering. Just exclude it for your own runs. We also recommend a very nice feature of this function: `zoom='auto'` that will try finding optimal zoom for your output.


################################# BD625 ###################################
BD626_07Apr18_165303driftadj.lux



d.lux<-readMTlux("./BD626_07Apr18_165303driftadj.lux")
d.lux<- subset(d.lux,select=c("Date","Light"))
d.lux$Light<-log(d.lux$Light)


lightImage(tagdata = d.lux, # light data
           offset = 18,     # adjusts the y-axis to put night (dark shades) in the middle
           zlim = c(0, 2), # y axis
           dt = 300)        # miniumn dark period

plot(d.lux$Date[3000:5000], d.lux$Light[3000:5000], type = "o", pch = 16, cex = 0.5)

seed <- as.POSIXct(locator(n=1)$x, origin  = "1970-01-01", tz = "GMT") # click at any time during the night

twl  <- findTwilights(tagdata = d.lux, 
                      threshold = 1, 
                      include = seed)

lightImage(tagdata = d.lux, 
           offset = 18, 
           zlim = c(0, 2),
           dt = 300)

tsimagePoints(twl$Twilight, 
              offset = 12, 
              pch = 16, 
              cex = 0.5,
              col = ifelse(twl$Rise, "dodgerblue", "firebrick"))

twl <- twilightEdit(twilights = twl, 
                    window = 4,           # two days before and two days after
                    outlier.mins = 20,    # difference in mins
                    stationary.mins = 25, # are the other surrounding twilights within 25 mins of one another
                    plot = TRUE)

twl <- subset(twl, Deleted == FALSE)

On_Bird <- as.POSIXct(c("2017-04-03", "2018-01-25"), tz = "GMT")

twl <- twl[twl$Twilight > On_Bird[1] & twl$Twilight < On_Bird[2],]

lightImage(d.lux,offset = 18, 
           zlim = c(0, 2),
           dt = 300)
tsimagePoints(twl$Twilight, offset = 19, pch = 16, cex = 0.5,
              col = ifelse(twl$Rise, "dodgerblue", "firebrick"))

abline(v  = On_Bird, col  ="orange", lwd = 3)

twl.lux <- subset(twl, Deleted == FALSE)


lux.tags<-BAStag2TAGS(raw = d.lux, 
                      twl = twl, 
                      threshold = 1)


write.csv(lux.tags,"lux_628.csv",quote=FALSE,row.names=FALSE)

Light.Data<-get.tags.data("lux_628.csv") 

CapLocs=c(-77.938763,18.042684) 

plot_slopes_by_location(Proc.data = Light.Data,location = CapLocs)

#Use abline to visualize potential calibration periods

# end of first calibration period
abline(v=as.POSIXct("2017-04-13"), col = "blue",lwd=2) 


Calibration.periods<-data.frame(   #This will create two lines of data
  calibration.start=as.POSIXct(c("2017-04-05")),   
  calibration.stop=as.POSIXct(c("2017-04-12")),
  lon=-77.938763, lat=18.042684) #use c() also for the geographic coordinates, if you have more than one calibration location (e. g.,  lon=c(5.43, 6.00), lat=c(52.93,52.94))

Calibration<-make.calibration(Proc.data = Light.Data, 
                              Calibration.periods = Calibration.periods,
                              model.ageing = FALSE,
                              plot.each = FALSE, 
                              plot.final = FALSE)

Grid<-make.grid(left=-85, bottom=15, right=-75, top=50,
                distance.from.land.allowed.to.use=c(-Inf, 50),  #Use infinity to withold any restrictions on migration paths
                distance.from.land.allowed.to.stay=c(-Inf, 50))

all.in<-make.prerun.object(Proc.data, Grid, start=c(-77.938763,18.042684), Calibration=Calibration)


nParticles=1e4     #just a quick trial
a= Sys.time()       #This lets you measure the analysis time

Result<-run.particle.filter(all.in, threads=-1,
                            nParticles=nParticles, known.last=FALSE,
                            precision.sd=25, check.outliers=T)
b= Sys.time()
b-a                 #how long did it take?


#PLOTTING
#Plot and save a simple map
map.FLightR.ggmap(Result)

#Plot lon lat graph
plot_lon_lat(Result)


