#PART 1

#installpackages
install.packages(c("sp", "fractaldim", "gstat")) # Run once to install

library(fractaldim)
library(sp)
library(gstat)

# Set the working directory
setwd("R:/2024/Fall/GEOG565/Instructors/Lab 5")

# Read in the dataset
data5 <- read.csv("R:/2024/Fall/GEOG565/Instructors/Lab 5/data/Lab5data.csv")

# Extract columns from the dataset as vectors
location = data5$location
intensity = data5$intensity
species.count = data5$species.count
soil = data5$soil

# Plot the original data
plot(x=location, y=intensity, xlab = "Location (distance m)", 
     ylab = "watts/m2", main = "Light Intensity", type="l")

plot(x=location, y=soil, xlab = "Location (distance m)", 
     ylab = "micrograms/g", main = "Soil N", type="l")

plot(x=location, y=species.count, xlab = "Location (distance m)", 
     ylab = "Number of Species", main = "Soil fauna", type="l")

# Spectral frequency for light intensity
x.spec1 <- spectrum(intensity, log="no", span=9, plot=FALSE)
spx1 <- x.spec1$freq
spy1 <- 2*x.spec1$spec
light_freq <- plot(spy1~spx1, xlab="frequency", ylab="spectral density", 
                   type="l", main = "Light Intensity", xlim=c(0, 0.3),
                   xaxp = c(0, 0.3, 15)
)

# Repeat for soil
x.spec2 <- spectrum(soil, log="no", span=9, plot=FALSE)
spx2 <- x.spec2$freq
spy2 <- 2*x.spec2$spec
soil_freq <- plot(spy2~spx2, xlab="frequency", ylab="spectral density", 
                  type="l", main = "Soil", xlim=c(0, 0.3),
                  xaxp = c(0, 0.3, 15)
)

# Repeat for species count
x.spec3 <- spectrum(species.count, log="no", span=9, plot=FALSE)
spx3 <- x.spec3$freq
spy3 <- 2*x.spec3$spec
species_freq <- plot(spy3~spx3, xlab="frequency", ylab="spectral density", 
                     type="l", main = "Species count", xlim=c(0, 0.3),
                     xaxp = c(0, 0.3, 15)
)

# Plot all of the spectral densities together
plot(spy1~spx1, xlab="frequency", ylab="spectral density", type="l", 
     ylim=range(c(spy1, spy2, spy3)), col = "green") 
par(new = TRUE)
plot(spy2~spx2, type="l", xlab="", ylab="", ylim=range(c(spy1,spy2, spy3)), col = "blue")
par(new = TRUE)
plot(spy3~spx3, type="l", xlab="", ylab="", ylim=range(c(spy1,spy2, spy3)), col = "cyan")
leg.txt <- c("Intensity", "Soil", "Species count") 
legend(list(x = 0.4,y = 50000), 
       legend = leg.txt,
       lty = c(1,1,1),
       col = c("green","blue", "cyan")
)


#PART 2

# Create the Cross-Correlogram
#ccf(intensity,species.count,lag.max=150,plot=FALSE)
plot(ccf(intensity, species.count, lag.max=150, plot=FALSE),type="l",col="red", ylab="Cross-Correlation",
     main="Cross-Correlogram", xlim=c(0,150))

# Create a Cross Variogram
datavar = data5
datavar$loc2 = 0
coordinates(datavar) <- ~location+loc2
g <- gstat(formula = datavar$intensity~datavar$species.count, data = datavar)
plot(variogram(g), main="Cross-Variogram")



#PART 3
# Run some Fractal dimension analysis
fd_intensity <- fd.estim.variogram(intensity, nlags = 60, plot.loglog = TRUE, 
                                   plot.allpoints = TRUE, legend.type = 'f', 
                                   main = 'Light intensity log-log plot')

fd_soil <- fd.estim.variogram(soil, nlags = 60, plot.loglog = TRUE, 
                              plot.allpoints = TRUE, legend.type = 'f', 
                              main = 'Soil N log-log plot')

fd_insects <- fd.estim.variogram(species.count, nlags = 60, plot.loglog = TRUE, 
                                 plot.allpoints = TRUE, legend.typ = 'f',
                                 main = 'Number of Species log-log plot')

# Get the slope from each model
fd_intensity$loglog[1]

fd_soil$loglog[1]

fd_insects$loglog[1]
