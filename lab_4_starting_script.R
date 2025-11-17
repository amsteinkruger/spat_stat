# install.packages(c("gstat","ncf","plotly","sp","tidyverse"))

library(gstat) 
library(ncf) 
library(plotly) 
library(sp) 
library(tidyverse) 

# setwd("R:/2025/Fall/GEOG565/Students/YOURID/Lab4")

# import the dataset
data = read.csv("data/geog565_lab4_data.csv")

# create 2d contour plot in ggplot
v.2D <- plot_ly(data, x = ~long, y = ~lat, z = ~alt,
                type = "contour", 
                contours = list(showlabels = TRUE)
                )
v.2D


# Create 3d plots 
z.3D <- as.matrix(data[,2:4] %>%
                    pivot_wider(names_from = long, values_from = alt) %>% 
                    select(!(lat)) )

v.3D <- plot_ly(z=z.3D) %>% 
  add_surface(contours = list(z = list(show=TRUE, 
                                       usecolormap=TRUE, 
                                       highlightcolor="#ff0000",
                                       project=list(z=TRUE))))

v.3D

################ STOP! Before proceeding make sure to copy and paste
################ your 2D and 3D plots to your answer sheet

# Compute the variogram
coordinates(data) <- ~long+lat 
g <- variogram(alt~1, data=data) 
plot(g)

# Compute the directional variograms at two orientations
g.dir <- variogram(alt~1, data=data, alpha = c(0, 90)) 
plot(g.dir)

# Fit a spherical semivariogram
# NOTE: Options for 'model' parameter: "Exp", "Sph", "Gau", "Lin", "Cir"
# The other parameters are guess values to initiate the variogram fitting procedure
g.fit <- fit.variogram(g, model = (vgm(psill = 14000, model="Sph", range = 2000, nugget=100 )))
g.fit

# Fit a correllogram
# Grab a brew, this will take a while...
fit1 <- correlog(data$long, data$lat, data$alt, increment = 2, resamp = 10) 
plot(fit1, type='l')
abline(h=0, col='red')

