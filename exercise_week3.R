# Computing and Data Analysis in Geosciences
# November 22, 2020
# Elena Geiger
# Week 3: Error and Uncertainty Analysis

# Exercise 1 --------------------------------------------------------------

# The distance to a building is estimated from a map to be about
# 2550 +/- 25 m. With a theodolite, a student determines the angle between the
# horizontal plane and the building to be 1?21' +/- 1'. What is the height of the
# building and the fractional uncertainty in this estimate? 

d <- 1
m <- 21

DMS2rad <- function(x){
  rad <- pi*(d+m/60)/180
  result <- print(rad, "rad")
  return (result)
}

# this approach does not work, so I go ahead with the values I calculated externally

# DMS to radians
rad <- 0.0236
errorrad <- 0.003

# fractional uncertainty

radfrac <- (errorrad/rad)*100
disfrac <- (25/2550)*100

# height of building

height <- round (tan(rad)*2550, digits = 2)
heighterror <- round (tan(errorrad)*25, digits = 2)

print(paste0("The hight of the building is ", height, " with an error of ", heighterror))
# I am not sure this is the correct way to propagate an error

# maybe more like this?
heighterror2 <- round (sqrt(radfrac^2 + disfrac^2), digits = 2)
print(paste0("The height of the building is ", height, " with an error of ", heighterror2))

# Exercise 2 --------------------------------------------------------------

# From compositional data you infer that two volcanic rock samples represent the
# initial and final pulse of volcanic activity. Your two samples have been
# radiometrically dated at 25.53 0.1 Ma and 29.66  0.2 Ma respectively. 
# What is the likely duration of volcanic activity (including the error?) 


duration <- 29.66 - 25.53
error <- 0.1 + 0.2

print(paste0("The likely duration of the vulcanic activity is ", duration, " Ma,
with an error of ", error, " Ma"))

# Exercise 3 --------------------------------------------------------------

# Load the datatable ex3_eqscals.txt. This is a table of real measurements of
# earthquake size, at multiple stations, from a single earthquake in California.
# There are 3 columns, 
# X (km) = the distance of the station from the earthquake
# r (m) = the estimated fault radius 
# Mo (Nm) = the seismic moment, which is related to earthquake magnitude. 
# Parameters r and Mo are estimates made from the seismogram recorded at each
# station, and they vary.      

# import data      
setwd("C:/Users/elena/Documents/Master/RCourse/errors_elenageiger22")
eqscals <- read.table("ex3_eqscals.txt", header = FALSE, sep = "")

# rename columns

names(eqscals)[names(eqscals) == "V1"] <- "X(km)"
names(eqscals)[names(eqscals) == "V2"] <- "r(m)"
names(eqscals)[names(eqscals) == "V3"] <- "Mo(Nm)"

# a) What is the mean, median, standard deviation, and median absolute deviation
# (MAD) for r and Mo? 

radius <- eqscals$`r(m)`
functionr <- c("mean", "median", "standard deviation", "MAD")
resr <- c(mean(radius), median(radius), sd(radius), mad(radius))
functionr <- data.frame(functionr, resr)

SMoment <- eqscals$`Mo(Nm)`
functionSM <- c("mean", "median", "standard deviation", "MAD")
resSM <- c(mean(SMoment), median(SMoment), sd(SMoment), mad(SMoment))
functionSM <- data.frame(functionSM, resSM)

# b) Make one descriptive plot each, of Mo and r (eg. histograms, boxplots,
# scatter, logged etc). Are there any obvious outliers that can cause problems?

# Mo

x11()
boxplot(eqscals$`Mo(Nm)`, 
        main = "Distrubution of the Seismic Moment (Mo)",
        horizontal = TRUE,
        xlab = "Seismic Moment (Nm)", 
        col = "coral")

# no obvious outliners detected

# r

x11()
boxplot(eqscals$`r(m)`,
        main = "Distribution of the Estimated Fault Radius",
        horizontal = TRUE,
        xlab = "Radius (m)",
        col = "blue")

# no obvious outliners detected

# c) Are there any outliers apparent, either from looking at the numbers or
# plotting? A good criterion is eliminating points exceeding 3*MAD from the
# median. Eliminate them to make a TRIMMED set, and recalculate the mean, median
# and standard deviation. What is your "best" estimate, and uncertainty, in Mo?

# all points that are more than 3 times the MAD away from the median have to be
# eliminated

numeric.vars <- sapply(eqscals, is.numeric)
i1 <-  Reduce(`&`, lapply(eqscals[numeric.vars], function(v) 
  (v > median(v) - 3* mad(v)) & (v < median(v) + 3 * mad(v))) )
trimmed <- eqscals[i1,]
nrow(trimmed) # 19
        
# There are no values that are 3*MAD below or above the median?
# since I could not figure out that any of the values are actual outliers, I 
# assume the mean is the "best" estimate for Mo

bestest <- mean(trimmed$`Mo(Nm)`)
print(paste0("The best estimate for Mo is ", bestest, " Nm" ))

# uncertainty 

unc <- (sd(trimmed$`Mo(Nm)`)/sqrt(19)) # standard error
print(paste0("The uncertainty for Mo is ", unc, " Nm, which is roughly 3.5 %" ))

# d) Calculate the "moment" of an earthquake 

Mw <- (log10(bestest)/1.5)- 6
Mwc <- (log10(unc)/1.5)- 6

print(paste0("The estimated moment of the earthquake is ", Mw, " with an uncertainty of ", Mwc))
