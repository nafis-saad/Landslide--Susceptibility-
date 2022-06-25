#####Landslide Susceptibility Mapping using Logistic Regression
dir.create("C:/Rangamati/R/workingDiIR")
dir.create("C:/Rangamati/R/workingDiIR/MyLibrary")
setwd("C:/Rangamati/R/workingDiIR")
.libPaths("C:/Rangamati/R/workingDiIR/MyLibrary")
.libPaths()
##Insatall Packages
install.packages("raster")
library(raster)
install.packages("rgeos")
library(rgeos)
install.packages("rgdal")
library(rgdal)
install.packages("maptools")
library(maptools)
## Loading raster files
list.files("My Data")

Elevation= raster("My Data/Elevation.tif")
Landuse= raster("My Data/Landuse.tif")
LanduseChange= raster("My Data/Change.tif")
Geology= raster("My Data/Geology.tif")
TWI= raster("My Data/TWI.tif")
SPI= raster("My Data/SPI.tif")
Slope= raster("My Data/slope.tif")
Road= raster("My Data/Road.tif")
Fault= raster("My Data/Fault.tif")
Profile= raster("My Data/Profile.tif")
Plan= raster("My Data/Plan.tif")
NDVI= raster("My Data/NDVI.tif")
Drainage= raster("My Data/Drainage.tif")
Aspect= raster("My Data/Aspect.tif")
Training= raster("My Data/Training Data.tif")
plot(Training)
##Resampling
Training_re= resample(Training,Elevation, resmaple= 'bilinear')
Training_re
Landuse_re= resample(Landuse,Elevation, resmaple= 'bilinear')
Landusechange_re= resample(LanduseChange,Elevation, resmaple= 'bilinear')
Geology_re= resample(Geology,Elevation, resmaple= 'bilinear')
TWI_re= resample(TWI,Elevation, resmaple= 'bilinear')
SPI_re= resample(SPI,Elevation, resmaple= 'bilinear')
Slope_re= resample(Slope,Elevation, resmaple= 'bilinear')
Road_re= resample(Road,Elevation, resmaple= 'bilinear')
Fault_re= resample(Fault,Elevation, resmaple= 'bilinear')
Profile_re= resample(Profile,Elevation, resmaple= 'bilinear')
Plan_re= resample(Plan,Elevation, resmaple= 'bilinear')
NDVI_re= resample(NDVI,Elevation, resmaple= 'bilinear')
Drainage_re= resample(Drainage,Elevation, resmaple= 'bilinear')
Aspect_re= resample(Aspect,Elevation, resmaple= 'bilinear')
#Create Resample Raster
dir.create("C:/Rangamati/R/workingDiIR/Resampled data")
setwd("C:/Rangamati/R/workingDiIR/Resampled data")
writeRaster(Training_re, "Resampled data/Training_re.tif", overwrite= TRUE)
writeRaster(Landuse_re, "Resampled data/Landuse_re.tif", overwrite= TRUE)
writeRaster(Landusechange_re, "Resampled data/Landusechange_re.tif", overwrite= TRUE)
writeRaster(Plan_re, "Resampled data/Plan_re.tif", overwrite= TRUE)
writeRaster(Profile_re, "Resampled data/Profile_re.tif", overwrite= TRUE)
writeRaster(NDVI_re, "Resampled data/NDVI_re.tif", overwrite= TRUE)
writeRaster(Slope_re, "Resampled data/Slope_re.tif", overwrite= TRUE)
writeRaster(Road_re, "Resampled data/Road_re.tif", overwrite= TRUE)
writeRaster(SPI_re, "Resampled data/SPI_re.tif", overwrite= TRUE)
writeRaster(TWI_re, "Resampled data/TWI_re.tif", overwrite= TRUE)
writeRaster(Geology_re, "Resampled data/Geology_re.tif", overwrite= TRUE)
writeRaster(Fault_re, "Resampled data/Fault_re.tif", overwrite= TRUE)
writeRaster(Drainage_re, "Resampled data/Drainage_re.tif", overwrite= TRUE)
writeRaster(Aspect_re, "Resampled data/Aspect_re.tif", overwrite= TRUE)
writeRaster(Elevation, "Resampled data/Elevation.tif", overwrite= TRUE)
list.files("Resampled Data")
Elevation= raster("Resampled Data/Elevation.tif")
Landuse= raster("Resampled Data/Landuse_re.tif")
LanduseChange= raster("Resampled Data/Landusechange_re.tif")
Geology= raster("Resampled Data/Geology_re.tif")
TWI= raster("Resampled Data/TWI_re.tif")
SPI= raster("Resampled Data/SPI_re.tif")
Slope= raster("Resampled Data/Slope_re.tif")
Road= raster("Resampled Data/Road_re.tif")
Fault= raster("Resampled Data/Fault_re.tif")
Profile= raster("Resampled Data/Profile_re.tif")
Plan= raster("Resampled Data/Plan_re.tif")
NDVI= raster("Resampled Data/NDVI_re.tif")
Drainage= raster("Resampled Data/Drainage_re.tif")
Aspect= raster("Resampled Data/Aspect_re.tif")
Training= raster("Resampled Data/Training_re.tif")
hist(Training)
plot(Slope)
plot(Training, add= TRUE)
## Stack Multiple Raster Files
Stack_List= list.files("C:/Rangamati/R/workingDiIR/Resampled data", pattern= "tif$", full.names= TRUE)
Rasters= stack(Stack_List)
names(Rasters)
head(Rasters)
value_table= getValues(Rasters)
head(value_table, n=6)
value_table= na.omit(value_table)
value_table= as.data.frame(value_table)
head(value_table, n=6)


value_table=getValues(Rasters)
head(value_table, n=6)

value_table=na.omit(value_table)
value_table=as.data.frame(value_table)
head(value_table, n=6)
data = as.matrix(value_table)
##Export value
##
write.table(value_table, "mydata1.txt", sep="\t") # export to text file
str(value_table)
data= read.csv("C:/Rangamati/R/workingDiIR/mydata1.csv")
data=read.table("C:/Rangamati/R/workingDiIR/mydata.txt")

# data= value_data, and choose the training column=Training
training.fit<-glm (Training_re~., data=data, family=binomial)### use all independents
summary(training.fit) ### display results
training.fit= glm(Training_re ~ . , data = value_table, family= binomial)
summary(training.fit)
## Anova
anova(training.fit, test="Chisq")


install.packages("pscl")
library(pscl)
pR2(training.fit)



confint(training.fit)


exp(training.fit$coefficients)
exp(confint(training.fit))
##
Predicted=predict(training.fit, type="response")
write.table(Predicted, "predicted values.txt", sep="\t")



residuals(training.fit, type="deviance")

## 7.confusion matrix: a tabular representation of Actual vs Predicted values. 
#This helps us to find the accuracy of the model and avoid overfitting.
table(value_table$Training, Predicted > 0.5)


install.packages("ROCR")
library(ROCR)
ROCRpred <- prediction(Predicted, data$Training_re)
ROCRperf <- performance(ROCRpred, 'tpr','fpr')
plot(ROCRperf)



## Produce Susceptibility Index
summary(training.fit)
Y= (Elevation *  (0.049977) + Slope* (0.098925) + Road * (0.042963) + Aspect * 0.152721)-4.536210
P= 1/(1+exp(Y*(-1)))
summary(P)
Landslide_Susceptibility_Index=P
plot(P)
writeRaster(P, "Resampled data/P.tif", overwrite= TRUE)
Elevation= raster("Resampled Data/.tif")



  
