
###Citation:
###Rochette S�bastien. (2018, Jan. 27). "Spatial correlation between rasters". Retrieved from https://statnmap.com/2018-01-27-spatial-correlation-between-rasters/.

library(raster)
library(dplyr)
install.packages("mapview")
install.packages("mapedit")
library(mapview)
library(mapedit)
library(sf)
library(readr)
library(ggplot2)

XGBOOST= raster("C:/Rangamati/Corrected Rangamati/Xgboost/Excel files/Prediction_XGBoostTunned_Landslides SM.tif")
RF= raster("C:/Rangamati/Corrected Rangamati/Xgboost/Excel files/Prediction_RF Tunned_Landslides SM.tif")
Correlation1=stack(XGBOOST, RF)
names(Correlation1)
plot(Correlation1)
##Overall Correlation
cor(values(Correlation1)[,1],
    values(Correlation1)[,2],
    use = "na.or.complete")

##Linear Regression
lm1 <- lm(values(Correlation1)[,2] ~ values(Correlation1)[,1])
summary(lm1)


resid_lm <- raster(Correlation1, 1) * NA
values(resid_lm)[-lm1$na.action] <- lm1$residuals


temp_chl_s_nb <- raster(Correlation1, 1)
values(temp_chl_s_nb) <- 1:ncell(Correlation1)

matrix_chl_s <- values(Correlation1) # stack as raster [MW]

focal_cor <- focal(
  x = temp_chl_s_nb,
  w = matrix(1, 5, 5),
  # fun = function(x, y = temp_chl_s){ # Original
  # cor(values(y)[x, 1], values(y)[x, 2], # Original
  # use = "na.or.complete")
  # },
  fun = function(x, y = matrix_chl_s){ # [MW]
    cor(y[x, 1], y[x, 2], # [MW]
        use = "na.or.complete")
  },
  filename = file.path("C:/Rangamati/Corrected Rangamati/Xgboost/Excel files", "focal_cor.tif"),
  overwrite = TRUE
)
warnings()
