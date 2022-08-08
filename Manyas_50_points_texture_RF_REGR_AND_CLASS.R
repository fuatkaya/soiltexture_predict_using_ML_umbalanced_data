library(randomForest)
library(ithir)
library(MASS)
library(caret)
library(ggplot2)
library(rasterVis)
library(lattice)
library(latticeExtra)
library(nnet)




###########################Modelleme

RF_Manyas_Texture_REGR <-read.csv(file.choose(),header =T, sep = ",") 
set.seed(2019)
RF_Manyas_Texture_REGR_model <-  as.data.frame(RF_Manyas_Texture_REGR)
View(RF_Manyas_Texture_REGR)
str(RF_Manyas_Texture_REGR)
summary(RF_Manyas_Texture_REGR)
par(mfrow = c(3,1))
plot(density (RF_Manyas_Texture_REGR_model$Clay), col='#2c7fb8', main="")
legend('topleft', legend=c("Clay"),
       col=c("#2c7fb8"), lty=2, cex=1)
plot(density (RF_Manyas_Texture_REGR_model$Sand), col='#d95f0e', main="")
legend('topleft', legend=c("Sand"),
       col=c("#d95f0e"), lty=3, cex=1)
plot(density (RF_Manyas_Texture_REGR_model$Silt), col='#31a354', main="")
legend('topleft', legend=c("Silt"),
       col=c("#31a354"), lty=4, cex=1)

####TrainRF_Manyas_P_Landsat_model <- sample(nrow(RF_Manyas_P_Landsat_model), 0.7 * nrow(RF_Manyas_P_Landsat_model))
####View(TrainRF_Manyas_P_Landsat_model)
#######str(TrainRF_Manyas_P_Landsat_model)
########[TrainRF_Manyas_P_Landsat_model, ]
###tuningclassification
ctrlrfclasskfold5landsat <- trainControl(method= "repeatedcv", number = 5, repeats = 3, search = "random")
RF_Manyas_Texture_REGR_model_tune <- train(Sinif~ coloration_mean_Manyas + 
                                         GNDVI_mean_Manyas + GRVI_mean_Manyas + Manyas_HGK_SYM_30m_Resample_2 + ndvi_mean_Manyas + Planform_curvature_Manyas_2 + 
                                         Profile_curvature_Manyas_2 + saturation_mean_Manyas + 
                                         Slope_Manyas2 + SPI_Manyas_2 + TWI_Manyas_2, method ="rf",
                                       data = RF_Manyas_Texture_REGR_model, trcontrol=ctrlrfclasskfold5landsat, importance = TRUE)
RF_Manyas_Texture_REGR_model_tune$finalModel  #control and modeltype



#Modeli Randomforest pakeinde kuracagiz

#####randomforestpackages
##RF_Manyas_P_Landsat_modelrandomforestpackages <- randomForest(Sinif~ coloration_mean_Manyas + 
##???GNDVI_mean_Manyas + GRVI_mean_Manyas + Manyas_HGK_SYM_30m_Resample_2 + ndvi_mean_Manyas + Planform_curvature_Manyas_2 + 
  ##Profile_curvature_Manyas_2 + saturation_mean_Manyas + 
  ###Slope_Manyas2 + SPI_Manyas_2 + TWI_Manyas_2, data = RF_Manyas_Texture_REGR_model,
                                                             # mtry=6, ntree= 500, importance = TRUE)
####RF_Manyas_P_Landsat_modelrandomforestpackages
varImpPlot(RF_Manyas_Texture_REGR_model_tune$finalModel)
####Lets continue

library(lattice)
library(ggplot2)

####importance
RF_Manyas_Texture_REGR_IMP <- varImp(RF_Manyas_Texture_REGR_model_tune$finalModel)
RF_Manyas_Texture_REGR_IMP

RF_Manyas_Texture_REGR_IMPORTANCE <- importance(RF_Manyas_Texture_REGR_model_tune$finalModel)

RF_Manyas_Texture_REGR_IMPORTANCE_dataframe <- as.data.frame(RF_Manyas_Texture_REGR_IMPORTANCE)
View(RF_Manyas_Texture_REGR_IMPORTANCE_dataframe)
write.table(RF_Manyas_Texture_REGR_IMPORTANCE_dataframe, "RF_Manyas_Texture_Class_IMPORTANCE.TXT", col.names = T, row.names =T, sep = ",")

########probbability value
probs_Manyas_Rf_fosfor <- fitted(RF_Manyas_P_Sentinel_modelrandomforestpackages_SON_model)
View(probs_Manyas_Rf_fosfor)
probs_Manyas_Rf_fosfor
##############################################




RF_Manyas_Texture_class_probs_all <-predict(RF_Manyas_Texture_REGR_model_tune$finalModel, type = "prob", newdata = RF_Manyas_Texture_REGR_model)



RF_Manyas_Texture_class_probs_all_export <- as.data.frame(RF_Manyas_Texture_class_probs_all)


write.table(RF_Manyas_Texture_class_probs_all_export, "RF_Manyas_Texture_class_probs_all_export.TXT", col.names = T, row.names = T, sep = ",")

####confusionmatrix
##Train-Calibration
predictRF_Manyas_Texture_class <- predict(RF_Manyas_Texture_REGR_model_tune$finalModel, type = "response", newdata = RF_Manyas_Texture_REGR_model, )
View(predictRF_Manyas_Texture_class)
table(predictRF_Manyas_Texture_class)

RF_Manyas_Texture_REGR_model_tune$finalModel

###results
goofcat(observed = RF_Manyas_Texture_REGR_model$Sinif,
        predicted = predictRF_Manyas_Texture_class)
confusionMatrix(RF_Manyas_Texture_REGR_model$Sinif, predictRF_Manyas_Texture_class)

library(sp)
library(raster)
library(rgdal)
library(nnet)
library(caret)
library(lattice)
library(ggplot2)
library(ithir)
library(MASS)
library(rasterVis)
library(lattice)
library(latticeExtra)
coloration_mean_Manyas + 
  GNDVI_mean_Manyas + GRVI_mean_Manyas + Manyas_HGK_SYM_30m_Resample_2 + ndvi_mean_Manyas + Planform_curvature_Manyas_2 + 
  Profile_curvature_Manyas_2 + saturation_mean_Manyas + 
  Slope_Manyas2 + SPI_Manyas_2 + TWI_Manyas_2
#covariates_load
plot(Profile_curvature_Manyas_2)
coloration_mean_Manyas <- raster("D:/Mediterranean and Middle-East Geoscience and Remote Sensing Symposium 2022/Manyas_28_09_2021_DEM_Satellite/coloration_mean_Manyas.tif")
GNDVI_mean_Manyas <- raster("D:/Mediterranean and Middle-East Geoscience and Remote Sensing Symposium 2022/Manyas_28_09_2021_DEM_Satellite/GNDVI_mean_Manyas.tif")
GRVI_mean_Manyas <- raster("D:/Mediterranean and Middle-East Geoscience and Remote Sensing Symposium 2022/Manyas_28_09_2021_DEM_Satellite/GRVI_mean_Manyas.tif")
Manyas_HGK_SYM_30m_Resample_2 <- raster("D:/Mediterranean and Middle-East Geoscience and Remote Sensing Symposium 2022/Manyas_28_09_2021_DEM_Satellite/Manyas_HGK_SYM_30m_Resample_2.tif")
ndvi_mean_Manyas<- raster("D:/Mediterranean and Middle-East Geoscience and Remote Sensing Symposium 2022/Manyas_28_09_2021_DEM_Satellite/ndvi_mean_Manyas.tif")
Planform_curvature_Manyas_2 <- raster("D:/Mediterranean and Middle-East Geoscience and Remote Sensing Symposium 2022/Manyas_28_09_2021_DEM_Satellite/Planform_curvature_Manyas_2.tif")
Profile_curvature_Manyas_2 <- raster("D:/Mediterranean and Middle-East Geoscience and Remote Sensing Symposium 2022/Manyas_28_09_2021_DEM_Satellite/Profile_curvature_Manyas_2.tif")
saturation_mean_Manyas <- raster("D:/Mediterranean and Middle-East Geoscience and Remote Sensing Symposium 2022/Manyas_28_09_2021_DEM_Satellite/saturation_mean_Manyas.tif")
Slope_Manyas2 <- raster("D:/Mediterranean and Middle-East Geoscience and Remote Sensing Symposium 2022/Manyas_28_09_2021_DEM_Satellite/Slope_Manyas2.tif")
SPI_Manyas_2 <- raster("D:/Mediterranean and Middle-East Geoscience and Remote Sensing Symposium 2022/Manyas_28_09_2021_DEM_Satellite/SPI_Manyas_2.tif")
TWI_Manyas_2 <- raster("D:/Mediterranean and Middle-East Geoscience and Remote Sensing Symposium 2022/Manyas_28_09_2021_DEM_Satellite/TWI_Manyas_2.tif")


covs_manyas_landsat_texture <- stack(coloration_mean_Manyas, 
                             GNDVI_mean_Manyas, GRVI_mean_Manyas, Manyas_HGK_SYM_30m_Resample_2,
                             ndvi_mean_Manyas, Planform_curvature_Manyas_2, Profile_curvature_Manyas_2, saturation_mean_Manyas, Slope_Manyas2, SPI_Manyas_2, TWI_Manyas_2)
compareRaster(coloration_mean_Manyas, Planform_curvature_Manyas_2) ##extent karsilastirmak için kullanisli
proj4string(covs_manyas_landsat_texture) <- CRS("+init=epsg:32635")

#Mapping_RF_khavr_Texture_class
RF_Manyas_Most_Probable_Soil_Texture_Class <- predict(covs_manyas_landsat_texture, RF_Manyas_Texture_REGR_model_tune$finalModel, type = "class",
                                                                    filename = "RF_Manyas_Texture__Most_Probable_Soil_Texture_Class.tif",format = "GTiff",
                                                                    overwrite = T, datatype = "INT2S")


Randomforest_RF_Manyas_Most_Probable_Soil_Texture_Class <- as.factor(RF_Manyas_Most_Probable_Soil_Texture_Class)
Randomforest_RF_Manyas_Most_Probable_Soil_Texture_Class

rat <- levels(Randomforest_Khavr__Most_Probable_Soil_Texture_Class)[[1]]
rat[["Texture_Class"]] <- c("CLo", "L", "SaCLo", "SaLo", "SiLo")
levels(Randomforest_Khavr__Most_Probable_Soil_Texture_Class) <- rat

area_colors <- c("#2b83ba", "#fdae61", "#d7191c", "#a6611a", "#c2a5cf")
# plot
levelplot(Randomforest_Khavr__Most_Probable_Soil_Texture_Class, col.regions = area_colors,
          xlab = "", ylab = "")


##Manyas_Texture####Rasterproduction probability each class
##CLAY
CLAY_RF_Manyas_Most_Probable_Soil_Texture_Class_Probable <- predict(covs_manyas_landsat_texture, RF_Manyas_Texture_REGR_model_tune$finalModel, type = "prob",
                                                                 index = 1, filename = "CLAY_RF_Manyas_Most_Probable_Soil_Texture_Class_Probable.tif", format = "GTiff", overwrite = T, datatype = "FLT4S")
##ClayLoam
ClayLoam_RF_Manyas_Most_Probable_Soil_Texture_Class_Probable <- predict(covs_manyas_landsat_texture, RF_Manyas_Texture_REGR_model_tune$finalModel, type = "prob",
                                                                     index = 2, filename = "ClayLoam_RF_Manyas_Most_Probable_Soil_Texture_Class_Probable.tif", format = "GTiff", overwrite = T, datatype = "FLT4S")
##SandyClay
SandyClay_RF_Manyas_Landsat_Soil_Phosphorous_Status_Probable <- predict(covs_manyas_landsat_texture, RF_Manyas_Texture_REGR_model_tune$finalModel, type = "prob",
                                                                        index = 3, filename = "SandyClay_RF_Manyas_Most_Probable_Soil_Texture_Class_Probable.tif", format = "GTiff", overwrite = T, datatype = "FLT4S")
##SandyClayLoam
SandyClayLoam_RF_Manyas_Landsat_Soil_Phosphorous_Status_Probable  <- predict(covs_manyas_landsat_texture, RF_Manyas_Texture_REGR_model_tune$finalModel, type = "prob",
                                                                     index = 4, filename = "SandyClayLoam_RF_Manyas_Most_Probable_Soil_Texture_Class_Probable.tif", format = "GTiff", overwrite = T, datatype = "FLT4S")
##SiltyClay
SiltyClay_RF_Manyas_Landsat_Soil_Phosphorous_Status_Probable <- predict(covs_manyas_landsat_texture, RF_Manyas_Texture_REGR_model_tune$finalModel, type = "prob",
                                                                      index = 5, filename = "SiltyClay_RF_Manyas_Most_Probable_Soil_Texture_Class_Probable.tif", format = "GTiff", overwrite = T, datatype = "FLT4S")

par(mfrow= c(2,1))




#ilk6önemli
##
###SPI_Manyas_2 
plot(RF_Manyas_Texture_REGR_model$Sinif, RF_Manyas_Texture_REGR_model$TWI_Manyas_2, main = "TWI", xlab = "Soil Texture Class", ylab = "TWI", col = "#7fc97f")
TWI_Manyas_2_RF_mean_ort <- by(RF_Manyas_Texture_REGR_model$TWI_Manyas_2 , RF_Manyas_Texture_REGR_model$Sinif, mean)
points(TWI_Manyas_2_RF_mean_ort, col = "#2c7fb8", pch = 18)




##GNDVI_mean_Manyas
plot(RF_Manyas_Texture_REGR_model$Sinif, RF_Manyas_Texture_REGR_model$GNDVI_mean_Manyas, main = "GNDVI Mean", xlab = "Soil Texture Class", ylab = "GNDVI Mean", col = "#fdae6b")
GNDVI_mean_Manyas_RF_mean_ort <- by(RF_Manyas_Texture_REGR_model$GNDVI_mean_Manyas , RF_Manyas_Texture_REGR_model$Sinif, mean)
points(GNDVI_mean_Manyas_RF_mean_ort, col = "#2c7fb8", pch = 18)
##########################
##################################
###########################################
###############################
################################
##################################
###############################
############################
##########################
##################################
###########################################
###############################
################################
##################################
###############################
############################
##########CLAY

ctrlrfclasskfold5landsat <- trainControl(method= "repeatedcv", number = 5, repeats = 3, search = "random")
RF_Manyas_Texture_REGRESSION_CLAY_model_tune <- train(Clay~ coloration_mean_Manyas + 
                                             GNDVI_mean_Manyas + GRVI_mean_Manyas + Manyas_HGK_SYM_30m_Resample_2 + ndvi_mean_Manyas + Planform_curvature_Manyas_2 + 
                                             Profile_curvature_Manyas_2 + saturation_mean_Manyas + 
                                             Slope_Manyas2 + SPI_Manyas_2 + TWI_Manyas_2, method ="rf",
                                           data = RF_Manyas_Texture_REGR_model, trcontrol=ctrlrfclasskfold5landsat, importance = TRUE)
RF_Manyas_Texture_REGRESSION_CLAY_model_tune  #control and modeltype


####RF_Manyas_Texture_model
varImpPlot(RF_Manyas_Texture_REGRESSION_CLAY_model_tune$finalModel)
####Lets continue

library(lattice)
library(ggplot2)

####importance
RF_Manyas_Texture_REGR_CLAY_IMP <- varImp(RF_Manyas_Texture_REGRESSION_CLAY_model_tune$finalModel)
RF_Manyas_Texture_REGR_CLAY_IMP

RF_Manyas_Texture_REGR_CLAY_IMPORTANCE <- importance(RF_Manyas_Texture_REGRESSION_CLAY_model_tune$finalModel)

RF_Manyas_Texture_REGR_CLAY_IMPORTANCE_dataframe <- as.data.frame(RF_Manyas_Texture_REGR_CLAY_IMPORTANCE)
View(RF_Manyas_Texture_REGR_CLAY_IMPORTANCE_dataframe)
write.table(RF_Manyas_Texture_REGR_CLAY_IMPORTANCE_dataframe, "RF_Manyas_Texture_REGR_CLAY_IMPORTANCE.TXT", col.names = T, row.names =T, sep = ",")



##All
predictRF_Manyas_Texture_REGRESSION_CLAY <- predict(RF_Manyas_Texture_REGRESSION_CLAY_model_tune$finalModel, newdata = RF_Manyas_Texture_REGR_model, )
View(predictRF_Manyas_Texture_REGRESSION_CLAY)
table(predictRF_Manyas_Texture_class)

RF_Manyas_Texture_REGRESSION_CLAY_model_tune

###results
goof(observed = RF_Manyas_Texture_REGR_model$Clay,
        predicted = predictRF_Manyas_Texture_REGRESSION_CLAY, plot.it = TRUE)



library(hydroGOF)
library(PerformanceAnalytics)
library(MLmetrics)
#training
nrmse(predictRF_Manyas_Texture_REGRESSION_CLAY, RF_Manyas_Texture_REGR_model$Clay)
MAPE(predictRF_Manyas_Texture_REGRESSION_CLAY, RF_Manyas_Texture_REGR_model$Clay)


##########SILT

ctrlrfclasskfold5landsat <- trainControl(method= "repeatedcv", number = 5, repeats = 3, search = "random")
RF_Manyas_Texture_REGRESSION_SILT_model_tune <- train(Silt~ coloration_mean_Manyas + 
                                                        GNDVI_mean_Manyas + GRVI_mean_Manyas + Manyas_HGK_SYM_30m_Resample_2 + ndvi_mean_Manyas + Planform_curvature_Manyas_2 + 
                                                        Profile_curvature_Manyas_2 + saturation_mean_Manyas + 
                                                        Slope_Manyas2 + SPI_Manyas_2 + TWI_Manyas_2, method ="rf",
                                                      data = RF_Manyas_Texture_REGR_model, trcontrol=ctrlrfclasskfold5landsat, importance = TRUE)
RF_Manyas_Texture_REGRESSION_SILT_model_tune$finalModel  #control and modeltype


####RF_Manyas_Texture_model
varImpPlot(RF_Manyas_Texture_REGRESSION_SILT_model_tune$finalModel)
####Lets continue

library(lattice)
library(ggplot2)

####importance
RF_Manyas_Texture_REGR_SILT_IMP <- varImp(RF_Manyas_Texture_REGRESSION_SILT_model_tune$finalModel)
RF_Manyas_Texture_REGR_SILT_IMP

RF_Manyas_Texture_REGR_SILT_IMPORTANCE <- importance(RF_Manyas_Texture_REGRESSION_SILT_model_tune$finalModel)

RF_Manyas_Texture_REGR_SILT_IMPORTANCE_dataframe <- as.data.frame(RF_Manyas_Texture_REGR_SILT_IMPORTANCE)
View(RF_Manyas_Texture_REGR_SILT_IMPORTANCE_dataframe)
write.table(RF_Manyas_Texture_REGR_SILT_IMPORTANCE_dataframe, "RF_Manyas_Texture_REGR_SILT_IMPORTANCE.TXT", col.names = T, row.names =T, sep = ",")



##All
predictRF_Manyas_Texture_REGRESSION_SILT <- predict(RF_Manyas_Texture_REGRESSION_SILT_model_tune$finalModel, newdata = RF_Manyas_Texture_REGR_model, )
View(predictRF_Manyas_Texture_REGRESSION_SILT)


RF_Manyas_Texture_REGRESSION_CLAY_model_tune

###results
goof(observed = RF_Manyas_Texture_REGR_model$Silt,
     predicted = predictRF_Manyas_Texture_REGRESSION_SILT, plot.it = TRUE)



library(hydroGOF)
library(PerformanceAnalytics)
library(MLmetrics)
#training
nrmse(predictRF_Manyas_Texture_REGRESSION_SILT, RF_Manyas_Texture_REGR_model$Silt)
MAPE(predictRF_Manyas_Texture_REGRESSION_SILT, RF_Manyas_Texture_REGR_model$Silt)


##########SILT

ctrlrfclasskfold5landsat <- trainControl(method= "repeatedcv", number = 5, repeats = 3, search = "random")
RF_Manyas_Texture_REGRESSION_SAND_model_tune <- train(Sand~ coloration_mean_Manyas + 
                                                        GNDVI_mean_Manyas + GRVI_mean_Manyas + Manyas_HGK_SYM_30m_Resample_2 + ndvi_mean_Manyas + Planform_curvature_Manyas_2 + 
                                                        Profile_curvature_Manyas_2 + saturation_mean_Manyas + 
                                                        Slope_Manyas2 + SPI_Manyas_2 + TWI_Manyas_2, method ="rf",
                                                      data = RF_Manyas_Texture_REGR_model, trcontrol=ctrlrfclasskfold5landsat, importance = TRUE)
RF_Manyas_Texture_REGRESSION_SAND_model_tune$finalModel  #control and modeltype


####RF_Manyas_Texture_model
varImpPlot(RF_Manyas_Texture_REGRESSION_SAND_model_tune$finalModel)
####Lets continue

library(lattice)
library(ggplot2)

####importance
RF_Manyas_Texture_REGR_SAND_IMP <- varImp(RF_Manyas_Texture_REGRESSION_SAND_model_tune$finalModel)
RF_Manyas_Texture_REGR_SAND_IMP

RF_Manyas_Texture_REGR_SAND_IMPORTANCE <- importance(RF_Manyas_Texture_REGRESSION_SAND_model_tune$finalModel)

RF_Manyas_Texture_REGR_SAND_IMPORTANCE_dataframe <- as.data.frame(RF_Manyas_Texture_REGR_SAND_IMPORTANCE)
View(RF_Manyas_Texture_REGR_SAND_IMPORTANCE_dataframe)
write.table(RF_Manyas_Texture_REGR_SAND_IMPORTANCE_dataframe, "RF_Manyas_Texture_REGR_SAND_IMPORTANCE.TXT", col.names = T, row.names =T, sep = ",")



##All
predictRF_Manyas_Texture_REGRESSION_SAND <- predict(RF_Manyas_Texture_REGRESSION_SAND_model_tune$finalModel, newdata = RF_Manyas_Texture_REGR_model, )
View(predictRF_Manyas_Texture_REGRESSION_SAND)




###results
goof(observed = RF_Manyas_Texture_REGR_model$Sand,
     predicted = predictRF_Manyas_Texture_REGRESSION_SAND, plot.it = TRUE)



library(hydroGOF)
library(PerformanceAnalytics)
library(MLmetrics)
#training
nrmse(predictRF_Manyas_Texture_REGRESSION_SAND, RF_Manyas_Texture_REGR_model$Sand)
MAPE(predictRF_Manyas_Texture_REGRESSION_SAND, RF_Manyas_Texture_REGR_model$Sand)




###############
##############
#################
#######MAPPIN#############
####CLAY
RF_Manyas_Texture_REGRESSION_CLAY_rf <- predict(covs_manyas_landsat_texture, RF_Manyas_Texture_REGRESSION_CLAY_model_tune$finalModel, "RF_Manyas_Texture_REGRESSION_CLAY.tif",
                         format = "GTiff", datatype = "FLT4S", overwrite = TRUE)
plot(RF_Manyas_Texture_REGRESSION_CLAY_rf,
     main = "RF_Manyas_Texture_REGRESSION 0-30 cm CLAY")


####SILT
RF_Manyas_Texture_REGRESSION_SILT <- predict(covs_manyas_landsat_texture, RF_Manyas_Texture_REGRESSION_SILT_model_tune$finalModel, "RF_Manyas_Texture_REGRESSION_SILT.tif",
                         format = "GTiff", datatype = "FLT4S", overwrite = TRUE)
plot(khavr_sand_rf,
     main = "Khavr Random Forest model predicted 0-30 cm Sand")


####SAND
RF_Manyas_Texture_REGRESSION_SAND_rf <- predict(covs_manyas_landsat_texture, RF_Manyas_Texture_REGRESSION_SAND_model_tune$finalModel, "RF_Manyas_Texture_REGRESSION_SAND.tif",
                         format = "GTiff", datatype = "FLT4S", overwrite = TRUE)
plot(khavr_sand_rf,
     main = "Khavr Random Forest model predicted 0-30 cm Sand")