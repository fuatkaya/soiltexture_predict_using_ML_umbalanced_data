library(randomForest)
library(ithir)
library(MASS)
library(caret)
library(ggplot2)
library(rasterVis)
library(lattice)
library(latticeExtra)
library(nnet)
library(rpart)
library(rattle)
modelLookup("")



###########################Modelleme

RF_Manyas_Texture_REGR <-read.csv(file.choose(),header =T, sep = ",") 
set.seed(2019)
RF_Manyas_Texture_REGR_model <-  as.data.frame(RF_Manyas_Texture_REGR)
View(RF_Manyas_Texture_REGR)
str(RF_Manyas_Texture_REGR)
summary(RF_Manyas_Texture_REGR)

####TrainRF_Manyas_P_Landsat_model <- sample(nrow(RF_Manyas_P_Landsat_model), 0.7 * nrow(RF_Manyas_P_Landsat_model))
####View(TrainRF_Manyas_P_Landsat_model)
#######str(TrainRF_Manyas_P_Landsat_model)
########[TrainRF_Manyas_P_Landsat_model, ]
###tuningclassification

DT_Manyas_Texture_REGR_model_tune <- train(Sinif~ coloration_mean_Manyas + 
                                             GNDVI_mean_Manyas + GRVI_mean_Manyas + Manyas_HGK_SYM_30m_Resample_2 + ndvi_mean_Manyas + Planform_curvature_Manyas_2 + 
                                             Profile_curvature_Manyas_2 + saturation_mean_Manyas + 
                                             Slope_Manyas2 + SPI_Manyas_2 + TWI_Manyas_2, method ="rpart",
                                           data = RF_Manyas_Texture_REGR_model, control = rpart.control(minsplit=10))
DT_Manyas_Texture_REGR_model_tune  #control and modeltype, 



#Modeli Randomforest pakeinde kuracagiz

#####randomforestpackages
##RF_Manyas_P_Landsat_modelrandomforestpackages <- randomForest(Sinif~ coloration_mean_Manyas + 
##???GNDVI_mean_Manyas + GRVI_mean_Manyas + Manyas_HGK_SYM_30m_Resample_2 + ndvi_mean_Manyas + Planform_curvature_Manyas_2 + 
##Profile_curvature_Manyas_2 + saturation_mean_Manyas + 
###Slope_Manyas2 + SPI_Manyas_2 + TWI_Manyas_2, data = RF_Manyas_Texture_REGR_model,
# mtry=6, ntree= 500, importance = TRUE)
####RF_Manyas_P_Landsat_modelrandomforestpackages

####Lets continue

library(lattice)
library(ggplot2)

####importance
DT_Manyas_Texture_REGR_IMP <- varImp(DT_Manyas_Texture_REGR_model_tune$finalModel)
DT_Manyas_Texture_REGR_IMP



DT_Manyas_Texture_REGR_IMPORTANCE_dataframe <- as.data.frame(DT_Manyas_Texture_REGR_IMP)
View(DT_Manyas_Texture_REGR_IMPORTANCE_dataframe)
write.table(DT_Manyas_Texture_REGR_IMPORTANCE_dataframe, "DT_Manyas_Texture_Class_IMPORTANCE.TXT", col.names = T, row.names =T, sep = ",")

########probbability value

##############################################




DT_Manyas_Texture_class_probs_all <-predict(DT_Manyas_Texture_REGR_model_tune$finalModel, type = "prob", newdata = RF_Manyas_Texture_REGR_model)



DT_Manyas_Texture_class_probs_all_export <- as.data.frame(DT_Manyas_Texture_class_probs_all)


write.table(DT_Manyas_Texture_class_probs_all_export, "DT_Manyas_Texture_class_probs_all_export.TXT", col.names = T, row.names = T, sep = ",")

####confusionmatrix
##Train-Calibration
predictDT_Manyas_Texture_class <- predict(DT_Manyas_Texture_REGR_model_tune$finalModel, type = "class", newdata = RF_Manyas_Texture_REGR_model )
View(predictDT_Manyas_Texture_class)
table(predictDT_Manyas_Texture_class)

DT_Manyas_Texture_REGR_model_tune$metric
###results
goofcat(observed = RF_Manyas_Texture_REGR_model$Sinif,
        predicted = predictDT_Manyas_Texture_class)
confusionMatrix(RF_Manyas_Texture_REGR_model$Sinif, predictDT_Manyas_Texture_class)
###graph
######memory.limit(size=112000) 
#######fancyRpartPlot(DT_Manyas_Texture_REGR_model_tune$finalModel, cex=0.55)
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
DT_Manyas_Most_Probable_Soil_Texture_Class <- predict(covs_manyas_landsat_texture, DT_Manyas_Texture_REGR_model_tune$finalModel, type = "class",
                                                      filename = "DT_Manyas_Texture__Most_Probable_Soil_Texture_Class.tif",format = "GTiff",
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
CLAY_DT_Manyas_Most_Probable_Soil_Texture_Class_Probable <- predict(covs_manyas_landsat_texture, DT_Manyas_Texture_REGR_model_tune$finalModel, type = "prob",
                                                                    index = 1, filename = "CLAY_DT_Manyas_Most_Probable_Soil_Texture_Class_Probable.tif", format = "GTiff", overwrite = T, datatype = "FLT4S")
##ClayLoam
ClayLoam_DT_Manyas_Most_Probable_Soil_Texture_Class_Probable <- predict(covs_manyas_landsat_texture, DT_Manyas_Texture_REGR_model_tune$finalModel, type = "prob",
                                                                        index = 2, filename = "ClayLoam_DT_Manyas_Most_Probable_Soil_Texture_Class_Probable.tif", format = "GTiff", overwrite = T, datatype = "FLT4S")
##SandyClay
SandyClay_DT_Manyas_Landsat_Soil_Phosphorous_Status_Probable <- predict(covs_manyas_landsat_texture, DT_Manyas_Texture_REGR_model_tune$finalModel, type = "prob",
                                                                        index = 3, filename = "SandyClay_DT_Manyas_Most_Probable_Soil_Texture_Class_Probable.tif", format = "GTiff", overwrite = T, datatype = "FLT4S")
##SandyClayLoam
SandyClayLoam_DT_Manyas_Landsat_Soil_Phosphorous_Status_Probable  <- predict(covs_manyas_landsat_texture, DT_Manyas_Texture_REGR_model_tune$finalModel, type = "prob",
                                                                             index = 4, filename = "SandyClayLoam_DT_Manyas_Most_Probable_Soil_Texture_Class_Probable.tif", format = "GTiff", overwrite = T, datatype = "FLT4S")
##SiltyClay
SiltyClay_DT_Manyas_Landsat_Soil_Phosphorous_Status_Probable <- predict(covs_manyas_landsat_texture, DT_Manyas_Texture_REGR_model_tune$finalModel, type = "prob",
                                                                        index = 5, filename = "SiltyClay_DT_Manyas_Most_Probable_Soil_Texture_Class_Probable.tif", format = "GTiff", overwrite = T, datatype = "FLT4S")

par(mfrow= c(2,1))




#ilk6önemli
##
###SPI_Manyas_2 
plot(RF_Manyas_Texture_REGR_model$Sinif, RF_Manyas_Texture_REGR_model$TWI_Manyas_2, main = "TWI", xlab = "Soil Texture Class", ylab = "TWI", col = "#dd1c77")
TWI_Manyas_2_RF_mean_ort <- by(RF_Manyas_Texture_REGR_model$TWI_Manyas_2 , RF_Manyas_Texture_REGR_model$Sinif, mean)
points(TWI_Manyas_2_RF_mean_ort, col = "#2c7fb8", pch = 18)




##GNDVI_mean_Manyas
plot(RF_Manyas_Texture_REGR_model$Sinif, RF_Manyas_Texture_REGR_model$ndvi_mean_Manyas, main = "NDVI Mean", xlab = "Soil Texture Class", ylab = "NDVI Mean", col = "#fc9272")
ndvi_mean_Manyas_Manyas_RF_mean_ort <- by(RF_Manyas_Texture_REGR_model$ndvi_mean_Manyas , RF_Manyas_Texture_REGR_model$Sinif, mean)
points(ndvi_mean_Manyas_Manyas_RF_mean_ort, col = "#2c7fb8", pch = 18)
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


DT_Manyas_Texture_REGRESSION_CLAY_model_tune <- rpart(Clay~ coloration_mean_Manyas + 
                                                        GNDVI_mean_Manyas + GRVI_mean_Manyas + Manyas_HGK_SYM_30m_Resample_2 + ndvi_mean_Manyas + Planform_curvature_Manyas_2 + 
                                                        Profile_curvature_Manyas_2 + saturation_mean_Manyas + 
                                                        Slope_Manyas2 + SPI_Manyas_2 + TWI_Manyas_2,
                                                      data = RF_Manyas_Texture_REGR_model)

DT_Manyas_Texture_REGRESSION_CLAY_model_tune

####RF_Manyas_Texture_model
####Lets continue

library(lattice)
library(ggplot2)

####importance
DT_Manyas_Texture_REGR_CLAY_IMP <- varImp(DT_Manyas_Texture_REGRESSION_CLAY_model_tune)
RF_Manyas_Texture_REGR_CLAY_IMP


DT_Manyas_Texture_REGR_CLAY_IMPORTANCE_dataframe <- as.data.frame(DT_Manyas_Texture_REGR_CLAY_IMP)
View(DT_Manyas_Texture_REGR_CLAY_IMPORTANCE_dataframe)
write.table(DT_Manyas_Texture_REGR_CLAY_IMPORTANCE_dataframe, "DT_Manyas_Texture_REGR_CLAY_IMPORTANCE.TXT", col.names = T, row.names =T, sep = ",")



##All
predictDT_Manyas_Texture_REGRESSION_CLAY <- predict(DT_Manyas_Texture_REGRESSION_CLAY_model_tune, newdata = RF_Manyas_Texture_REGR_model, )
View(predictDT_Manyas_Texture_REGRESSION_CLAY)
table(predictDT_Manyas_Texture_REGRESSION_CLAY)

RF_Manyas_Texture_REGRESSION_CLAY_model_tune

###results
goof(observed = RF_Manyas_Texture_REGR_model$Clay,
     predicted = predictDT_Manyas_Texture_REGRESSION_CLAY, plot.it = TRUE)



library(hydroGOF)
library(PerformanceAnalytics)
library(MLmetrics)
#training
nrmse(predictDT_Manyas_Texture_REGRESSION_CLAY, RF_Manyas_Texture_REGR_model$Clay)
MAPE(predictDT_Manyas_Texture_REGRESSION_CLAY, RF_Manyas_Texture_REGR_model$Clay)


##########SILT

ctrlrfclasskfold5landsat <- trainControl(method= "repeatedcv", number = 5, repeats = 3, search = "random")
DT_Manyas_Texture_REGRESSION_SILT_model_tune <- rpart(Silt~ coloration_mean_Manyas + 
                                                        GNDVI_mean_Manyas + GRVI_mean_Manyas + Manyas_HGK_SYM_30m_Resample_2 + ndvi_mean_Manyas + Planform_curvature_Manyas_2 + 
                                                        Profile_curvature_Manyas_2 + saturation_mean_Manyas + 
                                                        Slope_Manyas2 + SPI_Manyas_2 + TWI_Manyas_2,
                                                      data = RF_Manyas_Texture_REGR_model)
DT_Manyas_Texture_REGRESSION_SILT_model_tune
####RF_Manyas_Texture_model
####Lets continue

library(lattice)
library(ggplot2)

####importance
DT_Manyas_Texture_REGR_SILT_IMP <- varImp(DT_Manyas_Texture_REGRESSION_SILT_model_tune)




DT_Manyas_Texture_REGR_SILT_IMPORTANCE_dataframe <- as.data.frame(DT_Manyas_Texture_REGR_SILT_IMP)
View(DT_Manyas_Texture_REGR_SILT_IMPORTANCE_dataframe)
write.table(DT_Manyas_Texture_REGR_SILT_IMPORTANCE_dataframe, "DT_Manyas_Texture_REGR_SILT_IMPORTANCE.TXT", col.names = T, row.names =T, sep = ",")



##All
predictDT_Manyas_Texture_REGRESSION_SILT <- predict(DT_Manyas_Texture_REGRESSION_SILT_model_tune, newdata = RF_Manyas_Texture_REGR_model, )
View(predictDT_Manyas_Texture_REGRESSION_SILT)


RF_Manyas_Texture_REGRESSION_CLAY_model_tune

###results
goof(observed = RF_Manyas_Texture_REGR_model$Silt,
     predicted = predictDT_Manyas_Texture_REGRESSION_SILT, plot.it = TRUE)



library(hydroGOF)
library(PerformanceAnalytics)
library(MLmetrics)
#training
nrmse(predictDT_Manyas_Texture_REGRESSION_SILT, RF_Manyas_Texture_REGR_model$Silt)
MAPE(predictDT_Manyas_Texture_REGRESSION_SILT, RF_Manyas_Texture_REGR_model$Silt)


##########SILT

ctrlrfclasskfold5landsat <- trainControl(method= "repeatedcv", number = 5, repeats = 3, search = "random")
DT_Manyas_Texture_REGRESSION_SAND_model_tune <- rpart(Sand~ coloration_mean_Manyas + 
                                                        GNDVI_mean_Manyas + GRVI_mean_Manyas + Manyas_HGK_SYM_30m_Resample_2 + ndvi_mean_Manyas + Planform_curvature_Manyas_2 + 
                                                        Profile_curvature_Manyas_2 + saturation_mean_Manyas + 
                                                        Slope_Manyas2 + SPI_Manyas_2 + TWI_Manyas_2,
                                                      data = RF_Manyas_Texture_REGR_model)
DT_Manyas_Texture_REGRESSION_SAND_model_tune
####RF_Manyas_Texture_model
####Lets continue

library(lattice)
library(ggplot2)

####importance
DT_Manyas_Texture_REGR_SAND_IMP <- varImp(DT_Manyas_Texture_REGRESSION_SAND_model_tune)
RF_Manyas_Texture_REGR_SAND_IMP

RF_Manyas_Texture_REGR_SAND_IMPORTANCE <- importance(RF_Manyas_Texture_REGRESSION_SAND_model_tune$finalModel)

DT_Manyas_Texture_REGR_SAND_IMPORTANCE_dataframe <- as.data.frame(DT_Manyas_Texture_REGR_SAND_IMP)
View(DT_Manyas_Texture_REGR_SAND_IMPORTANCE_dataframe)
write.table(DT_Manyas_Texture_REGR_SAND_IMPORTANCE_dataframe, "DT_Manyas_Texture_REGR_SAND_IMPORTANCE.TXT", col.names = T, row.names =T, sep = ",")



##All
predictDT_Manyas_Texture_REGRESSION_SAND <- predict(DT_Manyas_Texture_REGRESSION_SAND_model_tune, newdata = RF_Manyas_Texture_REGR_model, )
View(predictDT_Manyas_Texture_REGRESSION_SAND)




###results
goof(observed = RF_Manyas_Texture_REGR_model$Sand,
     predicted = predictDT_Manyas_Texture_REGRESSION_SAND, plot.it = TRUE)



library(hydroGOF)
library(PerformanceAnalytics)
library(MLmetrics)
#training
nrmse(predictDT_Manyas_Texture_REGRESSION_SAND, RF_Manyas_Texture_REGR_model$Sand)
MAPE(predictDT_Manyas_Texture_REGRESSION_SAND, RF_Manyas_Texture_REGR_model$Sand)




###############
##############
#################
#######MAPPIN#############
####CLAY
DT_Manyas_Texture_REGRESSION_CLAY_dt <- predict(covs_manyas_landsat_texture, DT_Manyas_Texture_REGRESSION_CLAY_model_tune, "DT_Manyas_Texture_REGRESSION_CLAY.tif",
                                                format = "GTiff", datatype = "FLT4S", overwrite = TRUE)
plot(RF_Manyas_Texture_REGRESSION_CLAY_rf,
     main = "RF_Manyas_Texture_REGRESSION 0-30 cm CLAY")


####SILT
DT_Manyas_Texture_REGRESSION_SILT_dt <- predict(covs_manyas_landsat_texture, DT_Manyas_Texture_REGRESSION_SILT_model_tune, "DT_Manyas_Texture_REGRESSION_SILT.tif",
                                             format = "GTiff", datatype = "FLT4S", overwrite = TRUE)



####SAND
DT_Manyas_Texture_REGRESSION_SAND_rf <- predict(covs_manyas_landsat_texture, DT_Manyas_Texture_REGRESSION_SAND_model_tune, "DT_Manyas_Texture_REGRESSION_SAND.tif",
                                                format = "GTiff", datatype = "FLT4S", overwrite = TRUE)
