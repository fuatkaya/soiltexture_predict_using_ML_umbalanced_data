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

####TrainRF_Manyas_P_Landsat_model <- sample(nrow(RF_Manyas_P_Landsat_model), 0.7 * nrow(RF_Manyas_P_Landsat_model))
####View(TrainRF_Manyas_P_Landsat_model)
#######str(TrainRF_Manyas_P_Landsat_model)
########[TrainRF_Manyas_P_Landsat_model, ]
###tuningclassification
ctrlrfclasskfold5landsat <- trainControl(method= "repeatedcv", number = 5, repeats = 3, search = "random")


library(e1071)
###Tune_________1

SVR_Manyas_Texture_REGR_model_tune1 <- train(Sinif~ coloration_mean_Manyas + 
                                               GNDVI_mean_Manyas + GRVI_mean_Manyas + Manyas_HGK_SYM_30m_Resample_2 + ndvi_mean_Manyas + Planform_curvature_Manyas_2 + 
                                               Profile_curvature_Manyas_2 + saturation_mean_Manyas + 
                                               Slope_Manyas2 + SPI_Manyas_2 + TWI_Manyas_2,
                                             data = RF_Manyas_Texture_REGR_model, 
                                         method ="svmRadial", trainControl=ctrlrfclasskfold5landsat)
SVR_Manyas_Texture_REGR_model_tune1$finalModel

###Tune_________2
SVR_Manyas_Texture_REGR_model_tune2 <- tune(svm, Sinif ~ coloration_mean_Manyas + 
                                              GNDVI_mean_Manyas + GRVI_mean_Manyas + Manyas_HGK_SYM_30m_Resample_2 + ndvi_mean_Manyas + Planform_curvature_Manyas_2 + 
                                              Profile_curvature_Manyas_2 + saturation_mean_Manyas + 
                                              Slope_Manyas2 + SPI_Manyas_2 + TWI_Manyas_2,
                                            data = RF_Manyas_Texture_REGR_model,  
                                           kernel = "radial", 
                                           ranges = list(gamma= 2^(-2:2), cost = 2^(-4:10)),
                                           tune.control=tune.control(cross = 5))



SVR_Manyas_Texture_REGR_model_tune2$performances


SVR_Manyas_Texture_REGR_support_vector_reg <- svm(Sinif ~ coloration_mean_Manyas + 
                                                  GNDVI_mean_Manyas + GRVI_mean_Manyas + Manyas_HGK_SYM_30m_Resample_2 + ndvi_mean_Manyas + Planform_curvature_Manyas_2 + 
                                                  Profile_curvature_Manyas_2 + saturation_mean_Manyas + 
                                                  Slope_Manyas2 + SPI_Manyas_2 + TWI_Manyas_2,
                                                data = RF_Manyas_Texture_REGR_model, kernel="radial", cost=0.0625, gamma=0.25, sigma = 0.0639273689418667,probability=TRUE)

SVR_Manyas_Texture_REGR_support_vector_reg

w <- t(SVR_Manyas_Texture_REGR_support_vector_reg$coefs) %*% SVR_Manyas_Texture_REGR_support_vector_reg$SV
w <- apply(w, 2, function(v){sqrt(sum(v^2))})
w <- sort(w, decreasing = T)
print(w)





SVR_Manyas_Texture_REGR_support_vector_reg_IMPORTANCE_dataframe <- as.data.frame(w)
View(SVR_Manyas_Texture_REGR_support_vector_reg_IMPORTANCE_dataframe)
write.table(SVR_Manyas_Texture_REGR_support_vector_reg_IMPORTANCE_dataframe, "SVR_Manyas_Texture_Class_IMPORTANCE.TXT", col.names = T, row.names =T, sep = ",")






SVR_Manyas_Texture_class_probs_all <-attr(predict(SVR_Manyas_Texture_REGR_support_vector_reg, newdata = RF_Manyas_Texture_REGR_model, probability=TRUE, na.action = na.pass), "probabilities")



SVR_Manyas_Texture_class_probs_all_export <- as.data.frame(SVR_Manyas_Texture_class_probs_all)
View(SVR_Manyas_Texture_class_probs_all)

write.table(SVR_Manyas_Texture_class_probs_all_export, "SVR_Manyas_Texture_class_probs_all_export.TXT", col.names = T, row.names = T, sep = ",")

####confusionmatrix
##Train-Calibration
predictSVR_Manyas_Texture_class <- predict(SVR_Manyas_Texture_REGR_support_vector_reg, type = "class", newdata = RF_Manyas_Texture_REGR_model)
View(predictSVR_Manyas_Texture_class)
table(predictSVR_Manyas_Texture_class)



###results
goofcat(observed = RF_Manyas_Texture_REGR_model$Sinif,
        predicted = predictSVR_Manyas_Texture_class)
confusionMatrix(RF_Manyas_Texture_REGR_model$Sinif, predictSVR_Manyas_Texture_class)

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
SVR_Manyas_Most_Probable_Soil_Texture_Class <- predict(covs_manyas_landsat_texture, SVR_Manyas_Texture_REGR_support_vector_reg, type = "class",
                                                      filename = "SVR_Manyas_Texture__Most_Probable_Soil_Texture_Class.tif",format = "GTiff",
                                                      overwrite = T, datatype = "INT2S")


##Manyas_Texture####Rasterproduction probability each class
##CLAY
CLAY_SVR_Manyas_Most_Probable_Soil_Texture_Class_Probable <- predict(covs_manyas_landsat_texture, SVR_Manyas_Texture_REGR_support_vector_reg, type = "prob",
                                                                    index = 1, filename = "CLAY_SVR_Manyas_Most_Probable_Soil_Texture_Class_Probable.tif", format = "GTiff", overwrite = T, datatype = "FLT4S")
##ClayLoam
ClayLoam_SVR_Manyas_Most_Probable_Soil_Texture_Class_Probable <- predict(covs_manyas_landsat_texture, SVR_Manyas_Texture_REGR_support_vector_reg, type = "prob",
                                                                        index = 2, filename = "ClayLoam_SVR_Manyas_Most_Probable_Soil_Texture_Class_Probable.tif", format = "GTiff", overwrite = T, datatype = "FLT4S")
##SandyClay
SandyClay_SVR_Manyas_Landsat_Soil_Phosphorous_Status_Probable <- predict(covs_manyas_landsat_texture, SVR_Manyas_Texture_REGR_support_vector_reg, type = "prob",
                                                                        index = 3, filename = "SandyClay_SVR_Manyas_Most_Probable_Soil_Texture_Class_Probable.tif", format = "GTiff", overwrite = T, datatype = "FLT4S")
##SandyClayLoam
SandyClayLoam_SVR_Manyas_Landsat_Soil_Phosphorous_Status_Probable  <- predict(covs_manyas_landsat_texture, SVR_Manyas_Texture_REGR_support_vector_reg, type = "prob",
                                                                             index = 4, filename = "SandyClayLoam_SVR_Manyas_Most_Probable_Soil_Texture_Class_Probable.tif", format = "GTiff", overwrite = T, datatype = "FLT4S")
##SiltyClay
SiltyClay_SVR_Manyas_Landsat_Soil_Phosphorous_Status_Probable <- predict(covs_manyas_landsat_texture, SVR_Manyas_Texture_REGR_support_vector_reg, type = "prob",
                                                                        index = 5, filename = "SiltyClay_SVR_Manyas_Most_Probable_Soil_Texture_Class_Probable.tif", format = "GTiff", overwrite = T, datatype = "FLT4S")

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
###Tune_________2
SVR_Manyas_Texture_REGR_Clay <- tune(svm, Clay ~ coloration_mean_Manyas + 
                                              GNDVI_mean_Manyas + GRVI_mean_Manyas + Manyas_HGK_SYM_30m_Resample_2 + ndvi_mean_Manyas + Planform_curvature_Manyas_2 + 
                                              Profile_curvature_Manyas_2 + saturation_mean_Manyas + 
                                              Slope_Manyas2 + SPI_Manyas_2 + TWI_Manyas_2,
                                            data = RF_Manyas_Texture_REGR_model,  
                                            kernel = "radial", 
                                            ranges = list(gamma= 2^(-2:2), cost = 2^(-4:10)),
                                            tune.control=tune.control(cross = 5))



SVR_Manyas_Texture_REGR_Clay


SVR_Manyas_Texture_REGR_Clay_support_vector_reg <- svm(Clay ~ coloration_mean_Manyas + 
                                                    GNDVI_mean_Manyas + GRVI_mean_Manyas + Manyas_HGK_SYM_30m_Resample_2 + ndvi_mean_Manyas + Planform_curvature_Manyas_2 + 
                                                    Profile_curvature_Manyas_2 + saturation_mean_Manyas + 
                                                    Slope_Manyas2 + SPI_Manyas_2 + TWI_Manyas_2,
                                                  data = RF_Manyas_Texture_REGR_model, kernel="radial", cost=2, gamma=0.5)

SVR_Manyas_Texture_REGR_Clay_support_vector_reg$fitted

w <- t(SVR_Manyas_Texture_REGR_Clay_support_vector_reg$coefs) %*% SVR_Manyas_Texture_REGR_Clay_support_vector_reg$SV
w <- apply(w, 2, function(v){sqrt(sum(v^2))})
w <- sort(w, decreasing = T)
print(w)





SVR_Manyas_Texture_REGR_Clay_support_vector_reg_IMPORTANCE_dataframe <- as.data.frame(w)
View(SVR_Manyas_Texture_REGR_Clay_support_vector_reg_IMPORTANCE_dataframe)
write.table(SVR_Manyas_Texture_REGR_Clay_support_vector_reg_IMPORTANCE_dataframe, "SVR_Manyas_Texture_REGR_Clay_IMPORTANCE.TXT", col.names = T, row.names =T, sep = ",")




##All
predictSVR_Manyas_Texture_REGRESSION_CLAY <- predict(SVR_Manyas_Texture_REGR_Clay_support_vector_reg, newdata = RF_Manyas_Texture_REGR_model, )


RF_Manyas_Texture_REGRESSION_CLAY_model_tune

###results
goof(observed = RF_Manyas_Texture_REGR_model$Clay,
     predicted = predictSVR_Manyas_Texture_REGRESSION_CLAY, plot.it = TRUE)



library(hydroGOF)
library(PerformanceAnalytics)
library(MLmetrics)
#training
nrmse(predictSVR_Manyas_Texture_REGRESSION_CLAY, RF_Manyas_Texture_REGR_model$Clay)
MAPE(predictSVR_Manyas_Texture_REGRESSION_CLAY, RF_Manyas_Texture_REGR_model$Clay)


##########SILT

SVR_Manyas_Texture_REGR_Silt <- tune(svm, Silt ~ coloration_mean_Manyas + 
                                       GNDVI_mean_Manyas + GRVI_mean_Manyas + Manyas_HGK_SYM_30m_Resample_2 + ndvi_mean_Manyas + Planform_curvature_Manyas_2 + 
                                       Profile_curvature_Manyas_2 + saturation_mean_Manyas + 
                                       Slope_Manyas2 + SPI_Manyas_2 + TWI_Manyas_2,
                                     data = RF_Manyas_Texture_REGR_model,  
                                     kernel = "radial", 
                                     ranges = list(gamma= 2^(-2:2), cost = 2^(-4:10)),
                                     tune.control=tune.control(cross = 5))



SVR_Manyas_Texture_REGR_Silt


SVR_Manyas_Texture_REGR_Silt_support_vector_reg <- svm(Silt ~ coloration_mean_Manyas + 
                                                         GNDVI_mean_Manyas + GRVI_mean_Manyas + Manyas_HGK_SYM_30m_Resample_2 + ndvi_mean_Manyas + Planform_curvature_Manyas_2 + 
                                                         Profile_curvature_Manyas_2 + saturation_mean_Manyas + 
                                                         Slope_Manyas2 + SPI_Manyas_2 + TWI_Manyas_2,
                                                       data = RF_Manyas_Texture_REGR_model, kernel="radial", cost=0.0625, gamma=0.25)

SVR_Manyas_Texture_REGR_Silt_support_vector_reg

w <- t(SVR_Manyas_Texture_REGR_Silt_support_vector_reg$coefs) %*% SVR_Manyas_Texture_REGR_Silt_support_vector_reg$SV
w <- apply(w, 2, function(v){sqrt(sum(v^2))})
w <- sort(w, decreasing = T)
print(w)





SVR_Manyas_Texture_REGR_Silt_support_vector_reg_IMPORTANCE_dataframe <- as.data.frame(w)
View(SVR_Manyas_Texture_REGR_Silt_support_vector_reg_IMPORTANCE_dataframe)
write.table(SVR_Manyas_Texture_REGR_Silt_support_vector_reg_IMPORTANCE_dataframe, "SVR_Manyas_Texture_REGR_Silt_IMPORTANCE.TXT", col.names = T, row.names =T, sep = ",")





##All
predictSVR_Manyas_Texture_REGRESSION_SILT <- predict(SVR_Manyas_Texture_REGR_Silt_support_vector_reg, newdata = RF_Manyas_Texture_REGR_model, )
View(predictSVR_Manyas_Texture_REGRESSION_SILT)


RF_Manyas_Texture_REGRESSION_CLAY_model_tune

###results
goof(observed = RF_Manyas_Texture_REGR_model$Silt,
     predicted = predictSVR_Manyas_Texture_REGRESSION_SILT, plot.it = TRUE)



library(hydroGOF)
library(PerformanceAnalytics)
library(MLmetrics)
#training
nrmse(predictSVR_Manyas_Texture_REGRESSION_SILT, RF_Manyas_Texture_REGR_model$Silt)
MAPE(predictSVR_Manyas_Texture_REGRESSION_SILT, RF_Manyas_Texture_REGR_model$Silt)


##########SILT

SVR_Manyas_Texture_REGR_Sand <- tune(svm, Sand ~ coloration_mean_Manyas + 
                                       GNDVI_mean_Manyas + GRVI_mean_Manyas + Manyas_HGK_SYM_30m_Resample_2 + ndvi_mean_Manyas + Planform_curvature_Manyas_2 + 
                                       Profile_curvature_Manyas_2 + saturation_mean_Manyas + 
                                       Slope_Manyas2 + SPI_Manyas_2 + TWI_Manyas_2,
                                     data = RF_Manyas_Texture_REGR_model,  
                                     kernel = "radial", 
                                     ranges = list(gamma= 2^(-2:2), cost = 2^(-4:10)),
                                     tune.control=tune.control(cross = 5))



SVR_Manyas_Texture_REGR_Sand


SVR_Manyas_Texture_REGR_Sand_support_vector_reg <- svm(Sand ~ coloration_mean_Manyas + 
                                                         GNDVI_mean_Manyas + GRVI_mean_Manyas + Manyas_HGK_SYM_30m_Resample_2 + ndvi_mean_Manyas + Planform_curvature_Manyas_2 + 
                                                         Profile_curvature_Manyas_2 + saturation_mean_Manyas + 
                                                         Slope_Manyas2 + SPI_Manyas_2 + TWI_Manyas_2,
                                                       data = RF_Manyas_Texture_REGR_model, kernel="radial", cost=0.5, gamma=4)

SVR_Manyas_Texture_REGR_Sand_support_vector_reg

w <- t(SVR_Manyas_Texture_REGR_Sand_support_vector_reg$coefs) %*% SVR_Manyas_Texture_REGR_Sand_support_vector_reg$SV
w <- apply(w, 2, function(v){sqrt(sum(v^2))})
w <- sort(w, decreasing = T)
print(w)





SVR_Manyas_Texture_REGR_Sand_support_vector_reg_IMPORTANCE_dataframe <- as.data.frame(w)
View(SVR_Manyas_Texture_REGR_Sand_support_vector_reg_IMPORTANCE_dataframe)
write.table(SVR_Manyas_Texture_REGR_Sand_support_vector_reg_IMPORTANCE_dataframe, "SVR_Manyas_Texture_REGR_Sand_IMPORTANCE.TXT", col.names = T, row.names =T, sep = ",")





##All
predictSVR_Manyas_Texture_REGRESSION_SAND <- predict(SVR_Manyas_Texture_REGR_Sand_support_vector_reg, newdata = RF_Manyas_Texture_REGR_model, )
View(predictRF_Manyas_Texture_REGRESSION_SAND)




###results
goof(observed = RF_Manyas_Texture_REGR_model$Sand,
     predicted = predictSVR_Manyas_Texture_REGRESSION_SAND, plot.it = TRUE)



library(hydroGOF)
library(PerformanceAnalytics)
library(MLmetrics)
#training
nrmse(predictSVR_Manyas_Texture_REGRESSION_SAND, RF_Manyas_Texture_REGR_model$Sand)
MAPE(predictSVR_Manyas_Texture_REGRESSION_SAND, RF_Manyas_Texture_REGR_model$Sand)




###############
##############
#################
#######MAPPIN#############
####CLAY
SVR_Manyas_Texture_REGRESSION_CLAY_rf <- predict(covs_manyas_landsat_texture, SVR_Manyas_Texture_REGR_Clay_support_vector_reg, "SVR_Manyas_Texture_REGRESSION_CLAY.tif",
                                                format = "GTiff", datatype = "FLT4S", overwrite = TRUE)
plot(SVR_Manyas_Texture_REGRESSION_CLAY_rf,
     main = "SVR_Manyas_Texture_REGRESSION 0-30 cm CLAY")


####SILT
SVR_Manyas_Texture_REGRESSION_SILT <- predict(covs_manyas_landsat_texture, SVR_Manyas_Texture_REGR_Silt_support_vector_reg, "SVR_Manyas_Texture_REGRESSION_SILT.tif",
                                             format = "GTiff", datatype = "FLT4S", overwrite = TRUE)
plot(khavr_sand_rf,
     main = "Khavr Random Forest model predicted 0-30 cm Sand")


####SAND
SVR_Manyas_Texture_REGRESSION_SAND_rf <- predict(covs_manyas_landsat_texture, SVR_Manyas_Texture_REGR_Sand_support_vector_reg, "SVR_Manyas_Texture_REGRESSION_SAND.tif",
                                                format = "GTiff", datatype = "FLT4S", overwrite = TRUE)
plot(khavr_sand_rf,
     main = "Khavr Random Forest model predicted 0-30 cm Sand")