


library(shiny)
library(leaflet)
#library(rgdal)
library(terra)
library(sf)
library(here)
library(tmap)
library(DiagrammeR)
library(ggplot2)

library(readxl)


#South Asia country case study

#Temperature or Precipitation as Confounders - calculate Direct and Indirect effects on the Outcome variables of Interest




outcome_slope1 = vector(,34);
outcome_intercept = vector(,34);
outcome_slope2 = vector(,34);
outcome_slope3 = vector(,34);

outcome_slope1= matrix(, nrow = 34, ncol = 2);
outcome_slope2 = matrix(, nrow = 34, ncol = 2);
outcome_slope3 = matrix(, nrow = 34, ncol = 2);

newpredictions = vector(,37);

realcasesplot = vector(,34);
realcases = vector(,34);

#predictions2 = mat = matrix(, nrow = 34, ncol = 2);

modellfit =  matrix(, nrow = 34, ncol = 2);

gdp_agriculture_intercept = vector(,34)
gdp_agriculture_slope1  = matrix(, nrow = 34, ncol = 2);
gdp_agriculture_slope2  = matrix(, nrow = 34, ncol = 2);
gdp_agriculture_slope3  = matrix(, nrow = 34, ncol = 2);


predictions2 = vector(,34);


for (i in 1:34)
{
  if (i != 7  & i != 16 & i != 9 & i != 23 & i != 32)
    #Lack of maximum temperature : Gujarat (9), Lakshadweep (16 - islands in Indian Ocean) , Odisha (23), West Bengal (32)
    # Lack of GDP : Dadra and Nagar Haveli (7 - small teritory )
    {
    print(i)
    b1 = as.character(i)
    b2 = as.character(i+1)

    train_data <- sprintf("C%s:N%s",b1,b2)

    test_data <- sprintf("N%s:N%s",b1,b2)

    temperature_train <- read_excel("C:/backup/PredictionDisease/Soft Zenodo 2/Dataset.xlsx", sheet = "Maximum Temperature", range = train_data)#S38
    temperature_test<- read_excel("C:/backup/PredictionDisease/Soft Zenodo 2/Dataset.xlsx", sheet = "Maximum Temperature", range = test_data)#S38


    outcome_train <- read_excel("C:/backup/PredictionDisease/Soft Zenodo 2/Dataset.xlsx", sheet = "Outcome", range = train_data)#G38
    outcome_test <- read_excel("C:/backup/PredictionDisease/Soft Zenodo 2/Dataset.xlsx", sheet = "Outcome", range = test_data)#G38


    precipitation_train <- read_excel("C:/backup/PredictionDisease/Soft Zenodo 2/Dataset.xlsx", sheet = "Maximum Precipitation", range = train_data)#S38
    precipitation_test <- read_excel("C:/backup/PredictionDisease/Soft Zenodo 2/Dataset.xlsx", sheet = "Maximum Precipitation", range = test_data)#S38


    gdpagriculture_train <- read_excel("C:/backup/PredictionDisease/Soft Zenodo 2/Dataset.xlsx", sheet = "GDP Agriculture", range = train_data)#S38
    gdpagriculture_test <- read_excel("C:/backup/PredictionDisease/Soft Zenodo 2/Dataset.xlsx", sheet = "GDP Agriculture", range = test_data)#S38


    temperature_train_unlist <- unlist(temperature_train)
    outcome_train_unlist <- unlist(outcome_train)
    precipitation_train_unlist <- unlist(precipitation_train)
    gdpagriculture_train_unlist <- unlist(gdpagriculture_train)


    precipitation_test_unlist <- unlist(precipitation_test)
    gdpagriculture_test_unlist <- unlist(gdpagriculture_test)
    temperature_test_unlist <- unlist(temperature_test)
    outcome_test_unlist <- unlist(outcome_test)


    #outcome event of interest such as death, other disease, etc
    outcome <- glm(formula = outcome_train_unlist ~ temperature_train_unlist + gdpagriculture_train_unlist,family = "gaussian")

    #GDP from agriculture
    gdp_agriculture <- glm(formula = gdpagriculture_train_unlist ~ temperature_train_unlist,family = "gaussian")


    outcome_intercept[i] = outcome$coefficients[1] #intercept
    outcome_slope1[i,] = outcome$coefficients[2] #temperature
    outcome_slope2[i,] = outcome$coefficients[3] #gdp_agriculture


    gdp_agriculture_intercept[i] = gdp_agriculture$coefficients[1]  #intercept
    gdp_agriculture_slope1[i,] = gdp_agriculture$coefficients[2]  #temperature
    gdp_agriculture_slope2[i,] = gdp_agriculture$coefficients[3]  #

    #new_data <- data.frame(
    #  v2 = temperature_test_unlist,
    #  v3 = gdpagriculture_test_unlist,
    #  v4 = precipitation_test_unlist
    #)

    #realcases[i] = outcome_test_unlist

    #predictions2[i] <- predict(outcome, newdata = new_data, type ='response')

    #modellfit[i,] = extractAIC(outcome, scale=0,k = 2)


  }
}

#modellfit[is.na(modellfit)] <- 0
#modellfit[modellfit==-Inf] <- 0




Directeffect  = outcome_slope1[,1]   #temperature

Indirecteffect = outcome_slope2[,1]*gdp_agriculture_slope1[,1]     #gdp*temperature



# Put state names in order to be plotted/listed

#Direct effect
#The order of the numerical results is different from the order of the states in the shape file (.shp)
#Therefore it is needed to do this re-listing of the results so that to be able to plot the numerical results

Directeffect_plot = vector(,37);

Directeffect_plot[1] =  0   #Andaman & Nicobar

Directeffect_plot[2] =  Directeffect[5]  #Chandigarh

Directeffect_plot[3] =  Directeffect[7]  #Daman and Diu and Dadra and Nagar Haveli

Directeffect_plot[4] =  Directeffect[33]  #Delhi

Directeffect_plot[5] = Directeffect[10]   #Haryana

Directeffect_plot[6] =   Directeffect[13] #Jharkhand

Directeffect_plot[7] =   Directeffect[14] #Karnataka

Directeffect_plot[8] =  Directeffect[15]  #Kerala

Directeffect_plot[9] =  Directeffect[16]  #Lakshadweep

Directeffect_plot[10] = Directeffect[17]  #Madhya Pradesh

Directeffect_plot[11] = Directeffect[18]  #Maharashtra

Directeffect_plot[12] = Directeffect[23]  #Odisha

Directeffect_plot[13] = Directeffect[34]  #Puducherry

Directeffect_plot[14] = Directeffect[27]  #Tamilnadu

Directeffect_plot[15] = Directeffect[6]  #Chhattishgarh

Directeffect_plot[16] = Directeffect[28]  #Telengana

Directeffect_plot[17] = Directeffect[1]  #Andhra Pradesh

Directeffect_plot[18] = Directeffect[34]  #Puducherry

Directeffect_plot[19] = Directeffect[8]  #Goa

Directeffect_plot[20] = Directeffect[11]  #Himachal Pradesh

Directeffect_plot[21] = Directeffect[24]  #Punjab

Directeffect_plot[22] = Directeffect[25]  #Rajasthan

Directeffect_plot[23] = Directeffect[9]  #Gujarat

Directeffect_plot[24] = Directeffect[31]  #Uttarakhand

Directeffect_plot[25] =  Directeffect[30] #Uttar Pradesh

Directeffect_plot[26] =  Directeffect[26] #Sikkim

Directeffect_plot[27] =  Directeffect[3] #Assam

Directeffect_plot[28] =  Directeffect[2] #Arunachal Pradesh

Directeffect_plot[29] =  Directeffect[22] #Nagaland

Directeffect_plot[30] =  Directeffect[19] #Manipur

Directeffect_plot[31] =  Directeffect[21] #Mizoram

Directeffect_plot[32] = Directeffect[29]  #Tripura

Directeffect_plot[33] = Directeffect[20]  #Meghalaya

Directeffect_plot[34] = Directeffect[32]  #West Bengal

Directeffect_plot[35] = Directeffect[4]  #Bihar

Directeffect_plot[36] = 0 #Directeffect[]  #Ladakh

Directeffect_plot[37] =  Directeffect[12] #Jammu and Kashmir




#Indirect effect


Indirecteffect_plot = vector(,37);

Indirecteffect_plot[1] =  0   #Andaman & Nicobar

Indirecteffect_plot[2] =  Indirecteffect[5]  #Chandigarh

Indirecteffect_plot[3] =  Indirecteffect[7]  #Daman and Diu and Dadra and Nagar Haveli

Indirecteffect_plot[4] =  Indirecteffect[33]  #Delhi

Indirecteffect_plot[5] = Indirecteffect[10]   #Haryana

Indirecteffect_plot[6] =   Indirecteffect[13] #Jharkhand

Indirecteffect_plot[7] =   Indirecteffect[14] #Karnataka

Indirecteffect_plot[8] =  Indirecteffect[15]  #Kerala

Indirecteffect_plot[9] =  Indirecteffect[16]  #Lakshadweep

Indirecteffect_plot[10] = Indirecteffect[17]  #Madhya Pradesh

Indirecteffect_plot[11] = Indirecteffect[18]  #Maharashtra

Indirecteffect_plot[12] = Indirecteffect[23]  #Odisha

Indirecteffect_plot[13] = Indirecteffect[34]  #Puducherry

Indirecteffect_plot[14] = Indirecteffect[27]  #Tamilnadu

Indirecteffect_plot[15] = Indirecteffect[6]  #Chhattishgarh

Indirecteffect_plot[16] = Indirecteffect[28]  #Telengana

Indirecteffect_plot[17] = Indirecteffect[1]  #Andhra Pradesh

Indirecteffect_plot[18] = Indirecteffect[34]  #Puducherry

Indirecteffect_plot[19] = Indirecteffect[8]  #Goa

Indirecteffect_plot[20] = Indirecteffect[11]  #Himachal Pradesh

Indirecteffect_plot[21] = Indirecteffect[24]  #Punjab

Indirecteffect_plot[22] = Indirecteffect[25]  #Rajasthan

Indirecteffect_plot[23] = Indirecteffect[9]  #Gujarat

Indirecteffect_plot[24] = Indirecteffect[31]  #Uttarakhand

Indirecteffect_plot[25] =  Indirecteffect[30] #Uttar Pradesh

Indirecteffect_plot[26] =  Indirecteffect[26] #Sikkim

Indirecteffect_plot[27] =  Indirecteffect[3] #Assam

Indirecteffect_plot[28] =  Indirecteffect[2] #Arunachal Pradesh

Indirecteffect_plot[29] =  Indirecteffect[22] #Nagaland

Indirecteffect_plot[30] =  Indirecteffect[19] #Manipur

Indirecteffect_plot[31] =  Indirecteffect[21] #Mizoram

Indirecteffect_plot[32] = Indirecteffect[29]  #Tripura

Indirecteffect_plot[33] = Indirecteffect[20]  #Meghalaya

Indirecteffect_plot[34] = Indirecteffect[32]  #West Bengal

Indirecteffect_plot[35] = Indirecteffect[4]  #Bihar

Indirecteffect_plot[36] = 0 #Indirecteffectct[]  #Ladakh

Indirecteffect_plot[37] =  Indirecteffect[12] #Jammu and Kashmir


print(Directeffect_plot)

print(Indirecteffect_plot)



