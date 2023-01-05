# Homicides-with-firearm
This readme file will help you replicate the results in the paper titled "A dynamic factor modeling to predict homicides with firearm in the United States"

1. The extracted time series and the same series after their respective seasonal adjustment and detrending, are located in the file "time_series_original_y_sa_det-xlsx".
A code with an example to get the latter is in the "deseason.R" file.

2. A dataset as in December 2021 can be found in the file "datos_Monthly_homicides_1221.xlsx". The file to get the parameters for the model with this whole sample can be found in the file "DFM_MonthHom_guns_1factor.R".

The code is set so that an out of sample exercise can be performed when the datasets are provided. A second database as in november 2021 can be found in the file "datos_Monthly_Homicides_1121.xlsx" so the output with these two datasets can be obtained.

3. The files "Forecasts_OUT_back_tri.xlsx", "Forecasts_OUT_now_tri.xlsx", and "DataForPTTest_tri.xlsx" summarize the forecasts that are passed to get the HLN test and the PT test. Those test can be done with the HLN.R" file.
