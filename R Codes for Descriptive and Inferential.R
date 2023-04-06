library(shiny)
library(ggplot2)
library(dplyr)
library(scales)
library(shinydashboard)
library(tidyverse)
library(broom)
library(vctrs)
library(rlang)
library(shinythemes)
library(corrplot)
library(readr)
library(lubridate)
library(corrr)
library(zoo)
library(cowplot)
library(forcats)
library(lme4)
library(AICcmodavg)
library(knitr)
library(forecast)
library(tseries)

HDB_resale_price_index <- read.csv("C:/Users/Myat Noe/OneDrive - Singapore Management University/ASAR Group Project/Files for Report Submission/housing-and-development-board-resale-price-index-1q2009-100-quarterly.csv")
HDB_resale_main_data <- read.csv("C:/Users/Myat Noe/OneDrive - Singapore Management University/ASAR Group Project/Files for Report Submission/all_data_cleaned_final.csv")

######Price Index of HDB Resale Flats######

ggplot(data=HDB_resale_price_index, aes(x=quarter, y=index, group=1)) +
  geom_line(color="blue") + geom_point() + labs(title = "Price Index of HDB Resale Flats (2006 - 2022)", x = "Year", y = "Index") + theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust=1))

######Median HDB resale price per square meter######

ggplot(HDB_resale_main_data, aes(x = Year_Extract, y = PSM, color = flat_type)) + geom_line(stat = "summary", fun = "median") + scale_x_continuous(breaks=seq(2006,2022,1)) + labs(title = "HDB Resale Price Median (sqm) (2006 - 2022)", x = "Year", y = "Price per squared meter (SGD)") 

######Total flats sold by flat types######
# Group the data by year and flat type, and count the number of flats sold for each flat type in each year
totals_by_year_flat <- HDB_resale_main_data %>%
  group_by(Year_Extract,flat_type) %>%
  summarize(total_flats_sold = n())
#visualizing the table
ggplot(totals_by_year_flat, aes(x = Year_Extract, y = total_flats_sold, fill = flat_type)) +geom_col(colour = "black", position = "dodge") + scale_fill_brewer(palette = "Pastel1")
  

######Price per square meter by flat type boxplot######

HDB_resale_main_data_2020_2022 <- HDB_resale_main_data %>% filter(Year_Extract > 2019)
ggplot(HDB_resale_main_data_2020_2022, aes(x=flat_type,y = PSM, color=flat_type)) + 
  geom_boxplot() + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + labs(title = "HDB Resale Price (sqm) by Flat Type Boxplot (2020 - 2022)", x = "Flat Type", y = "Price per squared meter (SGD)")

######SORA Rates######
SORA <- c(3.14864705882353,	3.0318,	3.18011304347826,	3.07036315789474,	3.02614761904762,	3.23821818181818,	3.21810476190476,	3.19704545454545,	3.28057142857143,	3.3808619047619,	3.25466363636364,	3.148375,	3.12555238095238,	3.07462777777778,	2.48990909090909,	2.37075,	2.13978095238095,	2.16961904761905,	2.24765454545455,	2.30991818181818,	2.22236,	2.17713913043478,	1.71520952380952,	0.897805263157895,	1.18714090909091,	1.28870526315789,	0.762715,	0.514977272727273,	0.660525,	0.298666666666667,	0.358773913043478,	0.576528571428572,	1.10729545454545,	0.592990476190476,	0.40581,	0.380923809523809,	0.115889473684211,	0.176705,	0.291595454545454,	0.144419047619048,	0.088325,	0.146654545454545,	0.12904347826087,	0.269585,	0.220704761904762,	0.270945454545455,	0.27057,	0.107940909090909,	0.134145,	0.137161111111111,	0.104608695652174,	0.1082,	0.09252,	0.123281818181818,	0.111663636363636,	0.107247619047619,	0.115861904761905,	0.0799333333333333,	0.085615,	0.065995652173913,	0.0627238095238095,	0.0716833333333333,	0.0283478260869565,	0.07047,	0.067785,	0.0486090909090909,	0.0402238095238095,	0.0504095238095238,	0.0527954545454545,	0.04983,	0.0684952380952381,	0.0470666666666667,	0.0622526315789474,	0.0389666666666667,	0.0440363636363636,	0.02967,	0.0565863636363636,	0.0516,	0.0528272727272727,	0.0761190476190476,	0.083765,	0.0744227272727273,	0.0542904761904762,	0.059625,	0.0385409090909091,	0.0520888888888889,	0.048455,	0.0373,	0.0625142857142857,	0.039125,	0.0458608695652174,	0.044655,	0.0289619047619048,	0.0715272727272728,	0.0709333333333333,	0.0491428571428571,	0.0454142857142857,	0.03758,	0.0761857142857143,	0.0669571428571429,	0.083935,	0.0655761904761905,	0.0565727272727273,	0.0806190476190476,	0.0929909090909091,	0.0722952380952381,	0.12452,	0.143095454545455,	0.137404761904762,	0.28245,	0.250245454545455,	0.243852380952381,	0.21539,	0.219085714285714,	0.254886363636364,	0.278415789473684,	0.183025,	0.249959090909091,	0.423775,	0.278013636363636,	0.36581,	0.258210526315789,	0.299627272727273,	0.335695238095238,	0.215304761904762,	0.0966136363636364,	0.276355,	0.0975318181818182,	0.078747619047619,	0.0807571428571429,	0.0967909090909091,	0.215457142857143,	0.142185,	0.312525,	0.262839130434783,	0.419931578947368,	0.486785714285714,	0.476980952380952,	0.774671428571429,	0.750990909090909,	0.61802,	0.838880952380952,	0.693936363636364,	0.925945,	0.349459090909091,	1.00117894736842,	0.676690476190476,	0.638090476190476,	1.07528571428571,	0.884725,	1.17803181818182,	1.41991904761905,	1.00858,	1.1766,	1.4735,	1.60945,	1.45681818181818,	1.66298888888889,	1.41697619047619,	1.69737142857143,	1.82628571428571,	1.54864210526316,	1.55037391304348,	1.589695,	1.4066,	1.34105909090909,	1.14712857142857,	1.21406666666667,	1.09809523809524,	0.999735,	0.501277272727273,	0.257566666666667,	0.0493444444444444,	0.0945590909090909,	0.0892047619047619,	0.0928,	0.0983363636363637,	0.117377272727273,	0.109266666666667,	0.140663636363636,	0.181145,	0.176973684210526,	0.211339130434783,	0.116585714285714,	0.143431578947368,	0.125631818181818,	0.104333333333333,	0.13287619047619,	0.133590909090909,	0.174090476190476,	0.15867619047619,	0.210686956521739,	0.208257142857143,	0.279566666666667,	0.285473913043478,	0.30429,	0.886984210526316,	0.961854545454545,	1.783005,	1.88455909090909,	2.21493181818182,	3.144485,	3.19479090909091,	2.61265714285714)
plot(SORA)

SORA_ts <- ts(SORA,start=c(2006,1),frequency=12)
plot.ts(SORA_ts)

######PSM by flat types by region######

HDB_resale_main_data_2020_2022 <- HDB_resale_main_data %>% filter(Year_Extract > 2019)
ggplot(HDB_resale_main_data_2020_2022, aes(x=factor(Planning_Region_from_resale_data),y = PSM, color=factor(flat_type))) + 
  geom_boxplot() + labs(title = "HDB Resale Price (sqm) by Flat Type & Region Boxplot (2020 - 2022)", x = "Planning Region", y = "Price per squared meter (SGD)")

######PSM by region######
HDB_resale_main_data %>%
  group_by(Planning_Region_from_resale_data, Year_Extract) %>%
  summarise(median_price = mean(PSM)) %>%
  ungroup() %>%
  ggplot(aes(x = Year_Extract, y = median_price)) +
  geom_line(size = 1.5) +
  geom_point(size = 3, shape = 95) +
  facet_wrap(~ Planning_Region_from_resale_data, scales = "free_x", ncol = 2) +
  labs(x = "Year", y = "Median PSM Price") +
  theme_bw()+
  theme(legend.position = "bottom",
        panel.grid = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text = element_text(colour = "black"),
        panel.background = element_rect(fill = "white"),
        strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = "black"),
        plot.background = element_rect(fill = "lightgray"))

######Household financial ratios######
ggplot(HDB_resale_main_data, aes(x = Year_Extract)) +  geom_line(aes(y = liabilities_assest_ratio, color = "liabilities_assest_ratio")) +geom_line(aes(y = mortgage_asset_ratio, color = "mortgage_asset_ratio")) +labs(x = "Year", y = "Ratio", color = "Variable") +scale_color_manual(values = c("midnightblue", "darkgoldenrod1"))+ theme(legend.position = "bottom")

######Correlation Matrix######
# select the columns of interest
vars_of_interest <- c("Year_Extract", "PSM", "Household_Net_Worth", "Assets", 
                      "Mortgage_Loans","remaining_lease",
                      "CPF_intrest_rate","Average_of_SORA","Liabilities",
                      "floor_area_sqm","median_rent","liabilities_assest_ratio",
                      "mortgage_asset_ratio")

data_clean <- HDB_resale_main_data[complete.cases(HDB_resale_main_data[vars_of_interest]),]
# calculate the correlation matrix
correlation_matrix <- cor(data_clean[vars_of_interest])

corrplot(correlation_matrix, method="circle", type="upper", order="hclust", tl.col="black", tl.srt=45)

# calculate the correlation value of SORA vs PSM
cor(HDB_resale_main_data$Average_of_SORA,HDB_resale_main_data$PSM)

######Linear model Code######
#prep data for model 
model_dataset <- select(HDB_resale_main_data, -month, -street_name,-block,-resale_price,-Planning_Region_from_resale_data,-Average_of_SORA_Index)

# Replace missing values with 0
model_dataset$median_rent[is.na(model_dataset$median_rent)] <- 0

#check if there are any null values
colSums(is.na(model_dataset))

unique(model_dataset$town)
model_dataset$town<- factor(model_dataset$town, levels= c("ANG MO KIO", "BEDOK", "BISHAN", "BUKIT BATOK", 
                                                          "BUKIT MERAH", "BUKIT PANJANG", "BUKIT TIMAH", 
                                                          "CENTRAL AREA", "CHOA CHU KANG", "CLEMENTI", 
                                                          "GEYLANG", "HOUGANG", "JURONG EAST", "JURONG WEST", 
                                                          "KALLANG/WHAMPOA", "MARINE PARADE", "PASIR RIS", 
                                                          "PUNGGOL", "QUEENSTOWN", "SEMBAWANG", "SENGKANG", 
                                                          "SERANGOON", "TAMPINES", "TOA PAYOH", "WOODLANDS", 
                                                          "YISHUN"), 
                            labels= c('1','2','3','4','5','6','7','8','9','10','11','12','13','14',
                                      '15','16','17','18','19','20','21','22','23','24','25','26'))

unique(model_dataset$flat_type)

model_dataset$flat_type<- factor(model_dataset$flat_type, levels= c("2 ROOM", "3 ROOM", "4 ROOM", "5 ROOM", 
                                                                    "EXECUTIVE", "1 ROOM", "MULTI-GENERATION"), 
                                 labels= c('1','2','3','4','5','6','7'))

unique(model_dataset$storey_range)

model_dataset$storey_range<- factor(model_dataset$storey_range, levels= c("10 TO 12", "04 TO 06", "07 TO 09", "01 TO 03", "13 TO 15", 
                                                                          "19 TO 21", "16 TO 18", "22 TO 24", "25 TO 27", "28 TO 30", 
                                                                          "31 TO 33", "40 TO 42", "37 TO 39", "34 TO 36", "06 TO 10", 
                                                                          "01 TO 05", "11 TO 15", "16 TO 20", "21 TO 25", "26 TO 30", 
                                                                          "36 TO 40", "31 TO 35", "46 TO 48", "43 TO 45", "49 TO 51"), 
                                    labels= c('1','2','3','4','5','6','7','8','9','10','11','12','13','14',
                                              '15','16','17','18','19','20','21','22','23','24','25'))
unique(model_dataset$flat_model)
model_dataset$flat_model<- factor(model_dataset$flat_model, levels= c("Improved",  "New Generation",  "Model A",  "Adjoined flat",  
                                                                      "Simplified",  "Premium Apartment",  "Apartment",  "Maisonette",  
                                                                      "Model A2",  "Standard",  "Improved-Maisonette",  
                                                                      "Model A-Maisonette",  "Terrace",  "Multi Generation",  
                                                                      "Premium Maisonette",  "2-room",  "DBSS",  "Type S1",  "Type S2",  
                                                                      "Premium Apartment Loft",  "3Gen"), 
                                  labels= c('1','2','3','4','5','6','7','8','9','10','11','12','13','14',
                                            '15','16','17','18','19','20','21'))
#unique(model_dataset$Planning_Region_from_resale_data)
#model_dataset$Planning_Region_from_resale_data<- factor(model_dataset$Planning_Region_from_resale_data, 
# levels= c("North East Region",
#"East Region",
# "Central Region",
# "West Region",
# "North Region"), 
#labels= c('1','2','3','4','5'))
str(model_dataset)

my_data_encoded <- data.frame(model_dataset)
# Build multiple linear regression model
model <- lm(PSM ~ ., data = my_data_encoded)
summary(model)


# Build linear regression model PSM Vs sora rates
model.sora = lm(PSM ~ Average_of_SORA ,data = my_data_encoded)
# Display model summary
summary(model.sora)

######ARIMA Time Series Forecasting for Liabilities/Asset & Mortgage/Asset ratio######
#create Liabilities/Asset ratio & mortgage/ Asset ratio
HDB_resale_main_data$liabilities_asset_ratio <- HDB_resale_main_data$Liabilities/HDB_resale_main_data$Assets
HDB_resale_main_data$mortgage_asset_ratio <- HDB_resale_main_data$Mortgage_Loans/HDB_resale_main_data$Assets
str(HDB_resale_main_data)
ratios <- HDB_resale_main_data %>% distinct(month,liabilities_asset_ratio,mortgage_asset_ratio, .keep_all = TRUE) %>% select(month,liabilities_asset_ratio,mortgage_asset_ratio)
liabilities_asset_ratio <- c(ratios$liabilities_asset_ratio)
mortgage_asset_ratio <- c(ratios$mortgage_asset_ratio)
#Create time series for Liabilities/asset ratio
liabilities_asset_ratio_ts <- ts(liabilities_asset_ratio,start=c(2006,1),frequency=12)                            
liabilities_asset_ratio_decompose <- decompose(liabilities_asset_ratio_ts)
plot(liabilities_asset_ratio_decompose)
#ADF test to check whether time series is stationary
adf.test(liabilities_asset_ratio_ts)
# Determine best value of lambda
lambda <- BoxCox.lambda(liabilities_asset_ratio_ts)
#Use Auto.arima() to obtain best ARIMA model
liabilities_asset_ratio_ts_arima <- auto.arima(liabilities_asset_ratio_ts, lambda = lambda)
#Checking fit of the model
Box.test(liabilities_asset_ratio_ts_arima$residuals, type="Ljung-Box")
#Forecast for next 8 years at 95% confidence interval and plot forecasted values
forecast_liabilities_asset_ratio_ts <- forecast(liabilities_asset_ratio_ts_arima,h=96, level=c(95))
plot(forecast_liabilities_asset_ratio_ts)
#Forecast Summary
summary(forecast_liabilities_asset_ratio_ts)


#Create time series for Mortgage/ Asset ratio
mortgage_asset_ratio_ts <- ts(mortgage_asset_ratio,start=c(2006,1),frequency=12)                            
mortgage_asset_ratio_decompose <- decompose(mortgage_asset_ratio_ts)
plot(mortgage_asset_ratio_decompose)
#ADF test to check whether time series is stationary
adf.test(mortgage_asset_ratio_ts)
# Determine best value of lambda
lambda <- BoxCox.lambda(mortgage_asset_ratio_ts)
#Use Auto.arima() to obtain best ARIMA model
mortgage_asset_ratio_ts_arima <- auto.arima(mortgage_asset_ratio_ts, lambda = lambda)
#Checking fit of the model
Box.test(mortgage_asset_ratio_ts_arima$residuals, type="Ljung-Box")
#Forecast for next 8 years at 95% confidence interval and plot forecasted values
forecast_mortgage_asset_ratio_ts <- forecast(mortgage_asset_ratio_ts_arima,h=96, level=c(95))
plot(forecast_mortgage_asset_ratio_ts)
#Forecast Summary
summary(forecast_mortgage_asset_ratio_ts) 

######ARIMA Time Series Forecasting for 3/4/5 Room Flat Price######
#3 Room Flat

HDB_resale_main_data_3ROOM_median <- HDB_resale_main_data %>% filter(flat_type == "3 ROOM") %>% mutate(new_month=as.Date(month)) %>% mutate(new_month=as.numeric(as.factor(months(new_month)))) %>% group_by(Year_Extract,new_month) %>% summarise(median_resale_month=median(PSM))
#Create array of HDB 3 Room Median Price (PSM) data by month
HDB_3ROOM_median <- c(HDB_resale_main_data_3ROOM_median$median_resale_month)
#Create time series 
HDB_3ROOM_median_ts <- ts(HDB_3ROOM_median,start=c(2006,1),frequency=12)
#Decomposing time series to check trend/ seasonality & plotting decomposition
HDB_3ROOM_median_decompose <- decompose(HDB_3ROOM_median_ts)
plot(HDB_3ROOM_median_decompose)
#ADF test to check whether time series is stationary 
adf.test(HDB_3ROOM_median_ts)
# Determine best value of lambda
lambda_3room <- BoxCox.lambda(HDB_3ROOM_median_ts)
#Use Auto.arima() to obtain best ARIMA model
HDB_3ROOM_median_ts_arima <- auto.arima(HDB_3ROOM_median_ts, lambda_3room)
#Checking fit of the model
Box.test(HDB_3ROOM_median_ts_arima$residuals, type="Ljung-Box")
#Forecast for next 8 years at 95% confidence interval and plot forecasted values
forecast_HDB_3ROOM_median <- forecast(HDB_3ROOM_median_ts_arima,h=96, level=c(95))
plot(forecast_HDB_3ROOM_median)
#Forecast Summary
summary(forecast_HDB_3ROOM_median) 

#4 Room Flat
#Calculate HDB 4 Room Median Price (PSM) data by month
HDB_resale_main_data_4ROOM_median <- HDB_resale_main_data %>% filter(flat_type == "4 ROOM") %>% mutate(new_month=as.Date(month)) %>% mutate(new_month=as.numeric(as.factor(months(new_month)))) %>% group_by(Year_Extract,new_month) %>% summarise(median_resale_month=median(PSM))
#Create array of HDB 4 Room Median Price (PSM) data by month
HDB_4ROOM_median <- c(HDB_resale_main_data_4ROOM_median$median_resale_month)
#Create time series 
HDB_4ROOM_median_ts <- ts(HDB_4ROOM_median,start=c(2006,1),frequency=12)
#Decomposing time series to check trend/ seasonality & plotting decomposition
HDB_4ROOM_median_decompose <- decompose(HDB_4ROOM_median_ts)
plot(HDB_4ROOM_median_decompose)
#ADF test to check whether time series is stationary 
adf.test(HDB_4ROOM_median_ts)
# Determine best value of lambda
lambda_4room <- BoxCox.lambda(HDB_4ROOM_median_ts)
#Use Auto.arima() to obtain best ARIMA model
HDB_4ROOM_median_ts_arima <- auto.arima(HDB_4ROOM_median_ts, lambda = lambda_4room)
#Checking fit of the model
Box.test(HDB_4ROOM_median_ts_arima$residuals, type="Ljung-Box")
#Forecast for next 8 years at 95% confidence interval and plot forecasted values
forecast_HDB_4ROOM_median <- forecast(HDB_4ROOM_median_ts_arima,h=96, level=c(95))
plot(forecast_HDB_4ROOM_median)
#Forecast Summary
summary(forecast_HDB_4ROOM_median) 

#5 Room Flat
#Calculate HDB 5 Room Median Price (PSM) data by month
HDB_resale_main_data_5ROOM_median <- HDB_resale_main_data %>% filter(flat_type == "5 ROOM") %>% mutate(new_month=as.Date(month)) %>% mutate(new_month=as.numeric(as.factor(months(new_month)))) %>% group_by(Year_Extract,new_month) %>% summarise(median_resale_month=median(PSM))
#Create array of HDB 5 Room Median Price (PSM) data by month
HDB_5ROOM_median <- c(HDB_resale_main_data_5ROOM_median$median_resale_month)
#Create time series 
HDB_5ROOM_median_ts <- ts(HDB_5ROOM_median,start=c(2006,1),frequency=12)
#Decomposing time series to check trend/ seasonality & plotting decomposition
HDB_5ROOM_median_decompose <- decompose(HDB_5ROOM_median_ts)
plot(HDB_5ROOM_median_decompose)
#ADF test to check whether time series is stationary 
adf.test(HDB_5ROOM_median_ts)
# Determine best value of lambda
lambda_5room <- BoxCox.lambda(HDB_5ROOM_median_ts)
#Use Auto.arima() to obtain best ARIMA model
HDB_5ROOM_median_ts_arima <- auto.arima(HDB_5ROOM_median_ts, lambda = lambda_5room)
#Checking fit of the model
Box.test(HDB_5ROOM_median_ts_arima$residuals, type="Ljung-Box")
#Forecast for next 8 years at 95% confidence interval and plot forecasted values
forecast_HDB_5ROOM_median <- forecast(HDB_5ROOM_median_ts_arima,h=96, level=c(95))
plot(forecast_HDB_5ROOM_median)
#Forecast Summary
summary(forecast_HDB_5ROOM_median) 

