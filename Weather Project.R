
#The data are weather observations for the month of April for Amherst, MA from 1900 to 2015. 

#http://weather-warehouse.com/WeatherHistory/PastWeatherData_Amherst_Amherst_MA_April.html

# Read in the data. This is a tab-separated values file, with several columns
# Removed NA values from the vector 
# Created a time series object based on the total precipitation vector. The frequency is 1, and the series starts at 1900, 1. 

weather <- read.table(file = "/Users/ryancoulter/Desktop/Data for R/AmherstWeatherApril.txt", sep = "\t", header = TRUE)
rain <- weather$TotalPrecip
na.omit(rain)
rain.ts <- ts(rain, frequency = 1, start = c(1900,1))
na.omit(rain.ts)
plot(rain.ts)

#The trend is fairly horizontal with the cycle being large bounces of up and down. There is a little noise  in between the large jumps up and down. It would be categorized as being best fit by an additive model because the amplitude stays about the same wiht only a couple areas of noteable difference. It is not stationary because the jumps up and down seem to last the same amount of times each year. 


#Exponential smoothing. 

Used the HoltWinter method 

hw.pred <- HoltWinters(x = na.omit(rain.ts),beta = FALSE, gamma = FALSE)

hw.pred

#Plotted the fitted model (shows as a red line on the series), and printed the sum of squared errors (SSE). 

plot(hw.pred)
hw.pred$SSE


#Made predictions for "out of sample" data using the "forecast" library.
#Displayed the predictions to the console 

#install.packages("forecast")
library("forecast")
hw.pred2 <- forecast:::forecast.HoltWinters(hw.pred, h = 5)
hw.pred2
forecast:::plot.forecast(hw.pred2)



#Plotted the residuals to look for any patterns

plot.ts(hw.pred2$residuals)

#It has a pattern of bouncing up and down with a relatively even amount of possitive and negative errors. 


#Plotted a histogram of the residuals.

hist(hw.pred2$residuals)


