# webscraper for Q1 data 
# from http://www.boxofficemojo.com/quarterly/?chart=byquarter&quarter=Q1

# function that allows reading in data of the form $xxx,xxx,xxx.xx
setClass("AccountingNumber")
setAs("character", "AccountingNumber",
      function(from) as.numeric(gsub(",", "", gsub("[:$:]", "", from))))
      # data from webpage
library(RCurl)
library(XML)
# Q1 box office url
q1boxoffice.url <- paste("https://www.boxofficemojo.com/quarterly/?chart=byquarter&quarter=Q1")
# read webpage and store in memory
q1boxoffice.webpage <- htmlParse(getURL(q1boxoffice.url))
# create R dataset from webpage contents
q1boxoffice <- readHTMLTable(q1boxoffice.webpage,
                           header = TRUE, which = 4,
                           colClasses = c("numeric", "AccountingNumber", "Percent", "numeric",
                                        "AccountingNumber", "Percent", "character",
                                        "AccountingNumber", "Percent"))
# keep only year and gross
q1boxoffice <- q1boxoffice[, 1:2]
# change variable name so it doesn't have a space
names(q1boxoffice) <- c("year", "gross")

# webscraper for q2 data 
# from http://www.boxofficemojo.com/quarterly/?chart=byquarter&quarter=Q2

# function that allows reading in data of the form $xxx,xxx,xxx.xx
setClass("AccountingNumber")
setAs("character", "AccountingNumber",
      function(from) as.numeric(gsub(",", "", gsub("[:$:]", "", from))))
      # data from webpage

# q2 box office url
q2boxoffice.url <- paste("https://www.boxofficemojo.com/quarterly/?chart=byquarter&quarter=Q2")
# read webpage and store in memory
q2boxoffice.webpage <- htmlParse(getURL(q2boxoffice.url))
# create R dataset from webpage contents
q2boxoffice <- readHTMLTable(q2boxoffice.webpage,
                           header = TRUE, which = 4,
                           colClasses = c("numeric", "AccountingNumber", "Percent", "numeric",
                                        "AccountingNumber", "Percent", "character",
                                        "AccountingNumber", "Percent"))
# keep only year and gross
q2boxoffice <- q2boxoffice[, 1:2]
# change variable name so it doesn't have a space
names(q2boxoffice) <- c("year", "gross")

# webscraper for q3 data 
# from http://www.boxofficemojo.com/quarterly/?chart=byquarter&quarter=Q3

# function that allows reading in data of the form $xxx,xxx,xxx.xx
setClass("AccountingNumber")
setAs("character", "AccountingNumber",
      function(from) as.numeric(gsub(",", "", gsub("[:$:]", "", from))))
      # data from webpage

# q3 box office url
q3boxoffice.url <- paste("https://www.boxofficemojo.com/quarterly/?chart=byquarter&quarter=Q3")
# read webpage and store in memory
q3boxoffice.webpage <- htmlParse(getURL(q3boxoffice.url))
# create R dataset from webpage contents
q3boxoffice <- readHTMLTable(q3boxoffice.webpage,
                           header = TRUE, which = 4,
                           colClasses = c("numeric", "AccountingNumber", "Percent", "numeric",
                                        "AccountingNumber", "Percent", "character",
                                        "AccountingNumber", "Percent"))
# keep only year and gross
q3boxoffice <- q3boxoffice[, 1:2]
# change variable name so it doesn't have a space
names(q3boxoffice) <- c("year", "gross")

# webscraper for q4 data 
# from http://www.boxofficemojo.com/quarterly/?chart=byquarter&quarter=Q4

# function that allows reading in data of the form $xxx,xxx,xxx.xx
setClass("AccountingNumber")
setAs("character", "AccountingNumber",
      function(from) as.numeric(gsub(",", "", gsub("[:$:]", "", from))))
      # data from webpage

# q4 box office url
q4boxoffice.url <- paste("https://www.boxofficemojo.com/quarterly/?chart=byquarter&quarter=Q4")
# read webpage and store in memory
q4boxoffice.webpage <- htmlParse(getURL(q4boxoffice.url))
# create R dataset from webpage contents
q4boxoffice <- readHTMLTable(q4boxoffice.webpage,
                           header = TRUE, which = 4,
                           colClasses = c("numeric", "AccountingNumber", "Percent", "numeric",
                                        "AccountingNumber", "Percent", "character",
                                        "AccountingNumber", "Percent"))
# keep only year and gross
q4boxoffice <- q4boxoffice[, 1:2]
# change variable name so it doesn't have a space
names(q4boxoffice) <- c("year", "gross")

#add 'qtr' variable for each dataframe
q1boxoffice$qtr <- 1
q2boxoffice$qtr <- 2
q3boxoffice$qtr <- 3
q4boxoffice$qtr <- 4

#check each dataframe
str(q1boxoffice)
str(q2boxoffice)
str(q3boxoffice)
str(q4boxoffice)

#combine into one dataframe
boxoffice <- rbind(q1boxoffice, q2boxoffice, q3boxoffice, q4boxoffice)

#sort dataframe by year and qtr
boxoffice <- boxoffice[order(boxoffice$year, boxoffice$qtr),]

#check data
boxoffice

#remove row for current data
boxoffice <- boxoffice[-149,]

#check data
tail(boxoffice, n = 10)

#plot time series data
#attach ggplot library
library(ggplot2)
ggplot(boxoffice, aes(x = year, y = gross)) + geom_line() + geom_point()

#create separate plots by quarter to separate data visually

q1plot <- ggplot(subset(boxoffice, qtr == 1), aes(x = year, y = gross, color = qtr)) + geom_line()

q1plot + layer(data = subset(boxoffice, qtr == 2), geom = "line", stat = "identity", position = "identity") +
    layer(data = subset(boxoffice, qtr == 3), geom = "line", stat = "identity", position = "identity") +
    layer(data = subset(boxoffice, qtr == 4), geom = "line", stat = "identity", position = "identity")

#ARIMA Model assumptions: additive and constant mean change
#Data already appears additive
#To fulfill constant mean change, will filter to start in 1998

boxoffice_filter <- subset(boxoffice, year > 1997)
#check data
boxoffice_filter

#attach library for ARIMA model
library(astsa)

boxofficeARIMA <- sarima(boxoffice_filter$gross, 1, 1, 1, 1, 1, 1, 4)

#show estimates in table
boxofficeARIMA$ttable

boxoffice_future <- sarima.for(boxoffice_filter$gross, n.ahead = 12, 1, 1, 1, 1, 1, 1, 4)

#eyeball prediction of predictions to see if they look reasonable
boxoffice_future$pred

boxoffice_future_L <- boxoffice_future$pred - qnorm(0.975) * boxoffice_future$se
boxoffice_future_U <- boxoffice_future$pred + qnorm(0.975) * boxoffice_future$se

#combine predictions and 95% prediction intervals
boxoffice_future_tbl <- cbind(boxoffice_future$pred, boxoffice_future_L, boxoffice_future_U)

#look at table
boxoffice_future_tbl

tail(boxoffice_filter, n = 10)

#create final plot

plot(boxoffice$gross, type = "b", xlim = c(0, 160), ylim = c(200, 4000), axes = FALSE,
     xlab = "Year (by quarter)",
     ylab = "Box Office Revenue (in millions $)")

#add forecasst and prediction intervals

lines(149:160, boxoffice_future$pred, col = "red", type = "b", pch = 19)

lines(149:160, boxoffice_future_L, col = "gray", lwd = 2)

lines(149:160, boxoffice_future_U, col = "gray", lwd = 2)

#include axes
axis(1, at = c(0, 40, 80, 120, 160), labels = c(1982, 1992, 2002, 2012, 2022))
axis(2, at = c(0,1000,2000,3000,4000), labels = c(0,1000,2000,3000,4000))