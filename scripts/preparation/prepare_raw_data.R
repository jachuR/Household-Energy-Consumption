
# libraries ---------------------------------------------------------------


if (require(pacman) == FALSE) {
  install.packages("pacman")
}
pacman::p_load(LiblineaR, mlbench, caret, lattice, stringr, stringi, 
                dplyr, anchors, MASS, rgenoud, tidyselect, shinydashboard, 
                shiny, tidyr, BBmisc, forecast, imputeTS, gdata, 
                magclass, grid, urca, tseries, fracdiff, corrplot, 
                PerformanceAnalytics, xts, zoo, weathermetrics, plotly, 
                lubridate, data.table, readr, RMySQL, DBI, ggfortify, 
                ggplot2, stats, graphics, grDevices, utils, datasets, 
                methods, base, stringr,OpenStreetMap)


# global options ----------------------------------------------------------

options(lubridate.week.start = 1)
Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre1.8.0_101') # for 64-bit version 

# load data ---------------------------------------------------------------

###SQL
## Create a database connection  ####
con = dbConnect(MySQL(), user='deepAnalytics', password='Sqltask1234!', dbname='dataanalytics2018', host='data-analytics-2018.cbrosir2cswx.us-east-1.rds.amazonaws.com')
dbListFields(con,"yr_2006") #list of column names

#data for years 06-10
E06<- dbGetQuery(con, "SELECT Date, Time, Global_active_power, Sub_metering_1, Sub_metering_2, Sub_metering_3 FROM yr_2006")
E07<- dbGetQuery(con, "SELECT Date, Time, Global_active_power, Sub_metering_1, Sub_metering_2, Sub_metering_3 FROM yr_2007")
E08<- dbGetQuery(con, "SELECT Date, Time, Global_active_power, Sub_metering_1, Sub_metering_2, Sub_metering_3 FROM yr_2008")
E09<- dbGetQuery(con, "SELECT Date, Time, Global_active_power, Sub_metering_1, Sub_metering_2, Sub_metering_3 FROM yr_2009")
E10<- dbGetQuery(con, "SELECT Date, Time, Global_active_power, Sub_metering_1, Sub_metering_2, Sub_metering_3 FROM yr_2010")


# Exploring new data frames -----------------------------------------------

list_tables <-  list(E06,E07,E08,E09,E10)

#structure
k <- 06
for (i in list_tables) {
  cat("\nE",k," structure:","\n",sep = "")
  print(str(i))
  k = k+ 1
}

#summary
k <- 06  
for (i in list_tables) {
  cat("\nE",k," summary","\n",sep = "")
  print(summary(i))
  k = k+ 1
}

#first 5 rows
k <- 06
for (i in list_tables) {
  cat("\nE",k,"\n",sep = "")
  print(head(i))
  k = k+ 1
}

#last 5 rows
k <- 06
for (i in list_tables) {
  cat("\nE",k,"\n",sep = "")
  print(tail(i))
  k = k+ 1
}



# Combine tables into one dataframe ---------------------------------------

allYears <- bind_rows(E06,E07,E08,E09) # E10 we use as a test data
str(allYears)
summary(allYears)
head(allYears)
tail(allYears)



# DataTime ----------------------------------------------------------------

## Combine Date and Time attribute values in a new attribute column
allYears <-cbind(DateTime=paste(allYears$Date,allYears$Time),allYears, stringsAsFactors=FALSE) # (DateTime=) - new column name
head(allYears)
## Convert DateTime from string to POSIXct 
allYears$DateTime <- as.POSIXct(allYears$DateTime, "%Y-%m-%d %H:%M:%S", tz = "GMT") # najpierw jako time zone podajemy tą w której dane były zbierane, wygląda że w naszym przypadku GMT (nie ma zmian czasu na letni i zimowy)
## Add the time zone
attr(allYears$DateTime, "tzone") <- "Europe/Paris" #tu dopiero konwertujemy na czas na który chcemy przeliczyć (urządzenia były w GMT ale ludzie funkcjonowali w CET)
allYears$Date <- as.character(date(allYears$DateTime))
allYears$Time <- as.character(data.table::as.ITime(allYears$DateTime))
## Inspect the data types
str(allYears)
head(allYears)
tail(allYears)
allYears <- allYears %>%  filter(DateTime >="2007-01-01" & DateTime < "2010-01-01") #odrzucenie całego 2006 i jednej godziny która wskoczyła do 2010
summary(allYears)

## Lubridate ####
# Create "year" attribute with lubridate
allYears$year <- year(allYears$DateTime)
allYears$month <- month(allYears$DateTime)
allYears$day <- day(allYears$DateTime)
allYears$hour <- hour(allYears$DateTime)
allYears$minute <- minute(allYears$DateTime)
allYears$hq <- round_date(allYears$DateTime, "15 minutes")  #zaokrąglenie formatu do 15min
allYears$quarter <- quarters(allYears$DateTime)
allYears$weekday <- weekdays(allYears$DateTime)
allYears$week <- week(allYears$DateTime)
allYears <- allYears %>%                    #zminana nazw kolumn (najpierw nowa nazwa potem stara)
  rename(
    sm1_kitchen = Sub_metering_1,
    sm2_laundry_room = Sub_metering_2,
    sm3_water_AC = Sub_metering_3
  )

allYears$hq <- round_date(allYears$DateTime, "15 minutes")  #zaokrąglenie formatu do 15min
# allYears$hq <- as.character(format(round_date(allYears$DateTime, "15 minutes"),"%H:%M")) # zaokrąglenie do 15min i użycie tylko godin i minut

rm(con)
rm(E06,E07,E08,E09,E10,i,list_tables,k)

##total energy use
allYears <-mutate(allYears, total_sub = sm1_kitchen + sm2_laundry_room + sm3_water_AC)
allYears <- mutate(allYears, total_rest = Global_active_power*1000/60 - sm1_kitchen - sm2_laundry_room - sm3_water_AC)
allYears <-  allYears[c("DateTime","sm1_kitchen","sm2_laundry_room", "sm3_water_AC","total_sub", "total_rest",
                        "Date","Time","year","quarter","month","week","weekday","day","hour","hq",
                        "minute" )]

##additional date formats
# %Y-%m column
allYears <- mutate(allYears, year_month = format(DateTime, "%Y-%m"))
# %Y-%m-d%-h%column
allYears <- mutate(allYears, Y_m_d_h = format(DateTime, "%Y-%m-%d %H"))

saveRDS(allYears, file = "allYears.rds")


