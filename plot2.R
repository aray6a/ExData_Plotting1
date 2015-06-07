library(dplyr)
library(lubridate)

#Read houshelod_power_consumption.txt and returned cleaned data
getData <- function() {
  power <- read.csv( "household_power_consumption.txt", sep = ";", nrows = 2075259, colClasses = 'character' )
  power$Date <- as.Date( power$Date, format = "%d/%m/%Y" )
  power <- power %>% filter( power$Date >= '2007-02-01' & power$Date <= '2007-02-02')
  power$Global_active_power <- as.numeric( power$Global_active_power )
  power$Global_reactive_power <- as.numeric( power$Global_reactive_power )
  power$Voltage <- as.numeric( power$Voltage )
  power$Sub_metering_1 <- as.numeric( power$Sub_metering_1 )
  power$Sub_metering_2 <- as.numeric( power$Sub_metering_2 )
  power$Sub_metering_3 <- as.numeric( power$Sub_metering_3 )
  power$DateTime <- paste( as.character(power$Date), as.character( power$Time ) )
  power$DateTime <- parse_date_time(power$DateTime, "%Y-%m-%d %H:%M:%S")
  power
}

makePlot2 <- function( power = NULL ) {
  if ( is.null(power) ) {
    power <- getData()
  }
  power <- getData()
  power$DateTime <- paste( as.character(power$Date), as.character( power$Time ) )
  power$DateTime <- parse_date_time(power$DateTime, "%Y-%m-%d %H:%M:%S")
  png('plot2.png')
  plot( power$DateTime, power$Global_active_power, type = 'l', ylab = 'Global Active Power (kilowatts)', xlab = NA)
  dev.off()
  power
}
