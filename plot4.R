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

makePlot4 <- function( pdata = NULL ) {
  if ( is.null(pdata) ) {
    pdata <- getData()
  }
  png("plot4.png")
  par( mfrow = c(2,2))
  with( pdata, {
    #Plot number 2
    plot( DateTime, Global_active_power, type = 'l', ylab = 'Global Active Power', xlab = NA)
    #Voltage line graph
    plot( DateTime, Voltage, type = 'l', ylab = 'Voltage', xlab = 'datetime')
    #Plot number 3
    plot( DateTime, Sub_metering_1, xlab = NA, ylab = 'Energy sub metering', type = 'l')
    lines( DateTime, Sub_metering_2, type = 'l', col = 'red')
    lines( DateTime, Sub_metering_3, type = 'l', col = 'blue')
    legend( 'topright', c('Sub_metering_1', 'Sub_metering_2', 'Sub_metering_3'),
      lty = c(1,1,1),
      lwd = c(1.5, 1.5, 1.5 ),
      col = c('black', 'blue', 'red'))
    plot( DateTime, Global_reactive_power, type = 'l', xlab = 'datetime')
    })
    dev.off()
    pdata
}
