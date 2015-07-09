plot4 <- function (output = "screen") {

loadData <- function () {
# If the data doesn't already exist in the global environment 
# 1.Load all of the data into a temporary data.table
# 2. subset for only relevant rows (2 days in February)
# 3. Convert numerics to numbers (can't do this early because of the "?" characters instead of NA in source ) 
# 4. Removes separe Date and Time columns, which have been replaced by a combined column
if (!exists("powerData")) {
  fullData <-fread("household_power_consumption.txt",sep=";",na.strings = c("?"),
                   colClasses = rep("character",9)) 
  powerData<<-fullData[Date=="1/2/2007"|Date=="2/2/2007",]
  powerData<<-mutate(powerData,dateTime= as.POSIXlt(paste(powerData$Date,powerData$Time), format="%d/%m/%Y %H:%M:%S"),
         Global_active_power= as.numeric(Global_active_power),
         Global_reactive_power= as.numeric(Global_reactive_power),
         Voltage= as.numeric(Voltage),
         Global_intensity= as.numeric(Global_intensity),
         Sub_metering_1= as.numeric(Sub_metering_1),
         Sub_metering_2= as.numeric(Sub_metering_2),
         Sub_metering_3= as.numeric(Sub_metering_3))
  powerData$Date<<-NULL
  powerData$Time<<-NULL}
}

plot1 <- function()  {
# Original plot2 function.  

  plot(powerData$dateTime,powerData$Global_active_power,type="l", 
       ylab="Global Active Power(kilowatts)",
       xlab=" ")
}

plot2 <- function () {
  # Original plot 3  
  # Define plot parameters 
  legendText<-c("Sub_metering_1","Sub_metering_2","Sub_metering_3")
  plotColors=c("black","red","blue")
  
  with(powerData,plot(dateTime,Sub_metering_1,type="l", col=plotColors[1],
                      ylab="Energy sub metering",
                      xlab=" "))
  with(powerData,points(dateTime,Sub_metering_2,type="l", col=plotColors[2]))
  with(powerData,points(dateTime,Sub_metering_3,type="l", col=plotColors[3]))
  legend(x="topright",legend = legendText, text.width=100,cex=0.5,
         y.intersp = 0.5, 
         col = plotColors,  bty = "o", lwd=2)
}

# library(data.table)
# library(plyr)
  loadData()

  # After data is loaded, simply write the plot 
#   png(filename = "plot3.png",
#       width = 480, height = 480, units = "px", 
#        bg = "white")
#    
  # Define plot parameters 
  par(mar=c(5,5,1,1),mfcol=c(2,2),oma=c(0,0,2,0))
 
  plot1()
  plot2()
  
  # Plot 3 Voltage over time
  plot(powerData$dateTime,powerData$Voltage,type="l", 
       ylab="Voltage",
       xlab="datetime")
  
  # Plot 4 Voltage over time
  plot(powerData$dateTime,powerData$Global_reactive_power,type="l", 
       ylab="Global_reactive_power",
       xlab="datetime")
  
  # dev.off()
}