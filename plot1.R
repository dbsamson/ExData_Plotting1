plot1 <- function () {

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
  
  loadData()

  # After data is loaded, simply write the plot 
  png(filename = "plot1.png",
      width = 480, height = 480, units = "px", 
      bg = "white")
  
  par(mar=c(6,6,3,3))
  hist(powerData$Global_active_power,col="red",
       xlab="Global Active Power(kilowatts)",
       ylab="Frequency",main ="Global Active Power")
  dev.off()
}