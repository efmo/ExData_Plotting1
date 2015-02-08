plot4 <- function() {
    
    HPC3rows <- read.table("household_power_consumption.txt", header = TRUE, sep = ";", nrows = 3,
                           na.strings = "?", quote = "")
    classes <- sapply(HPC3rows, class)
    
    householdPowerConsumption <- read.table("household_power_consumption.txt", header = TRUE, sep = ";",
                                            na.strings = "?", quote = "", colClasses = classes)
    
    householdDateTimes <- paste(householdPowerConsumption$Date, householdPowerConsumption$Time)
    householdDateTimes <- strptime(householdDateTimes, "%d/%m/%Y %H:%M:%S")
    
    dayBegin <- as.Date("2007-02-01")
    dayEnd <- as.Date("2007-02-02")
    
    daysOfInterest <- which(as.Date(householdDateTimes) == dayBegin | as.Date(householdDateTimes) == dayEnd)
    householdPowerConsumption <- householdPowerConsumption[daysOfInterest,]
    householdPowerConsumption$Date <- NULL
    householdPowerConsumption$Time <- NULL
    householdPowerConsumption$DateTime <- householdDateTimes[daysOfInterest]
    
    days <- c("Thu", "Fri", "Sat")
    
    png("plot4.png")
    par("mfrow" = c(2,2))
    par("mar" = c(3,4,2,2))
    plot(householdPowerConsumption$Global_active_power, type = "l", ylab = "Global Active Power (kilowatts)", xaxt = "n")
    axis(1, at=c(1,length(daysOfInterest)/2,length(daysOfInterest)), labels = days)
    
    plot(householdPowerConsumption$Voltage, type = "l", ylab = "Voltage", xaxt = "n")
    axis(1, at=c(1,length(daysOfInterest)/2,length(daysOfInterest)), labels = days)
    
    plot(householdPowerConsumption$Sub_metering_1, type = "l", ylab = "Energy Sub metering", xaxt = "n", col = "BLACK")
    lines(householdPowerConsumption$Sub_metering_2, col = "RED")
    lines(householdPowerConsumption$Sub_metering_3, col = "BLUE")
    legend("topright", c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), lty = c(1,1,1), col = c("BLACK", "RED", "BLUE"), bty = "n")
    axis(1, at=c(1,1440,2880), labels = days)
    
    plot(householdPowerConsumption$Global_reactive_power, type = "l", ylab = "Global Reactive Power", xaxt = "n")
    axis(1, at=c(1,length(daysOfInterest)/2,length(daysOfInterest)), labels = days)
    
    dev.off()
}


