plot2 <- function() {
    
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
    png("plot2.png")
    plot(householdPowerConsumption$Global_active_power, type = "l", ylab = "Global Active Power (kilowatts)", xaxt = "n")
    axis(1, at=c(1,length(daysOfInterest)/2,length(daysOfInterest)), labels = days)
    
    dev.off()
}