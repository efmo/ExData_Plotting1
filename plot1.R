plot1 <- function() {
    
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
    
    png("plot1.png")
    hist(householdPowerConsumption$Global_active_power, col = "RED", xlab = "Global Active Power (kilowatts)", main = "Global Active Power")
    dev.off()
}