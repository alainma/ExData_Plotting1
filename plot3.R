plot3 <- function() {
    
    plotEngine <- function(plotter, pngname) {
        url <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
        zipped <- "ElectricPowerConsumption.zip"
        unzipped <- "household_power_consumption.txt"
        
        download <- function() {
            if (!file.exists(zipped) && !file.exists(unzipped)) {
                download.file(url, destfile = zipped)
            }
        }
        
        unzipfile <- function() {
            if (file.exists(zipped) && !file.exists(unzipped)) {
                unzip(zipfile = zipped)
            }
        }
        
        read <- function() {
            colnames <- read.table(file = unzipped, nrows = 1, sep = ";", stringsAsFactors = FALSE)
            data <- read.table(file = unzipped, skip = 66637, nrows = 2880, sep = ";")
            colnames(data) <- as.character(as.vector(colnames[1, ]))
            data <- cbind(data, DateTime = strptime(x = paste(data[, 1], data[, 2]), format = "%d/%m/%Y %H:%M:%S"))
        }
        
        # Download data if not exists
        print("Downloading data...")
        download()
        
        # Unzip data
        print("Unzipping data...")
        unzipfile()
        
        # Read data
        print("Reading data...")
        data <- read()
        
        # Make plot
        print("Plotting...")
        plotter(data)
        
        # Save it to PNG
        print("Saving to PNG...")
        dev.copy(png, file = pngname)
        dev.off()
        
        print("Done!")
    }
    
    plotter <- function(data) {
        par(mfrow = c(1, 1))
        plot(x = data$DateTime, 
             y = data$Sub_metering_1, 
             xlab = "", 
             ylab = "Energy sub metering",
             col = "black",
             fg = "black",
             type = "l")
        lines(x = data$DateTime, 
             y = data$Sub_metering_2, 
             col = "red",
             type = "l")
        lines(x = data$DateTime, 
              y = data$Sub_metering_3, 
              col = "blue",
              type = "l")
        legend("topright",
               c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"),
               lty = c(1, 1, 1),
               col = c("black", "blue", "red"))
    }
    
    Sys.setlocale("LC_TIME", "English")
    plotEngine(plotter, "plot3.png")
}