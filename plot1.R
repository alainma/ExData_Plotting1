plot1 <- function() {
    
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
        hist(data$Global_active_power, 
             xlab = "Global Active Power (kilowatts)", 
             col = "red", 
             main = "Global Active Power")
    }
    
    Sys.setlocale("LC_TIME", "English")
    plotEngine(plotter, "plot1.png")
}