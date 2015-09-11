# This script is used to create a histogram of Global Active Power data for 
# the dates 2007-02-01 and 2007-02-02 from the 'Individual household 
# electric power consumption Data Set'. This data set was made available 
# from the Exploratory Data Analysis course website at:
# https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip
# but can also be downloaded from the source at
# http://archive.ics.uci.edu/ml/machine-learning-databases/00235/household_power_consumption.zip
#
# Preconditions:
# Before running this script, downlaod the zip file from one of the sources
# listed above and unzip the file. Move the text file named
# 'household_power_consumption.txt' into the same directory as this script.
# Once RStudio has been launched, set the working directory to the directory 
# that contains this script and the text file, then source the script by
# typing the following in the console:
# > source("plot1.R")
#
# Execution: verified on RStudio Version 0.99.467
# Once the script has been sourced, execute the plot1 function
# by typing the following in the console:
# > plot1()
#
# Postconditions:
# After executing the command above, a .png file named 'plot1.png' containing the
# histogram as descibed above will be located in the working directory


# function to create a histogram of Global Active Power data for 
# the dates 2007-02-01 and 2007-02-02 and save it to a png file
# named 'plot1.png' in the working directory
plot1 <- function() {
    
    # Create a table from the first 20 rows of data from the text file.
    # This sample table will be used to generate the column classes, 
    # which will then be used to more efficiently create a table 
    # of the entire data set
    sampleTable <- read.table("household_power_consumption.txt", 
                        header = TRUE, sep = ";", 
                        nrows = 20, stringsAsFactors = FALSE)
    
    # generate a vector of column classes from the sample table
    colClasses <- sapply(sampleTable, class)
    
    # read the entire data set from the file into a table
    powerData <- read.table("household_power_consumption.txt",
                        header = TRUE, na.strings = c("?"), 
                        sep = ";", stringsAsFactors = FALSE,
                        colClasses = colClasses)
    
    # convert the character strings in the Date column to Dates
    powerData$Date <- as.Date(powerData$Date, format = "%d/%m/%Y")
     
    # create a subset of data for the desired dates: 2007-02-01 and 2007-02-02
    powerData_subset <- subset(powerData, 
                        Date == "2007-02-01" | Date == "2007-02-02")
    
    # specify the PNG graphics device
    png("plot1.png", 480, 480)
    
    # generate the desired histogram 
    hist(powerData_subset$Global_active_power, 
         xlab = "Global Active Power (kilowatts)",
         col="red", main = "Global Active Power")
    
    # close the PNG graphics device
    dev.off()
    
}