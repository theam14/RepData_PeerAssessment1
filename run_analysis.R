# Script responsable to perform the necessar operations to solve the questions 
# presented in Reproducible Research course, assessment 1

run_analysis <- function(src_dir = "../", input_file = "activity.csv")
{
    # Load data.table package
    suppressPackageStartupMessages(library('data.table', character.only = TRUE));
    
    ## Initialization of directoryy that contains the figures presented in this
    ## report
    dir.create(file.path(".", "figures"), showWarnings = FALSE);
    
    ## Perform the data load
    activityData <- loadData(src_dir);
    
    ## Process the evaluation of question 1
    processQuestion1(activityData);
    
    ## Perfome the evaluation of question 2
    stepsAcrossPeriods <- processQuestion2(activityData);
    
    completeActivityData <- processQuestion3(activityData, stepsAcrossPeriods);
    
    completeStepsAcrossPeriods <- processQuestion4(completeActivityData);
}

processQuestion1 <- function(activityData)
{
    ## From the loaded date, perfomrs the loop to calculate the sum of steos
    ## by date
    stepsPerDay <- activityData[!is.na(steps), lapply(.SD, sum), by = date, .SDcols = 1]
    

    ## Builds the histogram of steps per day
    hist(stepsPerDay$steps, col = "blue", 
         xlab = "Number of Steps per Day",
         main = "Histogram: Steps per Day")
    
    ## Save the plot in the paste figures
    invisible(dev.copy(png, filename = paste("figures/", "histQuestion1.png")));
    invisible(dev.off());
    
    ## Prints the mean and median of steps per day
    cat("Question 1 results:\n"
        ,"\tMeans total number of steps taken per day: ", mean(stepsPerDay$steps), "\n"
        ,"\tMedian total number of steps taken per day: ", median(stepsPerDay$steps), "\n");

}

processQuestion2 <- function(activityData)
{
    ## From the loaded date, perfomrs the loop to calculate the average of steos
    ## by interval, across all days.
    stepsAcrossPeriods <- activityData[!is.na(steps), lapply(.SD, mean), 
                                       by = interval, .SDcols = 1];
    
    ## Change the average steps column to "avgSteps"
    setnames(stepsAcrossPeriods, 2, "avgSteps");

    ## Perform the plot
    plot(stepsAcrossPeriods$interval, stepsAcrossPeriods$avgSteps,
         type = "l", xlab = "Intervals (min)", col = "black",
         ylab = "Average Number of Steps", 
         main = "Average Steps by interval (5 minutes sample)");
    
    ## Find the maximum average value
    maxAvg = max(stepsAcrossPeriods$avgSteps);
    
    ## Shows, in plot, the maximum average and corresponding interval.
    with(subset(stepsAcrossPeriods, avgSteps == maxAvg), 
         text(interval, 0.99*avgSteps, paste("Average = ", 
                                             format(round(avgSteps, 2), 
                                                    nsmall = 2), 
                                             "\nInterval = ", interval), 
              cex = 0.75, pos = 4));
    
    ## Save the plot in the paste figures
    invisible(dev.copy(png, filename = "figures/plotQuestion2.png"));
    invisible(dev.off());
    
    stepsAcrossPeriods
}

processQuestion3 <- function(activityData, stepsAcrossPeriods)
{
    ## Generates a dataset with the total of missing data per day
    missingData <- activityData[is.na(steps), ];
    
    cat("Question 3 results:\n"
        ,"\tTotal number of missing data: ", nrow(missingData), "\n");
    
    completeActivityData <- copy(activityData)
    
    ## Fill the missing data with the average of the corresponding period
    completeActivityData[is.na(steps), steps := as.integer(lapply(interval, 
            function(pInterval) {
                round(stepsAcrossPeriods[interval == pInterval, avgSteps]);
    }))];
    
    ## From the loaded date, perfomrs the loop to calculate the sum of steos
    ## by date
    stepsPerDay <- completeActivityData[!is.na(steps), lapply(.SD, sum), by = date, .SDcols = 1]
        
    ## Builds the histogram of steps per day
    hist(stepsPerDay$steps, col = "red", 
         xlab = "Number of Steps per Day",
         main = "Histogram: Steps per Day")
        
    ## Save the plot in the paste figures
    invisible(dev.copy(png, filename = paste("figures/", "histQuestion3.png")));
    invisible(dev.off());
        
    ## Prints the mean and median of steps per day
    cat("Question 3 results:\n"
        ,"\tMeans total number of steps taken per day: ", mean(stepsPerDay$steps), "\n"
        ,"\tMedian total number of steps taken per day: ", median(stepsPerDay$steps), "\n");
    
    completeActivityData
}

processQuestion4 <- function(activityData)
{
    # Load data.table package
    suppressPackageStartupMessages(library('lattice', character.only = TRUE));
    
    # Just to set the correct locale for comparison
    osType = .Platform$OS.type
    if(osType == "unix") 
    {
        invisible(Sys.setlocale("LC_TIME", "en_US.UTF-8"));
    } 
    else 
    {
        invisible(Sys.setlocale("LC_TIME", "English"));
    }
  
    # Load data.table package
    activityData[, isWeekend := factor( as.numeric(lapply(date, function(pdate) {
        name <- toupper(weekdays(pdate, abbreviate = T));
        (identical(name, "SUN") || identical(name, "SAT"))
    })), levels = c(1,0), labels = c("Weekend", "Weekday") )];
    
    ## From the loaded date, perfomrs the loop to calculate the average of steos
    ## by interval, across all days.
    stepsAcrossPeriods <- activityData[!is.na(steps), lapply(.SD, mean), 
                                       by = list(interval, isWeekend), 
                                       .SDcols = 1];
    
    ## Change the average steps column to "avgSteps"
    setnames(stepsAcrossPeriods, 3, "avgSteps");
    
    ## PLot the average of steps per period, for weehdays and weekends
    q4Plot <- xyplot(stepsAcrossPeriods$avgSteps ~ stepsAcrossPeriods$interval
           | stepsAcrossPeriods$isWeekend, 
           main="Number of Steps by Interval (5 minutes sample)", 
           ylab="Average Number of Steps", xlab="Interval", type = "l");
    
    print(q4Plot);
    
    ## Save the plot in the paste figures
    invisible(dev.copy(png, filename = paste("figures/", "plotQuestion4.png")));
    invisible(dev.off());
    
    stepsAcrossPeriods
}

loadData <- function(src_dir = "../", input_file = "activity.csv")
{
    # Read de file and create a data table object
    activityData <- fread(paste(src_dir, input_file, sep = ""), 
                          header = T);
    
    ## Change the information in column "date" from string to date type.
    activityData[, date := as.Date(date)];
}
