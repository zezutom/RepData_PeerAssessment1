###
# Reproducible Research: Peer Assessment 1
###

### Dependencies

### Common Functions

# Converts a provided vector into a numeric one
as_numbers <- function(v) {
  v <- as.numeric(v)
  
  # Flag all non-missing non-numeric values as missing
  v[is.nan(v)] <- NA  
}

### Change working directory to the script location
script.dir <- dirname(sys.frame(1)$ofile)
setwd(script.dir)

### Loading and preprocessing the data

## Load the data (i.e. read.csv())
df <- read.csv("activity.csv")

## Process/transform the data (if necessary) into a format suitable for your analysis

# Treat steps as numeric values
as_numbers(df$steps)

# Treat intervals as numeric values
as_numbers(df$interval)

# Convert cells of the 'date' column into date values in YYYY-MM-DD format
df$date <- as.Date(df$date, "%Y-%m-%d")

### Task 1: What is mean total number of steps taken per day?

# Summarize daily total steps, skip missing values
total_steps <- aggregate(list(Steps = df$steps), list(Date = df$date), sum, na.rm = TRUE)

# Render a histogram of daily total steps
png("plot1.png")
hist(total_steps$Steps, 
     breaks = 20, 
     main = "Mean Total Number of Steps Taken per Day",
     xlab = "Total Number of Steps Taken per Day")

# Additional aggregations along with visualization info
stats_df <- data.frame(
  Val = c(
    round(mean(total_steps$Steps)), 
    round(median(total_steps$Steps))),
  Col = c("blue", "red"),
  Lbl = c("Mean", "Median"))

# Plot each of the aggregations
apply(stats_df, 1, function(x) {
  abline(v=x["Val"], lwd = 2, col = x["Col"])
})

# Add a legend
legend('topright', lty = 1, lwd = 3, col = as.character(stats_df$Col),
       cex = .8,
       legend = apply(stats_df, 1, function(x) {
         paste(x["Lbl"], ": ", x["Val"])
       }))

# Render the graphics
dev.off()

### Task 2: What is the average daily activity pattern?

# Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, 
# averaged across all days (y-axis)
png("plot2.png")
with(avg_steps, {
  
  # Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
  max_steps <- avg_steps[which.max(Steps), ]
  
  # Time series
  plot(Interval, Steps, type = "l",
       main = "Average Daily Activity Pattern",
       xlab = "Five Minute Interval",
       ylab = "Average Number of Steps")  
  
  # Plot the found maximum
  max_step_col <- "blue"
  points(max_steps$Interval,  max_steps$Steps, col = max_step_col, cex = 2, lwd = 2, pch = 1)
  
  # Add a legend
  legend("topright",
         legend = paste("Maximum of", 
                        round(max(max_steps$Steps)), 
                        "steps found at interval", 
                        max_steps$Interval),
         text.col = max_step_col, cex = .9, bty = 'n')
})

# Render the graphics
dev.off()

### Task 3: Imputing missing values

### Task 4: Are there differences in activity patterns between weekdays and weekends?
