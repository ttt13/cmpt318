library(lubridate)
library(chron)
library(dplyr)
library(magrittr)


Datadf <- read.table("Data_Assignmet2.txt", header = TRUE, sep = ",")
data <- Datadf
n <- factor(data$Time)


a_time <- hms(as.character(n))
data$hour <- as.integer(hour(a_time))

newdata <- data[c("Date","Global_active_power")]
newdata$Date <- as.Date(newdata$Date, format='%d/%m/%Y')
averaged_days <- aggregate(Global_active_power ~ Date, newdata, mean)
#averaged_days <- averaged_days[order(as.Date(averaged_days$Date, format ="%d/%m/%y")),]
#averaged_days_2 <- newdata %.% group_by(Date) %.% summarize(mean(Global_active_power))

window_frame = array(data = NA, dim = nrow(averaged_days)-6)
i <- 1
j <- 7

while(j < nrow(averaged_days)) {
  test_window <- averaged_days %>% slice(i:j)
  window_frame[i] <- mean(test_window$Global_active_power, na.rm = TRUE)
  i = i+1
  j = j+1
}
plot(window_frame, type="l",
     main = "Global Active Power Average per Window",
     xlab = "Window", ylab = "Global Active Power Average")
