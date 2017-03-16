par(mfrow=c(1,2))

if (!exists("activityData")) {
  activityData <- read.csv("activity/activity.csv")
}

histStepsByDay <- function(data) {
  stepsTakenByDay <- data %>%
    group_by(date) %>%
    summarize(preTotalSteps = sum(steps)) %>%
    mutate(totalSteps = ifelse(is.na(preTotalSteps), 0, preTotalSteps))
  
  g <- ggplot(stepsTakenByDay, aes(x=as.Date(date), y=totalSteps, fill=totalSteps)) + geom_histogram(stat="identity")
  
  print(g)
  return(list(mean(stepsTakenByDay$totalSteps), median(stepsTakenByDay$totalSteps)))
}

# q2
activityResults <- histStepsByDay(activityData)

meanStepsTakenByDay <- activityResults[1] 
medianStepsTakenByDay <- activityResults[2]

validRows <- activityData %>% filter(!is.na(steps))
meanStepsTakenByInterval <- mean(validRows$steps)
medianStepsTakenByInterval <- median(validRows$steps)

# q4
# "impute" activityData by replacing NAs with mean steps taken each interval
activityDataRevised <- activityData %>%
  mutate(steps = ifelse(is.na(steps), meanStepsTakenByInterval, steps))

activityResultsRevised <- histStepsByDay(activityDataRevised)
meanStepsTakenByDayRevised <- activityResultsRevised[1] 
medianStepsTakenByDayRevised <- activityResultsRevised[2]

# q3
daysInExperiment <- length(unique(activityData$date))
stepsTakenByInterval <- activityData %>%
  mutate(zeroIfNa = ifelse(is.na(steps), 0, steps)) %>%
  group_by(interval) %>%
  summarize(steps = sum(zeroIfNa) / daysInExperiment)

g <- ggplot(stepsTakenByInterval, aes(x = interval, y = steps)) + geom_line()
print(g)

maxIndex <- which.max(stepsTakenByInterval$steps)
maxInterval <- stepsTakenByInterval[[maxIndex,1]]

# q5
stepsTakenPattern <- activityData %>%
  mutate(zeroIfNa = ifelse(is.na(steps), 0, steps), day=weekdays(as.Date(date))) %>%
  mutate(dayType = ifelse((day == "Saturday" | day == "Sunday"), "weekend", "weekday")) %>%
  group_by(dayType, interval) %>%
  summarize(steps = sum(zeroIfNa) / daysInExperiment)

stepsTakenPatternPlot <- ggplot(stepsTakenPattern, aes(x = interval, y = steps)) +
  geom_line() +
  facet_grid(dayType ~ .) +
  labs(y = "Number of steps", x = "Intervals") +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

print(stepsTakenPatternPlot)