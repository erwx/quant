# Title: Intro to Descriptive Statistics using R
# Date: 8/31/2023

# Functions ----
## R doesn't have a built in function for mode 
  mode <- function(x) {
    uniqx <- unique(x)
    uniqx[which.max(tabulate(match(x, uniqx)))]
  }

# R also doesn't have a built in function for population sd
  sd.p <- function(x) {
    sd(x)*sqrt((length(x)-1)/length(x))
    }

# Opening Data ----
## replace with your data here
data <- read.csv("labOne.csv")

# Quick Data Cleaning ----
colnames(data) <- c(
  "id", 
  "program", 
  "anxiety", 
  "sd_response", 
  "age_months", 
  "age_years", 
  "height_inches", 
  "height_feet", 
  "shoe" ,
  "commute", 
  "travel", 
  "school" ,
  "pets", 
  "pet_num", 
  "pet_type"
  )

# Descriptive Statistics ----

# _ 1. Mean of Single Variable ----
# Find mean of single variable
mean(data$age_years)
mean(data[["age_years"]])
mean(data[, "age_years"])

# _ 2. Find means for non-Instructor observations ----
student <- data[data$program != "Instructor", ]
mean(student$age_years)

# Histogram ----
      
# Type ?histogram to get a help page showing all the options      
hist(
  data$commute, 
  col = "hot pink",
  breaks = 10, 
  freq = TRUE,
  xlab = "Length of Commute (Miles)",
  main = "Distribution of Commuting Distance"
  )

# Bar Chart  ----
# In order for a bar chart to work, I need to convert a character vector to a factor (nominal variable)
data$program <- as.factor(data$program)
counts <- table(data$program) # This stores the number of times each type of MA/PhD program occurs as a vector
barplot(
  counts, 
  main="Number of Students by type of MA/PhD Program",
  xlab="Type of Program",
  ylab = "Count of Students",
  col = "maroon"
  )

# Scatter Plots ----

plot(data$anxiety, data$commute)

# Make it prettier 
plot(
  data$anxiety, data$commute, 
  main = "Stats Anxiety and Commute Time", 
  xlab = "Stats Axiety (1:low - 10:high)",
  ylab = "Commute Time", 
  pch = 19, 
  col = "purple"
  )

#Add a line of best fit (aka regression line)
abline(lm(data$commute~data$anxiety), col = "gold")
