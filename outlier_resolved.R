# installing the pre-requisite packages
install.packages("stats")
install.packages("e1071")

# loading the required libraries
library(stats)
library(e1071)

# loading the netflix_data_updated.csv
data <- read.csv("C:\\Users\\acer\\Dropbox\\PC\\Downloads\\netflix_data_updated.csv", header = TRUE)

# structure of the data
str(data)

# measure of central tendencies
# calculating the mean, median and mode of the data
date <- data$release_year
mean(date)
median(date)
mode(date)

# measure of variability
# calculating the standard deviation and variance of the data
sd(date)
var(date)

# calculating the inter-quartile ratio
quantile(date)

# calculating the z-score for the release years
unique_dates <- sort(unique(date)) 
z_scores <- (unique_dates-mean(date))/sd(date)
my_table <- data.frame(Column1 = unique_dates, Column2 = z_scores)
print(my_table)

# calculating data skewness
skew_value <- skewness(date)
cat("Skewness:", skew_value, "\n")

# calculating data kurtosis
kurtosis_value <- kurtosis(date)
cat("Kurtosis:", kurtosis_value, "\n")

# data visualization using histogram
hist(date, 
     main = "Release Year of Netflix Data", 
     xlab = "Years", 
     ylab = "Frequency", 
     col = "green", 
     border = "black")

# dropping the outliers to retrieve the most relevant data
updated_data <- subset(data, release_year > 2010 & release_year < 2020)

# checking the mean, median and mode of the updated data
updated_dates <- updated_data$release_year
mean(updated_dates)
median(updated_dates)
mode(updated_dates)

# verifying the measures of variability that is, sd and variance
sd(updated_dates)
var(updated_dates)

# calculating the inter-quartile ratio
quantile(updated_dates)

# calculating the z-score for the release years
unique_dates <- sort(unique(updated_dates)) 
z_scores <- (unique_dates-mean(date))/sd(date)
my_table <- data.frame(Column1 = unique_dates, Column2 = z_scores)
print(my_table)

# calculating data skewness and kurtosis
skewness(updated_dates)
kurtosis(updated_dates)

# Create a histogram with additional parameters
boxplot(updated_dates, 
        main = "Release Years of Netflix Data",           
        xlab = "Years",               
        ylab = "Frequency",             
        col = "pink",           
        border = "black")

# exporting the updated_data
write.csv(updated_data, "final_data.csv", row.names = FALSE)










mode = function(x){
  ta = table(x)
  tam = max(ta)
  if(all(ta == tam))
    mod = NA
  else
    if(is.numeric(x))
      mod = as.numeric(names(ta)[ta == tam])
  else
    mod = names(ta)[ta == tam]
  return(mod)
}







# 


