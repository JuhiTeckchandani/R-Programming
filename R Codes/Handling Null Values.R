setwd("D:/RISE - WPU/R Studio")
getwd()
details = read.csv("Employee Details.csv")
View(details)

# identify NA - is.na()
is.na(details)
which(is.na(details$Age))
which(is.na(details$Salary))
which(is.na(details$Name))
which(is.na(details$ID))
which(is.na(details$Department))

# Handling NAs
# 1. Omit function

data1 = na.omit(details)
View(data1)

# 2. replace with mean,median or mode
details$Age[is.na(details$Age)] <- median(details$Age, na.rm = TRUE)
View(details)

#details$Salary[is.na(details$Salary)] <- mean(details$Salary, na.rm = TRUE)
#View(details)

med = median(details$Salary,na.rm = T)
details$Salary = ifelse(is.na(details$Salary),med,details$Salary)
View(details)