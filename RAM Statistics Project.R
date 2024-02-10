# Loading the insurance_dataset from the local file
library(readr)
fp <- "C:\\Users\\HP\\Downloads\\insurance.csv"
insurance_data <- read.csv(fp)

# Data size
data_dimensions <- dim(insurance_data)
print(data_dimensions)

# Columns in the dataset
str(insurance_data)

# Checking missing values
sapply(insurance_data, function(x) sum(is.na(x)))

# Checking for duplicated rows and removal
duplicates <- duplicated(insurance_data)
duplicates_from_last <- duplicated(insurance_data, fromLast = TRUE)
all_duplicates <- duplicates | duplicates_from_last
insurance_data_duplicates <- insurance_data[all_duplicates, ]
print(insurance_data_duplicates)

total_rows <- nrow(insurance_data)
print(total_rows)

insurance_data_no_duplicates <- unique(insurance_data)
total_rows_after_removal <- nrow(insurance_data_no_duplicates)
print(total_rows_after_removal)

# Determining the type of each data variable
str(insurance_data)

# Bar chart of average insurance charges by Age Group
install.packages("ggplot2")
library(ggplot2)
insurance_data$age_group <- cut(insurance_data$age, breaks=seq(from=min(insurance_data$age), to=max(insurance_data$age), by=5), include.lowest = TRUE)
insurance_data_summary <- aggregate(charges ~ age_group, insurance_data, mean)
ggplot(insurance_data_summary, aes(x = age_group, y = charges)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  labs(x = "Age Group", y = "Average Charges", title = "Average Insurance Charges by Age Group") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 65, hjust = 1)) # Rotate the x-axis labels for better readability

# Create a pie chart with percentages
insurance_data$region <- factor(insurance_data$region)
region_counts <- table(insurance_data$region)
region_insurance_data <- as.data.frame(region_counts)
region_insurance_data$percentage <- region_insurance_data$Freq / sum(region_insurance_data$Freq) * 100
ggplot(region_insurance_data, aes(x = "", y = Freq, fill = Var1)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(x = NULL, y = NULL, fill = "Region", title = "Distribution of Regions") +
  theme_void() +
  theme(legend.position = "right") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), position = position_stack(vjust = 0.5))

# Stem & Leaf diagram of the bmi values
stem(insurance_data$bmi)

# Histogram for the 'children' variable
ggplot(insurance_data, aes(x = children, y = ..count..)) + # Set y to the count of each bin
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  geom_freqpoly(binwidth = 1, color = "red", size = 0.5) + # Add frequency polygon line
  labs(x = "Number of Children", y = "Count", title = "Distribution of Number of Children") +
  theme_minimal()

# Box plot for the 'age' variable
ggplot(insurance_data, aes(y = age)) +
  geom_boxplot(fill = "lightblue", color = "darkblue") +
  labs(y = "Age", title = "Box Plot of Age") +
  theme_minimal()

# Data Description
summary(insurance_data)

# Subset the data for smokers and non-smokers
smokers_data <- subset(insurance_data, smoker == "yes")
non_smokers_data <- subset(insurance_data, smoker == "no")

# Two Sample Test
shapiro.test(smokers_data$charges)
shapiro.test(non_smokers_data$charges)
install.packages("car")
library(car)
leveneTest(charges ~ smoker, data = insurance_data)
t_test_result <- t.test(charges ~ smoker, data = insurance_data)
print(t_test_result)

# Goodness of fit test
ks_test_result <- ks.test(insurance_data$charges, "pnorm", mean(insurance_data$charges), sd(insurance_data$charges))
print(ks_test_result)

# Chi Square test of Independents
chi_sq_result <- chisq.test(insurance_data$smoker, insurance_data$region)
print(chi_sq_result)

# Correlation test
correlation_result <- cor(insurance_data$age, insurance_data$charges)
print(correlation_result)

# Multiple linear regression model
model <- lm(charges ~ age + bmi + children + smoker, data = insurance_data)
summary(model)

# Two-Way ANOVA
anova_result <- aov(charges ~ smoker + region + smoker:region, data = insurance_data)
summary(anova_result)
