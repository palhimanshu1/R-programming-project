# Suppress warnings
options(warn = -1)

# Load necessary libraries
library(dplyr)          # For data manipulation
library(ggplot2)       # For data visualization
library(caret)         # For confusion matrix and other metrics
library(readr)         # For reading CSV files

# Load the datasets
churn_data <- read_csv('churn_data.csv')
customer_data <- read_csv("customer_data.csv")
internet_data <- read_csv("internet_data.csv")

# Display the first few rows of each dataset
head(churn_data)
head(customer_data)
head(internet_data)

# Merging on 'customerID'
df_1 <- inner_join(churn_data, customer_data, by = 'customerID')
# Final dataframe with all predictor variables
telecom <- inner_join(df_1, internet_data, by = 'customerID')

# Display the first few rows of the merged dataframe
head(telecom)

# Display column names
colnames(telecom)

# Check the dimensions of the dataframe
dim(telecom)

# Statistical summary of the dataframe
summary(telecom)

# Check the structure of the dataframe
str(telecom)

# Data Pre-processing
# Pre-process the Total Charges Feature
telecom$TotalCharges <- as.numeric(gsub(" ", NA, telecom$TotalCharges))

# Impute Total Charges using Monthly Charges and tenure
value <- telecom$MonthlyCharges * telecom$tenure
telecom$TotalCharges[is.na(telecom$TotalCharges)] <- value[is.na(telecom$TotalCharges)]

# Distribution of Total Charges Field
ggplot(telecom, aes(x = TotalCharges)) + 
  geom_histogram(binwidth = 50, fill = "blue", color = "black") + 
  labs(title = "Distribution of Total Charges", x = "Total Charges", y = "Count")

# Churn Split in terms of Counts
ggplot(telecom, aes(x = Churn)) + 
  geom_bar(fill = "lightblue") + 
  labs(title = "Counts of Churn", x = "Churn", y = "Count")

# Tenure Boxplot
ggplot(telecom, aes(x = as.factor(Churn), y = tenure)) + 
  geom_boxplot() + 
  labs(title = "Churn vs Tenure", x = "Churn", y = "Tenure")

# Pie charts for Contract types
create_pie_chart <- function(data, title) {
  ggplot(data, aes(x = "", y = Freq, fill = Var1)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar("y") +
    labs(title = title) +
    theme_void()
}

# Create pie charts for each contract type
pie_Contract_m2m <- as.data.frame(table(telecom$Churn[telecom$Contract == "Month-to-month"]))
create_pie_chart(pie_Contract_m2m, "Month to Month Contract")

pie_Contract_1y <- as.data.frame(table(telecom$Churn[telecom$Contract == "One year"]))
create_pie_chart(pie_Contract_1y, "One Year Contract")

pie_Contract_2y <- as.data.frame(table(telecom$Churn[telecom$Contract == "Two year"]))
create_pie_chart(pie_Contract_2y, "Two Year Contract")

# Whether being on Monthly Charges is influencing Churn
ggplot(telecom, aes(x = as.factor(Churn), y = MonthlyCharges)) + 
  geom_boxplot() + 
  labs(title = "Churn vs Monthly Charges", x = "Churn", y = "Monthly Charges")

# Pie charts for Internet Service types
pie_InternetService_fo <- as.data.frame(table(telecom$Churn[telecom$InternetService == "Fiber optic"]))
create_pie_chart(pie_InternetService_fo, "Fiber Optic")

pie_InternetService_dsl <- as.data.frame(table(telecom$Churn[telecom$InternetService == "DSL"]))
create_pie_chart(pie_InternetService_dsl, "DSL")

pie_InternetService_no <- as.data.frame(table(telecom$Churn[telecom$InternetService == "No"]))
create_pie_chart(pie_InternetService_no, "No Internet Service")

# Convert Binary Categorical Features (Yes/No) to 0/1
varlist <- c('PhoneService', 'PaperlessBilling', 'Churn', 'Partner', 'Dependents')

# Defining the map function
binary_map <- function(x) {
  as.numeric(factor(x, levels = c("No", "Yes")))
}

# Applying the function to the selected columns
telecom[varlist] <- lapply(telecom[varlist], binary_map)

# Display the first few rows of the modified dataframe
head(telecom)
