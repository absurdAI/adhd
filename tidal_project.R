# Load required libraries
library(qgraph)
library(bootnet)

# Load the CSV files into R
tidal_study <- read.csv("tidal_study_characteristics.csv")
tidal_demographic <- read.csv("tidal_demographic_characteristics.csv")
tidal_clinical <- read.csv("tidal_clinical_characteristics.csv")
tidal_measures <- read.csv("tidal_measures_administration.csv")

# View the first few rows of each dataset to ensure they loaded correctly
head(tidal_study)
head(tidal_demographic)
head(tidal_clinical)
head(tidal_measures)

# Check for missing values
colSums(is.na(tidal_clinical))
colSums(is.na(tidal_demographic))

# Subset the clinical and academic data from tidal_clinical
# Example: using ADHD diagnosis and school accommodations for the network

# Create a smaller dataset with only the variables we want
tidal_network_data <- tidal_clinical[, c("ADHD Diagnosis Predominantly Inattentive Type % (n)", 
                                         "ADHD Diagnosis Combined Type % (n)", 
                                         "ADHD Diagnosis Predominantly H/I % (n)", 
                                         "School Accommodations Individualized Education Plan % (n)", 
                                         "School Accommodations Section 504 Plan % (n)", 
                                         "School Accommodations None % (n)")]

# Estimate the network using EBICglasso
tidal_network <- estimateNetwork(tidal_network_data, default = "EBICglasso")

# Plot the network
plot(tidal_network)

# Check the column names in the tidal_clinical dataset
colnames(tidal_clinical)

# Install tidyr if you don't have it
# install.packages("tidyr")

# Load the tidyr package
library(tidyr)

# Pivot the data to wide format, turning characteristics into columns
tidal_clinical_wide <- pivot_wider(tidal_clinical, names_from = Characteristic, values_from = Value)

# View the reshaped dataset
head(tidal_clinical_wide)

# Load necessary libraries
library(dplyr)
library(stringr)

# Extract the first numeric value from each column and convert to numeric format
tidal_clinical_clean <- tidal_clinical_wide %>%
  mutate(across(everything(), ~ as.numeric(str_extract(., "^\\d+\\.\\d+|\\d+"))))

# View the cleaned dataset
head(tidal_clinical_clean)

# Create a smaller dataset with the relevant variables for the network model
tidal_network_data <- tidal_clinical_clean[, c("ADHD Diagnosis Predominantly Inattentive Type % (n)", 
                                               "ADHD Diagnosis Combined Type % (n)", 
                                               "ADHD Diagnosis Predominantly H/I % (n)", 
                                               "School Accommodations Individualized Education Plan % (n)", 
                                               "School Accommodations Section 504 Plan % (n)", 
                                               "School Accommodations None % (n)")]

# View the smaller dataset to ensure everything is correctly selected
head(tidal_network_data)

# Estimate the network using EBICglasso
tidal_network <- estimateNetwork(tidal_network_data, default = "EBICglasso")

# Plot the network
plot(tidal_network)

# Check for missing values in tidal_network_data
colSums(is.na(tidal_network_data))

# Check for infinite values in tidal_network_data
sum(is.infinite(tidal_network_data))

# Convert the data to a numeric matrix
tidal_network_data_matrix <- as.matrix(tidal_network_data)

# Check for infinite values in the numeric matrix
sum(is.infinite(tidal_network_data_matrix))

# Estimate the network using the matrix version of the data
tidal_network <- estimateNetwork(tidal_network_data_matrix, default = "EBICglasso")

# Plot the network
plot(tidal_network)

# Identify non-numeric columns
non_numeric_columns <- sapply(tidal_network_data_matrix, function(x) any(!is.numeric(as.numeric(x))))

# Remove non-numeric columns
tidal_network_data_clean <- tidal_network_data_matrix[, !non_numeric_columns]

# View the cleaned data to confirm that only numeric columns are left
head(tidal_network_data_clean)

# Estimate the network using the cleaned data
tidal_network <- estimateNetwork(tidal_network_data_clean, default = "EBICglasso")

# Plot the network
plot(tidal_network)

# Convert the cleaned data to a data frame
tidal_network_data_clean_df <- as.data.frame(t(tidal_network_data_clean))

# View the cleaned data frame to confirm the structure
head(tidal_network_data_clean_df)

# Estimate the network using the cleaned data frame
tidal_network <- estimateNetwork(tidal_network_data_clean_df, default = "EBICglasso")

# Plot the network
plot(tidal_network)

# Check for any non-numeric values in the cleaned data
non_numeric_check <- sapply(tidal_network_data_clean_df, function(x) sum(!is.finite(as.numeric(x))))

# Print columns with non-numeric values
non_numeric_check[non_numeric_check > 0]

# Check for columns with zero variance (i.e., the same value in all rows)
zero_variance_cols <- sapply(tidal_network_data_clean_df, function(x) var(as.numeric(x), na.rm = TRUE) == 0)

# Print the names of columns with zero variance
colnames(tidal_network_data_clean_df)[zero_variance_cols]

# Convert all columns to numeric (force conversion)
tidal_network_data_clean_df[] <- lapply(tidal_network_data_clean_df, function(x) as.numeric(as.character(x)))

# View the cleaned data to ensure conversion
head(tidal_network_data_clean_df)

# Re-check for columns with zero variance
zero_variance_cols <- sapply(tidal_network_data_clean_df, function(x) var(x, na.rm = TRUE) == 0)

# Print the names of columns with zero variance
colnames(tidal_network_data_clean_df)[zero_variance_cols]

# Remove zero variance columns
tidal_network_data_clean_df <- tidal_network_data_clean_df[, !zero_variance_cols]

# View the cleaned dataset
head(tidal_network_data_clean_df)

# Check the dimensions of the dataset
dim(tidal_network_data_clean_df)

