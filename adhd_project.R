# Load necessary libraries
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(mgm)
library(qgraph)

# Step 1: Load the dataset
adhd_data <- read.csv("C:/Users/poczt/OneDrive/Documents/learnig/ADHD_converted.csv")

# Print initial data information
initial_rows <- nrow(adhd_data)
initial_cols <- ncol(adhd_data)
print(paste("Initial number of rows:", initial_rows))
print(paste("Initial number of columns:", initial_cols))
print("Initial data structure:")
str(adhd_data)

# Step 2: Map diagnosis responses to 'Yes' or 'No'
diagnosis_mapping <- c(
  "no" = "No",
  "not formally diagnosed" = "No",
  "yes, formally diagnosed by a doctor or mental health professional" = "Yes"
)

adhd_cleaned <- adhd_data %>%
  mutate(
    diagnosis_response = trimws(tolower(have_you_ever_been_diagnosed_with_a_mental_illness)),
    diagnosed_bin = diagnosis_mapping[diagnosis_response]
  )

# Print data information after Step 2
rows_step2 <- nrow(adhd_cleaned)
print(paste("Number of rows after Step 2 (mapping diagnosis responses):", rows_step2))
print("Distribution of diagnosed_bin:")
print(table(adhd_cleaned$diagnosed_bin, useNA = "ifany"))
print("Check for NAs in diagnosed_bin:")
print(sum(is.na(adhd_cleaned$diagnosed_bin)))

# Step 3: Clean diagnosis descriptions for diagnosed students
adhd_cleaned <- adhd_cleaned %>%
  mutate(
    diagnosis_cleaned = if_else(
      diagnosed_bin == "Yes",
      tolower(if_you_have_been_diagnosed_formally_or_informally_please_list_the_diagnosis_diagnoses),
      NA_character_
    ),
    diagnosis_cleaned = str_replace_all(diagnosis_cleaned, "\\b0\\b", "not"),
    diagnosis_cleaned = str_replace_all(diagnosis_cleaned, "[[:punct:]]", " "),
    diagnosis_cleaned = str_squish(diagnosis_cleaned)
  )

# Print data information after Step 3
rows_step3 <- nrow(adhd_cleaned)
print(paste("Number of rows after Step 3 (cleaning diagnosis descriptions):", rows_step3))
print(paste("Number of non-NA diagnosis_cleaned entries:", sum(!is.na(adhd_cleaned$diagnosis_cleaned))))
print("Sample cleaned diagnosis descriptions:")
print(head(adhd_cleaned$diagnosis_cleaned, 5))

# Step 4: Create diagnosis flags
adhd_keywords <- c("adhd", "add", "attention deficit")

other_diagnoses_keywords <- c(
  "depression", "anxiety", "bipolar", "ocd", "ptsd", "autism",
  "eating disorder", "personality disorder", "insomnia", "substance use",
  "schizophrenia", "phobia", "panic disorder", "dissociative disorder",
  "psychotic disorder", "addiction"
)

adhd_cleaned <- adhd_cleaned %>%
  mutate(
    has_adhd = if_else(
      diagnosed_bin == "Yes" & str_detect(diagnosis_cleaned, paste(adhd_keywords, collapse = "|")),
      1, 0
    ),
    has_other_diagnosis = if_else(
      diagnosed_bin == "Yes" & str_detect(diagnosis_cleaned, paste(other_diagnoses_keywords, collapse = "|")),
      1, 0
    )
  )

# Print data information after Step 4
print("Distribution of has_adhd:")
print(table(adhd_cleaned$has_adhd))
print("Distribution of has_other_diagnosis:")
print(table(adhd_cleaned$has_other_diagnosis))

# Step 5: Create group labels
adhd_cleaned <- adhd_cleaned %>%
  mutate(
    group = case_when(
      has_adhd == 1 ~ "ADHD",
      has_adhd == 0 & has_other_diagnosis == 1 ~ "Other",
      diagnosed_bin == "No" ~ "No Diagnosis",
      TRUE ~ NA_character_
    )
  )

# Exclude rows where group is NA
adhd_cleaned_filtered <- adhd_cleaned %>%
  filter(!is.na(group))

# Print data information after Step 5
excluded_rows_step5 <- nrow(adhd_cleaned) - nrow(adhd_cleaned_filtered)
print(paste("Number of rows excluded due to NA in group:", excluded_rows_step5))
print("Distribution of groups:")
print(table(adhd_cleaned_filtered$group))

# Step 6: Select relevant variables for network analysis, including 'nbt_ave' and 'nbt_alql_ave'
network_data <- adhd_cleaned_filtered %>%
  select(
    group,           # Diagnosis group
    sex,             # Sex
    nbt_did_math,    # Did the student take math?
    psy1004_grade,   # Academic performance (Psychology grade)
    nbt_math,        # Math score
    matric_mark,     # High school academic performance
    nbt_ave,         # NBT average score
    nbt_alql_ave,    # NBT Algebra and Quantitative Literacy average
    bdi1_total, audit1_total, aas1_total, asrs1_total.x, bai1_total # Psychological measures
  )


# Print data information after Step 6
rows_step6 <- nrow(network_data)
cols_step6 <- ncol(network_data)
print(paste("Number of rows after Step 6 (selecting variables):", rows_step6))
print(paste("Number of columns after Step 6:", cols_step6))
print("Preview of network_data:")
print(head(network_data, 5))

# Step 7: Clean and recode data
network_data <- network_data %>%
  # Filter 'sex' to include only 'male' and 'female', then convert to numeric
  filter(tolower(sex) %in% c("male", "female")) %>%
  mutate(
    sex = if_else(tolower(sex) == "male", 1, 2)  # 1 for male, 2 for female
  )

# Print data information after Step 7 (filtering 'sex')
rows_step7 <- nrow(network_data)
print(paste("Number of rows after Step 7 (filtering 'sex' and recoding):", rows_step7))
print("Distribution of sex after recoding:")
print(table(network_data$sex))

# Recode 'group' variable to numeric codes
group_levels <- c("No Diagnosis", "Other", "ADHD")
network_data <- network_data %>%
  mutate(
    group = factor(group, levels = group_levels),
    group = as.numeric(group)
  )

# Print data information after recoding 'group'
print("Distribution of group after recoding:")
print(table(network_data$group, useNA = "ifany"))

# **Recode 'nbt_did_math' based on 'nbt_math'**
network_data <- network_data %>%
  mutate(
    nbt_did_math = case_when(
      !is.na(nbt_math) & nbt_math > 0 ~ 1,  # Did math
      is.na(nbt_math) | nbt_math == 0 ~ 0  # Did not do math
    )
  )

# Print data information after recoding 'nbt_did_math'
print("Distribution of nbt_did_math after recoding:")
print(table(network_data$nbt_did_math, useNA = "ifany"))

# Convert academic variables and psychological measures to numeric (if not already)
network_data <- network_data %>%
  mutate(
    psy1004_grade = as.numeric(psy1004_grade),
    nbt_math = as.numeric(nbt_math),
    matric_mark = as.numeric(matric_mark),
    bdi1_total = as.numeric(bdi1_total),
    audit1_total = as.numeric(audit1_total),
    aas1_total = as.numeric(aas1_total),
    asrs1_total.x = as.numeric(asrs1_total.x),
    bai1_total = as.numeric(bai1_total)
  )

# Print data structure after Step 7
print("Data structure after Step 7:")
str(network_data)

# Step 8: Remove rows with missing values in key variables (if any)
# Since we've recoded 'nbt_did_math' to exclude NAs, it should no longer have NAs
network_data <- network_data %>%
  drop_na()

# Print data information after Step 8
rows_step8 <- nrow(network_data)
print(paste("Number of rows after Step 8 (dropping NAs):", rows_step8))
print("Any NAs remaining in network_data:")
print(any(is.na(network_data)))

print("Distribution of group after dropping NAs:")
print(table(network_data$group))

print("Distribution of sex after dropping NAs:")
print(table(network_data$sex))

print("Distribution of nbt_did_math after dropping NAs:")
print(table(network_data$nbt_did_math))

# Step 9: Convert any remaining factors to numeric (already handled)
network_data <- network_data %>%
  mutate(across(everything(), ~ as.numeric(as.character(.))))

# Print data structure after Step 9
print("Final data structure after Step 9:")
str(network_data)

# Step 10: Standardize continuous variables
continuous_vars <- c("psy1004_grade", "nbt_math", "matric_mark",
                     "bdi1_total", "audit1_total", "aas1_total",
                     "asrs1_total.x", "bai1_total")

network_data <- network_data %>%
  mutate(across(all_of(continuous_vars), ~ scale(.)[, 1]))

# Print preview after scaling
print("Preview of network_data after scaling:")
print(head(network_data, 5))

# Step 11: Convert the cleaned dataset into a matrix
network_matrix <- as.matrix(network_data)

# Verify that all elements are numeric
storage_mode <- storage.mode(network_matrix)
print(paste("Storage mode of network_matrix:", storage_mode))

# Check for non-numeric elements
any_non_numeric <- any(is.na(as.numeric(network_matrix)))
print(paste("Any non-numeric elements in network_matrix:", any_non_numeric))

# Check the dimensions of the matrix
print(paste("Dimensions of network_matrix:", paste(dim(network_matrix), collapse = " x ")))

# Check the first few rows and columns
print("Preview of network_matrix:")
print(network_matrix[1:5, ])

# Step 12: Define types and levels for mgm
num_vars <- ncol(network_matrix)
types <- rep("g", num_vars)  # Start with all variables as Gaussian
levels <- rep(1, num_vars)   # All levels start at 1 (for continuous)

# Identify the categorical variables and update their types
categorical_vars <- c("group", "sex", "nbt_did_math")
categorical_indices <- which(colnames(network_matrix) %in% categorical_vars)
types[categorical_indices] <- "c"  # Set types for categorical variables

# Correctly assign levels for categorical variables
levels[categorical_indices] <- sapply(categorical_vars, function(var) length(unique(network_data[[var]])))

# Verify types and levels
types_levels_df <- data.frame(
  Variable = colnames(network_matrix),
  Type = types,
  Level = levels
)
print("Types and Levels for mgm:")
print(types_levels_df)

# Step 13: Fit the mixed graphical model using mgm
mgm_fit <- mgm(
  data = network_matrix,
  type = types,
  level = levels,
  k = 2,
  lambdaSel = "CV",        # Changed from 'EBIC' to 'CV'
  ruleReg = "AND"
)

# Check the summary of the fitted model
print("Summary of the mgm_fit:")
print(mgm_fit)

# Step 14: Extract and print edge weights
# Extract variable names
variable_names <- colnames(network_matrix)

# Extract the weighted adjacency matrix and signs
adjacency_matrix <- mgm_fit$pairwise$wadj
signs_matrix <- mgm_fit$pairwise$signs

# Combine weights and signs
weighted_adj_matrix <- adjacency_matrix * signs_matrix

# Set row and column names
colnames(weighted_adj_matrix) <- rownames(weighted_adj_matrix) <- variable_names

# Convert the adjacency matrix to a data frame of edges
edge_list <- data.frame(
  from = rep(variable_names, each = length(variable_names)),
  to = rep(variable_names, times = length(variable_names)),
  weight = as.vector(weighted_adj_matrix)
)

# Remove self-loops (edges from a node to itself)
edge_list <- edge_list[edge_list$from != edge_list$to, ]

# Remove duplicate edges (since the matrix is symmetric)
edge_list <- edge_list[!duplicated(t(apply(edge_list[, 1:2], 1, sort))), ]

# Remove zero-weight edges and any remaining NAs
edge_list <- edge_list[!is.na(edge_list$weight) & edge_list$weight != 0, ]

# Sort edges by absolute weight (from strongest to weakest)
edge_list <- edge_list[order(-abs(edge_list$weight)), ]

# Print the edge list
print("Edge List with Weights:")
print(edge_list)

# Step 15: Create Academic Performance Index (Simple Average)
academic_vars <- c("psy1004_grade", "nbt_math", "matric_mark", "nbt_ave", "nbt_alql_ave")

# Check if these variables exist in network_data
existing_academic_vars <- academic_vars[academic_vars %in% colnames(network_data)]
missing_academic_vars <- academic_vars[!(academic_vars %in% colnames(network_data))]

if(length(missing_academic_vars) > 0){
  warning(paste("The following academic variables are missing and will be excluded from the index:", 
                paste(missing_academic_vars, collapse = ", ")))
}

# Create the Academic Performance Index
network_data <- network_data %>%
  mutate(
    academic_performance_index = rowMeans(select(., all_of(existing_academic_vars)), na.rm = TRUE)
  )

# Print distribution of the Academic Performance Index
print("Distribution of Academic Performance Index:")
print(summary(network_data$academic_performance_index))

# Preview the Academic Performance Index
print("Preview of network_data with Academic Performance Index:")
print(head(network_data %>% select(group, academic_performance_index), 5))

# Optionally, include the Academic Performance Index in the network_matrix and mgm
# Uncomment the following lines if you wish to include it in the analysis

# network_data <- network_data %>%
#   mutate(
#     academic_performance_index = scale(academic_performance_index)[,1]
#   )
# network_matrix <- as.matrix(network_data)

# Export the edge list to a CSV file for full inspection
write.csv(edge_list, "C:/Users/poczt/OneDrive/Documents/learnig/edge_list.csv", row.names = FALSE)
print("Edge list exported to 'edge_list.csv' for comprehensive review.")

