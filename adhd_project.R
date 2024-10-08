# Load necessary libraries
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(mgm)
library(qgraph)

# Step 1: Load the dataset
adhd_data <- read.csv("C:/Users/poczt/OneDrive/Documents/learnig/ADHD_converted.csv")

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

# Step 3: Clean diagnosis descriptions for diagnosed students
# Only process diagnosis descriptions for students who have been formally diagnosed ('Yes')
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

# Step 4: Create diagnosis flags
# Define keywords for ADHD and other diagnoses
adhd_keywords <- c("adhd", "add", "attention deficit")

other_diagnoses_keywords <- c(
  "depression", "anxiety", "bipolar", "ocd", "ptsd", "autism",
  "eating disorder", "personality disorder", "insomnia", "substance use",
  "schizophrenia", "phobia", "panic disorder", "dissociative disorder",
  "psychotic disorder", "addiction", "other diagnosis keywords"  # Add any other relevant keywords
)

# Create flags
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

# Step 5: Create group labels
# According to your instructions:
# - 'ADHD': has_adhd == 1 (regardless of other diagnoses)
# - 'Other': has_adhd == 0 & has_other_diagnosis == 1
# - 'No Diagnosis': diagnosed_bin == 'No'

adhd_cleaned <- adhd_cleaned %>%
  mutate(
    group = case_when(
      has_adhd == 1 ~ "ADHD",
      has_adhd == 0 & has_other_diagnosis == 1 ~ "Other",
      diagnosed_bin == "No" ~ "No Diagnosis",
      TRUE ~ NA_character_
    )
  )

# Exclude rows where group is NA (i.e., diagnosed_bin == 'Yes' but diagnosis_cleaned didn't match any keywords)
adhd_cleaned <- adhd_cleaned %>%
  filter(!is.na(group))

# Step 6: Select relevant variables for network analysis
# Include group and academic variables
network_data <- adhd_cleaned %>%
  select(
    # Group variable
    group,
    # Binary variables
    sex,
    nbt_did_math,
    # Academic performance variables
    psy1004_grade, nbt_math, matric_mark,
    # Include any other variables you deem relevant
    # For example, psychological measures if desired
    bdi1_total, audit1_total, aas1_total, asrs1_total.x, bai1_total
  )

# Step 7 (Adjusted): Clean and recode data
# Recode 'sex' variable to numeric codes
network_data <- network_data %>%
  mutate(
    sex = case_when(
      tolower(sex) == "male" ~ 1,
      tolower(sex) == "female" ~ 2,
      TRUE ~ 3  # For 'Other' or any other entries
    )
  )

# Recode 'group' variable to numeric codes
# Create a mapping for 'group' levels
group_levels <- c("No Diagnosis", "Other", "ADHD")
network_data <- network_data %>%
  mutate(
    group = factor(group, levels = group_levels),
    group = as.numeric(group)
  )

# Ensure 'nbt_did_math' is numeric (should already be, but just in case)
network_data <- network_data %>%
  mutate(
    nbt_did_math = as.numeric(nbt_did_math)
  )

# Convert academic variables to numeric (already done)
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

# Remove rows with missing values in key variables
network_data <- network_data %>%
  drop_na()

# Step 8 (Adjusted): Prepare data for network psychometrics model
# Standardize continuous variables
continuous_vars <- c("psy1004_grade", "nbt_math", "matric_mark",
                     "bdi1_total", "audit1_total", "aas1_total",
                     "asrs1_total.x", "bai1_total")
network_data <- network_data %>%
  mutate(across(all_of(continuous_vars), ~ scale(.)[, 1]))

# Convert data to matrix for mgm
network_matrix <- as.matrix(network_data)

# Verify that all columns are numeric
str(network_matrix)

# Define types and levels for mgm
num_vars <- ncol(network_matrix)
types <- rep("g", num_vars)  # Initialize all as Gaussian
levels <- rep(1, num_vars)   # Initialize levels

# Identify categorical variables
categorical_vars <- c("group", "sex", "nbt_did_math")
categorical_indices <- which(colnames(network_matrix) %in% categorical_vars)

# Update types and levels for categorical variables
types[categorical_indices] <- "c"
levels[categorical_indices] <- sapply(network_data[, categorical_indices], function(x) length(unique(x)))

# Ensure 'types' and 'levels' have correct lengths
types <- types[1:num_vars]
levels <- levels[1:num_vars]

# Verify 'types' and 'levels'
data.frame(
  Variable = colnames(network_matrix),
  Type = types,
  Level = levels
) %>% print()

# Step 9: Fit the mixed graphical model using mgm
mgm_fit <- mgm(
  data = network_matrix,
  type = types,
  level = levels,
  k = 2,
  lambdaSel = "EBIC",
  ruleReg = "AND"
)

# Check the class of each column in network_matrix
sapply(network_matrix, class)

