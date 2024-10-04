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

# Step 7: Clean and recode data
# Recode 'sex' variable
network_data <- network_data %>%
  mutate(
    sex = case_when(
      tolower(sex) == "male" ~ "Male",
      tolower(sex) == "female" ~ "Female",
      TRUE ~ "Other"
    )
  )

# Recode 'nbt_did_math' to numeric
network_data <- network_data %>%
  mutate(
    nbt_did_math = case_when(
      tolower(nbt_did_math) == "yes" ~ 1,
      tolower(nbt_did_math) == "no" ~ 0,
      TRUE ~ NA_real_
    )
  )

# Convert academic variables to numeric
network_data <- network_data %>%
  mutate(
    psy1004_grade = as.numeric(psy1004_grade),
    nbt_math = as.numeric(nbt_math),
    matric_mark = as.numeric(matric_mark),
    # Convert psychological measures to numeric if included
    bdi1_total = as.numeric(bdi1_total),
    audit1_total = as.numeric(audit1_total),
    aas1_total = as.numeric(aas1_total),
    asrs1_total.x = as.numeric(asrs1_total.x),
    bai1_total = as.numeric(bai1_total)
  )

# Remove rows with missing values in key variables
network_data <- network_data %>%
  drop_na()

# Step 8: Prepare data for network psychometrics model
# Encode 'group' and 'sex' as factors
network_data <- network_data %>%
  mutate(
    group = factor(group),
    sex = factor(sex)
  )

# Standardize continuous variables
continuous_vars <- c("psy1004_grade", "nbt_math", "matric_mark", "bdi1_total", "audit1_total", "aas1_total", "asrs1_total.x", "bai1_total")
network_data <- network_data %>%
  mutate(across(all_of(continuous_vars), scale))

# Convert data to matrix for mgm
network_matrix <- as.matrix(network_data)

# Define types and levels for mgm
num_vars <- ncol(network_matrix)
types <- rep("g", num_vars)  # Initialize all as Gaussian
levels <- rep(1, num_vars)   # Initialize levels

# Identify categorical variables
categorical_vars <- c("group", "sex", "nbt_did_math")
categorical_indices <- which(colnames(network_matrix) %in% categorical_vars)

# Update types and levels for categorical variables
types[categorical_indices] <- "c"
levels[categorical_indices] <- sapply(network_data[, categorical_indices], function(x) nlevels(factor(x)))

# Ensure 'types' and 'levels' have correct lengths
types <- types[1:num_vars]
levels <- levels[1:num_vars]

# Step 9: Fit the mixed graphical model using mgm
mgm_fit <- mgm(
  data = network_matrix,
  type = types,
  level = levels,
  k = 2,
  lambdaSel = "EBIC",
  ruleReg = "AND"
)

# Step 10: View the summary of the fitted model
print(mgm_fit)

# Step 11: Visualize the network
# Extract adjacency matrix and signs
adjacency_matrix <- mgm_fit$pairwise$wadj
signs_matrix <- mgm_fit$pairwise$signs

# Combine to get the weighted adjacency matrix
weighted_adj_matrix <- adjacency_matrix * signs_matrix

# Set row and column names to variable names
colnames(weighted_adj_matrix) <- rownames(weighted_adj_matrix) <- colnames(network_matrix)

# Replace non-finite values with zero
weighted_adj_matrix[!is.finite(weighted_adj_matrix)] <- 0

# Plot the network
qgraph(
  weighted_adj_matrix,
  layout = "spring",
  labels = colnames(network_matrix),
  theme = "colorblind",
  edge.color = NULL,       # Use default coloring based on sign
  posCol = "darkgreen",
  negCol = "red",
  vsize = 7,
  label.cex = 1.2,
  edge.width = 1.5,
  title = "Mixed Graphical Model Network"
)
