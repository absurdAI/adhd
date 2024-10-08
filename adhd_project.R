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
adhd_keywords <- c("adhd", "add", "attention deficit")

other_diagnoses_keywords <- c(
  "depression", "anxiety", "bipolar", "ocd", "ptsd", "autism",
  "eating disorder", "personality disorder", "insomnia", "substance use",
  "schizophrenia", "phobia", "panic disorder", "dissociative disorder",
  "psychotic disorder", "addiction", "other diagnosis keywords"
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
adhd_cleaned <- adhd_cleaned %>%
  filter(!is.na(group))

# Step 6: Select relevant variables for network analysis
network_data <- adhd_cleaned %>%
  select(
    group,           # Diagnosis group
    sex,             # Sex
    nbt_did_math,    # Did the student take math?
    psy1004_grade,   # Academic performance (Psychology grade)
    nbt_math,        # Math score
    matric_mark,     # High school academic performance
    bdi1_total, audit1_total, aas1_total, asrs1_total.x, bai1_total # Psychological measures
  )

# Step 7: Clean and recode data

# Filter 'sex' variable to include only 'male' and 'female', then convert to numeric
network_data <- network_data %>%
  filter(tolower(sex) %in% c("male", "female")) %>%
  mutate(
    sex = if_else(tolower(sex) == "male", 1, 2)  # 1 for male, 2 for female
  )

# Recode 'group' variable to numeric codes
group_levels <- c("No Diagnosis", "Other", "ADHD")
network_data <- network_data %>%
  mutate(
    group = factor(group, levels = group_levels),
    group = as.numeric(group)
  )

# Recode 'nbt_did_math' from 'Yes'/'No' to 1/0
network_data <- network_data %>%
  mutate(
    nbt_did_math = case_when(
      tolower(nbt_did_math) == "yes" ~ 1,
      tolower(nbt_did_math) == "no" ~ 0,
      TRUE ~ NA_real_
    )
  )

# Convert academic variables and psychological measures to numeric
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

# Convert any remaining factors to numeric
network_data <- network_data %>%
  mutate(across(everything(), ~ as.numeric(as.character(.))))

# Step 8: Prepare data for network psychometrics model

# Standardize continuous variables
continuous_vars <- c("psy1004_grade", "nbt_math", "matric_mark",
                     "bdi1_total", "audit1_total", "aas1_total",
                     "asrs1_total.x", "bai1_total")
network_data <- network_data %>%
  mutate(across(all_of(continuous_vars), ~ scale(.)[, 1]))

# Convert the cleaned dataset into a matrix
network_matrix <- as.matrix(network_data)

# Verify that all elements are numeric
storage_mode <- storage.mode(network_matrix)
print(paste("Storage mode of network_matrix:", storage_mode))

# Check for non-numeric elements
any_non_numeric <- any(is.na(as.numeric(network_matrix)))
print(paste("Any non-numeric elements in network_matrix:", any_non_numeric))

# Define types and levels for mgm
num_vars <- ncol(network_matrix)
types <- rep("g", num_vars)  # Start with all variables as Gaussian
levels <- rep(1, num_vars)   # Initialize levels to 1

# Identify the categorical variables and update their types
categorical_vars <- c("group", "sex", "nbt_did_math")
categorical_indices <- which(colnames(network_matrix) %in% categorical_vars)
types[categorical_indices] <- "c"  # Set types for categorical variables

# Correctly assign levels for categorical variables
levels[categorical_indices] <- sapply(categorical_vars, function(var) length(unique(network_data[[var]])))

# Verify everything looks good before fitting the model
data.frame(
  Variable = colnames(network_matrix),
  Type = types,
  Level = levels
) %>% print()

# Check unique values in 'group'
print("Unique values in 'group':")
print(unique(network_data$group))

# Check unique values in 'sex'
print("Unique values in 'sex':")
print(unique(network_data$sex))

# Check unique values in 'nbt_did_math'
print("Unique values in 'nbt_did_math':")
print(unique(network_data$nbt_did_math))

# Step 9: Fit the mixed graphical model using mgm

mgm_fit <- mgm(
  data = network_matrix,
  type = types,
  level = levels,
  k = 2,
  lambdaSel = "EBIC",
  ruleReg = "AND"
)

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
  layout = "spring",                 # Layout algorithm
  labels = colnames(network_matrix), # Node labels
  theme = "colorblind",              # Colorblind-friendly theme
  edge.color = NULL,                 # Edge colors based on sign
  posCol = "darkgreen",              # Color for positive edges
  negCol = "red",                    # Color for negative edges
  vsize = 7,                         # Node size
  label.cex = 1.2,                   # Label size
  edge.width = 1.5,                  # Edge width scaling
  title = "Mixed Graphical Model Network"
)

# Create a vector of custom labels
custom_labels <- c(
  "Group",            # group
  "Sex",              # sex
  "Took Math",        # nbt_did_math
  "PSY1004 Grade",    # psy1004_grade
  "NBT Math Score",   # nbt_math
  "Matric Mark",      # matric_mark
  "BDI Total",        # bdi1_total
  "AUDIT Total",      # audit1_total
  "AAS Total",        # aas1_total
  "ASRS Total",       # asrs1_total.x
  "BAI Total"         # bai1_total
)

# Plot the network with custom labels
qgraph(
  weighted_adj_matrix,
  layout = "spring",
  labels = custom_labels,
  theme = "colorblind",
  edge.color = NULL,
  posCol = "darkgreen",
  negCol = "red",
  vsize = 7,
  label.cex = 1.2,
  edge.width = 1.5,
  title = "Mixed Graphical Model Network"
)

