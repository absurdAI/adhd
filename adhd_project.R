# Load necessary libraries
library(dplyr)
library(stringr)

# Step 1: Load the CSV file into R
adhd_data <- read.csv("C:/Users/poczt/OneDrive/Documents/learnig/ADHD_converted.csv")

# Step 2: Drop unnecessary columns to simplify the dataset
columns_to_drop <- c(
  "age", 
  "specify", 
  "home_language", 
  "if_yes_please_list_these_difficulties_and_or_symptoms",
  "if_you_have_ever_experienced_difficulties_and_or_symptoms_of_a_mental_illness_how_old_were_you_when_this_started",
  "was_this_diagnosis_made_before_or_after_you_left_high_school",
  "if_you_have_been_diagnosed_with_a_mental_illness_at_what_age_was_this"
)
adhd_cleaned <- adhd_data %>% select(-one_of(columns_to_drop))

# Step 3: Map 'have_you_ever_been_diagnosed_with_a_mental_illness' to 'Yes' and 'No'
diagnosis_mapping <- c(
  "no" = "No",
  "not formally diagnosed" = "No",
  "yes, formally diagnosed by a doctor or mental health professional" = "Yes"
)

adhd_cleaned <- adhd_cleaned %>%
  mutate(
    diagnosed_bin = diagnosis_mapping[trimws(tolower(have_you_ever_been_diagnosed_with_a_mental_illness))]
  )

# Verify the mapping
print("Distribution of 'diagnosed_bin' after mapping:")
print(table(adhd_cleaned$diagnosed_bin, useNA = "ifany"))

# Step 4: Filter the data to include only those who were formally diagnosed ("Yes")
diagnosed_yes <- adhd_cleaned %>%
  filter(diagnosed_bin == "Yes")

# Step 5: Clean the diagnosis descriptions in 'if_you_have_been_diagnosed_formally_or_informally_please_list_the_diagnosis_diagnoses'

# Correct the "0" issue by replacing " 0 " with " not "
diagnosed_yes <- diagnosed_yes %>%
  mutate(
    diagnosis_cleaned = tolower(if_you_have_been_diagnosed_formally_or_informally_please_list_the_diagnosis_diagnoses),
    diagnosis_cleaned = str_replace_all(diagnosis_cleaned, "\\b0\\b", "not")
  )

# Step 6: Define diagnostic categories and associated keywords
diagnosis_categories <- list(
  Depression = c("depression", "depressed", "major depressive disorder", "mdd", "pdd", "dysthymia", "mood disorder", "severe depressive episodes", "situational depression", "minor depression", "clinical depression"),
  Anxiety = c("anxiety", "panic disorder", "social anxiety", "performance anxiety", "generalized anxiety disorder", "gad", "chronic anxiety", "panic attacks", "somniphobia", "tension headaches"),
  ADHD = c("adhd", "add", "attention deficit"),
  Bipolar = c("bipolar"),
  OCD = c("ocd", "obsessive-compulsive"),
  PTSD = c("ptsd", "post[- ]traumatic stress"),
  Autism = c("autism", "asperger"),
  Eating_Disorder = c("anorexia", "bulimia", "eating disorder", "maladaptive daydreaming disorder"),
  Personality_Disorder = c("personality disorder", "bpd", "borderline", "dissociative disorder"),
  Tourette = c("tourette"),
  Insomnia = c("insomnia"),
  Substance_Use = c("substance use disorder", "addiction"),
  Other = c("gender dysphoria", "asthma", "tourettes syndrome")
)

# Initialize binary columns for each category
for (category in names(diagnosis_categories)) {
  diagnosed_yes[[category]] <- 0
}

# Step 7: Assign categories based on keywords
for (i in seq_len(nrow(diagnosed_yes))) {
  diagnosis_text <- diagnosed_yes$diagnosis_cleaned[i]
  
  # Remove punctuation and extra spaces
  diagnosis_text <- str_replace_all(diagnosis_text, "[[:punct:]]", " ")
  diagnosis_text <- str_squish(diagnosis_text)
  
  # Handle the "not" issue by removing phrases that indicate absence
  if (str_detect(diagnosis_text, "\\bnot\\b")) {
    next  # Skip this entry as it indicates no diagnosis
  }
  
  for (category in names(diagnosis_categories)) {
    keywords <- diagnosis_categories[[category]]
    pattern <- paste0("\\b(", paste(keywords, collapse = "|"), ")\\b")
    if (str_detect(diagnosis_text, pattern)) {
      diagnosed_yes[[category]][i] <- 1
    }
  }
}

# Step 8: Review the categorization
# Summarize the counts for each category
category_counts <- colSums(diagnosed_yes[, names(diagnosis_categories)])
print("Counts of each diagnostic category:")
print(category_counts)

# Step 9: Create a summary of diagnoses per individual
diagnosed_summary <- diagnosed_yes %>%
  select(sex, diagnosed_bin, diagnosis_cleaned, names(diagnosis_categories))

# View the first few rows of the summary
print("Sample of the categorized diagnoses:")
print(head(diagnosed_summary, n = 10))

# Optional: Save the categorized data to a new CSV file
# write.csv(diagnosed_summary, "Diagnosed_Categorized.csv", row.names = FALSE)

# CODE1: Check for Missing Data in the 'diagnosed_summary' Dataset

# Load necessary libraries
library(dplyr)
library(tidyr)

# Calculate the number of missing values per column
missing_data <- diagnosed_summary %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "missing_count") %>%
  arrange(desc(missing_count))

# Print the missing data summary
print("Missing Data Summary:")
print(missing_data)

# CODE2 Revised: Select All Data, Create Binary Diagnostic Variables, Merge with Additional Variables, and Check Missing Data

# Load necessary libraries
library(dplyr)
library(stringr)
library(tidyr)

# Step 1: Load the CSV file into R (if not already loaded)
# Ensure 'adhd_cleaned' is in your environment
# If not, uncomment and run the following line:
# adhd_cleaned <- read.csv("C:/Users/poczt/OneDrive/Documents/learnig/ADHD_converted.csv") %>% select(-one_of(columns_to_drop))

# Step 2: Create Binary Diagnostic Variables for All Students

# Define diagnostic categories and associated keywords
diagnosis_categories <- list(
  Depression = c("depression", "depressed", "major depressive disorder", "mdd", "pdd", "dysthymia", "mood disorder", "severe depressive episodes", "situational depression", "minor depression", "clinical depression"),
  Anxiety = c("anxiety", "panic disorder", "social anxiety", "performance anxiety", "generalized anxiety disorder", "gad", "chronic anxiety", "panic attacks", "somniphobia", "tension headaches"),
  ADHD = c("adhd", "add", "attention deficit"),
  Bipolar = c("bipolar"),
  OCD = c("ocd", "obsessive-compulsive"),
  PTSD = c("ptsd", "post[- ]traumatic stress"),
  Autism = c("autism", "asperger"),
  Eating_Disorder = c("anorexia", "bulimia", "eating disorder", "maladaptive daydreaming disorder"),
  Personality_Disorder = c("personality disorder", "bpd", "borderline", "dissociative disorder"),
  Tourette = c("tourette"),
  Insomnia = c("insomnia"),
  Substance_Use = c("substance use disorder", "addiction"),
  Other = c("gender dysphoria", "asthma", "tourettes syndrome")
)

# Initialize binary columns for each category in 'adhd_cleaned'
for (category in names(diagnosis_categories)) {
  adhd_cleaned[[category]] <- 0
}

# Clean the diagnosis descriptions
adhd_cleaned <- adhd_cleaned %>%
  mutate(
    diagnosis_cleaned = tolower(if_you_have_been_diagnosed_formally_or_informally_please_list_the_diagnosis_diagnoses),
    diagnosis_cleaned = str_replace_all(diagnosis_cleaned, "\\b0\\b", "not"), # Replace standalone "0" with "not"
    diagnosis_cleaned = str_replace_all(diagnosis_cleaned, "[[:punct:]]", " "),  # Remove punctuation
    diagnosis_cleaned = str_squish(diagnosis_cleaned)                            # Remove extra spaces
  )

# Assign categories based on keywords for all students
for (i in seq_len(nrow(adhd_cleaned))) {
  diagnosis_text <- adhd_cleaned$diagnosis_cleaned[i]
  
  # Handle the "not" issue by setting all categories to 0 if "not" is present
  if (str_detect(diagnosis_text, "\\bnot\\b")) {
    # Ensure all diagnostic categories are 0
    adhd_cleaned[i, names(diagnosis_categories)] <- 0
    next
  }
  
  for (category in names(diagnosis_categories)) {
    keywords <- diagnosis_categories[[category]]
    pattern <- paste0("\\b(", paste(keywords, collapse = "|"), ")\\b")
    if (str_detect(diagnosis_text, pattern)) {
      adhd_cleaned[[category]][i] <- 1
    }
  }
}

# Verify the categorization
category_counts <- colSums(adhd_cleaned[, names(diagnosis_categories)])
print("Counts of each diagnostic category:")
print(category_counts)

# Step 3: Create 'diagnosed_bin' to indicate if any diagnosis is present
adhd_cleaned <- adhd_cleaned %>%
  mutate(
    diagnosed_bin = if_else(rowSums(select(., all_of(names(diagnosis_categories)))) > 0, "Yes", "No")
  )

# Step 4: Create 'diagnosed_summary' with relevant variables
diagnosed_summary <- adhd_cleaned %>%
  select(
    sex,
    diagnosed_bin,
    diagnosis_cleaned,
    all_of(names(diagnosis_categories))
  )

# Step 5: Select Academic Performance Variables
academic_performance_vars <- adhd_cleaned %>%
  select(
    psy1004_grade,
    nbt_completed,
    nbt_year,
    nbt_al,
    nbt_math,
    nbt_ql,
    nbt_ave,
    nbt_did_math,
    nbt_alql_ave,
    matric_mark
  )

# Step 6: Select Psychological Measures
psychological_measures_vars <- adhd_cleaned %>%
  select(
    bdi1_total,
    audit1_total,
    aas1_total,
    asrs1_total.x,
    bai1_total
  )

# Step 7: Combine All Selected Variables with 'diagnosed_summary'
network_data <- diagnosed_summary %>%
  bind_cols(academic_performance_vars, psychological_measures_vars)

# Step 8: Check for Missing Data in 'network_data'
missing_data_network <- network_data %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "missing_count") %>%
  arrange(desc(missing_count))

# Print the missing data summary
print("Missing Data Summary for Network Variables:")
print(missing_data_network)

# Install psychonetrics if it's not already installed
if (!requireNamespace("psychonetrics", quietly = TRUE)) {
  install.packages("psychonetrics")
}

# Load the psychonetrics package
library(psychonetrics)

# CODE3 Revised: Convert to Character and Recode Binary Variables

# Load necessary libraries
library(dplyr)
library(stringr)

# Define binary variables including 'sex', 'diagnosed_bin', 'nbt_completed', 'nbt_did_math'
binary_vars <- c(
  names(diagnosis_categories),  # Diagnostic binary variables
  "sex",
  "diagnosed_bin",
  "nbt_completed",
  "nbt_did_math"
)

# Define continuous variables, excluding the binary ones
continuous_vars <- c(
  "psy1004_grade",
  "nbt_year",
  "nbt_al",
  "nbt_math",
  "nbt_ql",
  "nbt_ave",
  "nbt_alql_ave",
  "matric_mark",
  "bdi1_total",
  "audit1_total",
  "aas1_total",
  "asrs1_total.x",
  "bai1_total"
)

# Recode binary variables
network_data <- network_data %>%
  mutate(
    # Convert to character to ensure correct recoding
    sex = as.character(sex),
    diagnosed_bin = as.character(diagnosed_bin),
    nbt_completed = as.character(nbt_completed),
    nbt_did_math = as.character(nbt_did_math),
    
    # Recode 'sex': male = 0, female = 1, other = 2
    sex = case_when(
      tolower(sex) == "male" ~ 0,
      tolower(sex) == "female" ~ 1,
      tolower(sex) %in% c("other", "non-binary", "prefer not to say", "prefer to self-describe") ~ 2,
      TRUE ~ NA_real_  # Assign NA for any unexpected values
    ),
    
    # Recode 'diagnosed_bin': No = 0, Yes = 1
    diagnosed_bin = case_when(
      tolower(diagnosed_bin) == "no" ~ 0,
      tolower(diagnosed_bin) == "yes" ~ 1,
      TRUE ~ NA_real_
    ),
    
    # Recode 'nbt_completed': no = 0, yes = 1, treat blanks as 'no' (0)
    nbt_completed = case_when(
      tolower(nbt_completed) == "no" ~ 0,
      tolower(nbt_completed) == "yes" ~ 1,
      is.na(nbt_completed) | trimws(nbt_completed) == "" ~ 0,  # Treat blanks as 'no'
      TRUE ~ NA_real_  # Assign NA for any unexpected entries
    ),
    
    # Recode 'nbt_did_math': no = 0, yes = 1, treat blanks as 'no' (0)
    nbt_did_math = case_when(
      tolower(nbt_did_math) == "no" ~ 0,
      tolower(nbt_did_math) == "yes" ~ 1,
      is.na(nbt_did_math) | trimws(nbt_did_math) == "" ~ 0,  # Treat blanks as 'no'
      TRUE ~ NA_real_  # Assign NA for any unexpected entries
    )
  )

# CODE4: Verify the Recoding

# Print the summary of binary variables after recoding
print("Summary of Binary Variables After Recoding:")
print(network_data %>%
        select(all_of(binary_vars)) %>%
        summary())

# Additionally, inspect unique values to confirm
print("Unique entries in 'sex' after recoding:")
print(unique(network_data$sex))

print("Unique entries in 'diagnosed_bin' after recoding:")
print(unique(network_data$diagnosed_bin))

print("Unique entries in 'nbt_completed' after recoding:")
print(unique(network_data$nbt_completed))

print("Unique entries in 'nbt_did_math' after recoding:")
print(unique(network_data$nbt_did_math))

# CODE5: Exclude Cases with Any Missing Data

# Check how many rows have any NA values
rows_with_na <- network_data %>%
  filter(if_any(everything(), is.na)) %>%
  nrow()

print(paste("Number of rows with any NA values:", rows_with_na))

# Exclude rows with any NA values
network_data_clean <- network_data %>%
  drop_na()

print(paste("Number of rows after excluding missing data:", nrow(network_data_clean)))

# CODE6: Standardize Continuous Variables

network_data_clean <- network_data_clean %>%
  mutate(across(all_of(continuous_vars), ~ scale(.)[, 1]))

# Verify the changes
print("Structure of 'network_data_clean' After Standardization:")
str(network_data_clean)

# Optional: View summary statistics to confirm standardization
print("Summary of Continuous Variables After Standardization:")
print(network_data_clean %>%
        select(all_of(continuous_vars)) %>%
        summary())

# CODE7: Final Missing Data Check

missing_data_final <- network_data_clean %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "missing_count") %>%
  arrange(desc(missing_count))

print("Final Missing Data Summary for Network Variables:")
print(missing_data_final)

# CODE4: Exclude Cases with Any Missing Data

# Load necessary libraries
library(dplyr)
library(tidyr)

# Check how many rows have any NA values
rows_with_na <- network_data %>%
  filter(if_any(everything(), is.na)) %>%
  nrow()

print(paste("Number of rows with any NA values:", rows_with_na))

# Exclude rows with any NA values
network_data_clean <- network_data %>%
  drop_na()

print(paste("Number of rows after excluding missing data:", nrow(network_data_clean)))

# CODE7: Define the Network Model Specification

# Reinstall psychonetrics from CRAN
install.packages("psychonetrics")

# Load the psychonetrics package
library(psychonetrics)

# Confirm the package version
packageVersion("psychonetrics")

# List all objects in the psychonetrics package
ls("package:psychonetrics")

# Check if 'add_node' is among the functions in psychonetrics
"add_node" %in% ls("package:psychonetrics")

# Define the model specification
model_spec <- model("Network Model") %>%
  # Define the nodes (variables) in the network
  add_node("sex", type = "categorical") %>%
  add_node("diagnosed_bin", type = "binary") %>%
  add_node("Depression", type = "binary") %>%
  add_node("Anxiety", type = "binary") %>%
  add_node("ADHD", type = "binary") %>%
  add_node("Bipolar", type = "binary") %>%
  add_node("OCD", type = "binary") %>%
  add_node("PTSD", type = "binary") %>%
  add_node("Autism", type = "binary") %>%
  add_node("Eating_Disorder", type = "binary") %>%
  add_node("Personality_Disorder", type = "binary") %>%
  add_node("Tourette", type = "binary") %>%
  add_node("Insomnia", type = "binary") %>%
  add_node("Substance_Use", type = "binary") %>%
  add_node("Other", type = "binary") %>%
  add_node("psy1004_grade", type = "continuous") %>%
  add_node("nbt_year", type = "continuous") %>%
  add_node("nbt_al", type = "continuous") %>%
  add_node("nbt_math", type = "continuous") %>%
  add_node("nbt_ql", type = "continuous") %>%
  add_node("nbt_ave", type = "continuous") %>%
  add_node("nbt_alql_ave", type = "continuous") %>%
  add_node("matric_mark", type = "continuous") %>%
  add_node("bdi1_total", type = "continuous") %>%
  add_node("audit1_total", type = "continuous") %>%
  add_node("aas1_total", type = "continuous") %>%
  add_node("asrs1_total.x", type = "continuous") %>%
  add_node("bai1_total", type = "continuous")

# Print the model specification
print(model_spec)

# Install mgm package if not already installed
if (!requireNamespace("mgm", quietly = TRUE)) {
  install.packages("mgm")
  # Load the mgm package
  library(mgm)
  
  # Load other necessary libraries
  library(dplyr)
  library(stringr)
  library(tidyr)  
  # Create dummy variables for 'sex'
  network_data_clean <- network_data_clean %>%
    mutate(
      sex_male = if_else(sex == 0, 1, 0),
      sex_female = if_else(sex == 1, 1, 0),
      sex_other = if_else(sex == 2, 1, 0)
    ) %>%
    # Remove the original 'sex' variable as it's now represented by dummy variables
    select(-sex)
  # Update binary variables list to include dummy variables for 'sex'
  binary_vars_updated <- c(
    names(diagnosis_categories),  # Diagnostic binary variables
    "diagnosed_bin",
    "nbt_completed",
    "nbt_did_math",
    "sex_male",
    "sex_female",
    "sex_other"
  )
  
  # Continuous variables remain the same
  continuous_vars <- c(
    "psy1004_grade",
    "nbt_year",
    "nbt_al",
    "nbt_math",
    "nbt_ql",
    "nbt_ave",
    "nbt_alql_ave",
    "matric_mark",
    "bdi1_total",
    "audit1_total",
    "aas1_total",
    "asrs1_total.x",
    "bai1_total"
  )
  # Verify binary variables are numeric
  network_data_clean <- network_data_clean %>%
    mutate(across(all_of(binary_vars_updated), ~ as.numeric(.)))
  
  # Verify continuous variables are numeric
  network_data_clean <- network_data_clean %>%
    mutate(across(all_of(continuous_vars), ~ as.numeric(.)))
  
  # Check the structure
  str(network_data_clean)
  
  # Remove the 'diagnosis_cleaned' variable
  network_data_clean <- network_data_clean %>%
    select(-diagnosis_cleaned)
  
  # Verify the removal
  str(network_data_clean)

  # Final check for missing data
  missing_data_final <- network_data_clean %>%
    summarise(across(everything(), ~ sum(is.na(.)))) %>%
    pivot_longer(cols = everything(), names_to = "variable", values_to = "missing_count") %>%
    arrange(desc(missing_count))
  
  print("Final Missing Data Summary for Network Variables:")
  print(missing_data_final)

  # Define types: 1 for binary, 0 for continuous
  types <- c(
    rep(1, length(binary_vars_updated)),  # Binary variables
    rep(0, length(continuous_vars))       # Continuous variables
  )
  
  # Define levels: 2 for binary, 0 for continuous
  levels <- c(
    rep(2, length(binary_vars_updated)),  # Binary variables
    rep(0, length(continuous_vars))       # Continuous variables
  )

  # Fit the mixed graphical model with corrected arguments
  mgm_fit <- mgm(
    data = network_data_clean,
    type = types,
    level = levels,
    k = 2,
    lambdaSel = "EBIC",
    ruleReg = "AND"
  )
  
  # View the summary of the fitted model
  print(mgm_fit)
  
  
  
  
  