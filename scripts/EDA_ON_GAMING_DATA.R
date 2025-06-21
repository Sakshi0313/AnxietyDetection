#  *********************************** 1.INSTALL AND LOAD PACKAGES *****************************************************
install.packages("ggplot2")   # For data visualization
install.packages("dplyr")     # For data manipulation
install.packages("readr")     # For reading data
install.packages("tidyr")     # For data cleaning
install.packages("skimr")     # For dataset summary
install.packages("corrplot")  # For correlation matrix
install.packages("tidyverse")

# Load packages separately
library(ggplot2)   # Visualization
library(dplyr)     # Data manipulation
library(readr)     # Reading data files
library(tidyr)     # Data cleaning
library(skimr)     # Quick dataset summary
library(corrplot)  # Correlation matrix plotting

#  *********************************** 2. METADATA *****************************************************
# Read the dataset
gaming_data <- read_csv("D:/VIT/SEM 4/Data Science/CP/Shiny/gaming_anxiety_prediction/data/online_gamming_og")

# Check first few rows(By default it display 6 rows)
head(gaming_data)

# Number of rows and columns
dim(gaming_data)  # Output: (Rows, Columns)

# Column names
colnames(gaming_data)

# Data structure (types of each column)
#ALL COLUMN HAS ONLY TWO DATA TYPES EITHER DOUBLE OR CHARACTER
str(gaming_data)

# Summary of numerical and categorical data
summary(gaming_data)

# Detailed summary using skimr
skim(gaming_data)

# Count missing values in each column
colSums(is.na(gaming_data))

#  *********************************** 3. REMOVE USELESS COLUMNS *****************************************************

# This will remove 8 useless columns(Those who not contributes with project) and make the dataset cleaner.
gaming_data <- gaming_data %>% dplyr::select(-c('S. No.', highestleague, Timestamp, GADE, accept, Reference, Residence_ISO3, Birthplace_ISO3))

# This removes unwanted columns and updates the dataset.

colnames(gaming_data)

#  *********************************** 4. FIND AND HANDLE MISSING VALUES  *****************************************************

# Missing values can cause errors. We will Check which columns have missing values :
# 
# Decide how to handle them:
# 1.Remove columns if too many missing values.
# 2. Fill missing values with average (numeric columns) or "Unknown" (categorical columns).

# Count missing values in each column
colSums(is.na(gaming_data))

# 1. Fill Missing Numeric Values (Hours) with Mean:
  gaming_data$Hours[is.na(gaming_data$Hours)] <- mean(gaming_data$Hours, na.rm = TRUE)
  
# 2. Fill Missing Categorical Values (Work) with "Unknown":
  gaming_data$Work[is.na(gaming_data$Work)] <- "Unknown"

  #Verified 
  colSums(is.na(gaming_data))
    
# 3. Checked streams is useful or useless through corelation value and decision taken. 

  # Compute correlation
  cor_value <- cor(gaming_data$streams, gaming_data$GAD_T, use = "complete.obs")
  
  # Print correlation value
  print(paste("Correlation between streams and GAD_T:", cor_value))
  # It is 0.003 which states that very weak co relation matrix so removed.
  
  gaming_data <- gaming_data %>% select(-streams)

# 4.Checking League column. we created new column league_type and checked its impact. but it doesn't have impact so removed both base column and derived column.

ggplot(gaming_data, aes(x = League, y = GAD_T, fill = League)) +
  geom_boxplot() +
  labs(title = "Anxiety Levels Across Different Leagues", x = "League Type", y = "Anxiety Score")

#It has various types of Missing values like na,Na,NA.so firstly it converted into NA

#All na,Na,NA,None related values are converted to standard NA value

gaming_data$League <- ifelse(gaming_data$League %in% c("N/a","N/A","n/a","na","NA", "None", "not applicable", "Not Applicable", 
                                                       "Non-Ranked", "Never played", "No ranked games yet"), 
                             NA, gaming_data$League)

colSums(is.na(gaming_data))

#New column added named "League type" which has two categories Ranked and Unranked which created from League column data. 
gaming_data$League_Type <- ifelse(grepl("Gold|Diamond|Platinum|Silver|Bronze", gaming_data$League, ignore.case = TRUE), 
                                  "Ranked", "Unranked")


# League_Type has two categories (Ranked & Unranked), a t-test can check if the mean GAD_T scores differ significantly.

t_test_result <- t.test(GAD_T ~ League_Type, data = gaming_data)
print(t_test_result)

# •	The result is p-value =0.07 which is >0.05 so due to no strong evidence it removed.

# •	Based on the analysis, League_Type does not have a strong impact on anxiety levels. 

gaming_data <- gaming_data %>% select(-League, -League_Type)


#5. # This will remove 2 useless columns(Those who not contributes with project) and make the dataset cleaner.
gaming_data <- gaming_data %>% dplyr::select(-c('Birthplace', 'Residence'))

colSums(is.na(gaming_data))

#6. Check game column is useful or useless. then decide what to do 

length(unique(gaming_data$Game))  # Count unique games
table(gaming_data$Game)  # Frequency of each game


# Since we have a few unique games, we can classify them into game genres and analyze anxiety levels:
#   
# MOBA (Multiplayer Online Battle Arena) → League of Legends, Heroes of the Storm
# FPS (First-Person Shooter) → Counter Strike, Destiny
# MMORPG (Massively Multiplayer Online RPG) → World of Warcraft, Guild Wars 2
# Card/Strategy → Hearthstone, Starcraft 2
# RPG (Role-Playing Games) → Diablo 3, Skyrim


# This creates a new column Game_Genre that groups games into categories.

gaming_data$Game_Genre <- ifelse(gaming_data$Game %in% c("League of Legends", "Heroes of the Storm"), "MOBA",
                                 ifelse(gaming_data$Game %in% c("Counter Strike", "Destiny"), "FPS",
                                        ifelse(gaming_data$Game %in% c("World of Warcraft", "Guild Wars 2"), "MMORPG",
                                               ifelse(gaming_data$Game %in% c("Hearthstone", "Starcraft 2"), "Strategy",
                                                      ifelse(gaming_data$Game %in% c("Diablo 3", "Skyrim"), "RPG", "Other")))))

table(gaming_data$Game_Genre)

# Here...we are calculating the mean GAD_T score for each Game_Genre.
# This will show the average General Anxiety (GAD_T) and Social Anxiety (SPIN_T) for each genre.
#  If some genres have significantly higher or lower anxiety scores, the column might be useful.

aggregate(GAD_T ~ Game_Genre, data = gaming_data, FUN = mean)

aggregate(SPIN_T ~ Game_Genre, data = gaming_data, FUN = mean)

# Insights from the Results
# 1️ General Anxiety (GAD_T)
# 
# Players in RPG (5.51) and Other (5.49) genres have the highest average anxiety.
# MOBA (5.20) players also have above-average anxiety.
# FPS (5.00), MMORPG (4.92), and Strategy (4.93) have lower average anxiety.
# 2️ Social Anxiety (SPIN_T)
# 
# RPG players (24.12) have the highest social anxiety.
# MMORPG (20.92) and Other (20.70) also have elevated social anxiety.
# Strategy (18.45) and FPS (19.47) players have the lowest social anxiety


#  Perform a Statistical Test (ANOVA)
# This test checks whether the differences in anxiety between game genres are statistically significant.

anova_GAD <- aov(GAD_T ~ Game_Genre, data = gaming_data)
summary(anova_GAD)  # Check p-value for GAD_T

#This will gives results in : 

# p-value = 0.213 (greater than 0.05) → No significant difference in General Anxiety (GAD_T) across game genres.
# The F-value (1.422) is small, meaning the variation between groups is weak.
# Conclusion: Game Genre does NOT impact General Anxiety significantly.

anova_SPIN <- aov(SPIN_T ~ Game_Genre, data = gaming_data)
summary(anova_SPIN)  # Check p-value for SPIN_T

# #This will gives results in : 
# p-value = 0.000864 (less than 0.05) → Significant difference in Social Anxiety (SPIN_T) across game genres.
# The F-value (4.174) is moderate, meaning there is some variation between groups.
# Conclusion: Game Genre has a statistically significant impact on Social Anxiety (SPIN_T)

# Now decided to remove both the columns due to it does not affect it significantly.

gaming_data <- subset(gaming_data, select = -c(Game, Game_Genre))


colSums(is.na(gaming_data))

#7. Remove the platform column.

# In general, the Platform column (like PC, PlayStation, Xbox, etc.) is usually not very useful for predicting psychological aspects like anxiety (GAD_T, SPIN_T) 
# unless there is a strong reason to believe that a particular platform significantly affects gaming behavior or mental health


gaming_data <- subset(gaming_data, select = -Platform)


#8. remove the earnings column. 

# 1️ Check if both columns have similar values → If earnings and whyplay mostly contain the same information, keeping both is unnecessary.
# 2️ Compare their usefulness:
#   
#   earnings tells if the player plays for earning or for fun .
# whyplay explains why they play (for fun, competition, relaxation, etc.).
# Conclusion:
#   
#   whyplay provides more detailed insights about a player's motivation, which can help in anxiety prediction.
# earnings is a simpler Yes/No column, which can be derived from whyplay.
# Decision:
# Remove earnings (less informative)
# Keep whyplay (gives deeper insights into gaming behavior)


gaming_data <- subset(gaming_data, select = -earnings)

colSums(is.na(gaming_data))

#9. Handled the missing values of spin1 to spin17

mean(gaming_data$SPIN1, na.rm = TRUE)


# We avoid the mean because SPIN1 has discrete values (0 to 4), and filling with a decimal (e.g., 0.86) makes interpretation difficult.
# Instead, we use the median (median value) to maintain categorical consistency and data integrity.


median_value <- median(gaming_data$SPIN1, na.rm = TRUE)
print(median_value)
gaming_data$SPIN1[is.na(gaming_data$SPIN1)] <- median_value



# code to replace missing values with the median for all SPIN2 to SPIN17 columns in one go:

for (col in paste0("SPIN", 2:17)) {
  median_value <- median(gaming_data[[col]], na.rm = TRUE)
  gaming_data[[col]][is.na(gaming_data[[col]])] <- median_value
}

# This loop automatically calculates the median for each SPIN column and replaces missing values accordingly. 


colSums(is.na(gaming_data))

#10. Handling the missing value of Narcissism column. 

str(gaming_data$Narcissism)  # Check data type
summary(gaming_data$Narcissism)  # Get min, max, mean, etc.


hist(gaming_data$Narcissism, main = "Distribution of Narcissism Scores", col = "blue")


# Compute correlation with GAD_T
cor_value2 <- cor(gaming_data$Narcissism, gaming_data$GAD_T, use = "complete.obs")

# Print correlation value
print(paste("Correlation between Narcissim and GAD_T:", cor_value2))


# Compute correlation with SAD_T
cor_value2 <- cor(gaming_data$Narcissism, gaming_data$SWL_T, use = "complete.obs")



# Print correlation value
print(paste("Correlation between Narcissim and SWL_T:", cor_value2))

# FOR GAD_T => Since 0.07 is close to 0, it means almost no relationship between the two variables.
# FOR SAD_T => Since -0.014 is very close to 0, it means almost no relationship between the two variables.

# ✅ REMOVE the Narcissism column because:
#   
#   It has almost no impact on anxiety prediction.
# The correlation with General Anxiety is too weak to be useful.
# No correlation at all with Social Anxiety 

gaming_data <- subset(gaming_data, select = -Narcissism)


#11 . Handling missing values of SPIN_T column.

summary(gaming_data$SPIN_T)  # Get min, max, mean, median
hist(gaming_data$SPIN_T, main="SPIN_T Distribution", col="blue")  # Check normality

# Since the SPIN_T column is right-skewed, the best approach is to use median imputation because the median is less affected by extreme values (outliers).


median_value <- median(gaming_data$SPIN_T, na.rm = TRUE)
print(median_value)
gaming_data$SPIN_T[is.na(gaming_data$SPIN_T)] <- median_value


# Boxplot BEFORE outlier removal

boxplot(gaming_data$SPIN_T,
        main = "SPIN_T - Before Outlier Removal",
        ylab = "SPIN_T Values",
        col = "orange",
        horizontal = TRUE)

# Step 1: Impute missing values using median
median_value <- median(gaming_data$SPIN_T, na.rm = TRUE)
gaming_data$SPIN_T[is.na(gaming_data$SPIN_T)] <- median_value




# Step 2: Remove outliers using a slightly stricter IQR method
Q1 <- quantile(gaming_data$SPIN_T, 0.25)
Q3 <- quantile(gaming_data$SPIN_T, 0.75)
IQR_value <- Q3 - Q1
lower_bound <- Q1 - 1.25 * IQR_value   # was 1.5
upper_bound <- Q3 + 1.25 * IQR_value


gaming_data <- gaming_data[gaming_data$SPIN_T >= lower_bound & gaming_data$SPIN_T <= upper_bound, ]


boxplot(gaming_data$SPIN_T,
        main = "SPIN_T - After IQR Outlier Removal",
        ylab = "SPIN_T Values",
        col = "lightgreen",
        horizontal = TRUE)



# Step 3: Log transform (add 1 to avoid log(0))
gaming_data$SPIN_T_log <- log(gaming_data$SPIN_T + 1)

# Step 4: Remove new outliers using tighter Z-score method
z_scores <- scale(gaming_data$SPIN_T_log)
gaming_data <- gaming_data[abs(z_scores) <= 2.5, ]  # was ±3

# Step 5: Final histogram to check near-normal distribution
hist(gaming_data$SPIN_T_log,
     main = "Improved Log Transformed SPIN_T (Tighter Outlier Removal)",
     xlab = "Log(SPIN_T + 1)",
     col = "forestgreen")



colSums(is.na(gaming_data))


#write.csv(gaming_data, "D:/VIT/SEM 4/Data Science/CP/Shiny/gaming_anxiety_prediction/data/cleaned_gaming_dataset_final.csv", row.names = FALSE)

dim(gaming_data)
######################## DONE WITH HANDLING ALL MISSING VALUES ###################################################


# ADD EXTRA COLUMNS FROM EXISTING COLUMNS 


# 1. NEW PANIC_DISORDER COLUMN CREATED

gaming_data$Panic_Disorder <- ifelse(gaming_data$SPIN_T > 30 & gaming_data$GAD_T > 12, "High Panic Risk",
                                     ifelse(gaming_data$SPIN_T > 20 & gaming_data$GAD_T > 8, "Moderate Panic Risk", "No Panic Disorder"))

gaming_data$Panic_Disorder <- as.factor(gaming_data$Panic_Disorder)  # Convert to categorical factor


# ✔ High Panic Risk → If SPIN_T > 30 and GAD_T > 12
# ✔ Moderate Panic Risk → If SPIN_T > 20 and GAD_T > 8
# ✔ No Panic Disorder → Otherwise



# 2. NEW OCD_Risk COLUMN CREATED


gaming_data$OCD_Risk <- ifelse(
  gaming_data$SPIN_T > 25 & (gaming_data$Hours / 7) > 5, "High OCD Risk",
  ifelse(gaming_data$SPIN_T > 15 & (gaming_data$Hours / 7) >= 3, "Moderate OCD Risk", "No OCD")
)

gaming_data$OCD_Risk <- as.factor(gaming_data$OCD_Risk)  # Convert to categorical factor

# We convert weekly hours to daily hours by dividing by 7: gaming_data$Hours / 7
# If SPIN_T > 25 and plays more than 5 hours/day → "High OCD Risk"
# If SPIN_T > 15 and plays 3-5 hours/day → "Moderate OCD Risk"
# Else → "No OCD"


write.csv(gaming_data, "D:/VIT/SEM 4/Data Science/CP/Shiny/gaming_anxiety_prediction/data/cleaned_gaming_dataset_final.csv", row.names = FALSE)


# ==============================================================================================================================================================

# ADDING NEW COLUMN ANXITY TYPE FROM LABELLED DATA OF INDIVIDUALS 

# Load necessary libraries
library(tidyverse)

# Step 1: Load the dataset
gaming_data <- read.csv("D:/VIT/SEM 4/Data Science/CP/Shiny/gaming_anxiety_prediction/data/cleaned_gaming_dataset_final.csv")

# Step 2: Add Anxiety_Type column based on logical conditions
gaming_data$Anxiety_Type <- case_when(
  gaming_data$GAD_T > 10 ~ "GAD",
  gaming_data$SWL_T < 15 ~ "SWL",
  gaming_data$SPIN_T > 20 ~ "SAD",
  gaming_data$Panic_Disorder == "High Panic Risk " ~ "Severe Panic Disorder",
  gaming_data$Panic_Disorder == "Moderate Panic Risk" ~ "Moderate Panic Disorder",
  gaming_data$OCD_Risk == "High OCD Risk" ~ "Severe OCD",
  gaming_data$OCD_Risk == "Moderate OCD Risk" ~ "Moderate OCD",
  TRUE ~ "No Anxiety"
) 

# Step 3: Convert Anxiety_Type to a factor
gaming_data$Anxiety_Type <- as.factor(gaming_data$Anxiety_Type)

# Step 4: Save the modified dataset
write.csv(gaming_data, "D:/VIT/SEM 4/Data Science/CP/Shiny/gaming_anxiety_prediction/data/cleaned_gaming_dataset_final.csv", row.names = FALSE)


# Install the package if not already done
install.packages("DataExplorer")

# Load the package
library(DataExplorer)

# Generate HTML profiling report
create_report(gaming_data, 
              output_file = "gaming_data_profiling_report.html", 
              output_dir = getwd(), 
              y = "Anxiety_Type")  # Optional: set target variable
### _._ ###
