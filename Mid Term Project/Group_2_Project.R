install.packages("dplyr")
library(dplyr)
install.packages("readxl"); 
library(readxl); 

dataSet_1 <- read_excel("C:/Users/ASUS/OneDrive - American International University-Bangladesh/Desktop/Fall 24/IDS/MID/Project/Midterm_Dataset_Section(C).xlsx");
print(dataSet_1, n = nrow(dataSet_1)); 

no_of_col <- ncol(dataSet_1)
no_of_row <- nrow(dataSet_1)
cat("No of row in the dataset: ", no_of_row) 
cat("No of column in the dataset: ", no_of_col) 
str(dataSet_1) 

unique(dataSet_1$person_age)
unique(dataSet_1$person_gender)

remo_dupli_dataset <- distinct(dataSet_1);
remo_dupli_dataset 

cat ("No of row and column after removing duplicate instances: ", nrow(remo_dupli_dataset), ncol(remo_dupli_dataset))


fresh_dataset <- remo_dupli_dataset; 

unique(fresh_dataset$person_gender)
unique(fresh_dataset$person_education)
unique(fresh_dataset$person_home_ownership)
unique(fresh_dataset$loan_intent)
unique(fresh_dataset$previous_loan_defaults_on_file);

fresh_dataset$person_age[is.na(as.numeric(as.character(fresh_dataset$person_age)))]
fresh_dataset$person_income[is.na(as.numeric(as.character(fresh_dataset$person_income)))]
fresh_dataset$person_emp_exp[is.na(as.numeric(as.character(fresh_dataset$person_emp_exp)))]
fresh_dataset$loan_amnt[is.na(as.numeric(as.character(fresh_dataset$loan_amnt)))]
fresh_dataset$loan_int_rate[is.na(as.numeric(as.character(fresh_dataset$loan_int_rate)))]
fresh_dataset$loan_percent_income[is.na(as.numeric(as.character(fresh_dataset$loan_percent_income)))]
fresh_dataset$cb_person_cred_hist_length[is.na(as.numeric(as.character(fresh_dataset$cb_person_cred_hist_length)))]
fresh_dataset$credit_score[is.na(as.numeric(as.character(fresh_dataset$credit_score)))]
fresh_dataset$loan_status[is.na(as.numeric(as.character(fresh_dataset$loan_status)))]


deal_invalid_dataset <- fresh_dataset;

deal_invalid_dataset$person_home_ownership <- ifelse(
  substr(toupper(deal_invalid_dataset$person_home_ownership), 1, 2) == "OT", "OTHER",  
  ifelse(
    substr(toupper(deal_invalid_dataset$person_home_ownership), 1, 1) == "O", "OWN",  
    ifelse(
      substr(toupper(deal_invalid_dataset$person_home_ownership), 1, 1) == "R", "RENT",  
      ifelse(
        substr(toupper(deal_invalid_dataset$person_home_ownership), 1, 1) == "M", "MORTGAGE",  
        "NA"  
      )
    )
  )
)


unique(deal_invalid_dataset $person_home_ownership)


fresh_dataset <- deal_invalid_dataset;

deal_miss_value_dataset <- fresh_dataset;
colSums(is.na(deal_miss_value_dataset));
which(is.na(deal_miss_value_dataset$ person_age))

deal_miss_value_dataset <- na.omit(deal_miss_value_dataset); 
colSums(is.na(deal_miss_value_dataset));

install.packages('tidyr')
library(tidyr)

top_down_dataset <- fresh_dataset %>% fill(person_age,person_gender, person_education, person_income,loan_percent_income, loan_status, .direction = 'down')
colSums(is.na(top_down_dataset));

bottom_up_dataset <- fresh_dataset %>% fill(person_age,person_gender, person_education, person_income,loan_percent_income, loan_status, .direction = 'up')
colSums(is.na(bottom_up_dataset));



deal_miss_value_mode <- fresh_dataset;

mode_person_gender <- names(sort(table(deal_miss_value_mode$person_gender), decreasing = TRUE))[1]
deal_miss_value_mode$person_gender[is.na(deal_miss_value_mode$person_gender)] <- mode_person_gender

mode_person_education <- names(sort(table(deal_miss_value_mode$person_education), decreasing = TRUE))[1]
deal_miss_value_mode$person_education[is.na(deal_miss_value_mode$person_education)] <- mode_person_education

mode_person_home_ownership <- names(sort(table(deal_miss_value_mode$person_home_ownership), decreasing = TRUE))[1]
deal_miss_value_mode$person_home_ownership[is.na(deal_miss_value_mode$person_home_ownership)] <- mode_person_home_ownership

mode_loan_intent <- names(sort(table(deal_miss_value_mode$loan_intent), decreasing = TRUE))[1]
deal_miss_value_mode$loan_intent[is.na(deal_miss_value_mode$loan_intent)] <- mode_loan_intent

mode_previous_loan_defaults_on_file <- names(sort(table(deal_miss_value_mode$previous_loan_defaults_on_file), decreasing = TRUE))[1]
deal_miss_value_mode$previous_loan_defaults_on_file[is.na(deal_miss_value_mode$previous_loan_defaults_on_file)] <- mode_previous_loan_defaults_on_file


colSums(is.na(deal_miss_value_mode))


deal_miss_value_mean <- deal_miss_value_mode;

for(col_name in c("person_age", "person_income", "loan_percent_income", "loan_status")) {
  if(is.numeric(deal_miss_value_mean[[col_name]])) {
  
    column_mean <- mean(deal_miss_value_mean[[col_name]], na.rm = TRUE)
    deal_miss_value_mean[[col_name]][is.na(deal_miss_value_mean[[col_name]])] <- column_mean
    deal_miss_value_mean[[col_name]] <- round(deal_miss_value_mean[[col_name]], digits = 0)
  }
}

colSums(is.na(deal_miss_value_mean))


fresh_dataset <- deal_miss_value_dataset;

dataSet_num <- fresh_dataset; 
dataSet_num$person_gender <- factor(dataSet_num$person_gender, levels = c("male", "female"), labels = c(1,2));
dataSet_num$person_education <-  factor(dataSet_num$person_education, levels = c("High School", "Bachelor", "Master", "Associate", "Doctorate"), labels = c(1,2,3,4,5));
dataSet_num$loan_intent <-  factor(dataSet_num$loan_intent, levels = c("PERSONAL","EDUCATION","MEDICAL","VENTURE","HOMEIMPROVEMENT", "DEBTCONSOLIDATION"), labels = c(1,2,3,4,5, 6));
dataSet_num$person_home_ownership <-  factor(dataSet_num$person_home_ownership, levels = c("RENT","OWN","MORTGAGE","OTHER"), labels = c(1,2,3,4));
dataSet_num$previous_loan_defaults_on_file <-  factor(dataSet_num$previous_loan_defaults_on_file, levels = c("Yes", "No"), labels = c(1,2));
dataSet_num

fresh_dataset <- dataSet_num

detect_outlier <- function(dataframe, columns) {
  for (col in columns) {
    if (is.numeric(dataframe[[col]])) {
      Quantile1 <- quantile(dataframe[[col]], probs = 0.25)
      Quantile3 <- quantile(dataframe[[col]], probs = 0.75)
      IQR <- Quantile3 - Quantile1

      outlier_flags <- dataframe[[col]] > Quantile3 + (IQR * 1.5) | dataframe[[col]] < Quantile1 - (IQR * 1.5)

      outliers <- dataframe[[col]][outlier_flags]
      if (length(outliers) > 0) {
        cat("Outliers detected in column", col, ":\n")
        print(outliers)
      } else {
        cat("No outliers detected in column", col, "\n")
      }
    } else {
      cat("Column", col, "is not numeric, skipped\n")
    }
  }
}
detect_outlier(fresh_dataset, names(fresh_dataset))

remove_outlier <- function(dataframe, columns) {
  for (col in columns) {
    if (is.numeric(dataframe[[col]])) {
      Quantile1 <- quantile(dataframe[[col]], probs = 0.25)
      Quantile3 <- quantile(dataframe[[col]], probs = 0.75)
      IQR <- Quantile3 - Quantile1

      dataframe <- dataframe[!(
        dataframe[[col]] > Quantile3 + (IQR * 1.5) | 
          dataframe[[col]] < Quantile1 - (IQR * 1.5)
      ), ]
    }
  }
  return(dataframe)
}

without_outlier_data <- remove_outlier(fresh_dataset, names(fresh_dataset))
without_outlier_data
detect_outlier(without_outlier_data, names(without_outlier_data))

fresh_dataset <- without_outlier_data;

normalize_dataset <- fresh_dataset;

min_age <- min(normalize_dataset$person_age, na.rm = TRUE)
max_age <- max(normalize_dataset$person_age, na.rm = TRUE)
normalize_dataset$person_age <- (normalize_dataset$person_age - min_age) / (max_age - min_age)

min_income <- min(normalize_dataset$person_income, na.rm = TRUE)
max_income <- max(normalize_dataset$person_income, na.rm = TRUE)
normalize_dataset$person_income <- (normalize_dataset$person_income - min_income) / (max_income - min_income)

min_loan_amnt <- min(normalize_dataset$loan_amnt, na.rm = TRUE)
max_loan_amnt <- max(normalize_dataset$loan_amnt, na.rm = TRUE)
normalize_dataset$loan_amnt <- (normalize_dataset$loan_amnt - min_loan_amnt) / (max_loan_amnt - min_loan_amnt);

min_loan_int_rate <- min(normalize_dataset$loan_int_rate, na.rm = TRUE)
max_loan_int_rate <- max(normalize_dataset$loan_int_rate, na.rm = TRUE)
normalize_dataset$loan_int_rate <- (normalize_dataset$loan_int_rate - min_loan_int_rate) / (max_loan_int_rate - min_loan_int_rate);

min_credit_score <- min(normalize_dataset$credit_score, na.rm = TRUE)
max_credit_score <- max(normalize_dataset$credit_score, na.rm = TRUE)
normalize_dataset$credit_score <- (normalize_dataset$credit_score - min_credit_score) / (max_credit_score - min_credit_score );

normalize_dataset

fresh_dataset <- normalize_dataset; 

summary(fresh_dataset); 

calculate_stats <- function(dataset, columns) {
  for (column_name in columns) {
    column_data <- dataset[[column_name]]
    if (is.numeric(column_data)) {
      column_mean <- mean(column_data, na.rm = TRUE)
      column_median <- median(column_data, na.rm = TRUE)
      
      cat("Mean of column", column_name, "is", column_mean, "\n")
      cat("Median of column", column_name, "is", column_median, "\n")
      cat("\n")  
    } else {
      column_mode <- names(sort(table(column_data), decreasing = TRUE))[1]
      cat("Mode of column", column_name, "is", column_mode, "\n")
      cat("\n")
    }
  }
}


calculate_stats(fresh_dataset,names(fresh_dataset))


columns_to_analyze <- c(
  "person_age", 
  "person_income", "person_emp_exp",  
  "loan_amnt", "loan_int_rate", 
  "loan_percent_income","cb_person_cred_hist_length",
  "credit_score"
)
calculate_spread <- function(dataset, columns) {
  for (col_name in columns) {

    if (is.numeric(dataset[[col_name]])) {

      column_data <- dataset[[col_name]]

      column_range <- range(column_data, na.rm = TRUE)  
      column_iqr <- IQR(column_data, na.rm = TRUE)      
      column_sd <- sd(column_data, na.rm = TRUE)       
      column_variance <- var(column_data, na.rm = TRUE) 
      
      cat("For column", col_name, ":\n")
      cat("  Range:", column_range[2]- column_range[1], "\n")
      cat("  IQR:", column_iqr, "\n")
      cat("  Standard Deviation:", column_sd, "\n")
      cat("  Variance:", column_variance, "\n")
      cat("\n")  
      
    } 
  }
}

calculate_spread(fresh_dataset, columns_to_analyze)

class_distribution <- table(fresh_dataset$loan_status)
print(class_distribution)

if (class_distribution[1] > class_distribution[2]) {
  majority <- filter(fresh_dataset, loan_status == 0) 
  minority <- filter(fresh_dataset, loan_status == 1) 
} else {
  majority <- filter(fresh_dataset, loan_status == 1) 
  minority <- filter(fresh_dataset, loan_status == 0) 
}
set.seed(123)

oversampled_minority <- minority %>% sample_n(nrow(majority), replace = TRUE)
oversampled_data <- bind_rows(majority, oversampled_minority)
table(oversampled_data$loan_status)
oversampled_data

undersampled_majority <- majority %>% sample_n(nrow(minority), replace = FALSE)
undersampled_data <- bind_rows(undersampled_majority, minority)
table(undersampled_data$loan_status)
undersampled_data

fresh_dataset <- oversampled_data

