# Nicehair Case Code
# Copyright Pawel Gach, Bu Bu Zhang, Aleksandra Kusz 2024

# Libraries used:
library(skimr) # For initial data evaluation
library(tidyr) # For data manipulation
library(dplyr) # For data manipulation
library(forcats) # For factorisation
library(cluster) # For k-means clustering

# ------------------------------------------------------------------------------
# CUSTOMER SEGMENTATION
# ------------------------------------------------------------------------------

# Reading the data -------------------------------------------------------------

data <- read.csv("nicehair.csv")
# For data security, I'm keeping it locally with .gitignore
# You can easily download it with:
# data <- read.csv("paste-URL-here")

# Initial exploration ----------------------------------------------------------

skimr::skim(data)
View(data)

data <- subset(data, select = -X)
# Delete X, as it is clearly an index column

data$transaction_date <-
  as.Date(data$transaction_date, format = "%A, %B %d, %Y")
# transform the transaction_date column into an actual date
# %A for the full weekday name
# %B for the full month name
# %d for the day of the month as a decimal number
# %Y for the year with the century

# Handling NAs -----------------------------------------------------------------

# Another problem arises - approximately 60 k columns have NAs in the Google
# Analytics columns (from source_medium to revenue).

# With data completeness as my goal, I will delete them, as they are not
# useful for customer segmentation

# Then I keep columns without NAs in the revenue column
data <- data[!is.na(data$revenue),]

# The source_medium column contains two strings separated by "/"
# I'll split it into two
data <- data %>%
  separate(col = source_medium,
           into = c("source", "medium"),
           sep = "/")

# Replace NA values with (not set)
data$medium[is.na(data$medium)] <- "(not set)"


# Multiple Google Analytics columns also contain "(not set)" instead of NA
for (i in 1:ncol(data)) {
  if (is.character(data[, i])) {
    not_set_count <- sum(data[, i] == "(not set)")
    
    formatted_output <- sprintf("%-3s %-25s %-10s %-5s",
      i,
      colnames(data)[i],
      not_set_count,
      round(not_set_count / nrow(data), 2)
    )
    
    cat(formatted_output, "\n")
  }
}

# Notably, campaign_id has 0.45% of entries as (not set), however this is not
# a problem as not every sale has to be tied to a campaign

# device, however, has an incompletness rate of 0.89
# It also contains information which is similiar to operating_system in its
# purpose. Therefore, it can be deleted
data <- subset(data, select = -device)

# Data Preparation -------------------------------------------------------------

# user_id and transaction_id are long alphanumeric identifiers. I will transform
# them into normal ids for simplicity


# First, I keep the old mappings for possible later use
user_mapping <- data.frame(
  user_original = levels(factor(data$user_id)),
  user_numeric = seq_along(levels(factor(data$user_id)))
)

transaction_mapping <- data.frame(
  transaction_original = levels(factor(data$transaction_id)),
  transaciton_numeric = seq_along(levels(factor(data$transaction_id)))
)

# Then, I transform them
data$user_id <- as.numeric(factor(data$user_id))
data$transaction_id <- as.numeric(factor(data$transaction_id))


# Another issue is the product name column, which contains the same data
# as the product id column. To delete it, I create another mapping for possible
# later use

# Here I select unique product ID and product name pairs
product_mapping <- unique(data[, c("product_id", "product_name")])

# And now I delete the product name
data <- subset(data, select = -product_name)

# Furthermore, after deleting the NA Column, the country ID column only holds
# one value - DK. Therefore, it can be safely deleted
data <- subset(data, select = -country)

# In the dataset, each product takes up one line and the transaction id with all
# the relevant google analytics details is repeated multiple times. For the 
# purposes of customer segmentation, I don't need all the product ids. I will 
# only keep the rows with the highest values in the transaction line column for 
# each transaction id

data <- data %>%
  group_by(transaction_id) %>%
  filter(transaction_line_number == max(transaction_line_number)) %>%
  ungroup()

# Now, all the remaining google analytics columns can be factorised

cols <-
  c(
    "brand",
    "source",
    "medium",
    "campaign_id",
    "default_channel_group",
    "city",
    "browser",
    "operating_system"
  )

for (col in cols) {
  data[[col]] <- factor(data[[col]])
}

# Clustering -------------------------------------------------------------------

set.seed(123) # For reproducibility

k_data <- scale(data$revenue)

wss <-
  sapply(1:10, function(k) {
    kmeans(k_data, k, nstart = 10)$tot.withinss
  })

plot(1:10,
     wss,
     type = "b",
     xlab = "Number of Clusters",
     ylab = "Within groups sum of squares")
# It seems like 6 is the appropriate amount of clusters

km_res <- kmeans(k_data, centers = 6, nstart = 25)

# I add the clusters as a column in the data dataframe
data$cluster <- as.factor(km_res$cluster)

for(i in 1:6) {
  print(skim(data[data$cluster == i, ]))
  readline(prompt = "Press Enter to continue...")
}