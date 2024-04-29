# Nicehair Case Code
# Copyright BI Experts: Pawel Gach, Bu Bu Zhang, Aleksandra Kusz 2024

# Libraries used:
library(skimr) # For initial data evaluation
library(tidyr) # For data manipulation
library(dplyr) # For data manipulation
library(forcats) # For factorisation
library(cluster) # For k-means clustering
library(fastDummies) # For dummy encoding
library(arules) # For association rules mining
library(arulesViz) # For visualisation of ASM

# ------------------------------------------------------------------------------
# CUSTOMER SEGMENTATION
# ------------------------------------------------------------------------------

# Reading the data -------------------------------------------------------------

data <-
  read.csv("/Users/pawelgach/Documents/Aarhus Uni/Nicehair Case/nicehair.csv")
# For data security, I'm keeping it locally with .gitignore
# You can easily download it with:
# data <- read.csv("paste-URL-here")

# Initial exploration ----------------------------------------------------------

skimr::skim(data)

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

# A problem arises - approximately 60 k columns have NAs in the Google
# Analytics columns (from source_medium to revenue).

# With data completeness as my goal, I will delete them, as they are not
# useful for customer segmentation - I'll come back to them for product
# recommendation

# Here, I keep columns without NAs in the revenue column
data <- data[!is.na(data$revenue),]

# The source_medium column contains two strings separated by "/"
# I'll split it into two
data <- data %>%
  separate(col = source_medium,
           into = c("source", "medium"),
           sep = "/")

# Replace NA values in medium with (not set)
data$medium[is.na(data$medium)] <- "(not set)"


# Multiple Google Analytics columns also contain "(not set)" instead of NA
for (i in 1:ncol(data)) {
  if (is.character(data[, i])) {
    not_set_count <- sum(data[, i] == "(not set)")
    
    formatted_output <- sprintf(
      "%-3s %-25s %-10s %-5s",
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

# device, however, has an incompleteness rate of 0.89
# It also contains information which is similar to operating_system in its
# purpose. Therefore, it can be deleted
data <- subset(data, select = -device)

# Feature Engineering ----------------------------------------------------------

# user_id and transaction_id are long alphanumeric identifiers. I will transform
# them into normal ids for simplicity

# First, I keep the old mappings for possible later use
user_mapping <-
  data.frame(user_original = levels(factor(data$user_id)),
             user_numeric = seq_along(levels(factor(data$user_id))))

transaction_mapping <- data.frame(
  transaction_original = levels(factor(data$transaction_id)),
  transaciton_numeric = seq_along(levels(factor(
    data$transaction_id
  )))
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
# purposes of customer segmentation, I don't need all the product IDs. I will
# only keep the rows with the highest values in the transaction line column for
# each transaction id

data <- data %>%
  group_by(transaction_id) %>%
  filter(transaction_line_number == max(transaction_line_number)) %>%
  ungroup()

# Now, all the remaining Google Analytics columns can be factorised
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

# For data reduction, I lump all but the 5 most frequent levels in all factors
data_lump <- data %>%
  mutate_if(is.factor, ~ fct_lump(., n = 5))

# Now, I dummy encode the factor variables to prepare them for analysis
data_dummies <-
  dummy_cols(data_lump,
             select_columns = names(data)[sapply(data, class) == "factor"])

# Remove old factor variables
data_dummies <- data_dummies %>%
  select_if( ~ !is.factor(.))

# Remove columns irrelevant
data_dummies <-
  subset(data_dummies,
         select = -c(transaction_date,
                     product_id))

# Create a data frame with unique information about each user

# Group data by user_id
data_users <- data_dummies %>%
  group_by(user_id) %>%
  summarise(
    revenue = sum(revenue),
    # Sum of revenue for each user
    num_transactions = length(transaction_id),
    # Count of transactions for each user
    total_products = sum(transaction_line_number),
    # Total products bought
    # Apply any() across all dummy columns (columns 5 to 52)
    across(5:52, ~ as.integer(any(.x == 1)), .names = "{.col}_flag")
  )

# Resulting dataframe has 101168 x 52 variables, so reduction is necessary

# As user_id is now a unique identifier, we proceed without it
data_ready <- data_users[, -1]

# Make a histogram of revenue
hist(
  data_ready$revenue,
  main = "Histogram of Revenue (Full)",
  xlab = "Revenue",
  col = "blue",
  breaks = 30
)

# revenue is very skewed, so I make a histogram of revenue up to a limit
# Calculate the quantiles to define a reasonable range (limit outliers)
limits <- quantile(data_ready$revenue, probs = c(0, 0.95))
filtered_revenue <-
  data_ready$revenue[data_ready$revenue > limits[1] &
                       data_ready$revenue < limits[2]]
hist(
  filtered_revenue,
  main = "Histogram of Revenue (up to 95th Percentile)",
  xlab = "Revenue",
  col = "blue",
  breaks = 30
)

# I proceed without the highest outliers in revenue
data_ready <- data_ready %>%
  filter(revenue <= quantile(data_ready$revenue, 0.95, na.rm = T))
data_ready <- as.data.frame(data_ready)

# Also, we scale the numeric values
data_ready$revenue <- scale(data_ready$revenue)

# Final data reduction with PCA
(data_pca <- prcomp(data_ready, scale. = T))
summary(data_pca)

# Check how many components contribute to 95% of variance and extract them
cumulative_variance <- summary(data_pca)$importance[3, ]
(num_components_to_keep <- max(which(cumulative_variance <= 0.95)))
# 29 variables are enough

data_reduced <- data_pca$x[, 1:num_components_to_keep]


# Clustering - Non-hierarchical ------------------------------------------------

set.seed(123) # For reproducibility

# Find the appropriate amount of clusters
wss <-
  sapply(1:10, function(k) {
    kmeans(data_reduced, k, nstart = 10)$tot.withinss
  })

plot(1:10,
     wss,
     type = "b",
     xlab = "Number of Clusters",
     ylab = "Within groups sum of squares")
# It's hard to say, but it seems like 4 is the appropriate amount of clusters

# Perform k-means clustering
km_clust <- kmeans(data_reduced, centers = 4, nstart = 25)

# Add cluster assignments as a factor column for further analysis
data_ready$clust <- as.factor(km_clust$cluster)

# Display the size of each cluster
table(data_ready$clust)
# 1     2     3     4
# 52093 14857 20557  8606

# Find variables with the highest inter-cluster variance
# Calculate cluster means
cluster_means <- data_ready %>%
  group_by(clust) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE), .groups = 'drop')

# Calculate inter-cluster variance
# Ensure we only calculate variance for numeric columns
inter_cluster_variance <- cluster_means %>%
  summarise(across(where(is.numeric), var, na.rm = TRUE)) %>%
  pivot_longer(cols = everything(),
               names_to = "variable",
               values_to = "variance")

# Sort variables by highest inter-cluster variance
high_inter_cluster_variance <- inter_cluster_variance %>%
  arrange(desc(variance))

# View the variables with the highest inter-cluster variance
print(high_inter_cluster_variance, n = 20)

# 1 medium_ email_flag                         0.250
# 2 default_channel_group_Email_flag           0.249
# 3 source_(direct) _flag                      0.246
# 4 medium_ (none)_flag                        0.246
# 5 default_channel_group_Direct_flag          0.246
# 6 campaign_id_(not set)_flag                 0.241
# 7 source_Other_flag                          0.217
# 8 medium_ cpc_flag                           0.201
# 9 source_google _flag                        0.184
# 10 default_channel_group_Cross-network_flag   0.112
# Big fall-off after

# View average revenue across clusters
boxplot(
  data_ready$revenue ~ data_ready$clust,
  range = 10,
  varwidth = TRUE,
  notch = TRUE,
  main = "Revenue Distribution by Segment",
  xlab = "Segment",
  ylab = "Revenue"
)
# Unsignificant differences

# Perform ANOVA for variable validity
lapply(data_ready[, 1:51], function(x)
  summary(aov(x ~ clust, data = data_ready)))
# Nearly all significant

# Distribution of top variables
# 3 to skip revenue and total products
for (i in 1:10) {
  var_name <- high_inter_cluster_variance$variable[i]
  cat(var_name, "\n")
  
  count_table <- table(data_ready[[var_name]],
                       data_ready$clust)
  proportion_table <- prop.table(count_table, margin = 2)
  print(proportion_table)
  
  cat("--------------------------------------\n\n")
}

# No significant patterns, as the clusters do not differ in terms of revenue,
# Only through variables like source, medium and channels

# ------------------------------------------------------------------------------
# PRODUCT RECOMMENDATION
# ------------------------------------------------------------------------------

# I load the data again to start on a fresh dataset
data <-
  read.csv("/Users/pawelgach/Documents/Aarhus Uni/Nicehair Case/nicehair.csv")

# I simplify the transaction ID column
data$transaction_id <- as.numeric(factor(data$transaction_id))

# Create a list of transactions
trans_list <- data %>%
  group_by(transaction_id) %>%
  summarise(items = list(product_name), .groups = 'drop') %>%
  pull(items)

trans_list <- lapply(trans_list, unlist)
transactions <- as(trans_list, "transactions")

# Mine association rules
rules <-
  apriori(transactions,
          parameter = list(
            supp = 0.00001,
            # Set to very low due to large amount of products
            conf = 0.8,
            maxlen = 20,
            target = "rules"
          ))

# Inspect the top rules sorted by lift
top_rules <- head(sort(rules, by = "lift"), 1000)
plot(top_rules,
     method = "graph",
     engine = "htmlwidget",
     max = 1000)

summary(top_rules)
inspect(top_rules[1:100])
