#
#
#
#
#
#
#
#
#
#
#|  echo: false
#|  warning: false
library(dplyr)
library(tidyr)
library(reactable)
#
#
#
#
#
df <- data.frame(
u1 = c(5, 1, 4, NA, NA, NA, 2, 1),
u2 = c(5, NA, 3, 2, 5, NA, 2, 1),
u3 = c(1, 4, 2, 5, 2, NA, 5, 4),
u4 = c(4, 2, NA, 1, 5, 2, 3, 3)
)
reactable(df, defaultPageSize = 4, filterable = TRUE, searchable = TRUE)
#
#
#
#
#
# Define targets
target_user <- "u2"
target_item <- 6

# 1. Calculate correlations between the target user and all others
correlations <- cor(df, use = "pairwise.complete.obs")[, target_user]
# Remove the target user from the vector so we only have the neighbors
correlations <- correlations[names(correlations) != target_user] 

# 2. Calculate the mean rating for every user
user_means <- colMeans(df, na.rm = TRUE)

# 3. Isolate the neighbors' ratings specifically for Movie 6
# unlist() converts the 1-row data frame into a named numeric vector
neighbor_ratings <- unlist(df[target_item, names(correlations)])

# 4. Filter for neighbors who ACTUALLY rated Movie 6 AND have a valid correlation
valid <- !is.na(neighbor_ratings) & !is.na(correlations) & (correlations > 0)

sims_valid <- correlations[valid]
ratings_valid <- neighbor_ratings[valid]
means_valid <- user_means[names(sims_valid)]

# 5. Apply the collaborative filtering formula
target_mean <- user_means[target_user]

predicted_rating <- target_mean + 
  sum(sims_valid * (ratings_valid - means_valid)) / sum(abs(sims_valid))

# View the result
predicted_rating
#
#
#
#
#
#
#
#
# 1. Transpose df so movies (rows) become columns
movie_matrix <- t(df)

# Assign clear names to the movie columns (Movie 1 to Movie 8)
colnames(movie_matrix) <- paste0("Movie_", 1:8)

# 2. Calculate pairwise correlations between all movies
movie_correlations <- cor(movie_matrix, use = "pairwise.complete.obs")

# 3. Extract correlations for Movie 1 against all other movies (excluding Movie 1)
m1_correlations <- movie_correlations[1, -1]

# 4. Find the movie with the highest correlation
most_similar_movie <- names(which.max(m1_correlations))

# Print results
print(m1_correlations)
cat("The movie most similar to Movie 1 is:", most_similar_movie, "\n")
#
#
#
#
#
#
#
#
# Load the countries dataset
df <- read.csv("http://manctsui.github.io/datasets/countries.csv")
df$Agriculture_imputed <- ifelse(is.na(df$Agriculture), mean(as.numeric(df$ Agriculture), na.rm = TRUE), as.numeric(df$Agriculture))
df$Industry_imputed <- ifelse(is.na(df$Industry), mean(as.numeric(df$ Industry), na.rm = TRUE), as.numeric(df$Industry))

df_grouped <- df  |>
  group_by(Region) |>
  mutate(Agriculture_imputed = ifelse(is.na(Agriculture), mean(Agriculture, na.rm = TRUE), Agriculture),
         Industry_imputed = ifelse(is.na(Industry), mean(Industry, na.rm = TRUE), Industry)) |>


df_grouped |> select(Country, Region, Agriculture, Agriculture_imputed, Industry, Industry_imputed) |> head(6) |> reactable(defaultPageSize = 6, filterable = FALSE, searchable = FALSE)
#
#
#
#
#
#
# Count missing values before imputation
missing_before <- sum(is.na(df$Agriculture)) + sum(is.na(df$Industry))

# Count missing values after imputation
missing_after <- sum(is.na(df$Agriculture_imputed)) + sum(is.na(df$Industry_imputed))

# Calculate the number of countries filled in
countries_filled <- missing_before - missing_after

cat("Number of countries filled in with mean imputation:", countries_filled, "\n")

#
#
#
#
#
#
#
df <- data.frame(
temp = c(79, 74, 73, 76, 82, 72, 71, 79, 83, 70, 73, 75, 77),
chirps = c(44, 40, 38, 43, 48, 37, 34, 45, 51, 32, 35, 42, 43)
)
lm_model <- lm(temp ~ chirps, data = df)
summary(lm_model)

#
#
#
#
#
# Define the variables
x <- df$chirps
y <- df$temp

# Calculate the slope (b1) and intercept (b0)
b1 <- cor(x, y) *sd(y) / sd(x)
b0 <- mean(y) - b1 * mean(x)
cat("Slope (b1):", b1, "\n")
cat("Intercept (b0):", b0, "\n")
#
#
#
#
#
