library(tidyverse)

# Load weighted and unweighted results for comparison
weighted_multimodal <- read.csv("data/2007/multimodal_trip_combined_weighted_2007.csv")
unweighted_multimodal <- read.csv("data/2007/mutimodal_trip_combined_2007.csv")

weighted_single <- read.csv("data/2007/single_mode_weighted_2007.csv")

# Prepare comparison data for multimodal trips
# Create a key for merging
create_key <- function(df) {
  df %>%
    mutate(
      trip_key = paste(
        ifelse(is.na(trip1), "", trip1),
        ifelse(is.na(trip2), "", trip2),
        ifelse(is.na(trip3), "", trip3),
        ifelse(is.na(trip4), "", trip4),
        ifelse(is.na(trip5), "", trip5),
        ifelse(is.na(trip6), "", trip6),
        sep = "_"
      )
    ) %>%
    # Clean up the key by removing trailing underscores
    mutate(trip_key = str_replace_all(trip_key, "_+$", ""))
}

weighted_with_key <- weighted_multimodal %>%
  create_key() %>%
  select(trip_key, weighted_count = weighted_count)

unweighted_with_key <- unweighted_multimodal %>%
  create_key() %>%
  select(trip_key, unweighted_count = count)

# Merge for comparison
comparison <- full_join(weighted_with_key, unweighted_with_key, by = "trip_key") %>%
  mutate(
    weighted_count = ifelse(is.na(weighted_count), 0, weighted_count),
    unweighted_count = ifelse(is.na(unweighted_count), 0, unweighted_count),
    expansion_factor = ifelse(unweighted_count > 0, weighted_count / unweighted_count, NA),
    difference = weighted_count - unweighted_count
  ) %>%
  arrange(desc(weighted_count))

# Save comparison
write.csv(comparison, "data/2007/mode_combination_comparison_2007.csv", row.names = FALSE)

# Print summary statistics
cat("=== 2007 WEIGHTED vs UNWEIGHTED COMPARISON ===\n")
cat("Multimodal trips:\n")
cat("  Total weighted:", sum(comparison$weighted_count), "\n")
cat("  Total unweighted:", sum(comparison$unweighted_count), "\n")
cat("  Average expansion factor:", round(mean(comparison$expansion_factor, na.rm = TRUE), 2), "\n")

cat("\nSingle mode trips:\n")
cat("  Total weighted single mode:", sum(weighted_single$weighted_count), "\n")

cat("\nTop 10 trip combinations by weighted count:\n")
print(head(comparison %>% select(trip_key, weighted_count, unweighted_count, expansion_factor), 10))
