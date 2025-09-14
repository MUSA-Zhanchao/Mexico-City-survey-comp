library(tidyverse)

# Load weighted and unweighted results for comparison (non-Mexico City)
weighted_multimodal_non_mex <- read.csv("data/2007/multimodal_trip_combined_non_mex_weighted_2007.csv")
unweighted_multimodal_non_mex <- read.csv("data/2007/multimodal_trip_combined_non_mex_2007.csv")

weighted_single_non_mex <- read.csv("data/2007/single_mode_non_mex_weighted_2007.csv")
unweighted_single_non_mex <- read.csv("data/2007/single_mode_non_mex_2007.csv")

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

weighted_with_key_non_mex <- weighted_multimodal_non_mex %>%
  create_key() %>%
  select(trip_key, weighted_count = weighted_count)

unweighted_with_key_non_mex <- unweighted_multimodal_non_mex %>%
  create_key() %>%
  select(trip_key, unweighted_count = count)

# Merge for comparison
comparison_non_mex <- full_join(weighted_with_key_non_mex, unweighted_with_key_non_mex, by = "trip_key") %>%
  mutate(
    weighted_count = ifelse(is.na(weighted_count), 0, weighted_count),
    unweighted_count = ifelse(is.na(unweighted_count), 0, unweighted_count),
    expansion_factor = ifelse(unweighted_count > 0, weighted_count / unweighted_count, NA),
    difference = weighted_count - unweighted_count
  ) %>%
  arrange(desc(weighted_count))

# Prepare comparison data for single mode trips
single_comparison_non_mex <- full_join(
  weighted_single_non_mex %>% select(trip1, weighted_count),
  unweighted_single_non_mex %>% select(trip1, unweighted_count = count),
  by = "trip1"
) %>%
  mutate(
    weighted_count = ifelse(is.na(weighted_count), 0, weighted_count),
    unweighted_count = ifelse(is.na(unweighted_count), 0, unweighted_count),
    expansion_factor = ifelse(unweighted_count > 0, weighted_count / unweighted_count, NA),
    difference = weighted_count - unweighted_count
  ) %>%
  arrange(desc(weighted_count))

# Save comparisons
write.csv(comparison_non_mex, "data/2007/mode_combination_comparison_non_mex_2007.csv", row.names = FALSE)
write.csv(single_comparison_non_mex, "data/2007/single_mode_comparison_non_mex_2007.csv", row.names = FALSE)

# Print summary statistics
cat("=== 2007 NON-MEXICO CITY WEIGHTED vs UNWEIGHTED COMPARISON ===\n")
cat("Total multimodal trips (unweighted):", sum(comparison_non_mex$unweighted_count), "\n")
cat("Total multimodal trips (weighted):", sum(comparison_non_mex$weighted_count), "\n")
cat("Total single mode trips (unweighted):", sum(single_comparison_non_mex$unweighted_count), "\n")
cat("Total single mode trips (weighted):", sum(single_comparison_non_mex$weighted_count), "\n")

# Calculate overall expansion factors
overall_multimodal_expansion <- sum(comparison_non_mex$weighted_count) / sum(comparison_non_mex$unweighted_count)
overall_single_expansion <- sum(single_comparison_non_mex$weighted_count) / sum(single_comparison_non_mex$unweighted_count)

cat("Overall expansion factor (multimodal):", round(overall_multimodal_expansion, 2), "\n")
cat("Overall expansion factor (single mode):", round(overall_single_expansion, 2), "\n")

# Show top 10 trip combinations
cat("\n=== TOP 10 NON-MEXICO CITY MULTIMODAL TRIP COMBINATIONS ===\n")
print(head(comparison_non_mex, 10))

cat("\n=== NON-MEXICO CITY SINGLE MODE COMPARISON ===\n")
print(single_comparison_non_mex)

# Summary of expansion factors for multimodal trips
cat("\n=== EXPANSION FACTOR ANALYSIS (MULTIMODAL) ===\n")
expansion_summary <- comparison_non_mex %>%
  filter(!is.na(expansion_factor) & expansion_factor != Inf) %>%
  summarise(
    min_expansion = min(expansion_factor),
    max_expansion = max(expansion_factor),
    mean_expansion = mean(expansion_factor),
    median_expansion = median(expansion_factor)
  )
print(expansion_summary)
