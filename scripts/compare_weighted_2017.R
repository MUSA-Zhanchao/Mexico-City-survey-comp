library(tidyverse)

# Load weighted and unweighted results
weighted <- read.csv("data/2017/mode_combination_weighted_2017.csv")
unweighted <- read.csv("data/2017/mode_combinationw2mod_more_2017.csv")

# Rename columns for clarity
weighted <- weighted %>% rename(weighted_trips = weighted_n)
unweighted <- unweighted %>% rename(unweighted_trips = n)

# Merge results for comparison
comparison <- full_join(weighted, unweighted, by = "P5_14_merged") %>%
  replace_na(list(weighted_trips = 0, unweighted_trips = 0)) %>%
  arrange(desc(weighted_trips)) %>%
  mutate(
    weight_ratio = weighted_trips / unweighted_trips,
    weighted_percent = round(100 * weighted_trips / sum(weighted_trips), 2),
    unweighted_percent = round(100 * unweighted_trips / sum(unweighted_trips), 2)
  )

# Display top 15 mode combinations
cat("=== COMPARISON: WEIGHTED vs UNWEIGHTED ANALYSIS ===\n")
cat("Top 15 mode combinations by weighted trips:\n\n")

top_15 <- head(comparison, 15)
print(top_15)

cat("\n=== SUMMARY STATISTICS ===\n")
cat("Total weighted trips:", sum(weighted$weighted_trips), "\n")
cat("Total unweighted trips:", sum(unweighted$unweighted_trips), "\n")
cat("Average expansion factor:", round(sum(weighted$weighted_trips) / sum(unweighted$unweighted_trips), 2), "\n")

cat("\n=== TOP 5 MODE COMBINATIONS ===\n")
for(i in 1:5) {
  mode <- top_15$P5_14_merged[i]
  weighted_n <- top_15$weighted_trips[i]
  unweighted_n <- top_15$unweighted_trips[i]
  weighted_pct <- top_15$weighted_percent[i]
  unweighted_pct <- top_15$unweighted_percent[i]

  cat(sprintf("%d. %s:\n", i, mode))
  cat(sprintf("   Weighted: %s trips (%.2f%%)\n", format(weighted_n, big.mark=","), weighted_pct))
  cat(sprintf("   Unweighted: %s trips (%.2f%%)\n", format(unweighted_n, big.mark=","), unweighted_pct))
  cat(sprintf("   Expansion factor: %.1f\n\n", weighted_n/unweighted_n))
}

# Save comparison
write.csv(comparison, "data/2017/mode_combination_comparison_2017.csv", row.names = FALSE)
cat("Comparison saved to: data/2017/mode_combination_comparison_2017.csv\n")
