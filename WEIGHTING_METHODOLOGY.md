# Weighting Methodology for 2017 Mexico City Travel Survey Analysis

## Overview

This document explains the implementation of weighted analysis for the 2017 Mexico City Travel Survey data, addressing the need to confirm and implement weighted columns as referenced in Issue #12.

## Background

The original analysis (`scripts/2017.R`) counted raw survey responses using `n()` functions, which does not account for the survey design and population representation. Survey data typically includes expansion factors (weights) that must be applied to generate population estimates.

## Data Structure

### Available Weight Factor
- **Column**: `FACTOR` 
- **Description**: Expansion factor for converting survey responses to population estimates
- **Range**: 5.0 to 1,921.0
- **Mean**: 105.2

### Comparison: 2007 vs 2017
- **2007 data**: Uses `NFACTOR` column for weighting
- **2017 data**: Uses `FACTOR` column for weighting

## Implementation

### Changes Made

1. **Modified data selection** to include `FACTOR` column alongside transportation mode columns (`P5_14_*`)
2. **Updated row filtering logic** to use `select(., starts_with("P5_14"))` instead of `across(everything())` to exclude FACTOR from mode counting
3. **Replaced count aggregation** from `n()` to `sum(FACTOR)` in all summarization steps
4. **Added weighted output files** with clear naming convention

### Files Created/Modified

1. **scripts/2017.R** - Updated original script to include weighted analysis
2. **scripts/2017_weighted.R** - Standalone weighted analysis script  
3. **scripts/compare_weighted_2017.R** - Comparison between weighted and unweighted results
4. **data/2017/mode_combination_weighted_2017.csv** - Weighted mode combination results
5. **data/2017/mode_combination_comparison_2017.csv** - Side-by-side comparison

### Key Code Changes

**Original (unweighted)**:
```r
mode2_combined <- two_mode_combined %>%
  group_by(P5_14_merged) %>%
  summarise(n = n(), .groups = 'drop')
```

**Updated (weighted)**:
```r
mode2_combined <- two_mode_combined %>%
  group_by(P5_14_merged) %>%
  summarise(weighted_n = sum(FACTOR), .groups = 'drop')
```

## Results Summary

### Impact of Weighting
- **Unweighted total**: 73,481 trips
- **Weighted total**: 7,783,591 trips  
- **Average expansion factor**: 105.93

### Mode Distribution Comparison
The weighted analysis shows similar relative proportions but with proper population-level estimates:

| Mode Combination | Unweighted % | Weighted % | Expansion Factor |
|-----------------|--------------|------------|------------------|
| Bus_Metro       | 64.44%       | 64.08%     | 105.3           |
| Bus             | 7.92%        | 8.04%      | 107.4           |
| BRT_Bus         | 7.62%        | 7.61%      | 105.8           |

## Methodology Notes

1. **Consistent with survey design**: Weighting ensures results represent the Mexico City population, not just survey respondents
2. **Maintains original logic**: All mode classification and combination logic remains unchanged
3. **Backward compatibility**: Original unweighted files are preserved for comparison
4. **Clear naming convention**: Weighted files include "weighted" in their names for distinction

## Usage

### Running the Analysis
```bash
# Original script (now includes weighted analysis)
Rscript scripts/2017.R

# Standalone weighted analysis
Rscript scripts/2017_weighted.R

# Generate comparison report
Rscript scripts/compare_weighted_2017.R
```

### Output Files
- `mode_combinationw2mod_more_2017.csv` - Original unweighted results
- `mode_combination_weighted_2017.csv` - Population-weighted results
- `mode_combination_comparison_2017.csv` - Side-by-side comparison

## Recommendations

1. **Use weighted results** for population-level inferences and policy decisions
2. **Use unweighted results** for survey methodology validation and sample size reporting
3. **Apply consistent weighting** across all 2017 analyses for coherent results
4. **Consider updating 2007 analysis** to use NFACTOR for consistency across years