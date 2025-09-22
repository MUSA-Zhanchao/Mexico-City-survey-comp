# Municipal Level Analysis

This directory contains scripts for analyzing transportation mode usage at the municipal level for Mexico City (ENT=09).

## Files

### Analysis Scripts
- `ent_09_07.r` - Analysis for 2007 data
- `ent_09_17.r` - Analysis for 2017 data

### Output Files
- `export/ent09_trip_categories_07.csv` - 2007 results
- `export/ent09_trip_categories_17.csv` - 2017 results

## Trip Categories

The analysis categorizes trips into three main types:

1. **Metro-only trips** (`metro_only_count`): Trips using only Metro/subway, including internal transfers within the metro system
2. **Bus-only trips** (`bus_only_count`): Trips using only bus transportation modes
3. **Bus+Metro transfers** (`bus_metro_count`): Trips that involve transfers between bus and metro systems

## Data Structure

### 2007 Data Format
- Uses SORDENTRAN field with trip segments (1-7 characters)
- Mode codes: 1,2,6=Metro; 3=BRT; 4,5,7=Bus; Others=Other

### 2017 Data Format
- Uses P5_14_XX columns as mode indicators
- Mode mapping:
  - Metro: P5_14_05, P5_14_12, P5_14_13, P5_14_15
  - Bus: P5_14_02, P5_14_06, P5_14_08, P5_14_10, P5_14_18
  - BRT: P5_14_11
  - Walking: P5_14_14 (excluded from analysis)
  - Other: All remaining P5_14_XX columns

## Output Format

CSV files with columns:
- `MUN`: Municipality code (2-digit)
- `metro_only_count`: Number of metro-only trips
- `bus_only_count`: Number of bus-only trips
- `bus_metro_count`: Number of bus+metro transfer trips

## Usage

Run the analysis scripts from the project root directory:

```r
# For 2007 data
source('municipal_level_analysis/ent_09_07.r')

# For 2017 data
source('municipal_level_analysis/ent_09_17.r')
```

Both scripts will generate their respective output files in the `export/` subdirectory.

## Dependencies

Required R packages:
- `tidyverse`
- `foreign`

Install with:
```r
install.packages(c("tidyverse", "foreign"))
```

## Results Summary

### 2007 vs 2017 Comparison
- **Metro-only trips**: 8,726 (2007) → 7,307 (2017)
- **Bus-only trips**: 37,077 (2007) → 33,159 (2017)
- **Bus+Metro transfers**: 17,964 (2007) → 14,334 (2017)
- **Total municipalities**: 16 (both years)

All three categories show decreasing trends from 2007 to 2017, providing valuable insights for transportation planning and policy analysis.
