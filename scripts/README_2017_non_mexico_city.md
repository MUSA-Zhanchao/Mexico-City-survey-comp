# 2017 Outside Mexico City Trips Analysis

## Overview
This script (`2017_non_mexico_city.R`) analyzes 2017 transportation survey data for trips **outside** Mexico City (ENT != "09"), following the same methodology as the Mexico City analysis in `2017_ent.r`.

## Key Changes from `2017_ent.r`
- **Filter**: Changed from `ENT == "09"` (Mexico City) to `ENT != "09"` (outside Mexico City)
- **Variable naming**: All variables use `_outside` suffix instead of `_mex`
- **Output files**: All output files include "outside_mexico_city" in their names

## Generated Output Files
The script will generate the following CSV files in `data/2017/`:

### Multimodal Trip Analysis
1. `mode_combination_outside_mexico_city_2017.csv` - Unweighted multimodal trip combinations
2. `mode_combination_weighted_outside_mexico_city_2017.csv` - Weighted multimodal trip combinations

### Single Mode Analysis  
3. `single_mode_outside_mexico_city_2017.csv` - Unweighted single mode trips
4. `single_mode_weighted_outside_mexico_city_2017.csv` - Weighted single mode trips

### Mode Share Analysis
5. `mode_share_outside_mexico_city_2017.csv` - Unweighted mode share percentages
6. `mode_share_weighted_outside_mexico_city_2017.csv` - Weighted mode share percentages

## Data Processing
The script follows the same methodology as `2017_ent.r`:
- Filters weekday trips only (P5_3==1)
- Processes 1-6 mode combinations
- Applies transportation mode classifications (Bus, Metro, BRT, Other)
- Excludes walking from multimodal analysis
- Calculates both unweighted and weighted statistics using FACTOR column

## Usage
To run this script:
```r
source("scripts/2017_non_mexico_city.R")
```

## Dependencies
- foreign (for reading DBF files)
- tidyverse (for data manipulation)

## Related Files
- `2017_ent.r` - Equivalent analysis for Mexico City trips (ENT == "09")
- `2007_non_mexico_city.R` - 2007 analysis for outside Mexico City trips
- `2017.R` - Base 2017 analysis script