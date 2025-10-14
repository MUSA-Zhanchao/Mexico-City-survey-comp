# ENT OD Breakdown Analysis Summary

## Overview

This analysis examines travel patterns in the Mexico City metropolitan area by filtering trips based on origin and destination ENT (state) codes. The analysis focuses on two distinct travel patterns:

1. **City to City**: Trips within Mexico City proper (ENT 09 to ENT 09)
2. **Suburb to Suburb**: Trips within suburban areas (ENT 13 or 15 to ENT 13 or 15)

Trips between the city and the suburbs are excluded from this analysis to focus on internal travel patterns.

## Scripts Created

- `scripts/2017_ent_od_breakdown.r` - Analyzes 2017 survey data
- `scripts/2007_ent_od_breakdown.r` - Analyzes 2007 survey data

## Analysis Features

Both scripts perform comprehensive mode choice analysis following the patterns established in `2007.R` and `2017.R`:

### 2017 Analysis (`scripts/2017_ent_od_breakdown.r`)

- Filters trips by origin and destination ENT codes
- Processes single-mode trips
- Processes multi-mode trips (2-6 modes per trip)
- Handles walking as a special case (excluded from mode counts)
- Categorizes modes into: Bus, Metro, BRT, Taxi, Drive, Bicycle, Moto, Walk, Other
- Generates both unweighted and weighted (using FACTOR column) statistics
- Outputs separate CSV files for city-to-city and suburb-to-suburb patterns

### 2007 Analysis (`scripts/2007_ent_od_breakdown.r`)

- Parses SORDENTRAN field to extract trip segments
- Classifies each segment into mode categories: Metro (1,2,6), BRT (3), Bus (4,5,7), Other
- Identifies unique modes used in each trip
- Aggregates trips by mode combinations
- Generates both unweighted and weighted (using NFACTOR column) statistics
- Outputs separate summaries for single-mode and multimodal trips

## Output Files

### 2017 Data Outputs

- `data/2017/mode_combination_city_to_city_2017.csv` - Unweighted city trip mode combinations
- `data/2017/mode_combination_city_to_city_weighted_2017.csv` - Weighted city trip mode combinations
- `data/2017/mode_combination_suburb_to_suburb_2017.csv` - Unweighted suburb trip mode combinations
- `data/2017/mode_combination_suburb_to_suburb_weighted_2017.csv` - Weighted suburb trip mode combinations

### 2007 Data Outputs

- `data/2007/mode_combination_city_to_city_2007.csv` - All mode combinations for city trips
- `data/2007/mode_combination_city_to_city_weighted_2007.csv` - Weighted mode combinations
- `data/2007/single_mode_city_to_city_2007.csv` - Single mode city trips
- `data/2007/single_mode_city_to_city_weighted_2007.csv` - Weighted single mode city trips
- `data/2007/multimodal_city_to_city_2007.csv` - Multimodal city trips
- `data/2007/multimodal_city_to_city_weighted_2007.csv` - Weighted multimodal city trips
- Similar files for suburb-to-suburb patterns
