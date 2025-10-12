# ENT Origin-Destination Breakdown Analysis

This analysis breaks down trip data based on ENT (state) codes to understand travel patterns between different regions.

## Analysis Overview

The analysis filters trips into two categories:
1. **City to City**: Trips where both origin and destination are in ENT 09 (Mexico City proper)
2. **Suburb to Suburb**: Trips where both origin and destination are in ENT 13 (Hidalgo) or ENT 15 (Estado de México)

Trips that go from suburb to city or city to suburb are excluded from this analysis.

## Data Sources

### 2017 Data
- Origin ENT: From household data (THOGAR.DBF)
- Destination ENT: From trip data (P5_12_7 field)
- Mode information: From P5_14 fields

### 2007 Data
- Origin ENT: From household data (TVIVIENDA.DBF)
- Destination ENT: From trip data (CENTIDADDE field)
- Mode information: From SORDENTRAN field

## Scripts

- `scripts/2017_ent_od_breakdown.r`: Analyzes 2017 survey data
- `scripts/2007_ent_od_breakdown.r`: Analyzes 2007 survey data

## Output Files

### 2017 Data (data/2017/ent_od_breakdown/)
- `mode_share_2017.csv`: Detailed mode share for each trip category (unweighted)
- `mode_share_weighted_2017.csv`: Detailed mode share for each trip category (weighted by FACTOR)
- `transit_split_2017.csv`: Summary of transit vs non-transit trips (unweighted)
- `transit_split_weighted_2017.csv`: Summary of transit vs non-transit trips (weighted)

### 2007 Data (data/2007/ent_od_breakdown/)
- `mode_share_2007.csv`: Detailed mode share for each trip category (unweighted)
- `mode_share_weighted_2007.csv`: Detailed mode share for each trip category (weighted by NFACTOR)
- `transit_split_2007.csv`: Summary of transit vs non-transit trips (unweighted)
- `transit_split_weighted_2007.csv`: Summary of transit vs non-transit trips (weighted)

## Key Findings

### 2017 Data
| Category | Total Trips | Transit Trips | Transit Share | Non-Transit Trips | Non-Transit Share |
|----------|------------|---------------|---------------|-------------------|-------------------|
| City to City (ENT 09-09) | 110,481 | 27,758 | 25.1% | 82,723 | 74.9% |
| Suburb to Suburb (ENT 13/15-13/15) | 132,041 | 28,641 | 21.7% | 103,400 | 78.3% |

#### Weighted Results (2017)
| Category | Total Trips | Transit Trips | Transit Share | Non-Transit Trips | Non-Transit Share |
|----------|------------|---------------|---------------|-------------------|-------------------|
| City to City (ENT 09-09) | 11,787,615 | 2,941,550 | 25.0% | 8,846,065 | 75.0% |
| Suburb to Suburb (ENT 13/15-13/15) | 13,705,403 | 3,006,398 | 21.9% | 10,699,005 | 78.1% |

### 2007 Data
| Category | Total Trips | Transit Trips | Transit Share | Non-Transit Trips | Non-Transit Share |
|----------|------------|---------------|---------------|-------------------|-------------------|
| City to City (ENT 09-09) | 69,050 | 41,401 | 60.0% | 27,649 | 40.0% |
| Suburb to Suburb (ENT 13/15-13/15) | 47,476 | 28,380 | 59.8% | 19,096 | 40.2% |

#### Weighted Results (2007)
| Category | Total Trips | Transit Trips | Transit Share | Non-Transit Trips | Non-Transit Share |
|----------|------------|---------------|---------------|-------------------|-------------------|
| City to City (ENT 09-09) | 5,904,119 | 3,534,305 | 59.9% | 2,369,814 | 40.1% |
| Suburb to Suburb (ENT 13/15-13/15) | 5,122,020 | 3,011,247 | 58.8% | 2,110,773 | 41.2% |

## Mode Classification

### Transit Modes
- Bus
- Metro
- BRT (Bus Rapid Transit)
- Combinations of the above (e.g., Bus_Metro)

### Non-Transit Modes
- Walking
- Other (includes private vehicles, taxis, etc.)
- Any combination that includes non-transit modes

## Notes

1. Walking is excluded from multi-mode trip analysis to focus on motorized transport modes.
2. Transit share represents the percentage of trips using only transit modes (Bus, Metro, BRT).
3. The analysis shows a significant decrease in transit share from 2007 to 2017:
   - City to City: 60.0% (2007) → 25.1% (2017)
   - Suburb to Suburb: 59.8% (2007) → 21.7% (2017)
4. Both city-to-city and suburb-to-suburb trips show similar transit shares within each year.

## Running the Analysis

To regenerate the results:

```r
# For 2017 data
Rscript scripts/2017_ent_od_breakdown.r

# For 2007 data
Rscript scripts/2007_ent_od_breakdown.r
```

Required R packages:
- foreign
- tidyverse
