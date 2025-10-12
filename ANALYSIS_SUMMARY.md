# ENT OD Breakdown Analysis Summary

## Overview

This analysis examines travel patterns in the Mexico City metropolitan area by filtering trips based on origin and destination ENT (state) codes. The analysis focuses on two distinct travel patterns:

1. **City to City**: Trips within Mexico City proper (ENT 09 to ENT 09)
2. **Suburb to Suburb**: Trips within suburban areas (ENT 13 or 15 to ENT 13 or 15)

Trips between city and suburbs are excluded from this analysis to focus on internal travel patterns.

## Scripts Created

- `scripts/2017_ent_od_breakdown.r` - Analyzes 2017 survey data
- `scripts/2007_ent_od_breakdown.r` - Analyzes 2007 survey data

## Results Location

- 2017 results: `data/2017/ent_od_breakdown/`
- 2007 results: `data/2007/ent_od_breakdown/`
- Documentation: `data/ENT_OD_BREAKDOWN_README.md`

## Key Findings

### Transit Share Comparison (Weighted Data)

| Trip Type | 2007 Transit Share | 2017 Transit Share | Change |
|-----------|-------------------|-------------------|---------|
| City to City (ENT 09-09) | 59.9% | 25.0% | -34.9 pp |
| Suburb to Suburb (ENT 13/15-13/15) | 58.8% | 21.9% | -36.9 pp |

### Trip Volume Comparison (Weighted Data)

| Trip Type | 2007 Total | 2017 Total | Growth |
|-----------|-----------|-----------|---------|
| City to City (ENT 09-09) | 5,904,119 | 11,787,615 | +99.7% |
| Suburb to Suburb (ENT 13/15-13/15) | 5,122,020 | 13,705,403 | +167.6% |

## Key Insights

1. **Dramatic Decline in Transit Usage**: Both city and suburban internal trips show a significant decrease in transit share from 2007 to 2017 (approximately 35 percentage points).

2. **Substantial Growth in Trip Volume**: The total number of trips has nearly doubled for city trips and more than doubled for suburban trips over the 10-year period.

3. **Similar Transit Shares Within Years**: In both 2007 and 2017, city-to-city and suburb-to-suburb trips have very similar transit shares, suggesting regional patterns rather than location-specific differences.

4. **Rising Non-Transit Mode Use**: The growth in trips combined with declining transit share indicates a significant shift toward private vehicles and other non-transit modes.

## Mode Categories

### Transit Modes (included in transit share)
- Bus (including various bus types)
- Metro
- BRT (Bus Rapid Transit)
- Combinations of the above

### Non-Transit Modes
- Walking
- Private vehicles
- Taxis
- Other modes
- Any combination including non-transit modes

## How to Use

To regenerate the analysis:

```bash
# For 2017 data
Rscript scripts/2017_ent_od_breakdown.r

# For 2007 data
Rscript scripts/2007_ent_od_breakdown.r
```

## Output Files

Each analysis year produces four CSV files:

1. **mode_share_[year].csv**: Detailed breakdown of all mode combinations (unweighted)
2. **mode_share_weighted_[year].csv**: Detailed breakdown with survey weights applied
3. **transit_split_[year].csv**: Summary of transit vs non-transit trips (unweighted)
4. **transit_split_weighted_[year].csv**: Summary with survey weights applied

The weighted results are recommended for population-level analysis and comparison.
