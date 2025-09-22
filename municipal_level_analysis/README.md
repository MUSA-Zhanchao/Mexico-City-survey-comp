# Municipal Level Analysis for ENT=09 (Mexico City)

## Overview
This analysis examines trip distribution patterns across municipalities in Mexico City (ENT=09) using 2007 transportation survey data. The analysis categorizes trips into three main types based on transportation modes used.

## Trip Categories

### 1. Metro-only trips
- Trips using only Metro/subway systems (modes 1, 2, 6)
- Includes internal transfers within the metro system
- **Total**: 8,726 trips across 16 municipalities
- **Average per municipality**: 545 trips
- **Range**: 144 - 1,382 trips per municipality

### 2. Bus-only trips  
- Trips using only bus systems (modes 4, 5, 7)
- Includes internal transfers within bus systems
- **Total**: 37,077 trips across 16 municipalities  
- **Average per municipality**: 2,317 trips
- **Range**: 349 - 5,931 trips per municipality

### 3. Bus + Metro transfer trips
- Trips using both bus and metro systems (combination of modes)
- Represents intermodal transfer behavior
- **Total**: 17,964 trips across 16 municipalities
- **Average per municipality**: 1,123 trips  
- **Range**: 211 - 3,000 trips per municipality

## Key Findings

- **Total analyzed trips**: 63,767 (59.08% of all ENT=09 trips)
- **Spatial coverage**: All 16 municipalities have representation in each category
- **Bus dominance**: Bus-only trips represent 58.1% of categorized trips
- **Transfer behavior**: Bus+metro transfers represent 28.2% of categorized trips
- **Metro usage**: Metro-only trips represent 13.7% of categorized trips

## Top Municipalities by Trip Volume

1. **MUN 007**: 10,292 total trips (1,361 metro-only, 5,931 bus-only, 3,000 transfers)
2. **MUN 005**: 9,793 total trips (1,382 metro-only, 5,831 bus-only, 2,580 transfers)  
3. **MUN 010**: 6,027 total trips (753 metro-only, 3,561 bus-only, 1,713 transfers)

## Files

- `ent_09.r`: Main analysis script
- `../data/spatial_distribution_ent09_trip_categories.csv`: Output results by municipality

## Usage

```r
# Run the analysis
Rscript municipal_level_analysis/ent_09.r
```

## Transportation Mode Codes

Based on the 2007 EOD survey categorization:
- **Metro**: 1 (Metro), 2 (Light rail), 6 (Suburban rail)
- **BRT**: 3 (Metrobus/BRT)
- **Bus**: 4 (Public bus), 5 (Microbus), 7 (School/Company bus)
- **Other**: All other transportation modes