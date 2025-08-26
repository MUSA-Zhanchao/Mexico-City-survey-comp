# Mexico City Household Travel Survey Comparison across Years

Mexico City Travel Survey Comparison (2007-2017)

## 2007 structure
**Data processing completed**
- Single-mode classification
- multimodal connection analysis
- Weighted analysis available using `NFACTOR`

## 2017 Structure
**Data processing completed with weighted analysis**
- 5.14 (each mode is each column, like from `sklearn`)
- data directory available
- reaggregate and updated the files directories
- **Weighted analysis implemented using `FACTOR` column**
- Both weighted and unweighted results available for comparison

## Weighting Implementation

The 2017 analysis now includes proper population weighting using the `FACTOR` column:

- **Unweighted analysis**: Raw survey response counts (73,481 trips)
- **Weighted analysis**: Population-representative estimates (7,783,591 trips)
- **Average expansion factor**: 105.93 (`FACTOR`

### Key Files:
- `scripts/2017.R` - Main analysis script (includes both weighted and unweighted)
- `scripts/2017_weighted.R` - Standalone weighted analysis
- `scripts/compare_weighted_2017.R` - Comparison between methods
- `WEIGHTING_METHODOLOGY.md` - Detailed documentation

### Output Files:
- `mode_combinationw2mod_more_2017.csv` - Unweighted results
- `mode_combination_weighted_2017.csv` - **Weighted results (recommended for analysis)**
- `mode_combination_comparison_2017.csv` - Side-by-side comparison
