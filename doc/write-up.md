# Mexico city Household Travel Survey Comparison across Years (2007-2017)
Zhanchao Yang

## Overview

This analysis provides critical insight into how travel behaviors in Mexico City evolved between 2007 and 2017, particularly focusing on mode choice and the role of multimodal connectivity. Understanding these patterns is essential for transportation planning, as it highlights where high-capacity transit has gained or lost traction, how the private car’s role is shifting, and how multimodal integration has expanded. These findings serve as a foundation for policy implication on improving transit connectivity, investing in high-capacity corridors, and reducing dependence on private automobiles.

## Methodolgy

### Defining trip types
We classified each trip record as either:
- **Single-mode**: the trip used exactly one transportation mode. 
(*Note: Without considering mode transfer, even within big categories, e.g., bus → metrobús → bus is not considering single-mode bus trip; but bus → bus is still considered single-mode bus trip*)
- **Multimodal**: the trip used two or more **distinct** modes.

To keep cross-year comparisons consistent, we **excluded walking legs when classifying multimodal trips** because walking transfers are often under-reported. Practically, this means we removed “walk” from a trip’s mode list before classifying it. If a trip then had only one non-walk mode, it was treated as single-mode; if two or more non-walk modes remained, it was multimodal.  

Because the 2007 survey did not record walking, **2017 walking trips are analyzed descriptively but excluded from percentage calculations used for 2007–2017 comparisons**.

### De-duplication and order
Within a trip, we counted each mode **once**, regardless of repeats, and we ignored the **order** in which modes occurred.

- *Example (de-duplication):* `bus → metrobús → bus` is treated as **bus + metrobús** (not “bus” twice).  
- *Example (order):* `bus → metrobús` and `metrobús → bus` are both classified as **bus + metrobús**.

### Mode consolidation
To simplify categories, closely related sub-modes were merged into Metro, BRT, Non-BRT bus, and others (in multi-modes trips):
- **Tren + Metro** → **Metro** (fixed-guideway, high-capacity transit).  
- **Bus + Minibus** → **Bus** (non–high-capacity bus services).  
- **Drive+ bike+ motorcycle +etc.** → **others**

To facilitate comparison of multimodal patterns involving high-capacity transit, we combined BRT and Metro into a single high-capacity transit category.

### Data processing and reproducibility
All processing and analysis were performed in **R**. The scripts and input data are available in the accompanying **GitHub repository** (packaged as `.zip` files).

## Weighted vs Unweighted Comparison
There was little difference between the weighted and unweighted results in both 2007 and 2017. However, weighted estimates are preferred because they more accurately reflect the population and therefore provide a stronger basis for assessing mode shifts.

For single-mode trips, the weighted share was slightly lower in both years, with a more noticeable drop in 2017. Specifically, in 2017, weighted single-mode trips accounted for 79.25% (including walking), compared to 86.18% in the unweighted results.

All subsequent analyses in this report are based on the weighted results.

## Single-mode trips trends (2007 vs 2017)

Single-mode trips remained dominant in both 2007 and 2017. Excluding walking, their share fell from 76.68% in 2007 to 70.47% in 2017; when walking is included, the 2017 single-mode share is 79.25%.

By mode, non-BRT bus was the most commonly used in both years, rising from 42.47% to 44.28%. Automobile (driver) declined from 37.3% to 34.8%, while taxi increased from 8% to 8.21%. Metro/light rail decreased from 7.74% to 4.89%, whereas BRT grew from 0.25% to 1.79%. Other modes (e.g., bike, motorcycle) remained at roughly 5% across both years.

## Multimodal trips trends (2007 vs 2017)

High-capacity transit (BRT and Metro) plays a more prominent role in multimodal trips than in single-mode travel. In single-mode trips, high-capacity modes account for 7.88% in 2007 and 5.62% in 2017. By contrast, multimodal trips that include Metro represent 19.66% of all trips in 2007 and 22.71% in 2017. Bus is the primary feeder to high-capacity transit: among Metro-involved multimodal trips, 82.23% (2007) and 83.32% (2017) are bus + Metro combinations.

Overall, multimodality increased from 2007 to 2017. Bus-only trips decreased from 34.65% to 33.57%, and high-capacity-only (Metro/BRT) trips fell from 7.88% to 5.62%, while bus + high-capacity trips rose from 17.63% to 23.06%. 

## Key takeaways and insights

- **Single-mode travel remains dominant**, but its share declined over time. Excluding walking, single-mode trips dropped from 76.7% in 2007 to 70.5% in 2017.  

- **Bus use stayed on top** as the largest share of single-mode trips, while automobile driving declined slightly, and taxis saw marginal growth. Metro use decreased in single-mode trips, though BRT showed notable growth.  

- **Multimodality became more prevalent**, particularly combinations involving high-capacity transit. High-capacity-involved (BRT or Metro) multimodal trips grew from 20% in 2007 to 24% in 2017, with bus serving as the key feeder mode.  

- **Connectivity between bus and high-capacity transit strengthened**, with bus + Metro/BRT trips rising from 17.6% to 23.1% of all trips, indicating a better integrated public transportation networks, especially in high-capacity+bus connectivity.  

- **High-capacity transit is disproportionately important in multimodal contexts**, even as its share in single-mode trips fell. This suggests its primary role lies in enabling longer, more complex travel chains rather than acting as a stand-alone mode.  

Together, these results demonstrate that while Mexico City’s mobility remains bus-dominated with the higher share of multimodal connected trips. These patterns have direct implications for how future investments in infrastructure, integration, and service design can better support sustainable mobility.  
