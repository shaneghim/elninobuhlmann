# Does a Strong El Niño Raise California Flood Losses?

A Bühlmann credibility analysis of California NFIP flood claims, 1979–2025, ahead of the 2026–27 El Niño.

**Report:** https://shaneghim.github.io/elninobuhlmann/ (source: `interpret.qmd`)

## Questions
1. How much higher should a California flood insurer expect losses to be in a strong El Niño winter?
2. Do counties differ in how they react to El Niño, and how much should each county's own El Niño history be trusted?

## Key findings
- Strong El Niño winters have cost about 1.61 times an average winter, but that rests almost entirely on 1983 and 1998. Without 1998, the load falls to 1.06.
- Bühlmann credibility gives Z = 0: counties' apparent differences in El Niño response are consistent with noise, so one statewide adjustment applies everywhere.
- Expected 2026–27 NFIP losses: $46.4M (2025 dollars), with a range of $14.0M to $76.5M across sensitivity tests.

## Project structure
```
data_raw/      raw data (claims, CPI, ONI)
data_clean/    cleaned data, created by 01_clean.R
scripts/       01_clean.R, 02_credibility.R
output/        charts and result tables, created by 02_credibility.R
interpret.qmd  report
```

## How to run
1. Open the R project in RStudio.
2. Install packages: `install.packages(c("tidyverse", "actuar", "maps", "ggrepel"))`
3. Run `scripts/01_clean.R`, then `scripts/02_credibility.R`.
4. Render `interpret.qmd` for the report.

## Data sources
- FEMA OpenFEMA, [NFIP Redacted Claims v3](https://www.fema.gov/api/open/v3/NfipClaims)
- FRED, [CPIAUCSL](https://fred.stlouisfed.org/series/CPIAUCSL)
- NOAA CPC, [Oceanic Niño Index v6](https://cpc.ncep.noaa.gov/products/analysis_monitoring/enso/oni/v6/)

*This product uses the FEMA OpenFEMA API, but is not endorsed by FEMA.
