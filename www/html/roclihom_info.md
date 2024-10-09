##### Data Source

The primary input data consists of daily records extracted from the database of the National Meteorological Administration of Romania.

##### Temperature Data Processing

**1901–1960:** The daily average temperature (Tavg) was calculated as the mean of three climatological observations taken at 06, 12, and 18 UTC. A Köppen coefficient, determined based on the minimum temperature (Tmin) and the month of the year, was then applied.\
**1961–2023:** The daily Tavg was calculated as the arithmetic average of four climatological observations recorded at 00, 06, 12, and 18 UTC.

##### Precipitation Data Processing

For the entire analysis period, daily precipitation (PREC) was calculated as the total accumulation from 18 UTC on the previous day to 18 UTC on the current day, with the timestamp marking the end of the 24-hour accumulation period.

##### Quality Control and Data Homogenization

Quality control procedures, gap-filling, and homogenization were conducted using the methodology implemented in the Climatol software.

##### Trend Analysis

Trends were assessed using the Theil–Sen nonparametric estimator, while significance levels were evaluated using the Mann-Kendall test, both of which were implemented via the *EnvStats* R package.

The detailed methodology for this study is described in the manuscript '*RoCliHom - Long-term Homogenized Air Temperature and Precipitation Datasets in Romania, 1901–2023*' by Dumitrescu et al. (2024), submitted to the journal *Scientific Data*.
