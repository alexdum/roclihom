# RoCliHom - Romanian Homogenized Climate Dataset Explorer

RoCliHom is an interactive web application for exploring long-term homogenized air temperature and precipitation datasets in Romania.

**Explore the App Online**: [https://alexdum-roclihom.share.connect.posit.cloud/](https://alexdum-roclihom.share.connect.posit.cloud/)

## Data Source

The application uses the **RoCliHom** dataset, which is publicly available on Zenodo:

- **Title**: RoCliHom - Long-term homogenized air temperature and precipitation datasets in Romania, 1901 - 2023
- **Author**: Alexandru Dumitrescu
- **DOI**: [10.5281/zenodo.14880417](https://doi.org/10.5281/zenodo.14880417)
- **Zenodo Record**: [https://zenodo.org/records/14880417](https://zenodo.org/records/14880417)

### Dataset Description

The RoCliHom database contains temporally aggregated instrumental observations of diurnal temperature parameters (average, maximum, and minimum air temperature) and precipitation accumulation from **156 monitoring stations in Romania**, spanning the period from **1901 to 2023**. 

Key features of the dataset include:
- **Quality Control**: Automated and manual checks for consistency.
- **Homogenization**: Breakpoint detection and correction to account for historical inconsistencies (station relocations, instrument changes, etc.).
- **Metadata**: Includes station IDs, coordinates, altitude, and names.
- **Applications**: Specifically designed for analyzing long-term climate variability and trends in Romania.

## Features

- **Interactive Map**: Visualize station locations and multiannual climate statistics.
- **Time Series Analysis**: View and download historical trends for individual stations.
- **Trend Detection**: Integrated Theil-Sen slope estimation and Mann-Kendall trend testing.
- **Custom Basemaps**: Choose between Positron, Bright, and Satellite (Esri Imagery) backgrounds.