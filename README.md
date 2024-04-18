# XRF Data Parsing Tool for OLYMPUS VANTA pXRF Chemistry Datasets

![](https://www.portaspecs.com/wp-content/uploads/2019/05/sized_m-series_2_1024x1024.jpg)

# Requirements

-   OLYMPUS VANTA pXRF device
-   GeoChem export method

Ensure the CSV export follows the GeoChem format with Item notes labeled as `LAB_ID`. The ID should contain both letters and numbers for string value compatibility. Note that `<LOD` values must be replaced with `0` before file upload, as the program does not support `<LOD` readings.

# Features

This tool is tailored to parse XRF databases exported in the GeoChem format from the OLYMPUS VANTA pXRF. It selectively retains element composition and item IDs for analysis.

## Overview Tab

-   Provides elemental concentrations of selected elements, along with average and standard deviation for items read multiple times.
-   Displays minimum and maximum values from multiple readings of each item.

## Normalized Tab

-   Similar to the Overview tab, displaying elemental concentrations of selected elements normalized to 100% for each item.

## Data with High Standard Deviation Tab

-   Filters items and elements with standard deviations exceeding a user-defined threshold.
-   Users can adjust the "Deviation Percentage cutoff" to isolate data with high standard deviations.
-   Provides tables of averages and standard deviations, as well as minimum and maximum values.

## Plot:

-   Generates grouped bar plots depicting averages and standard deviation errors for each selected element and item based on normalized data.

This tool streamlines the analysis process for XRF data sets, offering flexible filtering options and insightful visualizations.
