#' Add seasonal columns to data
#'
#' Optimized to avoid date conversion for every row.
#' @param data DataFrame containing year and month columns
#' @return DataFrame with added season_id and season columns
add_seasonal_columns <- function(data) {
    # Define season mapping: 12,1,2->DJF; 3,4,5->MAM; 6,7,8->JJA; 9,10,11->SON
    # We can use a lookup vector.
    # Month is 1..12.
    season_lookup <- c(
        "DJF", "DJF", # 1, 2
        "MAM", "MAM", "MAM", # 3, 4, 5
        "JJA", "JJA", "JJA", # 6, 7, 8
        "SON", "SON", "SON", # 9, 10, 11
        "DJF" # 12
    )

    data %>%
        mutate(
            season_id = (12 * year + month) %/% 3,
            season = season_lookup[month]
        )
}

#' Filter seasonal data by date range and selected season
#'
#' @param data DataFrame with seasonal columns
#' @param selected_season Character, e.g., "DJF", "MAM"
#' @return Filtered DataFrame
filter_seasonal_data <- function(data, selected_season) {
    # Range: 1901-03-01 to 2023-11-30
    # This corresponds to:
    # Start: Year > 1901 OR (Year == 1901 and Month >= 3)
    # End: Year < 2023 OR (Year == 2023 and Month <= 11)

    data %>%
        filter(
            (year > 1901 | (year == 1901 & month >= 3)) &
                (year < 2023 | (year == 2023 & month <= 11)),
            season == selected_season
        )
}

#' Aggregate data by season with completeness check
#'
#' Only calculates the mean/sum if all 3 months of the season are present.
#' @param data DataFrame with season_id
#' @param selected_variable Character, "PREC" or other
#' @return Aggregated DataFrame
aggregate_seasonal <- function(data, selected_variable) {
    data %>%
        group_by(id, season_id, season, name, latitude, longitude, altitude) %>%
        # Calculate the "year" for the season (usually the year of the last month, or just max year in the group)
        # For DJF (Dec 2022, Jan 2023, Feb 2023), usually denoted as Winter 2023.
        # We can use max(year) as the season year.
        summarise(
            n_months = n(),
            value = if (selected_variable == "PREC") sum(value, na.rm = TRUE) else mean(value, na.rm = TRUE),
            year = max(year),
            .groups = "drop"
        ) %>%
        # STICT COMPLETENESS CHECK: Only keep seasons with 3 months
        filter(n_months == 3) %>%
        dplyr::select(-n_months)
}
