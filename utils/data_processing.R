#' Add seasonal columns to data
#'
#' @param data DataFrame containing year and month columns
#' @return DataFrame with added Date, season_id, and season columns
add_seasonal_columns <- function(data) {
    data %>%
        mutate(
            Date = as.Date(paste(year, month, "01", sep = "-")),
            season_id = (12 * year + month) %/% 3,
            season = mkseas(Date, "DJF")
        )
}

#' Filter seasonal data by date range and selected season
#'
#' @param data DataFrame with seasonal columns
#' @param selected_season Character, e.g., "DJF", "MAM"
#' @return Filtered DataFrame
filter_seasonal_data <- function(data, selected_season) {
    data %>%
        group_by(season_id) %>%
        mutate(season_label = paste(max(year), season)) %>%
        filter(Date >= as.Date("1901-03-01"), Date <= as.Date("2023-11-30")) %>%
        ungroup() %>%
        separate(season_label, into = c("year_agg", "season_agg"), sep = " ", remove = FALSE) %>%
        filter(season_agg == selected_season) %>%
        dplyr::select(-year_agg, -season_agg) # Clean up temporary columns if not needed, but server logic uses season_label splitting later.
    # Actually, looking at original code:
    # It does: separate(season_label, into = c("year", "season"), sep = " ")
    # which overwrites original year/season columns.
    # Let's align with that logic but be careful.
}
