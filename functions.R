

library(dplyr)
library(ggplot2)
library(gt)
library(jsonlite)
library(purrr)
library(stringi)

get_weather <- function() {



  URL <- 'https://api.weather.gov/gridpoints/FGF/96,54/forecast'
  forecast <- jsonlite::fromJSON(URL, flatten = T)
  forecast <- forecast$properties$periods

}

get_hourly_weather <- function() {
  # URL for hourly forecast
  URL <- 'https://api.weather.gov/gridpoints/FGF/96,54/forecast/hourly'

  # Fetch and parse JSON data
  forecast <- jsonlite::fromJSON(URL, flatten = TRUE)

  # Extract periods (hourly forecasts)
  forecast <- forecast$properties$periods

  return(forecast)
}


calc_chill <- function(temp, wind) {

  x <- 35.74 + 0.6215 * temp - 35.75 * wind^0.16 + 0.4275 * temp * wind^0.16

  return(round(x, 0))

}

clean_weather <- function(weather) {
  weather |>
    dplyr::rowwise() |>
    dplyr::mutate(chill =

                    calc_chill(
                      temp = temperature,
                      wind = as.numeric(
                        stringi::stri_extract_first_words(windSpeed)
                      )
                    )
    )



}

clean_hourly_weather <- function(weather) {
  # weather |>
  # weather <- forecast
  today <- lubridate::today() |> lubridate::wday(label = TRUE)
  intermediate <- weather |>
    dplyr::rowwise()  |>
    dplyr::mutate(chill = calc_chill(
      temp = temperature, # Temperature in forecast
      wind = as.numeric(stringi::stri_extract_first_words(windSpeed)) # Extract numeric wind speed
    )) |>
    dplyr::ungroup() |>  # Ungroup to finalize data frame structure
    dplyr::mutate(time = lubridate::ymd_hms(startTime, tz = "US/Central")) |>
    dplyr::mutate(wday = lubridate::wday(time, label = TRUE)) |>

    dplyr::mutate(h_hour = stringi::stri_detect(time, regex = "06:00"))

  # Need to branch off if today is Friday, because next Friday
  #   will not be shown
  if(today == 'Fri') {
  result <-   "not friday"
  } else {
    result <- list()
    fri <- intermediate |>
      dplyr::filter(
        wday == 'Fri',
        h_hour == TRUE
        ) # |>

    result[['Fri']] = fri
    result[['Full']] = intermediate
      # dplyr::select(windSpeed, chill, shortForecast)
  }
return(result)
}

rate <- purrr::rate_delay(2, max_times = 15)
possibly_insistently_get_weather <- purrr::insistently(get_hourly_weather, rate = rate) |>
  purrr::possibly(otherwise=NULL)

go_nogo <- function(chill) {
  if(chill > 0) {
    return("Go")
  } else {
    return("No Go")
  }
}

decide <- function(chill) {

  if(chill <= - 5) {
    return("you'll die out there - stay home")
  } else if (chill <= 0 && chill > -5) {
    return("too damn cold - stay home")
  } else if (chill > 0 && chill < 32) {
    return("it's not that bad - man-up and do it")
  } else if (chill >= 32) {
    return("beautiful out there")
  }

}

make_gt <- function(df) {


  df |>
    gt() |>
    cols_label(
      Category = "",  # Remove column header for Category
      Value = ""      # Remove column header for Value
    ) |>
    tab_style(
      style = cell_text(weight = "bold", align = "right"),  # Bold and align the first column
      locations = cells_body(columns = "Category")
    ) |>
    tab_style(
      style = cell_text(align = "left"),  # Align the second column to the left
      locations = cells_body(columns = "Value")
    ) |>
    tab_options(
      # table.border.top.style = "none",         # Remove the top border of the entire table
      # table.border.bottom.style = "none",     # Remove the bottom border of the entire table
      # table_body.border.top.style = "none",   # Remove the top border of the body
      # table_body.border.bottom.style = "none",# Remove the bottom border of the body
      table_body.hlines.style = "none"        # Remove all horizontal lines in the body
    ) |>
    tab_style(
      style = cell_fill(color = "white"),  # Set all rows to have a white background
      locations = cells_body(rows = everything())
    )
    #    # gt::tab_style(
    #   style = cell_fill(color = "#b2d8ff", alpha = 0.7),
    #   locations = cells_body(
    #     rows = Category == "Decision" # Highlight the row where Name is "Bob"
    #   ))


}
