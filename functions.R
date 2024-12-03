

library(dplyr)
library(ggplot2)
library(jsonlite)
library(purrr)
library(stringi)

get_weather <- function() {



  URL <- 'https://api.weather.gov/gridpoints/FGF/96,54/forecast'
  forecast <- jsonlite::fromJSON(URL, flatten = T)
  forecast <- forecast$properties$periods

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

# calc_chill(temp = 26, wind = 12)

rate <- purrr::rate_delay(2, max_times = 15)
possibly_insistently_get_weather <- purrr::insistently(get_weather, rate = rate) |>
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
