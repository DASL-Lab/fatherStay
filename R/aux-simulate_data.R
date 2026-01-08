#' Simulated matrix with reasonable columns
#'
#' @description Simulation for testing/example purposes
#'
simulate_data <- function(
  cnisp_samp_prop = 0.2, ptsos_samp_prop = 0.3,
  virulence = 0.05,
  to_nowcast = 10
) {

  dates <- seq(
    lubridate::ymd("2020-01-01"),
    lubridate::ymd("2021-12-31"),
    by = "day"
  )
  prov_cnisp <- list(
    "ON" = c(0.93, 115.5, 43.22 / 4),
    "AB" = c(0.95, 114.11, 41.89 / 4),
    "MB" = c(0.84, 9.72, 7.1 / 4),
    "NS" = c(0.81, 13.43, 8.71 / 4)
  )
  prov_ptsos <- list(
    "ON" = c(0.95, 194.51, 41.7 / 4),
    "AB" = c(0.91, 50.54, 17.64 / 4),
    "MB" = c(0.9, 23.15, 8.36 / 4),
    "NS" = c(0.75, 12.17, 6.77 / 4)
  )
  prov_rv <- list(
    "ON" = c(0.89, 544.42, 747.27),
    "AB" = c(0.97, 108.61, 95.45),
    "MB" = c(0.88, 192.31, 402.59),
    "NS" = c(0.94, 47.57, 53.74)
  )
  prov_dad <- list(
    "ON" = c(0.89, 325.03, 170.27 / 4),
    "AB" = c(0.93, 98.3, 44.24 / 4),
    "MB" = c(0.91, 32.03, 16.84 / 4),
    "NS" = c(0.84, 17.84, 10.72 / 4)
  )
  cnisp_ar <- simulate_ar(prov_cnisp, dates, intercept = FALSE)
  names(cnisp_ar)[3] <- "cnisp"
  ptsos_ar <- simulate_ar(prov_ptsos, dates, intercept = FALSE)
  names(ptsos_ar)[3] <- "ptsos"
  rv_ar <- simulate_ar(prov_rv, dates, round_to = 4, truncate = TRUE)
  names(rv_ar)[3] <- "npos"
  dad_ar <- simulate_ar(prov_dad, dates, intercept = FALSE)
  names(dad_ar)[3] <- "dad"

  dada <- dplyr::full_join(cnisp_ar, ptsos_ar, by = c("date", "prov")) |>
    dplyr::full_join(rv_ar, by = c("date", "prov")) |>
    dplyr::full_join(dad_ar, by = c("date", "prov"))

  # Based on assumptions about the data generating process
  # Included time series innovations just for fun
  dada <- dada |>
    dplyr::mutate(dad = dad + virulence * npos) |>
    dplyr::mutate(
      cnisp = cnisp + virulence * cnisp_samp_prop * dad,
      ptsos = ptsos + virulence * ptsos_samp_prop * dad
    ) |>
    dplyr::mutate(
      dad = ifelse(dad < 0, 0, round(dad, 0)),
      cnisp = ifelse(cnisp < 0, 0, round(cnisp, 0)),
      ptsos = ifelse(ptsos < 0, 0, round(ptsos, 0)),
      npos = round(npos, 0)
    ) |>
    dplyr::group_by(prov) |>
    dplyr::mutate(
      dad = dplyr::if_else(
        condition = date > lubridate::ymd("2021-12-21"), 
        true = NA, 
        false = dad
      )
    )
  dada
}


simulate_ar <- function(
  prov, dates, round_to = 0, intercept = TRUE, truncate = FALSE
) {
  lapply(
    names(prov),
    function(x) {
      this_ts = ifelse(intercept, prov[[x]][2], 0) +
        arima.sim(
          model = list(order = c(1, 0, 0), ar = prov[[x]][1]),
          n = length(dates),
          sd = prov[[x]][3]
        )
      res <- data.frame(
        date = dates,
        prov = x,
        count = round(this_ts, round_to)
      )
      if (truncate) res <- res |>
        dplyr::mutate(count = dplyr::if_else(count < 0, 0, count))
      res
    }
  ) |>
    dplyr::bind_rows()
}
