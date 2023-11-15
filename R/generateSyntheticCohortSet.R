# Copyright 2022 DARWIN EU (C)
#
# This file is part of OmopMockCdm
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

#' It creates a mock cdm_reference.


#' Function to generate synthetic Cohort
#'
#' @param cdm cdm object
#' @param tableName name of the cohort table
#' @param numberCohorts number of different cohort to create
#' @param cohortName name of the cohort within the table
#' @param recordPerson the expected number of record per person
#' @param seed random seed
#' @param cdmVersion version number of the cdm
#'
#' @return A cdm reference with the mock tables
#' @export
#'
generateSyntheticCohortSet <- function(cdm,
                                       tableName = "cohort",
                                       numberCohorts = 2,
                                       cohortName = paste0("cohort_", cohortId),
                                       recordPerson = 1,
                                       seed = NULL,
                                       cdmVersion = attr(cdm, "cdm_version")) {

  # initial checks
  # checkInput(
  #   cdm = cdm, tableName = tableName, cohortId = cohortId,
  #   cohortName = cohortName, numberRows = numberRows, seed = seed,
  #   cdmVersion = cdmVersion,
  #   .options = list(cdmRequiredTables = c("person", "observation_period"))
  # )

  if (!is.null(seed)) {
    set.seed(seed = seed)
  }

  #generate synthenic cohort id
  cohortId = 1:numberCohorts

  #number of rows per cohort
  numberRows <- recordPerson*(cdm$person |> nrow()) |> round()

  # generate cohort table
  cohort <- list()
  if (length(numberRows) == 1) {
    numberRows <- rep(numberRows, length(cohortId))
  }
  for (i in seq_along(cohortId)) {
    num <- numberRows[i]
    cohort[[i]] <- dplyr::tibble(
      cohort_definition_id = cohortId[i],
      subject_id = sample(
        x = cdm$person$person_id, size = num, replace = TRUE
      )
    ) |>
      addDates(
        start = "cohort_start_date", end ="cohort_end_date", observationPeriod = cdm$observation_period
      )
  }
  cohort <- dplyr::bind_rows(cohort)

  # generate cohort set table
  cohortSetTable <- dplyr::tibble(
    cohort_definition_id = cohortId, cohort_name = cohortName
  )

  # create class
  cdm[[tableName]] <- OMOPGenerics::generatedCohortSet(
    cohortTable = cohort,cohortSetTable = cohortSetTable, cohortName = tableName)

  return(cdm)
}





addDates <- function(x, start = "cohort_start_date",end = "cohort_end_date", observationPeriod) {

  if (sum(length(start),length(end)) > 0) {
      x <- x |>
        dplyr::mutate(!!start := stats::runif(dplyr::n(),max = 0.5)) |>
        dplyr::mutate(!!end := stats::runif(dplyr::n(), min = 0.51))

    cols <- c(start,end)
    sumsum <- paste0(".data[[\"", cols, "\"]]", collapse = " + ")
    x <- x |>
      dplyr::mutate(cum_sum = !!rlang::parse_expr(sumsum)) |>
      dplyr::mutate(cum_sum = .data$cum_sum + stats::runif(dplyr::n())) |>
      dplyr::mutate(dplyr::across(dplyr::all_of(cols), ~ .x / .data$cum_sum)) |>
      dplyr::select(-"cum_sum")
    observationPeriod <- observationPeriod |>
      dplyr::mutate(rand = stats::runif(dplyr::n())) |>
      dplyr::group_by(.data$person_id) |>
      dplyr::filter(.data$rand == min(.data$rand)) |>
      dplyr::ungroup() |>
      dplyr::select(-"rand")
    x <- x |>
      dplyr::inner_join(
        observationPeriod |>
          dplyr::mutate(date_diff = .data$observation_period_end_date -
                          .data$observation_period_start_date) |>
          dplyr::select(
            "person_id",
            "start" = "observation_period_start_date", "date_diff")
          ,
        by = c("subject_id" = "person_id")) |>
      dplyr::mutate(dplyr::across(
        dplyr::all_of(cols), ~ round(.x * .data$date_diff) + .data$start
      )) |>
      dplyr::select(-c("start", "date_diff"))
  }
  return(x)
}
