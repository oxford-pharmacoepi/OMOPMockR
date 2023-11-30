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
#'
#' @param cdmName Name of the cdm.
#' @param cdmVersion version number of the cdm
#' @param cdmVocabulary vocabulary tables of the cdm
#' @param person person table
#' @param observationPeriod observation_period table
#' @param nPerson number of mock person to create in person table
#' @param birthRange the date range of the person in person table
#' @param death death table.
#' @param conditionOccurrence Condition occurrence table.
#' @param drugExposure DrugExposure table.
#' @param procedureOccurrence Procedure occurrence table.
#' @param deviceExposure Device exposure table.
#' @param measurement Measurement table.
#' @param observation Observation table.
#' @param numberRecords number of records per person.
#' @param seed random seed
#'
#' @return A cdm reference with the mock tables
#' @export
#'
mockCdm <- function(cdmName ="mock cdm",
                    cdmVersion ="5.3",
                    cdmVocabulary = mockVocabularyCdm(),
                    person = NULL,
                    observationPeriod = NULL,
                    nPerson = 5,
                    birthRange = c("1950-01-01","2000-12-31"),
                    death = NULL,
                    conditionOccurrence = NULL,
                    drugExposure = NULL,
                    procedureOccurrence = NULL,
                    deviceExposure = NULL,
                    measurement = NULL,
                    observation = NULL,
                    numberRecords = c(default = 2),
                    seed = 1) {
  # check inputs
  checkInput(
    cdmName = cdmName,cdmVersion = cdmVersion,nPerson = nPerson,
    birthRange = birthRange,seed = seed,numberRecords = numberRecords,
    seed = seed, cdmTables = list(
      person = person, observationPeriod = observationPeriod, death = death,
      conditionOccurrence = conditionOccurrence, drugExposure = drugExposure,
      procedureOccurrence = procedureOccurrence,
      deviceExposure = deviceExposure, measurement = measurement,
      observation = observation)
  )

  if (!is.null(seed)) {
    set.seed(seed = seed)
  }

  #generate mock person and observation details
  person_id <- seq_len(nPerson)

  dob <-
    sample(seq(as.Date(birthRange[1]), as.Date(birthRange[2]), by =
                 "day"), length(person_id), replace = TRUE)

  observationDate <- obsDate(
    dob = dob, max = max(as.Date("2020-01-01"),
                         as.Date(as.Date(birthRange[2]))))


  gender <- sample(c(8507, 8532), length(person_id), TRUE)

  # set mock person if null
  if (is.null(person)) {
    person = dplyr::tibble(
      person_id = person_id,
      gender_concept_id = gender,
      year_of_birth = as.character(lubridate::year(dob)),
      month_of_birth = as.character(lubridate::month(dob)),
      day_of_birth = as.character(lubridate::day(dob))
    )
    person <- person |>
      dplyr::mutate(race_concept_id = NA, ethnicity_concept_id = NA)
  }

  if (!"race_concept_id" %in% names(person)) {
    person <- person |> dplyr::mutate(race_concept_id = NA)
  }

  if (!"ethnicity_concept_id" %in% names(person)) {
    person <- person |> dplyr::mutate(ethnicity_concept_id = NA)
  }

  # set observation period if null
  if (is.null(observationPeriod)) {
    observationPeriod = dplyr::tibble(
      observation_period_id = person_id,
      person_id = person_id,
      observation_period_start_date = as.Date(observationDate[[1]]),
      observation_period_end_date = as.Date(observationDate[[2]])
    )
  }
  if (!"period_type_concept_id" %in% names(observationPeriod)) {
    observationPeriod <-
      observationPeriod |> dplyr::mutate(period_type_concept_id = NA)
  }

  cdmTables <- list(person = person, observation_period = observationPeriod)



# Add in vocabulary tables
  if (!is.null(cdmVocabulary)){
    cdmTables <- c(cdmTables,cdmVocabulary)
  }

  cdm <- OMOPGenerics::cdmReference(
    cdmTables = cdmTables, cdmVersion = cdmVersion,
    cdmName = cdmName, cohortTables = list())

# generate clinical table if null
  clinical <- c(
    "death", "condition_occurrence", "drug_exposure", "procedure_occurrence",
    "device_exposure", "measurement", "observation"
  )
  for (tab in clinical) {
    if (is.null(cdm[[tab]])) {
      if (tab %in% names(numberRecords)) {
        records <- round(unname(numberRecords[tab]) * nrow(cdm$person))
      } else {
        records <-
          round(unname(numberRecords["default"]) * nrow(cdm$person))
      }
      cdm[[tab]] <- generateClinicalDataTable(cdm, tab, records, seed = seed)
    }
  }

  return(cdm)

}


#function to generate mock observational period date from a list of dob
obsDate <- function(dob = dob, max = "2020-01-01") {

  # Initialise vector for output
  start <- rep(as.Date(NA), length(dob))
  end <- rep(as.Date(NA), length(dob))
  #generate obs start and end date
  for (i in seq_along(dob)) {
    start[i] <- sample(seq(as.Date(dob[i]), as.Date(max), by =
                             "day"), 1)
    end[i] <- sample(seq(as.Date(start[i]), as.Date(max), by =
                           "day"), 1)
  }
  list(start,end)
}

#function to add dates to clinical table with reference to observation_period table
addDates <- function(x, cols, observationPeriod, seed = 5) {
  if (length(cols) > 0) {
    y = 1
    for (col in cols) {
      set.seed(seed)
      x <- x |>
        dplyr::mutate(!!col := stats::runif(dplyr::n())*y)
      y = y - stats::runif(1)
    }
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
            "start" = "observation_period_start_date", "date_diff"
          ),
        by = "person_id"
      ) |>
      dplyr::mutate(dplyr::across(
        dplyr::all_of(cols), ~ round(.x * .data$date_diff) + .data$start
      )) |>
      dplyr::select(-c("start", "date_diff"))
  }
  return(x)
}

#' Function to add a clinical data table in a cdm_reference.
#'
#' @param cdm A cdm_reference object.
#' @param tableName Name of the table to be created.
#' @param numberRows Number of records in the table.
#' @param seed Seed for random numbers reproducibility.
#' @param cdmVersion Verison of the cdm.
#'
#' @return clinical table
#'
#' @noRd
#'
generateClinicalDataTable <- function(cdm,
                                      tableName,
                                      numberRows,
                                      seed = 1,
                                      cdmVersion = attr(cdm, "cdm_version")) {
  # initial checks
  # checkInput(
  #   cdm = cdm, tableName = tableName, numberRows = numberRows, seed = seed,
  #   cdmVersion = cdmVersion,
  #   .options = list(cdmRequiredTables = c("person", "observation_period"))
  # )

  if (!is.null(seed)) {
    set.seed(seed = seed)
  }

  # id
  table <- dplyr::tibble(!!paste0(tableName, "_id") := seq_len(numberRows)) |>
    correctTable(
      tableName = tableName, cdmVersion = cdmVersion, warning = FALSE
    )

  # person_id
  if ("person_id" %in% colnames(table)) {
    table <- table |>
      dplyr::mutate("person_id" = sample(
        x = cdm$person |> dplyr::pull("person_id"), size = numberRows, replace = TRUE
      ))
  }

  if(tableName == "condition_occurrence") {
    tableName = "condition"
  }

  # dates
  dates <- paste0(tableName, c("_date", "_end_date", "_start_date"))
  dates <- dates[dates %in% colnames(table)]
  table <- table |> addDates(dates, cdm$observation_period, seed = seed)

  return(table)
}
