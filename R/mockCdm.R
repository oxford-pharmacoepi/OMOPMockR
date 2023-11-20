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
#' @param person person table
#' @param observationPeriod observation_period table
#' @param nPerson number of mock person to create in person table
#' @param birthRange the date range of the person in person table
#' @param seed random seed
#'
#' @return A cdm reference with the mock tables
#' @export
#'
mockCdm <- function(cdmName ="mock cdm",
                    cdmVersion ="5.3",
                    person = NULL,
                    observationPeriod = NULL,
                    nPerson = 5,
                    birthRange = c("1950-01-01","2000-12-31"),
                    seed = 1) {
  # check inputs
  # checkInput(
  #   seed = seed,
  #   cdmTables = list(person = person, observationPeriod = observationPeriod),
  #   nPerson = nPerson,
  #   birthRange = birthRange
  # )

  # set seed if not null
  if (!is.null(seed)) {
    set.seed(seed = seed)
  }

  #generate mock person and observation details
  person_id <- 1:nPerson

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

  }
  # set observation period if null
  if (is.null(observationPeriod)) {
    observation_period = dplyr::tibble(
      observation_period_id = person_id,
      person_id = person_id,
      observation_period_start_date = as.Date(observationDate[[1]]),
      observation_period_end_date = as.Date(observationDate[[2]])
    )

  }

  cdmTables <- list(person = person, observation_period = observation_period)

  cdm <- OMOPGenerics::cdmReference(cdmTables = cdmTables, cdmVersion = cdmVersion, cdmName = cdmName)

  return(cdm)

}


#function to generate mock observational period date from a list of dob
obsDate <- function(dob = dob, max = "2020-01-01") {

  # Initialise vector for output
  start <- rep(as.Date(NA), length(dob))
  end <- rep(as.Date(NA), length(dob))
  #generate obs start and end date
  for (i in seq_along(dob)) {
    start[i] <- sample(seq(as.Date(dob), as.Date(max), by =
                             "day"), 1)
    end[i] <- sample(seq(as.Date(start[i]), as.Date(max), by =
                           "day"), 1)
  }
  list(start,end)
}


