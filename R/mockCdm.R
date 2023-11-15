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
#' @param seed random seed
#'
#' @return A cdm reference with the mock tables
#' @export
#'
mockCdm <- function(cdmName ="mock cdm",
                    cdmVersion ="5.3",
                    person = NULL,
                    observationPeriod = NULL,
                    seed = 1) {
  # check inputs
  # checkInput(
  #   seed = seed,
  #   cdmTables = list(person = person, observationPeriod = observationPeriod)
  # )

  # set seed if not null
  if (!is.null(seed)) {
    set.seed(seed = seed)
  }
  # set mock person if null
  if (is.null(person)) {
    person = dplyr::tibble(
      person_id = c(1, 2, 3, 4),
      gender_concept_id = c(8507, 8532, 8532, 8507),
      year_of_birth = c(1993, 1991, 1995, 1998),
      month_of_birth = c(4, 7, 8, 10),
      day_of_birth = c(19, 5, 10, 22)
    )

  }

  # set observation period if null
  if (is.null(observationPeriod)) {
    observation_period = dplyr::tibble(
      observation_period_id = c(1, 2, 3, 4),
      person_id = c(1, 2, 3, 4),
      observation_period_start_date = as.Date(c(
        "2000-01-01", "2005-12-15", "1998-12-31", "2012-08-19"
      )),
      observation_period_end_date = as.Date(c(
        "2028-01-01", "2025-12-15", "2033-12-31", "2022-08-19"
      ))
    )

  }

  cdmTables <- list(person = person, observation_period = observation_period)

  cdm <- OMOPGenerics::cdmReference(cdmTables = cdmTables, cdmVersion = cdmVersion, cdmName = cdmName)

  return(cdm)

}



