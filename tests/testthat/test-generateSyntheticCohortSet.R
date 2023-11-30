test_that("cohort table class and name", {
  # class
  cdm <- mockCdm() |> generateSyntheticCohortSet()
  expect_true("generated_cohort_set" %in% class(cdm$cohort))

  #cohort name
  cdm <- mockCdm() |> generateSyntheticCohortSet(tableName = "mane")
  expect_true("generated_cohort_set" %in% class(cdm$mane))

  #adding multiple cohort table
  cdm <- mockCdm() |> generateSyntheticCohortSet(tableName = "mane") |>
    generateSyntheticCohortSet(tableName = "salah")
  expect_true(all(c("mane","salah") %in% names(cdm)))

})

test_that("cohort table functionality", {


  # number of different cohort
  cdm_1 <- mockCdm() |> generateSyntheticCohortSet(numberCohorts = 1)
  expect_true(cdm_1$cohort |>
                dplyr::select(cohort_definition_id)
              |> dplyr::pull() |> unique() |> length() == 1)

  cdm_10 <- mockCdm() |> generateSyntheticCohortSet(numberCohorts = 10)
  expect_true(cdm_10$cohort |>
                dplyr::select(cohort_definition_id)
              |> dplyr::pull() |> unique() |> length() == 10)

  # average record per person
  cdm <- mockCdm(nPerson = 5) |>
    generateSyntheticCohortSet(numberCohorts = 1, recordPerson = 2)
  expect_true(cdm$cohort |> dplyr::tally() |> dplyr::pull() == 10)
  cdm <- mockCdm(nPerson = 5) |>
    generateSyntheticCohortSet(numberCohorts = 1, recordPerson = 2.5)
  expect_true(cdm$cohort |> dplyr::tally() |> dplyr::pull() == 12)
  cdm <- mockCdm(nPerson = 10) |>
    generateSyntheticCohortSet(numberCohorts = 1, recordPerson = 2.5)
  expect_true(cdm$cohort |> dplyr::tally() |> dplyr::pull() == 25)

  # different number of record per a cohort
  cdm <- mockCdm(nPerson = 5) |>
    generateSyntheticCohortSet(numberCohorts = 2, recordPerson = 2)
  expect_true(cdm$cohort |>
                dplyr::filter(cohort_definition_id == 1) |>
                dplyr::tally() |> dplyr::pull() == 10)
  expect_true(cdm$cohort |>
                dplyr::filter(cohort_definition_id == 2) |>
                dplyr::tally() |> dplyr::pull() == 10)

  cdm <- mockCdm(nPerson = 5) |>
    generateSyntheticCohortSet(numberCohorts = 2, recordPerson = c(2,3))
  expect_true(cdm$cohort |>
                dplyr::filter(cohort_definition_id == 1) |>
                dplyr::tally() |> dplyr::pull() == 10)
  expect_true(cdm$cohort |>
                dplyr::filter(cohort_definition_id == 2) |>
                dplyr::tally() |> dplyr::pull() == 15)

  cdm <- mockCdm(nPerson = 5) |>
    generateSyntheticCohortSet(numberCohorts = 2, recordPerson = c(2,3))
  expect_true(cdm$cohort |>
                dplyr::filter(cohort_definition_id == 1) |>
                dplyr::tally() |> dplyr::pull() == 10)
  expect_true(cdm$cohort |>
                dplyr::filter(cohort_definition_id == 2) |>
                dplyr::tally() |> dplyr::pull() == 15)

  expect_error(mockCdm(nPerson = 5) |>
    generateSyntheticCohortSet(numberCohorts = 3, recordPerson = c(2,3)))

  #cohort set name

  cdm <- mockCdm(nPerson = 5) |>
    generateSyntheticCohortSet(numberCohorts = 2, recordPerson = c(2,3))

  expect_true(attributes(cdm$cohort)$cohort_set
              |> dplyr::tally() |> dplyr::pull() == 2)

  expect_true(all(attributes(cdm$cohort)$cohort_set |>
              dplyr::select("cohort_name") |>
              dplyr::pull() == c("cohort_1","cohort_2")))

  cdm <- mockCdm(nPerson = 5) |>
    generateSyntheticCohortSet(
      numberCohorts = 2, cohortName = c("c_1","c_2"))

  expect_true(all(attributes(cdm$cohort)$cohort_set |>
                    dplyr::select("cohort_name") |>
                    dplyr::pull() == c("c_1","c_2")))

  expect_error(mockCdm(nPerson = 5) |>
    generateSyntheticCohortSet(
      numberCohorts = 3, cohortName = c("c_1","c_2")))

})

