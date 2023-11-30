test_that("mock person and observation_period", {

  # check nPerson is working
  cdm <- mockCdm(nPerson = 5)
  expect_true(cdm$person |> dplyr::tally() |> dplyr::pull() == 5)
  cdm <- mockCdm(nPerson = 20)
  expect_true(cdm$person |> dplyr::tally() |> dplyr::pull() == 20)
  # check gender id
  expect_true(all(c(8507,8532) ==
                    cdm$person |> dplyr::select("gender_concept_id") |> unique() |> dplyr::pull()))

  #check birthdayRange is working
  cdm <- mockCdm(nPerson = 5, birthRange = c("1950-01-01","1950-12-31"))
  expect_true(cdm$person |> dplyr::select("year_of_birth") |> unique() == 1950)

  #check observation_period working

  cdm <- mockCdm(nPerson = 5)

  expect_no_error(cdm$observation_period)
  expect_true(cdm$observation_period |> dplyr::tally() |> dplyr::pull() == 5)


})
