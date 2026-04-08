test_that("species=NULL pulls in detections of all species", {
  sswids::connect_to_sswidb(db_version = 'PROD')
  daterange <-
    create_season_dates(
      min_date = "-01-01",
      max_date = "-01-31",
      years = 2019
    )

  detections <- query_detections(conn = conn, species = NULL, grid = "SSWI", daterange = daterange, prec = 0.95)
  nspecies <- length(unique(detections$species))
  expect_gte(object = nspecies, expected = 31)
})


test_that("warning shows when duplicate detections", {
  sswids::connect_to_sswidb(db_version = 'PROD')
  daterange <-
    create_season_dates(
      min_date = "-05-12",
      max_date = "-05-13",
      years = 2019
    )
  expect_warning(query_detections(conn = conn, species = "Fox, Red", grid = "SSWI", daterange = daterange, prec = 0.95))
})

