test_that("trail type column included", {
  sswids::connect_to_sswidb(db_version = 'PROD')
  daterange <-
    create_season_dates(
      min_date = "-01-01",
      max_date = "-01-31",
      years = 2019
    )

  effort <- query_effort(conn = conn, grid = "SSWI", daterange = daterange, prec = 0.95)

  expect_true(any(colnames(effort) %in% "trailtype"))
})


