test_that("Menominee county isn't in spatial plot", {
  sswids::connect_to_sswidb(db_version = 'PROD')
  df <- readRDS("./BearCPUE2019-2024.rds")

  spatialplotBearCounty <- spatial_plot(conn=conn, df=df, days_active_threshold = 4,ppn_class_threshold = 0.95,
                                            n_occasions_annual_threshold = 11)
  spatialplotbuild <- ggplot2::ggplot_build(spatialplotBearCounty[[1]])
  ggplot.df <- spatialplotbuild$plot$data
  Menominee <- ggplot.df%>%filter(COUNTY_NAM == "Menominee")
  expect_equal(Menominee$mean, as.numeric(NA))
})
