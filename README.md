
# sswids

<!-- badges: start -->
<!-- badges: end -->

R package for the [Snapshot Wisconsin](https://dnr.wisconsin.gov/topic/research/projects/snapshot) Decision Support Team

## Installation

You can install the development version of sswids from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("SnapshotWisconsin/sswids", build_vignettes = TRUE)
```
Including `build_vignettes = TRUE` is the only way the vignette is included and accessible via the `utils::browseVignettes()` or `vignettes()` functions.To render vignette use 1 of the 2 functions below. 
My suggestion would be `browsevignettes("sswids")` as it renders it in internet window instead of within
Rstudio help window.
``` r
# open vignettes
browseVignettes("sswids")
vignette("DataPullTutorial")
```
## Clone repository

- On GitHub repo, go to Code tab and copy URL
- Open RStudio > File > New Project > Version Control > Git
- Paste repo URL and browse to directory where you want to clone the package on your computer
- Click on `.proj` file to open project in RStudio in the future

## Add/modify functions

- Load `devtools`
- Create new function: `use_r('new_function_name')`
- Write function in new R script that pops up
- Click somewhere inside function, then go to Code > Insert Roxygen Skeleton to add documentation
- Save R script
- Check the function is working, use `load_all()` to do tests with function right away
- Run `check()` to make sure there are no errors
- Run `document()` to make sure documentation is updated

## Commit/push to Github

- Click on `Git` tab
- Click `Staged` box next to new or modified file
- Click `Commit`
- Enter commit message
- Click `Push`

Then update R package by first removing with `remove.packages()` or `devtools::install_github("SnapshotWisconsin/sswids", force = TRUE)`

## Update spatial data

- Open `data-raw/make_spatial_data.R`
- Grab DNR GIS files using `arcpullr` package or drop shapefiles into `data-raw`
- Copy `OAS/Winiarski/snapshot/sswids_gis` into `C:/sswids_gis` to access large shapefiles/rasters
