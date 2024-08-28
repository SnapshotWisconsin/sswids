# sswids (development version)



# sswids v0.0.0.9001

Changes:

* Edits to data cleaning functions (rm_noloc_data, rm_bad_batches, remove_low_precision_lat_long
, merge_nearby_cameras, detect_remove_overlap, spatial_subset) to convert to a list based workflow instead of applying function to one of 3 dataframes and then filtering the other 2 manually.

* Small edit to calculate_proportion_classified to remove autoclassified unknown animals as autoclassification was not previously integrated.

* Edit to write_raw_data function including optional argument to allow for the modification of file names to prevent overwriting data if necessary.

* added argument to summarize_detections function "summary_value" that allows you to specify whether you want sum of triggers or maximum count per cam site id-occasion-years.

* Creation of NEWS.md file.

* Updated .Rmd/vignette to illustrate data pull workflow of new version.


# sswids v0.0.0.9000 (development version)

* Creation of NEWS.md, no previous change to version despite changes
