# sswids (development version)

# sswids v0.0.0.9004
7/17/2025

*add spatial_plot and temporal_plot functions to help in development of figures for advisory committees
*add optional argument "event_threshold" to summarize detections function to allow for grouping triggers into events based on a time threshold 
*added new vignette to reflect overhaul of data pull process
*new deer management unit spatial layer


# sswids v0.0.0.9003
4/4/2025

*Overhaul of data pull process with DS_Location_Effort table. Pull in the location and effort data as one. Modify it first -- removing bad locations, merging close locations, remove overlapping effort and assigning cam_site_id. Aggregate by occasion and remove too little classification effort. Then bring in detections join and summarize by occasion.

# sswids v0.0.0.9002

* Updated sswids_spatial_layers.rda with new wolf zones, DMUs, and county layer to resolve NAs and errors when joining
  these layers
  
* Updated vignette with Jen's feedback to make workflow more clear.

* Fixed weird arranging of occasions due to ntile use in create_sampling_occasions.

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
