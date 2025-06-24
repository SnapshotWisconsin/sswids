#' Spatial Plots
#'
#' Produces a list of spatial plots summarizing animal detections by a chosen spatial layer.
#'
#' @param specieslist a vector of species names for which you want to plot spatial data, defaults to NULL pulling in all columns formatted as SPECIES_AMT.
#' @param x a data frame output by the data pull workflow
#' @param mgmtlayer spatial layer from `list_spatial_layers()`, defaults to counties
#' @param days_active_threshold Numeric, scalar. Minimum number of days a camera needs to have been functioning within an occasion for a cam site id x year x occasion to be included in spatial plot.
#' @param ppn_class_threshold Numeric, scalar. Proportion of photos classified within an occasion required for a cam site id x year x occasion to be included in a spatial plot.
#' @param n_occasions_annual Numeric, scalar. Minimum number of occasions required for a cam site x year to be included in a spatial plot.
#' @param spatialgroup character, column name in mgmtlayer that denotes either the zone names or county names to summarize camera data by. Defaults to COUNTY_NAM.
#' @param young logical, defaults to FALSE, should spatial plots be made for YOUNG_AMT
#'
#' @return a named list of ggplot objects
#'
#' @import ggplot2
#' @export
#'
#' @examples
spatial_plot <- function (x, specieslist=NULL, mgmtlayer=get_spatial_data("counties"), days_active_threshold, ppn_class_threshold, n_occasions_annual, spatialgroup="COUNTY_NAM", young=FALSE){

  g <- DBI::dbGetQuery(conn,"SELECT
  sswi_metadata_zoon_key_seq_no,
  sswi_metadata_zoon_map_seq_no,
  metadata_name,
  zoon_key
  FROM
  g83100.sswi_metadata_zoon_key_ref;")


  if(!is.null(specieslist)){
    specieslist <- toupper(specieslist)

  }

  species <- stringr::str_extract(colnames(x), pattern =  ".*(?=_AMT)")
  specieslist <- species[species != "" & !is.na(species)]



  if(young==FALSE){
    specieslist <- specieslist[!grepl(x = specieslist, pattern = "YOUNG")]
    x <- x%>%select(-matches("YOUNG"))
  }

  cat("Making plots for:", specieslist)

  if(!(spatialgroup %in% colnames(effort_by_occ_df_counttriggers_sf))){
    x = mgmtlayer %>% select({{spatialgroup}})%>%
      sf::st_join(sf::st_transform(effort_by_occ_df_counttriggers_sf, st_crs(mgmtlayer)))
  }

  ppn.byyear = x %>%
    dplyr::filter(days_active >= days_active_threshold) %>%
    dplyr::filter(prop_classified >= ppn_class_threshold) %>%
    dplyr::group_by(.data[[spatialgroup]],cam_site_id,season) %>%
    dplyr::summarise(n.occ = dplyr::n(),
                     dplyr::across(tidyselect::matches("[A-Z]*_AMT", ignore.case = FALSE), ~ifelse(sum(.)>0,1,0),
                     .names = "{sub('_AMT','_det',col)}")) %>%
    dplyr::filter(n.occ >= n_occasions_annual) %>%
    dplyr::group_by(.data[[spatialgroup]],season) %>%
    dplyr::summarise(n.sites = dplyr::n(),
                     dplyr::across(tidyselect::matches("[A-Z]*_det", ignore.case = FALSE), ~sum(.)/n.sites,
                     .names = "{sub('_det','_ppn',col)}")) %>%
    dplyr::group_by(.data[[spatialgroup]]) %>%
    dplyr::summarise(n.sites = sum(n.sites),
                     dplyr::across(tidyselect::matches("[A-Z]*_ppn", ignore.case = FALSE), ~mean(.),
                     .names = "{sub('_ppn','_mean',col)}"),
                     dplyr::across(tidyselect::matches("[A-Z]*_ppn", ignore.case = FALSE), ~sd(.),
                     .names = "{sub('_ppn','_sd',col)}"))%>%
    tidyr::pivot_longer(-c(.data[[spatialgroup]], n.sites, geometry),
                        names_to = c("Spp", ".value"),
                        names_pattern="(.*)_(mean|sd)" ) #regex grouping to deal with different columns for different species/scenarios(e.g.BEAR_ADULT_AMT or FOXRED_AMT and FOXGRAY_AMT)








  titles <- janitor::make_clean_names(sub("(FOX|SKUNK|PIG|CRANE|CAT|DOG|GROUSE)(.*)", "\\2 \\1",
                                 x = specieslist), case = "title")
  nspecies <- length(specieslist)


  plotlist <- lapply(seq(1:nspecies), function(x)
    ggplot(filter(dataframe, grepl(Spp, pattern = specieslist[x]))) +
      geom_sf(color="grey", mapping=aes(fill=mean)) +
      geom_sf(data=mgmtlayer, color="white", lwd=1,fill=NA) +
      labs(title=stringr::str_wrap(sprintf("Adult %s -- Proportion of Snapshot Camera with a Detection", titles[x]),80)) +
      scale_fill_continuous() +
      theme(legend.title = element_blank(),
            legend.text=element_text(size=16),
            plot.title = element_text(size=22,hjust = 0.5),
            axis.text.x = element_text( size = 16),
            axis.text.y = element_text( size = 16)))

  names(plotlist) <- specieslist
  return(plotlist)
}
