#' Spatial Plots
#'
#' @param specieslist a vector of species names for which you want to plot spatial data
#' @param x a data frame output by the data pull workflow
#' @param mgmtlayer spatial layer from `list_spatial_layers()`
#' @param days_active_threshold Numeric, scalar. Minimum number of days a camera needs to have been functioning within an occasion for a cam site id x year x occasion to be included in spatial plot.
#' @param ppn_class_threshold Numeric, scalar. Proportion of photos classified within an occasion required for a cam site id x year x occasion to be included in a spatial plot.
#' @param n_occasions_annual Numeric, scalar. Minimum number of occasions required for a cam site x year to be included in a spatial plot.
#' @param spatialgroup character, column name in mgmtlayer that denotes either the zone names or county names to summarize camera data by.
#'
#' @return a named list of ggplot objects
#' @export
#'
#' @examples
spatial_plot <- function (specieslist, x, mgmtlayer, days_active_threshold, ppn_class_threshold, n_occasions_annual, spatialgroup){


  ppn.byyear = x %>%
    dplyr::filter(days_active >= days_active_threshold) %>%
    dplyr::filter(prop_classified >= ppn_class_threshold) %>%
    group_by({{spatialgroup}},cam_site_id,season) %>%
    summarise(n.occ = n(),
              across(matches("[A-Z]*_AMT", ignore.case = FALSE), ~ifelse(sum(.)>0,1,0),
                     .names = "{sub('_AMT','_det',col)}")) %>%
    filter(n.occ >= n_occasions_annual_threshold) %>%
    group_by({{spatialgroup}},season) %>%
    summarise(n.sites = n(),
              across(matches("[A-Z]*_det", ignore.case = FALSE), ~sum(.)/n.sites,
                     .names = "{sub('_det','_ppn',col)}")) %>%
    group_by({{spatialgroup}}) %>%
    summarise(n.sites = sum(n.sites),
              across(matches("[A-Z]*_ppn", ignore.case = FALSE), ~mean(.),
                     .names = "{sub('_ppn','_mean',col)}"),
              across(matches("[A-Z]*_ppn", ignore.case = FALSE), ~sd(.),
                     .names = "{sub('_ppn','_sd',col)}"))


  map.ppn.byyear = mgmtlayer %>%
    st_transform(., crs = 3071) %>%
    left_join(st_drop_geometry(ppn.byyear))%>%
    pivot_longer(-c({{spatialgroup}}, n.sites, geometry),
                 names_to = c("Spp", ".value"),
                 names_sep="_" )





  titles <- make_clean_names(sub("(FOX|SKUNK)(.*)", "\\2 \\1",
                                 x = specieslist), case = "title")
  nspecies <- length(specieslist)


  plotlist <- lapply(seq(1:nspecies), function(x)
    ggplot(filter(dataframe, grepl(Spp, pattern = specieslist[x]))) +
      geom_sf(color="grey", mapping=aes(fill=mean)) +
      geom_sf(data=mgmtlayer, color="white", lwd=1,fill=NA) +
      labs(title=str_wrap(sprintf("Adult %s -- Proportion of Snapshot Camera with a Detection", titles[x]),80)) +
      scale_fill_continuous() +
      theme(legend.title = element_blank(),
            legend.text=element_text(size=16),
            plot.title = element_text(size=22,hjust = 0.5),
            axis.text.x = element_text( size = 16),
            axis.text.y = element_text( size = 16)))

  names(plotlist) <- specieslist
  return(plotlist)
}
