
#' Estimate temporal trends from Snapshot data
#'
#' Use year round data to decompose detections into seasonal and trend components for
#' possible use in annual decision making products. Meant to be used with data pulled
#' year round and with > 4 years of data or it will throw errors. Returns a list of
#' ggplot objects based on the number of species. Can only be used with one spatial
#' layer by which to summarise.
#'
#' @param conn connection to the Snapshot database from `connect_to_sswidb()`
#' @param df a sf data frame output by the data pull workflow
#' @param mgmtlayer character, spatial layer from `list_spatial_layers()`
#' @param days_active_threshold Numeric, scalar. Minimum number of days a camera needs to have been functioning within an occasion for a cam site id x year x occasion to be included in temporal plot.
#' @param ppn_class_threshold Numeric, scalar. Proportion of photos classified within an occasion required for a cam site id x year x occasion to be included in a temporal plot.
#' @param spatialgroup character, column name in mgmtlayer that denotes either the zone names or county names to summarize camera data by. Not built to handle more than one spatial group
#' @param combine_cols logical, should species age/sex columns be summed together? Defaults to TRUE
#'
#' @return
#' @export
#'
#' @examples


temporal_plot <- function (conn, df, mgmtlayer, days_active_threshold, ppn_class_threshold, spatialgroup, combine_cols=TRUE){

  daterange <- df%>%group_by(season)%>% #recreate date ranges from data frame
    dplyr::summarise(start_date=as.Date(min(start_date)), end_date=as.Date(max(end_date)))%>%
    sf::st_drop_geometry()

  # if(any(check_season_dates(daterange)$season_length_days < 365)){
  #   stop("Year round data is needed for temporal plots")
  # }

  if(mgmtlayer == "counties"){
    warning("Are you sure you want to plot temporal trends by county? Thats 72 lines in 1 plot, and probably won't work for most species")
  }

  if(length(grep(pattern = "[A-Z]*_AMT", x = colnames(df), value = TRUE)) > 1 & combine_cols == TRUE){
  df <- combine_species_cols(conn = conn, df=df) # helper function can be found in utils.R
  }

  if(!(spatialgroup %in% colnames(df))){
    df <- df %>%
      sf::st_join(
        .,
        sf::st_transform(get_spatial_data(mgmtlayer), 4326),
        join = sf::st_within
      )
  }



  df2 <- df %>%
    filter(days_active >= days_active_threshold) %>%
    filter(prop_classified >= ppn_class_threshold) %>%
    mutate(across(matches("[A-Z]*_AMT", ignore.case = FALSE),
                  ~ifelse(.>0,1,0), .names = "{sub('_AMT', '_binary',col)}"),
           lat=scale(sf::st_coordinates(.)[,2]),
           lon=scale(sf::st_coordinates(.)[,1]),
           camera_version2=as.factor(ifelse(camera_version == "V4", 2, 1)))%>%
    sf::st_drop_geometry()

  colnames(df2)[which(colnames(df2) == spatialgroup)] <- "zone" #can't handle multiple spatial groups
  df2$year <- df2$season+2018
  df2$camera_version <- as.factor(df2$camera_version)
  df2$zone <- as.factor(df2$zone)
  df2$cam_site_id <- as.factor(df2$cam_site_id)



  #table for number of sites, should I include this? also need to think about this for plot_spatial
  table.temporal.camsites.byocc <-df2%>%group_by(year, occ, zone)%>%dplyr::summarise(num.sites=dplyr::n(), .groups = "drop_last")%>%
    group_by(year, zone)%>%dplyr::summarise(mean.num.sites=mean(num.sites), .groups = "drop_last")

  nocc <- length(unique(df2$occ))
  knots <- list(occ = c(0.5, nocc+0.5))
  nyears <- length(unique(df2$season))

  species <- stringr::str_extract(colnames(df), pattern =  ".*_AMT")
  specieslist <- species[species != "" & !is.na(species)]


  message(cat("Making plots for:", specieslist))

  titles <- janitor::make_clean_names(sub("(FOX|SKUNK)(.*)", "\\2 \\1", x = gsub(pattern = "_AMT", replacement = "", x = specieslist)), case = "title")
  nspecies <- length(specieslist)


  #####              make new data data frame for prediction     #######
  ### calculate evenly spaced points in management zones for prediction
  cellsize <- rep(sqrt(2.59e7), 2)#10mi^2, arbitrary choice
  MgmtLayer <- get_spatial_data(mgmtlayer)
  CountiesNoDoor <- get_spatial_data("counties")%>%filter(COUNTY_NAM != "Door")%>%
    sf::st_transform(., sf::st_crs(MgmtLayer))%>%sf::st_union()
  nzones <- length(unique(df2$zone))
  MgmtLayer <- MgmtLayer%>%sf::st_make_valid()%>%
    sf::st_cast(., "POLYGON")%>%mutate(area = sf::st_area(.)) %>% group_by(.data[[spatialgroup]])%>%
    dplyr::arrange(desc(area))%>% #all zones will have to be the biggest areas, which seems feasible
    dplyr::slice(1)%>% #get the biggest polygon for each management zone
    ungroup()%>%sf::st_intersection(., CountiesNoDoor)
  zonenames <- MgmtLayer%>%select(tidyselect::matches(spatialgroup))%>%
    sf::st_drop_geometry()%>%dplyr::distinct()%>%dplyr::arrange(.data[[spatialgroup]])%>%dplyr::pull()
  splitzones <- MgmtLayer%>%dplyr::group_split(.data[[spatialgroup]])%>%
    purrr::map( ~ sf::st_transform(.x, crs=3071))%>%purrr::set_names(~zonenames)
  zonegrids <- splitzones%>%purrr::map( ~ sf::st_make_grid(.x, cellsize, what="centers"))
  zonegrids2 <- zonegrids%>%purrr::map2(.x=., .y=splitzones, ~ as.data.frame(do.call(rbind, sf::st_intersection(.x, .y)))%>%dplyr::rename("X"="V1", "Y"="V2"))
  #calculate knots from potential knots
  set.seed(1)
  knots <- zonegrids2%>%purrr::map( ~ as.data.frame(fields::cover.design(.x, 10)$design)%>%sf::st_as_sf(., coords=c("X","Y"), crs=3071))
  knots2 <- knots%>%dplyr::bind_rows(., .id="zone")%>%sf::st_transform(., crs=4326)%>%
    mutate(lat=(sf::st_coordinates(.)[,2] - attr(df2$lat, "scaled:center"))/attr(df2$lat, "scaled:scale"),
           lon=(sf::st_coordinates(.)[,1] - attr(df2$lon, "scaled:center"))/attr(df2$lon, "scaled:scale"))%>%
    sf::st_drop_geometry()

  Covs <- expand.grid(season=unique(df2$season),
                      camera_version2=levels(df2$camera_version2),
                      occ=seq(1,52, by=1))%>%dplyr::arrange(season, occ)
  newdata <- dplyr::cross_join(knots2, Covs)
  newdata$zone <- as.factor(newdata$zone)

  #set up to loop through species dataframes
  binomlist <- lapply(seq(1:nspecies), function(i){

    spp <- gsub(x = specieslist, pattern = "_AMT", replacement = "")[i]
    AMTcols <- grep(pattern = "_AMT", x = colnames(df2), value=TRUE)
    BINARYcols <-   grep(pattern = "_binary", x = colnames(df2), value=TRUE)
    notcols <- grep(pattern = spp, x = c(AMTcols, BINARYcols), value=TRUE, invert=TRUE)

    speciesframe <- df2%>%dplyr::select(-any_of(notcols))
    colnames(speciesframe)[grep(pattern = spp, x = colnames(speciesframe))] <- c("Spp_AMT", "Spp_binary")






    #model with year x occ interaction as well as occ x zone interaction



      if(spp == "BEAVER") {

        BeaverDF <- speciesframe%>%mutate(across(c(lat, lon), ~.x * attr(.x, "scaled:scale") + attr(.x, "scaled:center"), .names = "{.col}_unscaled"))%>%
          sf::st_as_sf(., coords=c("lon_unscaled", "lat_unscaled"), crs=4326)%>%sf::st_join(., sf::st_transform(get_spatial_data("beaver_zones"), crs = 4326))%>%
          filter(ZONE != "D")%>%sf::st_drop_geometry()%>%dplyr::select(-zone)%>%dplyr::rename(zone=ZONE)%>%mutate(zone=as.factor(zone))

        colnames(BeaverDF)[grep(pattern = "BEAVER", x = colnames(BeaverDF))] <- c("Spp_AMT", "Spp_binary")

        Beaver.newdata <- expand.grid(zone=unique(BeaverDF$zone), season=unique(BeaverDF$season),
                                      camera_version2=unique(BeaverDF$camera_version2),
                                      occ=seq(1,52, by=1))%>%dplyr::arrange(season, zone, occ)

        BamPre <- mgcv::bam(Spp_binary ~ zone * season + camera_version2 +
                                 s(occ, bs = "cc", k=nocc, by=zone),
                               data = BeaverDF,
                               family = binomial,
                               knots = knots,
                               discrete=TRUE,
                               nthreads=2)

        meansBAMPre <- modelbased::estimate_means(BamPre, by = c("zone", "season"), newdata=Beaver.newdata)%>%
          mutate(time=(nocc/2)+nocc*(season-1))#takes a while
        occBAMPre <- modelbased::estimate_means(BamPre, by = c("zone", "season", "occ"), newdata=Beaver.newdata)%>%mutate(time=occ+nocc*(season-1))


      }else{


        BamPre <- mgcv::bam(Spp_binary ~ zone * season + camera_version2 + s(lat, lon, k=60) +
                                 s(occ, bs = "cc", k=nocc, by=zone),
                               data = speciesframe,
                               family = binomial,
                               knots = knots,
                               discrete=TRUE,
                               nthreads=2)


        meansBAMPre <- modelbased::estimate_means(BamPre, by = c("zone", "season"), newdata=newdata)%>%mutate(time=(nocc/2)+nocc*(season-1))#takes a while
        occBAMPre <- modelbased::estimate_means(BamPre, by = c("zone", "season", "occ"), newdata=newdata)%>%mutate(time=occ+nocc*(season-1))

      }


    if(spp == "BEAVER"){
      legendlabels <- levels(BeaverDF$zone)
    }else{
      legendlabels <- levels(speciesframe$zone)
    }



    plottemp <- ggplot() +
      geom_line(data=occBAMPre, aes(x = time, y = Probability, color=zone), lwd=0.5) +
      geom_line(data=meansBAMPre, aes(x = time, y = Probability, color=zone), lwd=2) +
      geom_pointrange(data=meansBAMPre, aes(x= time, y= Probability,ymin = CI_low, ymax = CI_high, color=zone), size=1, lwd=1) +
      labs(title=stringr::str_wrap(sprintf("Weekly Probability of %s Detection at Snapshot Sites", titles[i]),75),
           y = "Probability",
           x = "Time",
           subtitle = sprintf("Year Round, %s - %s", min(speciesframe$year), max(speciesframe$year))) +
      geom_vline(xintercept=seq(1,(nocc+1)*nyears,nocc)) +
      scale_x_continuous(labels = seq(min(speciesframe$year),max(speciesframe$year),1), breaks = seq(nocc/2,nocc*nyears,nocc)) +
      scale_color_brewer(palette= "Set2",
                         name = "Mgmt Zone",
                         labels = legendlabels) +
      theme_minimal() +
      theme(legend.text = element_text(size=12),
            legend.title = element_text(size=14),
            axis.text = element_text(size=12),
            axis.title = element_text(size=14))

    return(plottemp)
  })

  names(binomlist) <- specieslist
  binomlist <- append(binomlist, list(samplesizetable=table.temporal.camsites.byocc))
  return(binomlist)
}
