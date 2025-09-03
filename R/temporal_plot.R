
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
#' @param mgmtlayer spatial layer from `list_spatial_layers()`
#' @param days_active_threshold Numeric, scalar. Minimum number of days a camera needs to have been functioning within an occasion for a cam site id x year x occasion to be included in temporal plot.
#' @param ppn_class_threshold Numeric, scalar. Proportion of photos classified within an occasion required for a cam site id x year x occasion to be included in a temporal plot.
#' @param spatialgroup character, column name in mgmtlayer that denotes either the zone names or county names to summarize camera data by. Not built to handle more than one spatial group
#'
#' @return
#' @export
#'
#' @examples


temporal_plot <- function (conn, df, mgmtlayer, days_active_threshold, ppn_class_threshold, spatialgroup){

  if(any(check_season_dates(df%>%group_by(season)%>% #recreate date ranges from data frame
                        dplyr::summarise(start_date=as.Date(min(start_date)), end_date=as.Date(max(end_date)))%>%
                        sf::st_drop_geometry())$season_length_days < 365)){
    stop("Year round data is needed for temporal plots")
  }

  if(mgmtlayer == "counties"){
    warning("Are you sure you want to plot temporal trends by county? Thats 72 lines in 1 plot, and probably won't work for most species")
  }

  if(length(grep(pattern = "[A-Z]*_AMT", x = colnames(df), value = TRUE)) > 1){
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


  #All Furbearers
  df.byocc <- df %>%
    filter(days_active >= days_active_threshold) %>%
    filter(prop_classified >= ppn_class_threshold) %>%
    mutate(across(matches("[A-Z]*_AMT", ignore.case = FALSE), ~ifelse(.>0,1,0), .names = "{sub('_AMT', '_binary',col)}")) %>%
    group_by(season,occ,.data[[spatialgroup]]) %>%
    dplyr::summarise(across(matches("[A-Z]*_AMT", ignore.case = FALSE), ~sum(.),.names = "{sub('_AMT', '_sum',col)}"),
              across(matches("[A-Z]*_binary", ignore.case = FALSE), ~sum(.),.names = "{sub('_binary', '_occ',col)}"),
              num.sites = dplyr::n(),
              num.days = sum(days_active)) %>%
    mutate(across(matches("[A-Z]*_occ", ignore.case = FALSE), ~./num.sites,.names = "{sub('_occ', '_propocc',col)}"),
           across(matches("[A-Z]*_sum", ignore.case = FALSE), ~./num.days,
                  .names = "{sub('_sum','_trigsperday',col)}"))%>%
    mutate(yearocc = paste0(season+201,stringr::str_pad(occ, width=2, side="left", pad="0"))) %>%
    dplyr::arrange(yearocc) %>%
    group_by(yearocc) %>%
    mutate(time = dplyr::cur_group_id())

  #By zone
  df.byocc.long = df.byocc %>%
    select(time, season,occ,any_of(spatialgroup),num.sites,
           matches("_occ")) %>%
    tidyr::pivot_longer(cols=matches("_occ"), names_pattern  = "(.*)_occ", names_to = "Spp")%>%
    mutate(year=season+2018) #this may need to be modified

  df.byocc.long[[spatialgroup]] <- as.factor(df.byocc.long[[spatialgroup]])
  df.byocc.long$binomresponse <- with(df.byocc.long, cbind(value, num.sites - value))

  #table for number of sites, should I include this? also need to think about this for plot_spatial
  table.temporal.camsites.byocc <-df.byocc.long%>%select(year,occ,tidyselect::all_of(spatialgroup), num.sites)%>%sf::st_drop_geometry()

  species <- stringr::str_extract(colnames(df), pattern =  ".*_AMT")
  specieslist <- species[species != "" & !is.na(species)]


  cat("Making plots for:", specieslist)

  titles <- janitor::make_clean_names(sub("(FOX|SKUNK)(.*)", "\\2 \\1", x = unique(df.byocc.long$Spp)), case = "title")
  nspecies <- length(specieslist)

  #set up to loop through species dataframes
  binomlist <- lapply(seq(1:nspecies), function(i){
    speciesframe <- filter(df.byocc.long, grepl(Spp, pattern = unique(df.byocc.long$Spp)[i]))
    colnames(speciesframe)[which(colnames(speciesframe) == spatialgroup)] <- "zone" #can't handle multiple spatial groups

    nocc <- length(unique(speciesframe$occ))
    knots <- list(occ = c(0.5, nocc+0.5))
    nyears <- length(unique(speciesframe$season))


    #model with year x occ interaction as well as occ x zone interaction
    m2y <- mgcv::gam(binomresponse ~ zone + s(season, k=nyears, by=zone) + s(occ, bs = "cc", k=nocc, by=zone) +
                       ti(season, occ, bs = c("tp", "cc")),
               data = speciesframe,
               family = binomial,
               knots = knots)




    newdatay <- expand.grid(season=unique(speciesframe$season), zone=unique(speciesframe$zone), occ=nocc/2)

    occeffects <- paste("s(occ)", paste0("zone",unique(speciesframe$zone)), sep=":")
    #predict just year trend
    yrtrendm2y <- gratia::fitted_values(m2y, data=newdatay, exclude=c(occeffects,"ti(season,occ)"))%>%mutate(time=rep(seq(nocc/2, length.out=nyears, by=nocc),length(unique(speciesframe$zone))))


    #predict whole model

    occtrendm2y <- gratia::fitted_values(m2y)%>%mutate(time=rep(1:(nocc*nyears), each=length(unique(speciesframe$zone))))



    plottemp <-  ggplot() +
      geom_line(data=occtrendm2y, aes(x = time, y = .fitted, color=zone), lwd=0.5) +
      geom_line(data=yrtrendm2y, aes(x = time, y = .fitted, color=zone), lwd=2) +
      geom_pointrange(data=yrtrendm2y, aes(x= time, y= .fitted,ymin = .lower_ci, ymax = .upper_ci, color=zone), size=1, lwd=1) +
      labs(title=stringr::str_wrap(sprintf("Weekly Proportion of Snapshot Camera Sites with %s detections", titles[i]),75),
           y = "Proportion of sites",
           x = "Time",
           subtitle = sprintf("Year Round, %s - %s", min(speciesframe$year), max(speciesframe$year))) +
      geom_vline(xintercept=seq(1,(nocc+1)*nyears,nocc)) +
      scale_x_continuous(labels = seq(min(speciesframe$year),max(speciesframe$year),1), breaks = seq(nocc/2,nocc*nyears,nocc)) +
      scale_color_brewer(palette = "Set2",
                         name = "Mgmt Zone",
                         labels = unique(speciesframe$zone)) +
      scale_fill_brewer(palette = "Set2",
                        name = "Mgmt Zone",
                        labels = unique(speciesframe$zone))

    return(plottemp)
  })

  names(binomlist) <- specieslist
  return(binomlist)
}
