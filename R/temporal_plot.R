
#' Title
#'
#' @param specieslist
#' @param dataframe
#' @param year.lines
#' @param year.ticks
#'
#' @return
#' @export
#'
#' @examples


temporal_plot <- function (specieslist, dataframe, year.lines, year.ticks){
  titles <- make_clean_names(sub("(FOX|SKUNK)(.*)", "\\2 \\1", x = specieslist), case = "title")
  nspecies <- length(specieslist)

  plotlist <- lapply(seq(1:nspecies), function(x)
    ggplot(filter(dataframe, grepl(name, pattern = specieslist[x])),aes(x=time,y=value,col=zone)) +
      geom_smooth(span=0.1, lwd=0.3,se=FALSE) +
      geom_smooth(method="gam", formula= y ~ s(x, k=6)) +
      geom_vline(xintercept=year.lines) +
      theme(legend.position="right",
            legend.title=element_text(size=16),
            legend.text=element_text(size=14),
            plot.title = element_text(size=22),
            plot.subtitle=element_text(size=18),
            axis.ticks.x=element_blank()) +
      labs(title=str_wrap(sprintf("Weekly Proportion of Snapshot camera sites with %s detections", titles[x]),75),
           y = "Proportion of sites",
           x = "Time",
           subtitle = "Year Round, 2019 - 2024") +
      scale_x_continuous(breaks = year.ticks,
                         labels = seq(2019,2024,1)) +
      coord_cartesian(ylim=c(0,NA), expand = FALSE) + #xlim=c(53, 365),
      scale_color_brewer(palette = "Set2",
                         name = "Furbearer Zone",
                         labels = c("Northern Zone","Southern Zone")))
  names(plotlist) <- specieslist
  return(plotlist)
}
