
#' Summarize by Occasion
#'
#' @param dataframe
#' @param yearadd
#'
#' @return
#' @export
#'
#' @examples
#'
#'


summarize_by_occasion <- function(dataframe, yearadd){

dfTemporal <- dfAll#%>%filter(start_date >= "2018-12-31" & start_date  <= "2024-07-01")#%>%filter(season %in% c(2:7))

df.byocc <- dfTemporal %>%
  filter(days_active >= days_active_threshold) %>%
  filter(prop_classified >= ppn_class_threshold) %>%
  mutate(across(matches("[A-Z]*_AMT", ignore.case = FALSE), ~ifelse(.>0,1,0), .names = "{sub('_AMT', '_binary',col)}")) %>%
  group_by(season,occ,zone) %>%
  summarise(across(matches("[A-Z]*_AMT", ignore.case = FALSE), ~sum(.),.names = "{sub('_AMT', '_sum',col)}"),
            across(matches("[A-Z]*_binary", ignore.case = FALSE), ~sum(.),.names = "{sub('_binary', '_occ',col)}"),
            num.sites = n(),
            num.days = sum(days_active)) %>%
  mutate(across(matches("[A-Z]*_occ", ignore.case = FALSE), ~./num.sites,.names = "{sub('_occ', '_propocc',col)}"),
         across(matches("[A-Z]*_sum", ignore.case = FALSE), ~./num.days,
                .names = "{sub('_sum','_trigsperday',col)}"))%>%
  mutate(yearocc = paste0(season+yearadd,str_pad(occ, width=2, side="left", pad="0"))) %>%
  arrange(yearocc) %>%
  group_by(yearocc) %>%
  mutate(time = cur_group_id())

df.byocc.both1 = df.byocc %>%
  select(time, season,occ,zone,
         matches("_propocc")) %>%
  pivot_longer(cols=matches("_propocc"))%>%
  mutate(year=season+yearadd)
}
