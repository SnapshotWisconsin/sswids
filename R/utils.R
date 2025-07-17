
n_digits <- function(x) {

  # determine number of digits after decimal place for lat/long
  max(
    stringr::str_length(
      stringr::str_replace(
        x,
        ".*\\.(\\d+)",
        "\\1"
      )
    )
  )

}


date_overlap <- function(x, y) {

  # this is the same as the Overlap function from DescTools
  # just given a more informative name here
  x <- cbind(apply(rbind(x), 1L, min), apply(rbind(x), 1L,
                                             max))
  y <- cbind(apply(rbind(y), 1L, min), apply(rbind(y), 1L,
                                             max))
  maxdim <- max(nrow(x), nrow(y))
  x <- x[rep(1L:nrow(x), length.out = maxdim), , drop = FALSE]
  y <- y[rep(1L:nrow(y), length.out = maxdim), , drop = FALSE]
  d1 <- x[, 2L]
  idx <- x[, 2L] > y[, 2L]
  d1[idx] <- y[idx, 2L]
  d2 <- y[, 1L]
  idx <- x[, 1L] > y[, 1L]
  d2[idx] <- x[idx, 1L]
  d <- d1 - d2
  d[d <= 0L] <- 0L
  unname(d)

}


activity_by_date <- function(df) {

  # create individual dates for a date range when a camera was active

  # create sequence(s) of date(s); sometimes there are overlapping
  # start and end dates due to batches being uploaded
  seq.Date(as.Date(df$start_date), as.Date(df$end_date), by = 'day') %>%
    tibble::as_tibble() %>%
    dplyr::rename(date_active = value) %>%
    dplyr::arrange(date_active)

}


add_year <- function(...) {

  # add a year if filtering photo table for a query where season dates overlap two different years
  if(as.numeric(stringr::str_sub(min_date, 2, 3)) > as.numeric(stringr::str_sub(max_date, 2, 3))) {

    1

  } else {

    0

  }

}


detect_overlap <- function(df) {

  # detect overlap in batches/camera_location_seq_no's
  df %>%
    # we need the start/end date of the next batch to calculate overlap
    dplyr::mutate(
      next_start_date = dplyr::lead(start_date),
      next_end_date = dplyr::lead(end_date)
    ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      days_overlap_next = date_overlap(c(start_date, end_date), c(next_start_date, next_end_date))
    ) %>%
    dplyr::ungroup() %>%
    # calculate the max days of overlap by camera_location_seq_no and year
    dplyr::summarise(days_overlap_next = max(days_overlap_next, na.rm = TRUE)) %>%
    dplyr::pull(days_overlap_next)

}


combine_species_cols <- function(conn, df) {

  MetaKey <- DBI::dbGetQuery(conn,
                             "SELECT
  g83100.sswi_metadata_ref.metadata_group_code,
  g83100.sswi_metadata_ref.metadata_name
  FROM
  g83100.sswi_metadata_ref")%>%filter(stringr::str_detect(METADATA_GROUP_CODE, "^PHOTO_TAG"))%>%
    filter(stringr::str_detect(METADATA_NAME, "_AMT$"))

  MetaKey$METADATA_GROUP_CODE <- gsub(pattern = "PHOTO_TAG_", replacement = "", MetaKey$METADATA_GROUP_CODE)
  MetaKey$METADATA_GROUP_CODE <-paste0(MetaKey$METADATA_GROUP_CODE, "_AMT")

  amtcols <- grep(pattern = "[A-Z]*_AMT", x = colnames(df), value = TRUE)
  newcolnames <- MetaKey$METADATA_GROUP_CODE[match(amtcols, MetaKey$METADATA_NAME)]
  combinecollist <- split(newcolnames, newcolnames)
  combinecollist <- combinecollist[lapply(combinecollist, function (x) length(x) > 1)==TRUE]
  #colnames(df)[grep("[A-Z]*_AMT", colnames(df))] <- newcolnames

  for (i in 1:length(combinecollist)) {
    matchstring <- sub("([A-Z]+)(_AMT)","\\1.*\\2",x = unique(combinecollist[[i]]))
    colstomerge <- grep(pattern = matchstring, x = colnames(df))
    colname <- combinecollist[[i]][[1]]
    speciessum <- rowSums(sf::st_drop_geometry(df[,colstomerge]))
    df <- cbind(df, speciessum)
    colnames(df)[length(df)-1] <- colname
    df <- df[-(colstomerge)]
    }
    return(df)
  }
