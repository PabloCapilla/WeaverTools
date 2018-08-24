#' @title Calculate group composition using, the most famous, Careers table.
#' @description This function takes time interval and calculates group composition.
#' @param Careers name of the sparrow weaver project careers table loaded in R
#' @param career_startDate character type. Name of the column specifying residence start times in Careers table, for GC calculations
#' @param career_endDate character type. Name of the column specifying residence end times in Careers table, for GC calculations
#' @param career_birdID character type. Name of the column specifying bird ID in Careers table. Please, note that this name has to be common across the Careers and All Birds tables
#' @param career_season character type. Name of the column specifying season in Careers table
#' @param season for a specific time interval, define the season
#' @param group group name for group composition calculation
#' @param interval vector of length 2. Each element follwing format = %Y-%m-%d. The first element corresponds to the start date of the interval
#' @param all_birds name of the All Birds table loaded in R
#' @param maturationTime Number of days a bird is considered as a fledgling after hatching. It serves to count birds inthe first days of life
#' @param time_steps number of intervals to divide period of gc calculations
#' @return A vector with the following information
#' @export
wbsw_gc <- function (Careers,
                     career_startDate,
                     career_endDate,
                     career_birdID,
                     career_season,
                     season,
                     group,
                     interval,
                     all_birds,
                     maturationTime,
                     time_steps) {
  groupSizeTotal <- base::as.numeric(NA)
  groupSizeMale <- base::as.numeric(NA)
  groupSizeFemale <- base::as.numeric(NA)
  groupSizeUnk <- base::as.numeric(NA)
  RecentFledglings <- base::as.numeric(NA)
  SeasonFledglings <- base::as.numeric(NA)
  logic <- base::as.character(NA)
  start <- base::as.Date(interval[1], format = "%Y-%m-%d")
  end <- base::as.Date(interval[2], format = "%Y-%m-%d")
  if (base::any(base::names(Careers) == "GROUP")) {
    subset.careers <- Careers[base::as.character(Careers[["GROUP"]]) ==
                                group, ]
  } else {
    stop("Please, check that 'GROUP' is a valid column name in Careers")
  }
  if (base::nrow(subset.careers) == 0) {
    groupSizeTotal <- NA
    groupSizeMale <- NA
    groupSizeFemale <- NA
    groupSizeUnk <- NA
    RecentFledglings <- NA
    SeasonFledglings <- NA
    logic <- "No Career information for this group"
    return(base::c(Total = groupSizeTotal, Males = groupSizeMale,
                   Females = groupSizeFemale, Unk = groupSizeUnk, UnmatF = RecentFledglings,
                   SeasonFledglings = SeasonFledglings, logic = logic))
    next()
  }
  if (base::sort(subset.careers[[career_startDate]])[1] > end) {
    groupSizeTotal <- NA
    groupSizeMale <- NA
    groupSizeFemale <- NA
    groupSizeUnk <- NA
    RecentFledglings <- NA
    SeasonFledglings <- NA
    logic <- "Interval starts before Career records"
    return(base::c(Total = groupSizeTotal, Males = groupSizeMale,
                   Females = groupSizeFemale, Unk = groupSizeUnk, UnmatF = RecentFledglings,
                   SeasonFledglings = SeasonFledglings, logic = logic))
  } else {
    # provided that there are data, now the calculation is done
    sample_df <- data.frame(sampling_vector = seq(lubridate::ymd(start) + time_steps/2,
                                                  lubridate::ymd(end) - time_steps/2,
                                                  time_steps),
                            groupSizeTotal = as.numeric(NA),
                            groupSizeMale = as.numeric(NA),
                            groupSizeFemale = as.numeric(NA),
                            groupSizeUnk = as.numeric(NA),
                            RecentFledglings = as.numeric(NA),
                            SeasonFledglings = as.numeric(NA),
                            logic = NA)


    for(t in 1:nrow(sample_df)){
      sample_gc <- subset.careers[subset.careers[[career_startDate]] <= sample_df$sampling_vector[t] &
                                    subset.careers[[career_endDate]] >= sample_df$sampling_vector[t],]
      sample_gc <- sample_gc[stats::complete.cases(sample_gc[[career_birdID]]),]
      sample_gc[[career_birdID]] <- base::as.character(sample_gc[[career_birdID]])

      # find bird types
      if (base::nrow(sample_gc) == 0) {
        sample_df$groupSizeTotal[t] <- NA
        sample_df$groupSizeMale[t] <- NA
        sample_df$groupSizeFemale[t] <- NA
        sample_df$groupSizeUnk[t] <- NA
        sample_df$RecentFledglings[t] <- NA
        sample_df$SeasonFledglings[t] <- NA
        logic <- "No Careers for this group-interval"
      } else {
        sample_gc$index <- base::seq(from = 1, to = nrow(sample_gc),
                                     by = 1)
        if (base::any(base::names(all_birds) == "z.Start.Date") ||
            base::any(base::names(all_birds) == "Start.Date")) {

          ## NA in zStart?
          all_birds$z.Start.Date_OK <- ifelse(all_birds$z.Start.Date == "",
                                              as.character(all_birds$Start.Date),
                                              as.character(all_birds$z.Start.Date))

          all_birds$start_inPop <- lubridate::dmy(all_birds$z.Start.Date_OK) +
            (lubridate::dmy(all_birds$Start.Date) - lubridate::dmy(all_birds$z.Start.Date_OK))
          to_join <- all_birds[, base::c(career_birdID,
                                         "start_inPop")]
          to_join$BIRD.ID <- as.character(to_join$BIRD.ID)
          subset2 <- dplyr::left_join(x = sample_gc, y = to_join,
                                      by = career_birdID)
          removed.fledglings2 <- subset2$index[subset2$Start.Context ==
                                                 "FLEDGED" & base::difftime(start, base::as.Date(subset2$start_inPop,
                                                                                                 format = "%Y-%m-%d"),
                                                                            units = "days") < maturationTime]
          removed.fledglings1 <- subset2$index[subset2$Start.Context ==
                                                 "FLEDGED" & subset2[[career_season]] == season]
          if (base::length(removed.fledglings1) == 0) {
            subset2 <- subset2
          } else {
            for (e in 1:base::length(removed.fledglings1)) {
              subset2 <- subset2[subset2$index != removed.fledglings1[e],]
            }
          }
          sample_df$groupSizeTotal[t] <- base::nrow(subset2)
          sample_df$groupSizeMale[t] <- base::nrow(subset2[subset2$SEX ==
                                                             "M", ])
          sample_df$groupSizeFemale[t] <- base::nrow(subset2[subset2$SEX ==
                                                               "F", ])
          sample_df$groupSizeUnk[t] <- base::nrow(subset2[subset2$SEX ==
                                                            "U", ])
          sample_df$RecentFledglings[t] <- base::length(removed.fledglings2)
          sample_df$SeasonFledglings[t] <- base::length(removed.fledglings1)
          sample_df$logic[t] <- "All good"
        }
        else {
          stop("Please, use 'z.Start.Date' and 'Start.Date' as column names in All Birds")
        }
      }
    }
    return(base::apply(X = sample_df[,c(2:7)], 2, FUN = function(X){mean(X, na.rm = TRUE)}))
  }
}
