#' @title Calculate group composition using, the most famous, Careers table.
#' @description This function takes time interval and calculates group composition.
#' @param Careers name of the sparrow weaver project careers table loaded in R
#' @param career_startDate character type. Name of the column specifying residence start times in Careers table, for GC calculations
#' @param career_endDate character type. Name of the column specifying residence end times in Careers table, for GC calculations
#' @param career_birdID character type. Name of the column specifying bird ID in Careers table. Please, note that this name has to be common across the Careers and All Birds tables
#' @param career_season character type. Name of the column specifying season in Careers table
#' @param season for a specific time interval, define the season
#' @param group group name for group composition calculation
#' @param interval vector of length 2. Each element follwing format = "%Y-%m-%d". The first element corresponds to the start date of the interval
#' @param all_birds name of the All Birds table loaded in R
#' @param maturationTime Number of days a bird is considered as a fledgling after hatching. It serves to count birds inthe first days of life
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
                     maturationTime) {


  #####
  # names to store data
  groupSizeTotal <- base::as.numeric(NA)
  groupSizeMale <- base::as.numeric(NA)
  groupSizeFemale <- base::as.numeric(NA)
  groupSizeUnk <- base::as.numeric(NA)
  RecentFledglings <- base::as.numeric(NA)
  SeasonFledglings <- base::as.numeric(NA)
  logic <- base::as.character(NA)


  # interval for careers
  start <- base::as.Date(interval[1], format="%Y-%m-%d")
  end <- base::as.Date(interval[2], format="%Y-%m-%d")


  # subset of Careers for group
  if(base::any(base::names(Careers) == "GROUP")){
    subset.careers <- Careers[base::as.character(Careers[["GROUP"]])==group,]
  } else {
    stop("Please, check that 'GROUP' is a valid column name in Careers")
  }

  # if there is no Careers for a group
  if (base::nrow(subset.careers)==0) {
    groupSizeTotal <- NA
    groupSizeMale <- NA
    groupSizeFemale <- NA
    groupSizeUnk <- NA
    RecentFledglings <- NA
    SeasonFledglings <- NA

    logic <- "No Career information for this group"
    return (base::c(Total=groupSizeTotal, Males=groupSizeMale, Females=groupSizeFemale, Unk=groupSizeUnk,
                    UnmatF=RecentFledglings,  SeasonFledglings = SeasonFledglings,
                    logic=logic))
    next()}

  if (base::sort(subset.careers[[career_startDate]])[1] > end) {    # if there is no CAREERS for that interval
    groupSizeTotal <- NA
    groupSizeMale <- NA
    groupSizeFemale <- NA
    groupSizeUnk <- NA
    RecentFledglings <- NA
    SeasonFledglings <- NA
    logic <- "Interval starts before Career records"

  } else {
    # subset 2
    subset2 <- subset.careers[subset.careers[[career_startDate]] <= start & subset.careers[[career_endDate]] >= end,]
    subset2 <-  subset2[complete.cases(subset2[[career_birdID]]),]
    subset2[[career_birdID]] <- base::as.character(subset2[[career_birdID]])
    if (base::nrow(subset2)==0){
      groupSizeTotal <- NA
      groupSizeMale <- NA
      groupSizeFemale <- NA
      groupSizeUnk <- NA
      RecentFledglings <- NA
      SeasonFledglings <- NA
      logic <- "No Careers for this group-interval"
    } else {
      subset2$index <- base::seq(from=1, to=nrow(subset2), by=1)
      ### fledglings present?
      ### fledglings under development
      # new column for the Start Time from All Birds
      if(base::any(base::names(all_birds) == "z.Start.Date") || base::any(base::names(all_birds) == "Start.Date")) {

        all_birds$start_inPop <- lubridate::dmy(all_birds$z.Start.Date) + (lubridate::dmy(all_birds$Start.Date) - lubridate::dmy(all_birds$z.Start.Date))
        to_join <- all_birds[, base::c(career_birdID, "start_inPop")]

        # include prev Start column in subset2
        subset2 <- dplyr::left_join(x = subset2, y = to_join, by = career_birdID)


        removed.fledglings2 <- subset2$index[subset2$Start.Context=="FLEDGED" &
                                               base::difftime(start, base::as.Date(subset2$start_inPop,
                                                                                   format="%Y-%m-%d"),
                                                              units="days") < maturationTime]


        ## fledgling from the season to remove
        removed.fledglings1 <- subset2$index[subset2$Start.Context=="FLEDGED" &
                                               subset2[[career_season]]==season]
        if (base::length(removed.fledglings1)==0){
          subset2 <- subset2
        } else{
          for (e in 1:base::length(removed.fledglings1)){
            subset2 <- subset2[subset2$index!=removed.fledglings1[e],]
          }
        }

        groupSizeTotal <- base::nrow (subset2)
        groupSizeMale <-  base::nrow (subset2[subset2$SEX=="M",])
        groupSizeFemale <- base::nrow (subset2[subset2$SEX=="F",])
        groupSizeUnk <- base::nrow (subset2[subset2$SEX=="U",])
        RecentFledglings <- base::length(removed.fledglings2)
        SeasonFledglings <- base::length(removed.fledglings1)
        logic <- "All good"
      } else {
        stop("Please, use 'z.Start.Date' and 'Start.Date' as column names in All Birds")
      }
    }
    return (base::data.frame(Total=groupSizeTotal,
                             Males=groupSizeMale,
                             Females=groupSizeFemale,
                             Unk=groupSizeUnk,
                             Immat_F=RecentFledglings,
                             SeasonFledglings = SeasonFledglings,
                             logic=logic))
  }
}
