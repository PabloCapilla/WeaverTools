#' @title Within-individual centering function
#' @description Partition of variation within- and among-individuals following the method summarised by Van de Pol & Wright (2009) (doi: 10.1016/j.anbehav.2008.11.006)
#' @param data data frame
#' @param centering_by individual ID
#' @param cluster higher level structure. E.g. levels in centering_by further structured in cluster. NULL as default
#' @param variable character name or vector of characters. Variables to be partitioned.If more than one,please, concatenate using 'c'.
#' @return New variables added to the end of the original data frame with 'mean_' prefix for the mean value of variable for every centering_by level and prefix 'cent' for the within-indivual term.
#' @export
d_centering <- function(data,
                        centering_by,
                        cluster = NULL,
                        variable){
  # set up
  if(base::is.null(cluster)){
    mean_df <- data %>%
      dplyr::group_by_(centering_by)%>%
      dplyr::summarise_at(variable, base::mean, na.rm = TRUE)
    cluster <- centering_by
  } else {
    mean_df <- data %>%
      dplyr::group_by_(cluster,centering_by) %>%
      dplyr::summarise_at(variable, base::mean, na.rm = TRUE)
  }
  # store for data
  store_data <- base::list(NA)

  pbar <- utils::txtProgressBar(min = 1,
                                max = nrow(mean_df),
                                style = 3)

  for (n in 1:base::nrow(mean_df)){
    group_obs <- base::data.frame(mean_df[n,centering_by])[1,1]
    group_cluster <- base::data.frame(mean_df[n,cluster])[1,1]


    group_df <- data[data[[centering_by]]==group_obs &
                       data[[cluster]]==group_cluster,]

    if(centering_by == cluster){
      offset <- 1
    } else {
      offset <- 2
    }

    for(c in 1:base::length(variable)){
      var_int <- variable[c]
      mean_obs <- base::as.numeric(stats::na.omit(mean_df[mean_df[[centering_by]]==group_obs &
                                                           mean_df[[cluster]]==group_cluster,(c+offset)]))

      base::assign(value = base::paste("cent_", var_int, sep=""), x = "new_cent")
      base::assign(value = base::paste("mean_", var_int, sep=""), x = "new_mean")

      group_df[[new_cent]] <- group_df[[var_int]] - mean_obs
      group_df[[new_mean]] <- base::rep(mean_obs, length = base::nrow(group_df))

    }
    store_data[[n]] <- group_df
    utils::setTxtProgressBar(pb = pbar, value = n)
  }
  return(data.table::rbindlist(store_data))
}



