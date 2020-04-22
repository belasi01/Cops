#' Update the COPS instrument detection limit table
#'
#' @param detection.limit is a data frame with four column named as
#' c("waves", "EdZ" ,  "EuZ" ,  "LuZ").
#'
#' @examples  # suppose you want to increase the detection limit of the EdZ 875 channel to 5e-4
#' df <- detection.limit # get the table from the .GlobalEnv
#' ix.875 = which(df$waves == 875)
#' df$EdZ[ix.875] <- 5e-4
#' cops.detection.limit(df)
#' # Suppose 565 channel is missing in the
#' # detection.limit table and you want to add it.
#' new.channel = c(565, 5e-4, 5e-4, 4e-5)
#' df<-rbind(df, new.channel) # Add the channel
#' df<-df[order(df$waves),] # order the data frame
#' cops.detection.limit(df) # update the Global variable
#' detection.limit
#'
#' @author Simon Belanger
#' @export

cops.detection.limit <- function(detection.limit) {
  assign("detection.limit", detection.limit, env = .GlobalEnv)
  rm(detection.limit)
}
