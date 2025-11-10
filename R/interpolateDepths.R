interpolateDepths <- function(df, var = "Temp", min, max, interval, sinkends = FALSE) {

  if ((df[var] %>% summarise_all(class)) != "numeric") { stop ("Please give me a number!") }
  if (class(df$Depth) != "numeric") { stop ("Depths must be numeric") }
  if (class(df$Temp) != "numeric") { stop ("Temperature must be numeric") }

  # set up the depths

  depth_df <- as.data.frame(seq(min, max, by = interval))
  names(depth_df) <- c("Depth")

  # go to the data we have

  df <- dcast(df[, c("Date", "Depth", var)], Depth ~ Date, mean)
  df <- merge(df, depth_df, all = TRUE)
  df[df == "NaN"] <- NA

  if (sinkends == TRUE) {

    for(i in 2:ncol(df)) { df[nrow(df), i] <- last(na.omit(df[i])) }
    for(i in 2:ncol(df)) { df[1, i] <- first(na.omit(df[i])) }

  }

  df <- na.approx(df); df <- as.data.frame(df)
  df <- melt(df, id.vars = c("Depth"))
  names(df) <- c("Depth", "Date", var)

  return(df)

}
