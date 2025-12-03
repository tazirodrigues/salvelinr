#' @title diagnose
#'
#' @description Produces basic plots looking at the number of detections for VR2 data.
#'
#' @param df A data frame object
#' @param time_col Name (*not* in quotes) of the time column in the data frame. Must be POSIX. Defaults to DateTime_LT.
#' @param fish_col Name (*not* in quotes) of the column with fish names in data frame.
#' @param sensors Boolean value related to whether the transmitters have sensors. Defaults to FALSE.
#' @param sensor_col Name (*not* in quotes) of the column that indicates sensor type. Defaults to NA.
#' @param sensor_vals Name (*not* in quotes) of the column with sensor values. Defaults to NA.
#'
#' @return Suite of plots looking at the number of VR2 detections over the time period.
#' @examples
#' @export
#' @importFrom

diagnose <- function(df, time_col = DateTime_LT, fish_col, sensors = FALSE, sensor_col = NA, sensor_vals = NA) {

  ## number of fish per day

  a <- df %>%
    mutate(Date = as.Date({{time_col}}, "%Y-%m-%d")) %>%
    group_by(Date) %>%
    summarise(num = n_distinct({{fish_col}})) %>%
    ggplot() +
    geom_point(aes(x = Date, y = num)) +
    labs(x = NULL, y = "Number of Fish Detected") +
    theme_light() +
    theme(axis.title = element_text(face = "bold"))

  b <- df %>%
    mutate(Date = as.Date({{time_col}}, "%Y-%m-%d")) %>%
    group_by(Date) %>%
    summarise(num = n()) %>%
    ggplot() +
    geom_point(aes(x = Date, y = num)) +
    labs(x = "Date", y = "Number of Detections") +
    theme_light() +
    theme(axis.title = element_text(face = "bold"))

  c <- df %>%
    mutate(Hour = format({{time_col}}, "%H")) %>%
    group_by(Hour) %>%
    summarise(num = n()) %>%
    ggplot() +
    geom_point(aes(x = Hour, y = num)) +
    labs(x = "Hour", y = "Number of Detections") +
    theme_light() +
    theme(axis.title = element_text(face = "bold"))

  if(sensors == FALSE) { a + b + c + plot_layout(ncol = 1, heights = c(1, 2, 2)) }
  else {

    d <- df %>%
      mutate(Date = as.Date({{time_col}}, "%Y-%m-%d")) %>%
      group_by(Date, {{sensor_col}}) %>%
      summarise(num = n()) %>%
      mutate(sensor = {{sensor_col}}) %>%
      ggplot() +
      geom_point(aes(x = Date, y = num, colour = sensor)) +
      labs(x = NULL, y = "Number of Sensor Readings", colour = "Sensor \nType") +
      theme_light() +
      theme(axis.title = element_text(face = "bold"))

    e <- df %>%
      mutate(Date = as.Date({{time_col}}, "%Y-%m-%d")) %>%
      group_by(Date, {{fish_col}}, {{sensor_col}}) %>%
      summarise(mean_sensor = mean({{sensor_vals}})) %>%
      group_by(Date, {{sensor_col}}) %>%
      summarise(grand_mean = mean(mean_sensor)) %>%
      mutate(sensor = {{sensor_col}}) %>%
      ggplot() +
      geom_point(aes(x = Date, y = grand_mean)) +
      facet_wrap(~sensor, scales = "free_y") +
      labs(x = "Date", y = "Grand Mean Sensor Value") +
      theme_light() +
      theme(axis.title = element_text(face = "bold"))

    a + b + c + d + e + plot_layout(ncol = 1, heights = c(1, 2, 2, 2, 2))

  }

}
