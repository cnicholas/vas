

create_hist <- function(rsg_data,
                        rsg_selected = "All",
                        bins = 25) {
  data <- rsg_data$full
  meta <- rsg_data$meta
  message("In create_hist")
  if (rsg_selected == 'All') {
    hist_data <- data %>% select(!!meta$response_symbol) %>% pull()

  } else{
    hist_data <- data %>% filter(!!meta$rsg_name_symbol == rsg_selected) %>%
      select(!!meta$response_symbol) %>% pull()

  }

  title <- paste("Histogram for RSG: ", rsg_selected)
  hist(
    hist_data,
    breaks = bins,
    main = title,
    xlab = meta$response
  )

}
