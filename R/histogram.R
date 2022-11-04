

create_hist <- function(data, meta,
                        rsg_selected = "All",
                        bins = 25) {

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
