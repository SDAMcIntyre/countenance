read_acq_text <- function(file_name, delim, keep_channels) {
  print(paste("Reading file", file_name))
  # extract numbers from 2nd row to get sample duration in ms and convert to sampling rate
  samp_rate_hz <- file_name %>%
    read_lines(skip = 1, n_max = 1) %>%
    str_extract("[0-9]+") %>%
    as.numeric() %>%
    ( function(x) (1 / (x / 1000)) )

  # extract numbers from 3rd row to get number of channels in file
  raw_n_channels <- read_lines(file_name,
                              skip = 2, n_max = 1) %>%
    str_extract("[0-9]+") %>%
    as.numeric()

  # read channel names
  # just get the odd ones for the names (even rows are measurement units)
  raw_channel_names <- read_lines(file_name,
                                 skip = 3,
                                 n_max = 2*raw_n_channels)[seq(1,raw_n_channels*2, 2)]

  # get the column numbers for the data and stimulus channels by matching the names
  columns_to_keep <- raw_channel_names %>%
    str_which(paste(keep_channels, collapse = "|"))

  # read in the data, keep only the wanted channels
  raw_acq_data <- read_delim(file_name,
                           delim = delim,
                           skip = raw_n_channels*2+5,
                           col_names = FALSE,
                           col_types = cols())[,columns_to_keep]
  # put the names back
  names(raw_acq_data) <- raw_channel_names[columns_to_keep]

  # add time variable
  raw_acq_data <- raw_acq_data %>%
    mutate(time_sec = seq(0,n()-1)/samp_rate_hz)

  return(raw_acq_data)
}
