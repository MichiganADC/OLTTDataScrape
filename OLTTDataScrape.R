#!/usr/bin/env Rscript

# get all directories
dirs_list <- list.dirs(path = "./OLTT Data", recursive = FALSE, full.names = FALSE)
# dirs_list
# file.path("./OLTT Data", dirs_list)

# for each directory, get the list of files in that directory,
# then pull specific data out of each kind of csv
oltt_lst <- lapply(X = dirs_list, function(x) { # x is folder name: 1035, 1036 ... 1448, 1449
  file_list = list.files(file.path("OLTT Data", x))
  data_row <- c(as.numeric(x), rep(NA, 8))
  for (file in file_list) {
    if (grepl(pattern = "Cued Recall", x = file)) {
      cr <- readr::read_csv(file.path("OLTT Data", x, file), na = "", trim_ws = TRUE, skip = 4, col_names = TRUE)
      data_row[6] <- cr[nrow(cr), "avg error cm"]
      data_row[7] <- cr[nrow(cr), "average time"]
    } else if (grepl(patter = "dottest", x = file)) {
      dot <- readr::read_csv(file.path("OLTT Data", x, file), na = "", trim_ws = TRUE, skip = 4, col_names = TRUE)
      data_row[2] <- dot[nrow(dot), "avg error cm"]
      data_row[3] <- dot[nrow(dot), "average time"]
    } else if (grepl(pattern = "Free Recall", x = file)) {
      fr <- readr::read_csv(file.path("OLTT Data", x, file), na = "", trim_ws = TRUE, skip = 4, col_names = TRUE)
      data_row[4] <- fr[nrow(fr), "avg error cm"]
      data_row[5] <- fr[nrow(fr), "average time"]
    } else if (grepl(pattern = "Recognition", x = file)) {
      rec <- readr::read_csv(file.path("OLTT Data", x, file), na = "", trim_ws = TRUE, skip = 4, col_names = TRUE)
      data_row[8] <- rec[nrow(rec), "total correct"]
      data_row[9] <- rec[nrow(rec), "avg time"]
    }
  }
  data.frame(matrix(data_row, nrow = 1, byrow = TRUE))
})
# flatten the list of returned data frames
oltt_df <- do.call(rbind, oltt_lst)
# name the columns/fields to match REDCap
names(oltt_df) <- c("UDS_ID", "dot_cal_aerr", "dot_cal_at", "fr_aerr", "fr_at", 
                    "cr_aerr", "cr_at", "rt_correct", "ra_time")
names(oltt_df)

# convert "NA" strings to real NAs
oltt_df[oltt_df == "NA"] <- NA
oltt_df

# make sure each column is numeric
oltt_df <- lapply(oltt_df, as.numeric)
# convert oltt_df back to data frame (lapply makes it a list)
oltt_df <- data.frame(oltt_df)
# class(oltt_df) 
# class(oltt_df$dot_cal_aerr)
oltt_df
       