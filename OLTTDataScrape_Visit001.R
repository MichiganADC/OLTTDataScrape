#!/usr/bin/env Rscript

library(dplyr)

## Get all directories
dir_path <- "./OLTT Data/Visit 001/"
dirs_list <- list.dirs(path = dir_path, recursive = FALSE, full.names = FALSE)
dirs_list <- dirs_list[grepl(pattern = "^\\d{3,}$", x = dirs_list)]

## For each directory, get the list of files in that directory,
## ... then pull specific data out of each kind of csv
oltt_lst <- lapply(X = dirs_list, function(x) { # x is folder name: 1035, 1036 ... 1448, 1449
  file_list = list.files(file.path(dir_path, x))
  data_row <- c(as.numeric(x), rep(NA, 8))
  cat(paste("Processing", x, "...\n"))
  for (file in file_list) {
    if (grepl(pattern = "Cued Recall", x = file)) {
      cr <- suppressWarnings(readr::read_csv(file.path(dir_path, x, file), na = "", 
                            trim_ws = TRUE, skip = 4, col_names = TRUE,
                            col_type = readr::cols()))
      data_row[6] <- cr[nrow(cr), "avg error cm"]
      data_row[7] <- cr[nrow(cr), "average time"]
    } else if (grepl(patter = "dottest", x = file)) {
      dot <- suppressWarnings(readr::read_csv(file.path(dir_path, x, file), na = "", 
                             trim_ws = TRUE, skip = 4, col_names = TRUE,
                             col_type = readr::cols()))
      data_row[2] <- dot[nrow(dot), "avg error cm"]
      data_row[3] <- dot[nrow(dot), "average time"]
    } else if (grepl(pattern = "Free Recall", x = file)) {
      fr <- suppressWarnings(readr::read_csv(file.path(dir_path, x, file), na = "", 
                            trim_ws = TRUE, skip = 4, col_names = TRUE,
                            col_type = readr::cols()))
      data_row[4] <- fr[nrow(fr), "avg error cm"]
      data_row[5] <- fr[nrow(fr), "average time"]
    } else if (grepl(pattern = "Recognition", x = file)) {
      rec <- suppressWarnings(readr::read_csv(file.path(dir_path, x, file), na = "", 
                             trim_ws = TRUE, skip = 4, col_names = TRUE,
                             col_type = readr::cols()))
      data_row[8] <- rec[nrow(rec), "total correct"]
      data_row[9] <- rec[nrow(rec), "avg time"]
    }
  }
  data.frame(matrix(data_row, nrow = 1, byrow = TRUE))
})

## Flatten the list of returned data frames
oltt_df <- do.call(rbind, oltt_lst)

## Name the columns/fields to match REDCap
names(oltt_df) <- c("ptid", "dot_cal_aerr", "dot_cal_at", "fr_aerr", "fr_at", 
                    "cr_aerr", "cr_at", "rt_correct", "ra_time")
# names(oltt_df)

## Convert "NA" strings to real NAs
oltt_df[oltt_df == "NA"] <- NA

## Make sure each column is numeric
oltt_df <- lapply(oltt_df, as.numeric)

## Convert oltt_df back to data frame (lapply makes it a list)
oltt_df <- data.frame(oltt_df)

oltt_df <- oltt_df %>% 
  mutate(redcap_event_name = if_else(ptid <= 1041, "visit_2_arm_1",
                                     ifelse(ptid >= 1042, "visit_1_arm_1", NA))) %>% 
  mutate(ptid = paste0("UM0000", ptid)) %>% 
  select(ptid, redcap_event_name, everything())

## Add `oltt_complete` field; 
## ... if all values aren't NA, 2 for "complete"
## ... else, 0 for "incomplete"
oltt_df <- oltt_df %>% 
  mutate(oltt_complete = ifelse(!is.na(ptid) & !is.na(redcap_event_name) & 
                                  !is.na(dot_cal_aerr) & !is.na(dot_cal_at) &
                                  !is.na(fr_aerr)      & !is.na(fr_at) &
                                  !is.na(cr_aerr)      & !is.na(cr_at) &
                                  !is.na(rt_correct)   & !is.na(ra_time), 2, 0))

# sapply(X = names(oltt_df), FUN = function(x) hist(oltt_df[, x], main = x, xlab = ""))

filename <- paste0("oltt_scraped_data_v1_", Sys.Date(), ".csv")
cat(paste("Writing csv:", paste0(Sys.Date(), "/", filename), "\n"))

system(paste0("mkdir ", Sys.Date()))
write.csv(oltt_df, paste0(Sys.Date(), "/", filename), row.names = FALSE, na = "")

cat(paste("Done.\n"))
