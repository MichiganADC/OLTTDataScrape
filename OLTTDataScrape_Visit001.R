#!/usr/bin/env Rscript

suppressMessages(library(dplyr))


# **** ----
# USEFUL VARS ----
source("~/Box/Documents/R_helpers/config.R")
source("~/Box/Documents/R_helpers/helpers.R")


## Get all directories
dir_path <- "./OLTT Data/Visit 001/"
dirs_list <- list.dirs(path = dir_path, recursive = FALSE, full.names = FALSE)
dirs_list <- dirs_list[grepl(pattern = "^\\d{3,}$", x = dirs_list)]


## Get IDs of those folks who've already been entered in REDCap
records_raw <- dirs_list %>% 
  paste0("UM", strrep("0", 8 - nchar(.)), .)
records_raw_oldcohort <- records_raw[records_raw <= "UM00001041"]
records_raw_newcohort <- records_raw[records_raw > "UM00001041"]

records_oldcohort <- records_raw_oldcohort %>% paste(collapse = ",")
records_newcohort <- records_raw_newcohort %>% paste(collapse = ",")

fields_iv_raw <-
  c(
    "ptid"
    , "form_date"
    , "header_complete"
    , "ivp_a1_complete"
    , "ivp_a2_complete"
    , "ivp_a3_complete"
    , "ivp_a4_complete"
    , "ivp_a5_complete"
    , "ivp_b1_complete"
    , "ivp_b4_complete"
    , "ivp_b5_complete"
    , "ivp_b6_complete"
    , "ivp_b7_complete"
    , "ivp_b8_complete"
    , "ivp_b9_complete"
    , "ivp_c2_complete"
    , "ivp_d1_complete"
    , "ivp_d2_complete"
  )
fields_iv <- fields_iv_raw %>% paste(collapse = ",")

fields_fv_raw <- 
  c(
    "ptid"
    , "form_date"
    , "header_complete"
    , "fvp_a1_complete"
    , "fvp_a2_complete"
    , "fvp_a3_complete"
    , "fvp_a4_complete"
    , "fvp_a5_complete"
    , "fvp_b1_complete"
    , "fvp_b4_complete"
    , "fvp_b5_complete"
    , "fvp_b6_complete"
    , "fvp_b7_complete"
    , "fvp_b8_complete"
    , "fvp_b9_complete"
    , "fvp_c1_complete"
    , "fvp_c2_complete"
    , "fvp_d1_complete"
    , "fvp_d2_complete"
  )
fields_fv <- fields_fv_raw %>% paste(collapse = ",")

json_u3_oldcohort <-
  get_rc_data_api(uri     = REDCAP_API_URI,
                  token   = REDCAP_API_TOKEN_UDS3n,
                  fields  = fields_fv,
                  records = records_oldcohort)
json_u3_newcohort <-
  get_rc_data_api(uri     = REDCAP_API_URI,
                  token   = REDCAP_API_TOKEN_UDS3n,
                  fields  = fields_iv,
                  records = records_newcohort)

df_u3_oldcohort <- jsonlite::fromJSON(json_u3_oldcohort %>% na_if(""))
df_u3_newcohort <- jsonlite::fromJSON(json_u3_newcohort %>% na_if(""))

df_u3_oldcohort_cln <- df_u3_oldcohort %>% 
  filter(redcap_event_name == "visit_2_arm_1")
df_u3_newcohort_cln <- df_u3_newcohort %>% 
  filter(redcap_event_name == "visit_1_arm_1")

df_u3_oldcohort_cln_cmp <- df_u3_oldcohort_cln %>% 
  filter(header_complete == 2
         , fvp_a1_complete == 2
         , fvp_a2_complete == 2
         , fvp_a3_complete == 2
         , fvp_a4_complete == 2
         # , fvp_a5_complete == 2
         , fvp_b1_complete == 2
         , fvp_b4_complete == 2
         , fvp_b5_complete == 2
         , fvp_b6_complete == 2
         , fvp_b7_complete == 2
         , fvp_b8_complete == 2
         , fvp_b9_complete == 2
         # , fvp_c1_complete == 2
         # , fvp_c2_complete == 2
         , fvp_d1_complete == 2
         , fvp_d2_complete == 2)
df_u3_newcohort_cln_cmp <- df_u3_newcohort_cln %>% 
  filter(header_complete == 2
         , ivp_a1_complete == 2
         , ivp_a2_complete == 2
         , ivp_a3_complete == 2
         , ivp_a4_complete == 2
         , ivp_a5_complete == 2
         , ivp_b1_complete == 2
         , ivp_b4_complete == 2
         , ivp_b5_complete == 2
         , ivp_b6_complete == 2
         , ivp_b7_complete == 2
         , ivp_b8_complete == 2
         , ivp_b9_complete == 2
         # , ivp_c2_complete == 2
         , ivp_d1_complete == 2
         , ivp_d2_complete == 2)

df_u3_cln_cmp <- bind_rows(df_u3_oldcohort_cln_cmp, df_u3_newcohort_cln_cmp)

records_cmp <- df_u3_cln_cmp %>% 
  distinct(ptid) %>% 
  pull() %>% 
  stringr::str_extract("\\d{8}$") %>% 
  as.integer() %>% 
  as.character()

dirs_list_fltr <- dirs_list[as.integer(dirs_list) %in% records_cmp]


## For each directory, get the list of files in that directory,
## ... then pull specific data out of each kind of csv
oltt_lst <- lapply(X = dirs_list_fltr, function(x) { 
  # x is folder name: 1035, 1036 ... 1448, 1449
  file_list = list.files(file.path(dir_path, x))
  data_row <- c(as.numeric(x), rep(NA, 8))
  cat(paste("Processing", x, "...\n"))
  for (file in file_list) {
    if (grepl(pattern = "Cued Recall", x = file)) {
      cr <- suppressWarnings(
        readr::read_csv(file.path(dir_path, x, file), na = "", 
                        trim_ws = TRUE, skip = 4, col_names = TRUE,
                        col_type = readr::cols()))
      data_row[6] <- cr[nrow(cr), "avg error cm"]
      data_row[7] <- cr[nrow(cr), "average time"]
    } else if (grepl(patter = "dottest", x = file)) {
      dot <- suppressWarnings(
        readr::read_csv(file.path(dir_path, x, file), na = "", 
                        trim_ws = TRUE, skip = 4, col_names = TRUE,
                        col_type = readr::cols()))
      data_row[2] <- dot[nrow(dot), "avg error cm"]
      data_row[3] <- dot[nrow(dot), "average time"]
    } else if (grepl(pattern = "Free Recall", x = file)) {
      fr <- suppressWarnings(
        readr::read_csv(file.path(dir_path, x, file), na = "", 
                        trim_ws = TRUE, skip = 4, col_names = TRUE,
                        col_type = readr::cols()))
      data_row[4] <- fr[nrow(fr), "avg error cm"]
      data_row[5] <- fr[nrow(fr), "average time"]
    } else if (grepl(pattern = "Recognition", x = file)) {
      rec <- suppressWarnings(
        readr::read_csv(file.path(dir_path, x, file), na = "", 
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
  mutate(ptid = paste0("UM", strrep(0, 8-nchar(ptid)), ptid)) %>% 
  mutate(redcap_event_name = case_when(
    ptid <= "UM00001041" ~ "visit_2_arm_1",
    ptid >  "UM00001041" ~ "visit_1_arm_1",
    TRUE ~ NA_character_
  )) %>% 
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

filename <- paste0("oltt_scraped_data_v1_", Sys.Date(), ".csv")
cat(paste("Writing csv:", paste0(Sys.Date(), "/", filename), "\n"))

system(paste0("mkdir ", Sys.Date()))
write.csv(oltt_df, 
          paste0(Sys.Date(), "/", filename), 
          row.names = FALSE, na = "")

cat(paste("Done.\n"))
