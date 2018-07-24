library(tidyverse)

# get filenames and read in data

filepath <- "raw_data//fitness//"

filenames <- list.files(filepath, pattern="*.xlsx", full.names=TRUE)

dat <- map(filenames, readxl::read_xlsx)

# Cut out bits of table until it's only what we want

dat <- map(dat, ~`[`(., 1:12,))

for (i in seq_along(dat)) {
  names(dat[[i]])[1] <- "well_row"
}

# Reformat data to tidy format

dat <- map(dat, gather, well_col, OD, -well_row)

# Add some identifier variables

fix_wells <- function(df) {
  df %>% 
    mutate(well_ID = str_c(well_row, well_col)) %>%
    select(well_ID, everything())
}

dat <- map(dat, fix_wells)

# Outputting the tidy data

filename_prefixes <- str_extract(filenames, ".+?(?=\\.xlsx)") 
output_filenames <- str_c(filename_prefixes, "_Long.csv")

map2(dat, output_filenames, write_csv)

# DONE

# OLD ---------------------------------------------------------------------

# dat <- dat[1:12,]
# 
# names(dat)[1] <- "well_row"
# 
# (dat_long <- gather(dat, well_col, OD, -well_row))
# 
# dat_long <- dat_long %>% 
#   mutate(well_ID = str_c(well_row, well_col)) %>%
#   select(well_ID, everything())
# 
# dat_long
# 
# write_csv(dat_long, "24Hr.P1CombosOD_Plate2_long.csv")
