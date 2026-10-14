

# load libraries ----------------------------------------------------------

library(tidyverse)
library(janitor)

# load data ---------------------------------------------------------------

# list file names 
folder_name <- 'data'

files <- list.files(folder_name, full.names = TRUE)

files <- files[which(!str_detect(files, '.gz'))]

pids <- str_extract(files, "(?<=/).+(?=_EI)")

files <- files[which(!is.na(pids))]

# remove empty rows (tutorial level, net empty)

# fix the column names

# if cleans everything up, we'll merge the files together

# from there, figure out a way to clean up those participant ids that were acutally links


# loading loop
for (i in 1:length(files)) {
  
  myd <- read_csv(files[i], show_col_types = F)
  
  myd <- myd |> 
    select(
      `Prolific Worker ID*`:frameRate, Block:EI_Total_Points, EI_Task.started, EI_Task.stopped
    )
  
  # completion time
  ct <- max(myd$EI_Task.stopped, na.rm = TRUE) - min(myd$EI_Task.started, na.rm = TRUE)
  
  clean_myd <- myd |> 
    select(-c(EI_Task.started, EI_Task.stopped)) |> 
    drop_na()
  
  colnames(clean_myd) <- make_clean_names(colnames(clean_myd))
  
  clean_myd <- clean_myd |> 
    add_column(pid = pids[i], completion_time = ct) |> 
    relocate(pid)
  
  if (i == 1) {
    d <- clean_myd
  }
  else {
    # append to existing dataset
    d <- bind_rows(d, clean_myd)
  }
  
  print(i)
}

summary(d)

dir.create('data/cleaned')

write_csv(d, 'data/cleaned/ei_90_pp_cleaned.csv')
