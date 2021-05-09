main_dir <-file.path(getwd())  # Main project folder
drafts <-file.path(main_dir,"drafts")
data_dir <- file.path(main_dir, "Data")
processed_dir <- file.path(data_dir, "processed")
raw_dir <- file.path(data_dir, "raw_csv")

export_dir <- file.path(data_dir, "export")

maps_dir <- file.path(data_dir, "maps")
maps_zip_dir <- file.path(maps_dir, "zip")

data_dict_fullpath <- file.path(raw_dir, "weights-measures-data-dictionary.csv")
data_dict <- read_csv(data_dict_fullpath)