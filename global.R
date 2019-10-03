

source("R/load_packages.R")

available_rds <- dir("data")

available_datasets <- tools::file_path_sans_ext(basename(available_rds))

for(i in seq_along(available_rds)){
	assign(available_datasets[i], readRDS(file.path("data", available_rds[i])))
}


current_ids <- c()
plot_settings <- list()

