

source("R/load_packages.R")

available_rds <- c("primavera_fasering_20191030.rds", 
	                     "primavera_overzicht_20191030.rds")

available_datasets <- tools::file_path_sans_ext(basename(available_rds))

for(i in seq_along(available_rds)){
	assign(available_datasets[i], readRDS(file.path("data", available_rds[i])))
}


current_ids <- c()
plot_settings <- list()

