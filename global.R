

source("R/load_packages.R")

if(!dir.exists("cache"))dir.create("cache")
if(!dir.exists("cache/palettes"))dir.create("cache/palettes")

available_rds <- dir("data")

available_datasets <- tools::file_path_sans_ext(basename(available_rds))

for(i in seq_along(available_rds)){
	assign(available_datasets[i], readRDS(file.path("data", available_rds[i])))
}


current_ids <- c()
plot_settings <- list()


# Available color palettes
color_palettes <- c(tools::file_path_sans_ext(dir("cache/palettes", pattern = "[.]json$")), 
                        "rich.colors", rownames(brewer.pal.info))
