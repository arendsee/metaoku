# Parse the config file

if(!file.exists('config')){
  stop("No config found, please run make")
}

source('config')
config <- list()
config$data_dir        <- file.path(getwd(), DATA_DIR)
config$save_dir        <- file.path(getwd(), SAVE_DIR)
config$fields          <- FIELDS
config$descriptions    <- DESCRIPTIONS
config$data_pat        <- DATA_PAT
config$max_levels      <- MAX_LEVELS
config$max_prop        <- MAX_PROP
config$max_length      <- MAX_LENGTH

if(file.exists(file.path(config$data_dir, DESCRIPTIONS))){
    config$home_tab <- file.path(config$data_dir, DESCRIPTIONS)
} else {
    config$home_tab <- file.path(getwd(), 'pages', 'home.md')
}

config$access <- ACCESS
config$help_tab <- file.path(getwd(), 'pages', 'help.md')
config$about_tab <- file.path(getwd(), 'pages', 'about.md')
