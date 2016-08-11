#------------------------------------------------------------------------------|
# setup ----
#------------------------------------------------------------------------------|

# process args

master.script.dir <- args[1]
input.params.location <- args[2]
inputfiles.dir <- args[3]
outputfiles.dir <- args[4]


#******************* CHECKING for India_GtG... 
# TODO: remove stuff in PSSE2PLEXOS that is India specific
# but for now, just skip it
india.repo = any(strsplit(getwd(),split="/")[[1]]=='India_GtG')


#install.packages("pacman")
pacman::p_load(cowplot, plyr, dplyr, ggplot2, grid, gridExtra, gtools, 
               knitr, lubridate, reshape2, data.table, RSQLite, stringr, psych, igraph) 
pacman::p_load(openxlsx)  
# had to follow instructions here: https://github.com/awalker89/openxlsx, 
# including installing Rtools from here: 
# https://cran.r-project.org/bin/windows/Rtools/
# and making sure PATH variable was edited appropriately 
# (check the "edit path" box during installation)


source(file.path(master.script.dir, "SourceScripts/functions.R"))


#------------------------------------------------------------------------------|
# input file parameters ----
#------------------------------------------------------------------------------|

# grab input parameters from parameter file passed in by user

source(input.params.location)


#------------------------------------------------------------------------------|
# function definition ----
#------------------------------------------------------------------------------|

if (!(choose.input %in% c("raw.psse", "pre.parsed"))) {    
    
    stop(paste("Please set 'choose.input' in input_params",
               "to 'raw.psse' or 'pre.parsed'"))
}


runAllFiles <- function () {
    
    # only parse psse if need to
    if (choose.input == 'raw.psse') {
        message("importing PSSE files...")
        source(file.path(master.script.dir, "SourceScripts",
                         "a-1-parse-psse.R"))
        source(file.path(master.script.dir, "SourceScripts",
                         "a-2-reformat-psse.R"))
    }
    
    # proceed with rest of data compilation
    message("creating tables...")
    source(file.path(master.script.dir, "SourceScripts",
                     "b_create_sheet_tables.R"))
    
    message("populating tables...")
    source(file.path(master.script.dir, "SourceScripts",
                     "c1_populate_sheet_tables_with_raw_tables.R"))
    source(file.path(master.script.dir, "SourceScripts",
                     "c2_more_data_population.R"))
    source(file.path(master.script.dir, "SourceScripts",
                     "c3_create_scenarios_and_models.R"))
    
    message("cleaning tables...")
    source(file.path(master.script.dir, "SourceScripts",
                     "d_data_cleanup.R"))
    
    # check data if need to 
    if(data.checks.and.plots == TRUE){
        message("checking data and creating summary plots")
        source(file.path(master.script.dir, "SourceScripts",
                         "e_summarize_and_check_compiled_database.R"))
    }
    
    # export tables
    message("exporting tables...")
    source(file.path(master.script.dir, "SourceScripts",
                     "f_export_to_excel.R"))
}


#------------------------------------------------------------------------------|
# run all scripts ----
#------------------------------------------------------------------------------|

runAllFiles()
