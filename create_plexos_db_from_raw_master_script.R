#------------------------------------------------------------------------------|
# setup ----
#------------------------------------------------------------------------------|

# process args
master.script.dir <- args[1]
input.params.location <- args[2]
inputfiles.dir <- args[3]
outputfiles.dir <- args[4]

# export workbook?
if (!exists("export.wb")) export.wb <- TRUE


#------------------------------------------------------------------------------|
# get needed packages and functions ----
#------------------------------------------------------------------------------|

#******************* CHECKING for India_GtG... 
# TODO: remove stuff in PSSE2PLEXOS that is India specific
# but for now, just skip it
india.repo = any(strsplit(getwd(),split="/")[[1]]=='India_GtG')


if (!("pacman" %in% installed.packages()[, "Package"])) {
    install.packages("pacman")
}

if (!("openxlsx" %in% installed.packages()[, "Package"]) & export.wb == TRUE) {
    stop(paste0("export.wb is set to TRUE but openxlsx is not installed.", 
                " either set export.wb to FALSE or, to export an excel file,", 
                " please install openxlsx. follow",
                " instructions here: https://github.com/awalker89/openxlsx", 
                " including installing Rtools from here:",  
                " https://cran.r-project.org/bin/windows/Rtools/", 
                " and making sure PATH variable was edited",  
                " (check the 'edit path' box during installation)"))
}

pacman::p_load(cowplot, ggplot2, data.table, igraph, openxlsx, RPostgreSQL) 

source(file.path(master.script.dir, "SourceScripts/functions.R"))


#------------------------------------------------------------------------------|
# fill in and check inputs ----
#------------------------------------------------------------------------------|

# make sure inputfiles.dir exists
if (exists("inputfiles.dir")) {
    if (!dir.exists(inputfiles.dir)) {
        stop(sprintf("inputfiles.dir set to %s, which does not exist"))
    }
}

# open connection to db
if (exists("inputfiles.db")) {
    conn = dbConnect(drv = inputfiles.db$drv, 
                     host = inputfiles.db$host, 
                     dbname = inputfiles.db$dbname, 
                     user = inputfiles.db$user, 
                     password = inputfiles.db$password)
}


#------------------------------------------------------------------------------|
# input file parameters ----
#------------------------------------------------------------------------------|

# grab input parameters from parameter file passed in by user

source(input.params.location)


#------------------------------------------------------------------------------|
# fill in and check more inputs ----
#------------------------------------------------------------------------------|
# these could be defined in the input parameters file

# set default choose.input to 'pre.parsed' if not set
if (!exists("choose.input")){
    choose.input <- "pre.parsed"
} else {
    if (!(choose.input %in% c("raw.psse", "pre.parsed"))) {    
        
        stop(paste("Please set 'choose.input' in input_params",
                   "to 'raw.psse' or 'pre.parsed'"))
    }
}

# set plexos.version to 7 if not provided
if (!exists("plexos.version")) {
    message("plexos.version not provided. setting to 7.")
    plexos.version <- 7
}


#------------------------------------------------------------------------------|
# function definition ----
#------------------------------------------------------------------------------|

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
    
    # check data, create plots if need to
    # by default, generate the plots
    if(!exists("data.check.plots")){data.check.plots <- TRUE}
    if(data.check.plots == TRUE){
        message("checking data and creating summary plots...")
    }else{
        message("checking data...")
    }
    source(file.path(master.script.dir, "SourceScripts",
                     "e_summarize_and_check_compiled_database.R"))
    
    # export tables
    
    if (export.wb) {
        message("exporting tables...")
        source(file.path(master.script.dir, "SourceScripts",
                         "f_export_to_excel.R"))
    } else {
        message("export.wb set to false. skipping export.")
    }
    
}


#------------------------------------------------------------------------------|
# run all scripts ----
#------------------------------------------------------------------------------|

runAllFiles()


#------------------------------------------------------------------------------|
# close up ----
#------------------------------------------------------------------------------|

if (exists("inputfiles.db")) {
    dbDisconnect(conn)
}
