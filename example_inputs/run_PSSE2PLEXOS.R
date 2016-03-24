#------------------------------------------------------------------------------|
# set working directory ----
#------------------------------------------------------------------------------|

# set working directory

if (interactive()) {
  t=try(dirname(sys.frame(1)$ofile),silent = T)
  if(inherits(t, "try-error")) {
    warning("Make sure you are in the PSSE2PLEXOS submodule path")
  } else {
    script.dir = dirname(sys.frame(1)$ofile)
    setwd(script.dir)
  }
} else {
  dir = getSrcDirectory(function(x) {x})
  m <- regexpr("(?<=^--file=).+", commandArgs(), perl=TRUE)
  script.dir <- dirname(regmatches(commandArgs(), m))
  if(length(script.dir) == 0) 
    stop("can't determine script dir: please call the script with Rscript")
  if(length(script.dir) > 1) 
    stop("can't determine script dir: more than one '--file' argument detected")
  setwd(script.dir)
}


#------------------------------------------------------------------------------|
# USER INPUT: set input parameters ----
#------------------------------------------------------------------------------|

# point to location of PSSE2PLEXOS master script
master.script.dir <- 'PSSE2PLEXOS'

# input parameters file, relative to location of this script
input.params <- 'input_params.R'

# directory of input files
inputfiles.dir <- 'InputFiles'

# directory to export to
outputfiles.dir <- 'OutputFiles'


#------------------------------------------------------------------------------|
# run ----
#------------------------------------------------------------------------------|

args <- c(master.script.dir, input.params, inputfiles.dir, outputfiles.dir)

source('PSSE2PLEXOS/create_plexos_db_from_raw_master_script.R')
