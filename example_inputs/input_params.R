
# This script is part of the PSSE2PLEXOS repository to function properly, it 
# should be inserted into a data repository as a git submodule. The PSSE2PLEXOS 
# submodule should reside in a folder with two other folders: InputData 
# (containing input *.csv and *.raw files), and OutputData

# To Run:
# 1. create an argument string (example below)
# 2. run this code by either a. or b.
#   a. source('create_plexos_db_from_raw_master_script.r')
#   b. Rscript create_plexos_db_from_raw_master_script.r

# With the exception of the PSSE file (required) the following input arguments 
#   will be processed if they are defined:
# 1. PSSE.raw file name
# 2. region re-mapping .csv file name
# 3. zone re-mapping .csv file name
# 4. generator fuel mapping .csv file name
# 5. regional load file def .csv name
# 6. new RE gens to be added csv name

#example arg strings:
args = c("All_India_Peak_Oct_2015_version31_without_outage_lines.raw",
  "map_new_regions.csv",
  "map_new_zones.csv",
  "map_generators_to_fuel.csv",
  "map_region_to_loadfile.csv",
  "2014_State_RE_gens.csv")
# args = c("2014_Peak_LuzVis v31.raw",
#   "",
#   "",
#   "map_generators_to_fuel.csv",
#   "map_region_to_loadfile.csv","")




#------------------------------------------------------------------------------|
# input file parameters ----
#------------------------------------------------------------------------------|
# For information about the contents and required format of these input files, 
# see "Required Input Files" section in the readme.
# All files in input parameters and generic imports must be in "InputFiles" 
# directory. Exported Excel workbook will be saved in "OutputFiles" directory.


# ---- required/generic inputs ----

# PSSE file
raw.file.path <- args[1]

# remap nodes' regions and zones? files are required if rename.regions or 
# rename.zones are set to TRUE
map.newregion.file <- args[2]
rename.regions = ifelse(map.newregion.file=='',F,T)
map.newzone.file <- args[3]
rename.zones = ifelse(map.newzone.file=='',F,T)

# add generators to fuel
map.gen.to.fuel.file <- args[4]

# point each region to a load file
map.region.to.load.RE.file <- args[5]

# add new generators
RE.gen.file <- args[6]
add.RE.gens = ifelse(RE.gen.file=='',F,T)

# add properties to objects. 
# files should be of the form: one column with names of all affected objects, 
# all other columns named a string of the exact name of the plexos property to 
# be added. 
# List elemtn structure: list where first element is name of input file and 
# second is a named list of arguments of add_to_properties_sheet. Required args 
# are names.col (name of column holding object names), object.class, and 
# collection.name
object.property.list <- list(

  # generator VOM charges
  list("map_generators_to_vom_charges.csv", 
      list(names.col = 'Generator.Name',
          object.class = 'Generator',
          collection.name = 'Generators')),
  
  # generator outages
  list("generator_outages.csv", 
      list(names.col = 'Generator.Name',
          object.class = 'Generator',
          collection.name = 'Generators')),

  # units not yet in operation in 2014 -- MAYBE later, add 2014 scenario?
  list("units_turned_off.csv",
      list(names.col = 'Generator.Name',
          object.class = 'Generator',
          collection.name = 'Generators',
          overwrite = TRUE)),
    
  # hydro limits
  list("hydro_lims_monthly_GWh.csv",
      list(names.col = 'Generator.Name',
          object.class = 'Generator',
          collection.name = 'Generators',
          pattern.col = 'month',
          scenario.name = 'Hydro: monthly energy limits',
          period.id = '3')),
  
  list("hydro_lims_weekly_GWh.csv",
      list(names.col = 'Generator.Name',
          object.class = 'Generator',
          collection.name = 'Generators',
          pattern.col = 'week',
          scenario.name = 'Hydro: weekly energy limits')),
  
  list("hydro_lims_daily_min.csv",
      list(names.col = 'Generator.Name',
          object.class = 'Generator',
          collection.name = 'Generators',
          pattern.col = 'day',
          scenario.name = 'Hydro: daily min limits'))
  
)

# add generator properties that have to be mapped by fuel.
# list element structure: list, with the first element the character vector 
# name of the input file, the second the arguments for merge_property_by_fuel 
# (name of property columns and optional tag to multiply by max capacity), and 
# the third the arguments for add_to_properties_sheet (see above)
generator.property.by.fuel.list <- list(
  
  list("map_min_gen_by_fuel.csv", 
      fuel.map.args = list(prop.cols = c('Min Stable Level'), 
          mult.by.max.cap = TRUE), 
      add.to.prop.args = list(
          object.class = 'Generator',
          collection.name = 'Generators',
          overwrite = TRUE)), 
  
  list("map_ramps_by_fuel.csv",
      fuel.map.args = list(prop.cols = c('Max Ramp Up', 'Max Ramp Down'),
          mult.by.max.cap = TRUE),
      add.to.prop.args = list(
          object.class = 'Generator',
          collection.name = 'Generators')),
  
  list("min_up_and_down_time_by_fuel.csv",
      fuel.map.args = list(prop.cols = c('Min Up Time', 'Min Down Time')),
      add.to.prop.args = list(
          object.class = 'Generator',
          collection.name = 'Generators'))
)
  
# generic interface names list. 
# list of vectors with named elements pointing to files with interface names, 
# properties, memberships, and flow coefficients.
# scenario-ing is done in script c3
interfaces.files.list <- list(
  # zonal interfaces
  c(names = 'interface_names_2014_zonal.csv', 
    properties = 'interface_properties_2014_zonal.csv',
    memberships = 'interface_memberships_2014_zonal.csv', 
    flowcoefs = 'interface_line_flow_coefficients_2014_zonal.csv'),
  
  # regional interfaces
  c(names = 'interface_names_2014_regional.csv', 
    properties = 'interface_properties_2014_regional.csv',
    memberships = 'interface_memberships_2014_regional.csv', 
    flowcoefs = 'interface_line_flow_coefficients_2014_regional.csv')
)

# define as many files as needed for generic imports
# currently, this defines horizons, ST and MT schedules, reports, and models
generic.import.files <- c(

  "import_STSchedule_MTSchedule_Performance_Transmission_Production.csv", 
  "import_diagnostic.csv",
                          
  "import_report_default.csv",
  "import_report_detailed.csv",                 #currently in all models
  
  #PASA
  "PASA.csv"
)

# compact generic files format (different file for each object type)
compact.generic.import.files <- list(
  
  c('import_models.csv', 'model'), 
  c('import_horizons.csv', 'horizon')
)


# ---- other input parameters ----

units.to.delete.file <- "units_retired_by_2014.csv"

start.cost.file <- "start_cost_by_fuel.csv"

enforced.interstate.lines.file <- "lines_to_enforce.csv"

isolated.nodes.to.remove.file <- "isolated_nodes_to_remove.csv"

wheeling.charge.cases.files <- list(
  `Add $2 Wheeling Charge` = "wheeling_charges_2.csv", 
  `Add $3/4 Mixed Wheeling Charge` = "wheeling_charges_34mix.csv", 
  `Add $5 Wheeling Charge` = "wheeling_charges_5.csv")


# ---- define output parameters ----

output.wb.name <- "ae_3_24_params_split_test.xlsx"