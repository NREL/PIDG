# setup
pacman::p_load(data.table, openxlsx)

inputfiles.dir <- "~/GitHub/India_GtG/Process_for_PLEXOS_import/InputFiles_tester"
india.repo = TRUE

outputfiles.dir <- "~/GitHub/India_GtG/Process_for_PLEXOS_import/OutputFiles"
output.wb.name <- "ae_7_6_test_new_framework"



# variables

# 
# node.file <- file.path(root.dir, "outputs_a-3_corrected_psse/node.data.csv")
# line.file <- file.path(root.dir, "outputs_a-3_corrected_psse/line.data.csv")
# generator.file <- file.path(root.dir, "outputs_a-3_corrected_psse/generator.data.csv")
# transformer.file <- file.path(root.dir, "outputs_a-3_corrected_psse/transformer.data.csv")
# load.file <- file.path(root.dir, "outputs_a-3_corrected_psse/load.data.csv")

# inputfiles
source("~/GitHub/India_GtG/Process_for_PLEXOS_import/input_params_cea_wo_status0_or_stranded_node_lines_tester.R")

choose.db <- "cea"

# call
root.dir <- "~/GitHub/India_GtG/Process_for_PLEXOS_import/PSSE2PLEXOS/Update/"
source(file.path(root.dir, "compile/functions.R"))
source(file.path(root.dir, "compile/b_create_sheet_tables.R"))
source(file.path(root.dir, "compile/c1_populate_sheet_tables_with_raw_tables.R"))
source(file.path(root.dir, "compile/c2_more_data_population.R"))
source(file.path(root.dir, "compile/c3_create_scenarios_and_models.R"))
source(file.path(root.dir, "compile/d_data_cleanup.R"))
source(file.path(root.dir, "compile/e_export_to_excel.R"))
