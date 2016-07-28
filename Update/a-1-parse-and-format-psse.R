# a-parse-psse.R 
# 
# ----
# 
# reads in a psse file, parses tables contained in the file, renames columns
# according to psse file version, and writes out tables
# 
# inputs: 
#   * one .raw data file containing psse system information
#   * location where outputs should be saved
# outputs: 
#   * all renamed tables in the file as separate csv files
#   * metadata (original file name, psse version, mva base, names of tables, 
#       empty tables, skipped tables, size of tables)
#   
# constraints and assumptions:
#   * assumes psse version 31. to change this, alter the column-renaming step
#   * assumes 3 non-data lines (see variable header.end)
#   * assumes that only 2-winding transformers exist. some inefficient code is 
#       provided for separating 2- and 3-winding transformers (see below), 
#       but cleaning 3-winding transformer data needs to be written 
#


#------------------------------------------------------------------------------|
# fake user inputs (for devel) ----
#------------------------------------------------------------------------------|

root.dir <- "~/GitHub/India_GtG/Process_for_PLEXOS_import/PSSE2PLEXOS/Update"

raw.file.path <- file.path(root.dir, "inputs/Base Case_2021-22-Peak-Demand_edit.raw")

output.dir1 <- file.path(root.dir, "outputs_a-1_raw_psse")

output.dir2 <- file.path(root.dir, "outputs_a-2_reformatted_psse2")



#------------------------------------------------------------------------------|
# setup ----
#------------------------------------------------------------------------------|

# load packages
pacman::p_load(data.table)

# make sure output.dir exists
if (!dir.exists(output.dir1)) {
    dir.create(output.dir1, recursive = TRUE)

}

if (!dir.exists(output.dir2)) {
    dir.create(output.dir2, recursive = TRUE)

}


#------------------------------------------------------------------------------|
# source first script and write out tables ----
#------------------------------------------------------------------------------|

source(file.path(root.dir, "compile/a-1-parse-psse.R"))

# write out csv files
for (tab.name in done.tables) {
    write.csv(get(tab.name), 
              file.path(output.dir1, paste0(tab.name, ".csv")),
              row.names = FALSE, 
              quote = FALSE)
}

# write out report
conn <- file(file.path(output.dir1, "00-metadata.txt"))

writeLines(c(as.character(Sys.time()), "\n\n",
             paste("psse file parsed:", basename(raw.file), "\n"),
             paste("psse version:", psse.version, "\n"),
             paste("mva base:", mva.base, "\n\n"),
             paste("tables processed:\n\t-", paste0(done.tables, collapse = "\n\t- "), "\n\n"),
             paste("tables skipped:\n\t-", paste0(skip.tables, collapse = "\n\t- "), "\n\n"),
             paste("empty tables:\n\t-", paste0(no.data.vec, collapse = "\n\t- "), "\n\n"),
             "----------\n\n",
             paste("other information:\n\t-", paste0(tab.info, collapse = "\n\t- "))
             ), 
           conn, 
           sep = "")

close(conn)

# clean up
rm(tab.name, conn)


#------------------------------------------------------------------------------|
# source second script and write out tables ----
#------------------------------------------------------------------------------|

source(file.path(root.dir, "compile/a-2-reformat-psse.R"))

for (tab.name in reformatted.tables) {
    
    write.csv(get(tab.name), 
              file.path(output.dir2, paste0(tab.name, ".csv")),
              row.names = FALSE, 
              quote = FALSE)
}

# clean up
rm(tab.name)
