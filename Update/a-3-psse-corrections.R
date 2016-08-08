# a-3-psse-corrections.R
# 
# ----------
# 
# implement corrections and reformats to psse file
# not sure where this should go; this is very much project-specific
# 
# region remap
# zone remap
# no categories--that can stay in compilation
# individual corrections
# 

#------------------------------------------------------------------------------|
# fake user inputs for now ----
#------------------------------------------------------------------------------|

root.dir <- "~/GitHub/India_GtG/Process_for_PLEXOS_import/PSSE2PLEXOS/Update"

input.dir <- file.path(root.dir, "outputs_a-2_reformatted_psse")
output.dir <- file.path(root.dir, "outputs_a-3_corrected_psse")

regions <- file.path(root.dir, "inputs/node_region_cea.csv")
zones <- file.path(root.dir, "inputs/node_zone_cea.csv")

remap.nodes <- file.path(root.dir, "inputs/node_region_zone_cea.csv")

copy.data.loc <- file.path(root.dir, "../../InputFiles_tester/base_network")

# types of corrections

# node region/zone remapping
remap.node.file <- file.path(root.dir, "correction_inputs/node_region_zone_cea.csv")

# corrections
adjust.max.cap.file <- file.path(root.dir, "correction_inputs/adjust_max_cap_cea.csv")
adjust.line.reactance.file <- file.path(root.dir, "correction_inputs/adjust_line_reactance_cea.csv")
standard.lines.file <- file.path(root.dir, "correction_inputs/standard_flow_lines.csv")
standard.tfmrs.file <- file.path(root.dir, "correction_inputs/standard_flow_tfmrs.csv")

# individual properties
# brief script


#------------------------------------------------------------------------------|
# setup ----
#------------------------------------------------------------------------------|

# load packages
pacman::p_load(data.table)

# make sure output.dir exists
if (!dir.exists(output.dir)) {
    dir.create(output.dir, recursive = TRUE)
}

# make sure copy.data.loc exists
if (!dir.exists(copy.data.loc)) {
    dir.create(copy.data.loc, recursive = TRUE)
}

generator.data <- fread(file.path(input.dir, "generator.data.table.csv"))
line.data <- fread(file.path(input.dir, "line.data.table.csv"))
load.data <- fread(file.path(input.dir, "load.data.table.csv"))
node.data <- fread(file.path(input.dir, "node.data.table.csv"))
transformer.data <- fread(file.path(input.dir, "transformer.data.table.csv"))


# Node, Region, Zone (Zone optional). if any data is missing, original will be 
# used

## get colorders
node.colorder <- colnames(node.data)
generator.colorder <- colnames(generator.data)
line.colorder <- colnames(line.data)

if (exists("transformer.data")) transformer.colorder <- colnames(transformer.data)
if (exists("load.data")) load.colorder <- colnames(load.data)


#------------------------------------------------------------------------------|
# helper functions ----
#------------------------------------------------------------------------------|
replace_data <- function(orig.table, merge.on, new.data) {
    # assumes that first col is merge.on
    # assumes other columns are named identical to property name 
    # for now, requires that the new property is already define in PSSE file
    new.cols <- colnames(new.data)
    prop.cols <- new.cols[-1]
    
    # check
    if (!all(prop.cols %in% colnames(orig.table))) {
        
        problem.cols <- prop.cols[!(prop.cols %in% colnames(orig.table))]
        verb <- ifelse(length(problem.cols) > 1, 
                       " aren't columns in ", 
                       " isn't a column in ")
        
        stop(paste0(paste(problem.cols, collapse = ", "),
                    verb,
                    deparse(substitute(orig.table)), 
                    " table. Please adjust data in ",
                    deparse(substitute(new.data))))
        
    }
    
    orig.table <- merge(orig.table, new.data, 
                        by.x = merge.on, by.y = new.cols[1], 
                        all.x = TRUE)
    
    # merge and replace data in original column with new data
    for (col in prop.cols) {
        
        orig.table[!is.na(get(paste0(col, ".y"))), 
                   (paste0(col, ".x")) := get(paste0(col, ".y"))]
        
        setnames(orig.table, paste0(col, ".x"), col)
        orig.table[,(paste0(col, ".y")) := NULL]
    }

    return(orig.table)    
}



#------------------------------------------------------------------------------|
# corrections ----
#------------------------------------------------------------------------------|

if (exists("remap.nodes")) {
    
    if (file.exists(file.path(remap.node.file))) {
        
        # read in data
        remap.nodes <- fread(file.path(remap.node.file), 
                             colClasses = "character")
        
        col.names <- colnames(remap.nodes)
        
        # check to make sure right columns exist
        if ("Node" %in% col.names & any(c("Region", "Zone") %in% col.names)) {
            
            message("... reassigning node regions (and/or zones) from %s",
                remap.node.file)
            
            
        # pull out which areas to remap (reg/zone/both)
        col.names <- col.names[col.names %in% c("Region", "Zone")]
        
        setnames(remap.nodes, col.names, paste0(col.names, "_new"))
        
        # replace data and clean up
        node.data <- merge(node.data, remap.nodes, by = "Node", all.x = TRUE)
        
        if ("Region" %in% col.names) {
            node.data[!is.na(Region_new), Region := Region_new]
            node.data[, Region_new := NULL]
        }
         
        if ("Zone" %in% col.names) {
            node.data[!is.na(Zone_new), Zone := Zone_new]
            node.data[, Zone_new := NULL]
        }
        
        # clean up
        rm(col.names)
            
        } else { #  if ("Node" %in% col.names ... 
            
            message(paste0(">>  %s does not contain the correct column names ",
                           "(Node and Region and/or Zone) ... skipping"),
                    remap.node.file)
        }

    } else { #  if (file.exists(file.path(remap.node.file)))
        
        message(sprintf(">>  %s does not exist ... skipping", remap.node.file))
    }
    
} else { #  if (exists("remap.nodes")) 
    
    message("remap.node.file does not exist. leaving nodes in original regions/zones")
}
 # report - unique(old regions and zones) unique(new regions and zones), point to source file



## other individual corrections


# using replace_data

adjust.max.cap <- fread(adjust.max.cap.file)
generator.data <- replace_data(generator.data, "Generator", adjust.max.cap)

adjust.line.reactance <- fread(adjust.line.reactance.file)
line.data <- replace_data(line.data, "Line", adjust.line.reactance)


# replace only ratings on lines and transformers where rating is zero
# lines
standard.lines <- fread(standard.lines.file)

line.data.zeros <- line.data[`Max Flow` == 0 | `Max Flow` == 9999]
line.data.zeros <- replace_data(line.data.zeros, "Voltage.From", standard.lines)
line.data.zeros <- line.data.zeros[,.(Line, `Max Flow`)]

line.data <- replace_data(line.data, "Line", line.data.zeros)

# tfmrs
standard.tfmrs <- fread(standard.tfmrs.file, colClasses = "numeric")

tfmr.data.zeros <- transformer.data[Rating == 0]
tfmr.data.zeros <- replace_data(tfmr.data.zeros, "Voltage.To", standard.tfmrs)
tfmr.data.zeros <- tfmr.data.zeros[,.(Transformer, Rating)]

transformer.data <- replace_data(transformer.data, "Transformer", tfmr.data.zeros)

# clean up
rm(adjust.max.cap, adjust.line.reactance, standard.lines, standard.tfmrs, 
   line.data.zeros, tfmr.data.zeros)



# ---- needs script ----
line.data[`Max Flow` > 4000, `Max Flow` := 4000]
line.data[`Min Flow` < -4000, `Min Flow` := -4000]



# adjust line Min Flow in case Max Flow changed
line.data[,`Min Flow` := -1 * `Max Flow`]

# TODO
# fix reactance of one line
# list("corrections/line_reactance_adjustments_cea.csv",
#   list(overwrite = TRUE)),
# fix ella's

#------------------------------------------------------------------------------|
# set orders again. do this from the original order (w poss addition of zone in node) ----
#------------------------------------------------------------------------------|

setcolorder(node.data, node.colorder)
setcolorder(line.data, line.colorder)
setcolorder(generator.data, generator.colorder)

if (exists("transformer.data")) setcolorder(transformer.data, transformer.colorder)
if (exists("load.data")) setcolorder(load.data, load.colorder)


#------------------------------------------------------------------------------|
# write out for now ----
#------------------------------------------------------------------------------|

to.write <- ls(pattern = "\\.data$")

for (tab.name in to.write) {
    write.csv(get(tab.name), 
              file.path(output.dir, paste0(tab.name, ".csv")),
              row.names = FALSE, 
              quote = FALSE)
}

if (exists("copy.data.loc")) {
    for (tab.name in to.write) {
        write.csv(get(tab.name), 
              file.path(copy.data.loc, paste0(tab.name, ".csv")),
              row.names = FALSE, 
              quote = FALSE)
    }
}

rm(tab.name, to.write)