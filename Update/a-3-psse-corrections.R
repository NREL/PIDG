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

output.dir <- file.path(root.dir, "outputs_a-3_corrected_psse")

regions <- file.path(root.dir, "inputs/node_region_cea.csv")
zones <- file.path(root.dir, "inputs/node_zone_cea.csv")

remap.nodes <- file.path(root.dir, "inputs/node_region_zone_cea.csv")

copy.data.loc <- file.path(root.dir, "../../InputFiles_tester/base_network")

# types of corrections

# node region/zone remapping
remap.node.file <- file.path(root.dir, "correction_inputs/node_region_zone_cea.csv")

adjust.max.cap.file <- file.path(root.dir, "correction_inputs/adjust_max_cap_cea.csv")

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

generator.data <- fread(file.path(root.dir, "outputs_a-2_reformatted_psse/generator.data.csv"))
line.data <- fread(file.path(root.dir, "outputs_a-2_reformatted_psse/line.data.csv"))
load.data <- fread(file.path(root.dir, "outputs_a-2_reformatted_psse/load.data.csv"))
node.data <- fread(file.path(root.dir, "outputs_a-2_reformatted_psse/node.data.csv"))
transformer.data <- fread(file.path(root.dir, "outputs_a-2_reformatted_psse/transformer.data.csv"))


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
replace_data <- function(orig.table, type, new.data) {
    # assumes that first col is type
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
                        by.x = type, by.y = new.cols[1], 
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

generator.data[Generator == "GEN_354013_GMR_400_1", `Max Capacity` := 685]
generator.data[Generator == "GEN_354013_GMR_400_2", `Max Capacity` := 685]

line.data[Line == "211570_261131_2_CKT", Reactance := 0.35456]

line.data[`Max Flow` > 4000, `Max Flow` := 4000]
line.data[`Min Flow` < -4000, `Min Flow` := -4000]

line.data[Line %in% c("326004_326007_1_CKT", 
                      "326005_326006_1_CKT"), 
          `:=`(`Max Flow` = 28, `Min Flow` = -28)]

line.data[Line %in% c("361062_361235_1_CKT"), 
          `:=`(`Max Flow` = 80, `Min Flow` = -80)]

line.data[Line %in% c("352043_352097_1_CKT",
                      "372060_372193_1_CKT",
                      "372098_372211_1_CKT",
                      "312035_312036_1_CKT",
                      "372052_372087_1_CKT"), 
          `:=`(`Max Flow` = 200, `Min Flow` = -200)]

line.data[Line %in% c("184432_364008_1_CKT",
                      "314002_314024_2_CKT",
                      "314002_314025_3_CKT",
                      "314013_314014_1_CKT",
                      "314015_314049_4_CKT",
                      "314016_314049_1_CKT",
                      "354016_354027_1_CKT",
                      "354016_354028_2_CKT",
                      "354030_364015_1_CKT",
                      "354031_364015_2_CKT",
                      "364011_364019_1_CKT",
                      "364011_364020_2_CKT",
                      "414005_414405_1_CKT",
                      "414010_414410_1_CKT",
                      "434011_434411_1_CKT",
                      "434027_434427_1_CKT"), 
          `:=`(`Max Flow` = 870, `Min Flow` = -870)]

line.data[Line %in% c("374019_374021_1_CKT", 
                      "374020_374021_1_CKT"), 
          `:=`(`Max Flow` = 28, `Min Flow` = -2186)] # these are in Ella's

## standard data
# defines stadards with [name = kV level] = [element = MW flow limit]
standard.flow.lims <- c("132" = "80", "220" = "200", "400" = "870", 
  "765" = "2200", 
  # these next ones are from looking at most commnon flow limits on these lines
  "11" = "30", "33" = "33", "66" = "28", "100" = "80", "110" = "80", 
  "230" = "200",
  #and these ones are rough guesses
  "0.6" = "10", "69" = "30", "115" = "80", "22.9" = "20", "34.5" = "34.5",
  "13.8" = "30", "138" = "150")

standard.flow.lims <- data.table(Voltage = as.numeric(names(standard.flow.lims)), 
                                 corrected_maxflow = as.numeric(standard.flow.lims))

line.data <- merge(line.data,
                   standard.flow.lims, 
                   by = "Voltage",
                   all.x = TRUE)

line.data[`Max Flow` == 0 & !is.na(corrected_maxflow), 
          `:=`(`Max Flow` = corrected_maxflow, 
               `Min Flow` = -1 * corrected_maxflow)]


line.data[,corrected_maxflow := NULL]

# standard transformer data (names = kV.To, data = rating)
standard.flow.tfmr.lims <- c("220" = "315", "132" = "100", "110" = "100", 
   "66" = "100", "69" = "100", "138" = "100", "13.8" = "100")

standard.flow.tfmr.lims <- data.table(kV.To = as.numeric(names(standard.flow.tfmr.lims)),
                                      cor_Rating = as.numeric(standard.flow.tfmr.lims))

transformer.data <- merge(transformer.data, 
                          standard.flow.tfmr.lims, 
                          by = "kV.To",
                          all.x = TRUE)

transformer.data[Rating == 0 & !is.na(cor_Rating), Rating := cor_Rating]
transformer.data[,cor_Rating := NULL]


# adjust max capacity
adjust.max.cap <- fread(adjust.max.cap.file)

generator.data <- replace_data(generator.data, "Generator", adjust.max.cap)



# TODO
# adjust_max_cap_cea?
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