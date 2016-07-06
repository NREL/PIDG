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

regzones <- file.path(root.dir, "inputs/node_region_zone_cea.csv")

#------------------------------------------------------------------------------|
# setup ----
#------------------------------------------------------------------------------|

# load packages
pacman::p_load(data.table)

# make sure output.dir exists
if (!dir.exists(output.dir)) {
    dir.create(output.dir, recursive = TRUE)
}

generator.data <- fread(file.path(root.dir, "outputs_a-2_reformatted_psse/generator.data.csv"))
line.data <- fread(file.path(root.dir, "outputs_a-2_reformatted_psse/line.data.csv"))
load.data <- fread(file.path(root.dir, "outputs_a-2_reformatted_psse/load.data.csv"))
node.data <- fread(file.path(root.dir, "outputs_a-2_reformatted_psse/node.data.csv"))
transformer.data <- fread(file.path(root.dir, "outputs_a-2_reformatted_psse/transformer.data.csv"))


# Node, Region, Zone (Zone optional). if any data is missing, original will be 
# used
regzones <- fread(regzones, colClasses = "character")

## get colorders
node.colorder <- colnames(node.data)
generator.colorder <- colnames(generator.data)
line.colorder <- colnames(line.data)

if (exists("transformer.data")) transformer.colorder <- colnames(transformer.data)
if (exists("load.data")) load.colorder <- colnames(load.data)


#------------------------------------------------------------------------------|
# corrections ----
#------------------------------------------------------------------------------|

## remap regions and zones
col.names <- colnames(regzones)
col.names <- col.names[col.names != "Node"]

setnames(regzones, col.names, paste0(col.names, "_new"))

# add Zone column if needed
if ("Zone" %in% col.names & !("Zone" %in% colnames(node.data))) {
    node.data[, Zone := NA]
}

# replace data and clean up
node.data <- merge(node.data, regzones, by = "Node", all.x = TRUE)

node.data[!is.na(Region_new), Region := Region_new]
node.data[, Region_new := NULL]
 
if ("Zone" %in% col.names) {
    node.data[!is.na(Zone_new), Zone := Zone_new]
    node.data[, Zone_new := NULL]
}

if (node.data[,all(is.na(Zone))]) {
    node.data[,Zone := NULL]
}

# clean up
rm(col.names)


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
          `:=`(`Max Flow` = 28, `Min Flow` = -28)] # these are in Ella's

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
          `Max Flow` := corrected_maxflow]

line.data[,corrected_maxflow := NULL]

# adjust_max_cap_cea?


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

rm(tab.name, to.write)