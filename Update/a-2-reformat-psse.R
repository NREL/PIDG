

# nodes
# lines
# generators
# transformers

# add owners with each

# corrections:
#   - regions, zone mapping
#   - individual properties (reactance, max cap, etc)
#   - report these


# b-process-psse-data.R 
# 
# ----
# 
# accpets folder with psse data and makes it nicer 
# 
# inputs: 
#   * file paths of nodes, lines, transformers, generators tables
# outputs: 
#   * 
#   
# constraints and assumptions:
#   * only nodes, lines. transformers, generators tables
#


# decide what to do with load data
# add checks to make sure files exist

# all DC lines are on
# generator owner 1 used only
# line kV is from.kV (some are different) 
# lines in regions, interstate later
# generator units later
# not including overload rating
# assumes no load is turned off; only active power; ignoring owner <- do this 
#       all in corrections
# add owner later
# 
# line flow is ratingB or, if that is zero, the max of ratings A and B
#  

#------------------------------------------------------------------------------|
# fake user inputs ----
#------------------------------------------------------------------------------|

root.dir <- "~/GitHub/India_GtG/Process_for_PLEXOS_import/PSSE2PLEXOS/Update/outputs_a-1_raw_psse"

# required data
node.file <- file.path(root.dir, "Bus.table.csv")
line.file <- file.path(root.dir, "Branch.table.csv")
generator.file <- file.path(root.dir, "Generator.table.csv")

# optional data
line.dc.file <- file.path(root.dir, "DC.line.table.csv")
transformer.file <- file.path(root.dir, "Transformer.Table.csv")
    
# optional extras
zone.file <- file.path(root.dir, "Zone.table.csv")
region.file <- file.path(root.dir, "Area.interchange.table.csv")
owner.file <- file.path(root.dir, "Owner.table.csv")
load.file <- file.path(root.dir, "Load.table.csv")

output.dir <- file.path(root.dir, "../outputs_a-2_reformatted_psse")


#------------------------------------------------------------------------------|
# setup ----
#------------------------------------------------------------------------------|

# load packages
pacman::p_load(data.table)

# make sure output.dir exists
if (!dir.exists(output.dir)) {
    dir.create(output.dir, recursive = TRUE)
}

# read in files
node.data <- fread(node.file, colClasses = "character")
line.data <- fread(line.file, colClasses = "character")
generator.data <- fread(generator.file, colClasses = "character")

if (exists("line.dc.file")) line.dc.data <- fread(line.dc.file, 
                                                  colClasses = "character")
if (exists("transformer.file")) transformer.data <- fread(transformer.file, 
                                                          colClasses = "character")

if (exists("zone.file")) zone.data <- fread(zone.file, colClasses = "character")
if (exists("region.file")) region.data <- fread(region.file, colClasses = "character")
if (exists("owner.file")) owner.data <- fread(owner.file, colClasses = "character")
if (exists("load.file")) load.data <- fread(load.file, colClasses = "character")    

# clean up
rm(node.file, line.file, generator.file)
if (exists("line.dc.file")) rm(line.dc.file)
if (exists("transformer.file")) rm(transformer.file)
if (exists("zone.file")) rm(zone.file)
if (exists("region.file")) rm(region.file)
if (exists("owner.file")) rm(owner.file)
if (exists("load.file")) rm(load.file)


# track which to write out
reformatted.tables <- c()



#------------------------------------------------------------------------------|
# node.data ----
#------------------------------------------------------------------------------|

## rename columns, clean up node.data
node.data <- node.data[,.(Node = paste(node.number, node.name, kV, sep = "_"),
                          node.number, # leave in for merging, delete later
                          Voltage = kV,
                          region.number,
                          zone.number,
                          owner.number)]


## optionally, add region, zone, and owner names. otherwise, rename columns

# regions
if (exists("region.data")) {
    
    node.data <- merge(node.data, 
                       region.data[,.(region.number, Region = region.name)],
                       by = "region.number",
                       all.x = TRUE)
    
    node.data[,region.number := NULL]
    
} else {
    
    if ("region.number" %in% colnames(node.data)) {
        setnames(node.data, "region.number", "Region") }
}

# zones
if (exists("zone.data")) {
    
    node.data <- merge(node.data, 
                       zone.data[,.(zone.number, Zone = zone.name)],
                       by = "zone.number",
                       all.x = TRUE)
    
    node.data[,zone.number := NULL]
    
} else {
    
    if ("zone.number" %in% colnames(node.data)) {
        setnames(node.data, "zone.number", "Zone") }
}

# owners
if (exists("owner.data")) {
    
    node.data <- merge(node.data, 
                       owner.data[,.(owner.number, Owner = owner.name)],
                       by = "owner.number",
                       all.x = TRUE)
    
    node.data[,owner.number := NULL]
    
} else {

    if ("owner.number" %in% colnames(node.data)) {
        setnames(node.data, "owner.number", "Owner") }
}

# add to list to write out
reformatted.tables <- c(reformatted.tables, "node.data")


#------------------------------------------------------------------------------|
# load.data ----
#------------------------------------------------------------------------------|

## optionally add load
if (exists("load.data")) {
    load.data <- load.data[,.(node.number, 
                              Status = status, 
                              Type = load.type,
                              Load = active.power.MW)]
    
    # add node name
    load.data <- merge(load.data, 
                       node.data[,.(node.number, Node)], 
                       by = "node.number", 
                       all.x =TRUE)
    
    # clean up
    load.data[,node.number := NULL]
    
    setcolorder(load.data, c("Node", "Type", "Status", "Load"))
}

# add to list to write out
reformatted.tables <- c(reformatted.tables, "load.data")


#------------------------------------------------------------------------------|
# line.data ----
#------------------------------------------------------------------------------|

## line data
line.data <- line.data[,.(Line = paste(node.from.number, node.to.number, 
                                       id, "CKT", sep = "_"),
                          node.from.number, 
                          node.to.number,
                          Resistance = resistance.pu,
                          Reactance = reactance.pu,
                          ratingA = as.numeric(ratingA), 
                          ratingB = as.numeric(ratingB),
                          ratingC = as.numeric(ratingC),
                          Status = status,
                          Length = length)]

# choose line rating (either ratingB or the max of ratings A and C)
line.data[,`Max Flow` := {temp = apply(line.data[,.(ratingA,ratingC)],1,max);
                          ifelse(ratingB != "0", ratingB, temp)}]

line.data[,c("ratingA", "ratingB", "ratingC") := NULL]

line.data[,`Min Flow` := `Max Flow` * -1]

               
# if DC lines exist, add them
if (exists("line.dc.data")) {
    
    line.dc.data <- line.dc.data[,.(Line = paste(node.from.number, 
                                                 node.to.number, id.num, 
                                                 "CKT", sep = "_"),
                                    node.from.number, 
                                    node.to.number,
                                    Resistance = resistance.pu,
                                    `Max Flow` = max.flow.MW,
                                    `Min Flow` = as.numeric(max.flow.MW) * -1,
                                    Status = 1
                                    )]
    
    line.data <- rbindlist(list(line.data, line.dc.data), 
                           use.names = TRUE, 
                           fill = TRUE)
}


# add Node From and Node To names
line.data <- merge(line.data, 
                   node.data[,.(node.from.number = node.number, 
                                `Node From` = Node,
                                Voltage)],
                   by = "node.from.number", 
                   all.x = TRUE)

line.data <- merge(line.data, 
                   node.data[,.(node.to.number = node.number, 
                                `Node To` = Node )],
                   by = "node.to.number", 
                   all.x = TRUE)

# categorize lines
line.data[is.na(Reactance), Type := "DC"]
line.data[!is.na(Reactance), Type := "AC"]

line.data[Type == "DC", Line := paste0(Line, "_DC")]

# clean up
line.data[,c("node.from.number", "node.to.number") := NULL]

setcolorder(line.data, c("Line", "Node From", "Node To", "Type", "Voltage", 
                         "Max Flow", "Min Flow", "Resistance", "Reactance",
                         "Status", "Length"))

# add to list to write out
reformatted.tables <- c(reformatted.tables, "line.data")


#------------------------------------------------------------------------------|
# generator.data ----
#------------------------------------------------------------------------------|

generator.data <- generator.data[,.(node.number,
                                    id, 
                                    `Max Capacity` = max.capacity.MW,
                                    `Min Stable Level` = min.output.MW,
                                    Status = status)]

# change to generator name
generator.data <- merge(generator.data, 
                        node.data[,.(node.number, Node)], 
                        by = "node.number", 
                        all.x = TRUE)

generator.data[,Generator := paste("GEN", Node, id, sep = "_")]

# clean up
generator.data[,c("node.number", "id") := NULL]

setcolorder(generator.data, c("Generator", "Node", "Max Capacity",  
                              "Min Stable Level", "Status"))

# add to list to write out
reformatted.tables <- c(reformatted.tables, "generator.data")


#------------------------------------------------------------------------------|
# transformer.data ----
#------------------------------------------------------------------------------|

if (exists("transformer.data")) {
    transformer.data <- transformer.data[,.(Transformer = paste(node.from.number, 
                                                                node.to.number, 
                                                                id, 
                                                                "tfmr", 
                                                                sep = "_"),
                                            node.from.number,
                                            node.to.number,
                                            Resistance = resistance.pu,
                                            Reactance = reactance.pu,
                                            Rating = rating.MW,
                                            Status = status)]

    # add Node From and Node To names
    transformer.data <- merge(transformer.data, 
                              node.data[,.(node.from.number = node.number,
                                           `Node From` = Node,
                                           kV.From = Voltage)],
                              by = "node.from.number",
                              all.x = TRUE)    
        
    transformer.data <- merge(transformer.data, 
                              node.data[,.(node.to.number = node.number,
                                           `Node To` = Node,
                                           kV.To = Voltage)],
                              by = "node.to.number",
                              all.x = TRUE)
    
    # clean up
    transformer.data[,c("node.to.number", "node.from.number") := NULL]
    
    setcolorder(transformer.data, c("Transformer", "Node From", "Node To",
                                    "kV.From", "kV.To", "Rating", 
                                    "Resistance", "Reactance", "Status"))
    
    # add to list to write out
    reformatted.tables <- c(reformatted.tables, "transformer.data")
}

# clean up node
node.data[, node.number := NULL]

#------------------------------------------------------------------------------|

for (tab.name in reformatted.tables) {
    
    write.csv(get(tab.name), 
              file.path(output.dir, paste0(tab.name, ".csv")),
              row.names = FALSE, 
              quote = FALSE)
}

# clean up
rm(tab.name)
