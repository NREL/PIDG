

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
# 
# root.dir <- "~/GitHub/India_GtG/Process_for_PLEXOS_import/PSSE2PLEXOS/Update/outputs_a-1_raw_psse"
# 
# # required data
# node.file <- file.path(root.dir, "Bus.table.csv")
# line.file <- file.path(root.dir, "Branch.table.csv")
# generator.file <- file.path(root.dir, "Generator.table.csv")
# 
# # optional data
# line.dc.file <- file.path(root.dir, "DC.line.table.csv")
# transformer.file <- file.path(root.dir, "Transformer.Table.csv")
#     
# # optional extras
# zone.file <- file.path(root.dir, "Zone.table.csv")
# region.file <- file.path(root.dir, "Area.interchange.table.csv")
# owner.file <- file.path(root.dir, "Owner.table.csv")
# load.file <- file.path(root.dir, "Load.table.csv")
# 
# output.dir <- file.path(root.dir, "../outputs_a-2_reformatted_psse")


#------------------------------------------------------------------------------|
# setup ----
#------------------------------------------------------------------------------|

# load packages
# pacman::p_load(data.table)
# 
# # make sure output.dir exists
# if (!dir.exists(output.dir)) {
#     dir.create(output.dir, recursive = TRUE)
# }
# 
# # read in files
# node.data <- fread(node.file, colClasses = "character")
# line.data <- fread(line.file, colClasses = "character")
# generator.data <- fread(generator.file, colClasses = "character")
# 
# if (exists("line.dc.file")) line.dc.data <- fread(line.dc.file, 
#                                                   colClasses = "character")
# if (exists("transformer.file")) transformer.data <- fread(transformer.file, 
#                                                           colClasses = "character")
# 
# if (exists("zone.file")) zone.data <- fread(zone.file, colClasses = "character")
# if (exists("region.file")) region.data <- fread(region.file, colClasses = "character")
# if (exists("owner.file")) owner.data <- fread(owner.file, colClasses = "character")
# if (exists("load.file")) load.data <- fread(load.file, colClasses = "character")    
# 
# # clean up
# rm(node.file, line.file, generator.file)
# if (exists("line.dc.file")) rm(line.dc.file)
# if (exists("transformer.file")) rm(transformer.file)
# if (exists("zone.file")) rm(zone.file)
# if (exists("region.file")) rm(region.file)
# if (exists("owner.file")) rm(owner.file)
# if (exists("load.file")) rm(load.file)


# clean environment of skipped and empty tables, rename tables to use

node.data.table <- Bus.table
line.data.table <- Branch.table
generator.data.table <- Generator.table
line.dc.data.table <- DC.line.table
transformer.data.table <- Transformer.table
zone.data.table <- Zone.table
region.data.table <- Area.interchange.table
owner.data.table <- Owner.table
load.data.table <- Load.table

# clean up from initial parsing script
rm(list=c(skip.tables, done.tables))


# track which to write out
reformatted.tables <- c()


#------------------------------------------------------------------------------|
# node.data.table ----
#------------------------------------------------------------------------------|

## rename columns, clean up node.data.table
node.data.table <- node.data.table[,.(Node = paste(node.number, node.name, kV, sep = "_"),
                                      node.number, # for merging, delete later
                                      Voltage = kV,
                                      region.number,
                                      zone.number,
                                      owner.number)]


## optionally, add region, zone, and owner names. otherwise, rename columns

# regions
if (exists("region.data.table")) {
    
    node.data.table <- merge(node.data.table, 
                             region.data.table[,.(region.number, Region = region.name)],
                             by = "region.number",
                             all.x = TRUE)
    
    node.data.table[,region.number := NULL]
    
} else {
    
    if ("region.number" %in% colnames(node.data.table)) {
        setnames(node.data.table, "region.number", "Region") }
}

# zones
if (exists("zone.data.table")) {
    
    node.data.table <- merge(node.data.table, 
                             zone.data.table[,.(zone.number, Zone = zone.name)],
                             by = "zone.number",
                             all.x = TRUE)
    
    node.data.table[,zone.number := NULL]
    
} else {
    
    if ("zone.number" %in% colnames(node.data.table)) {
        setnames(node.data.table, "zone.number", "Zone") }
}

# owners
if (exists("owner.data.table")) {
    
    node.data.table <- merge(node.data.table, 
                             owner.data.table[,.(owner.number, Owner = owner.name)],
                             by = "owner.number",
                             all.x = TRUE)
    
    node.data.table[,owner.number := NULL]
    
} else {
    
    if ("owner.number" %in% colnames(node.data.table)) {
        setnames(node.data.table, "owner.number", "Owner") }
}

# add to list to write out
reformatted.tables <- c(reformatted.tables, "node.data.table")


#------------------------------------------------------------------------------|
# load.data.table ----
#------------------------------------------------------------------------------|

## optionally add load
if (exists("load.data.table")) {
    load.data.table <- load.data.table[,.(node.number, 
                                          Status = status, 
                                          Type = load.type,
                                          Load = active.power.MW)]
    
    # add node name
    load.data.table <- merge(load.data.table, 
                             node.data.table[,.(node.number, Node)], 
                             by = "node.number", 
                             all.x =TRUE)
    
    # clean up
    load.data.table[,node.number := NULL]
    
    setcolorder(load.data.table, c("Node", "Type", "Status", "Load"))
}

# add to list to write out
reformatted.tables <- c(reformatted.tables, "load.data.table")


#------------------------------------------------------------------------------|
# line.data.table ----
#------------------------------------------------------------------------------|

## line data
line.data.table <- line.data.table[,.(Line = paste(node.from.number, node.to.number, 
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
line.data.table[,`Max Flow` := {temp = apply(line.data.table[,.(ratingA,ratingC)], 
                                             1, max);
ifelse(ratingB != "0", ratingB, temp)}]

# line.data.table[,c("ratingA", "ratingB", "ratingC") := NULL]

line.data.table[,`Min Flow` := `Max Flow` * -1]


# if DC lines exist, add them
if (exists("line.dc.data.table")) {
    
    line.dc.data.table <- line.dc.data.table[,.(Line = paste(node.from.number, 
                                                             node.to.number, 
                                                             id.num, 
                                                             "CKT", sep = "_"),
                                                node.from.number, 
                                                node.to.number,
                                                Resistance = resistance.pu,
                                                `Max Flow` = max.flow.MW,
                                                `Min Flow` = as.numeric(max.flow.MW) * -1,
                                                Status = 1
    )]
    
    line.data.table <- rbindlist(list(line.data.table, line.dc.data.table), 
                                 use.names = TRUE, 
                                 fill = TRUE)
}


# add Node From and Node To names
line.data.table <- merge(line.data.table, 
                         node.data.table[,.(node.from.number = node.number, 
                                            `Node From` = Node,
                                            Voltage.From = Voltage)],
                         by = "node.from.number", 
                         all.x = TRUE)

line.data.table <- merge(line.data.table, 
                         node.data.table[,.(node.to.number = node.number, 
                                            `Node To` = Node,
                                            Voltage.To = Voltage)],
                         by = "node.to.number", 
                         all.x = TRUE)

# categorize lines
line.data.table[is.na(Reactance), Type := "DC"]
line.data.table[!is.na(Reactance), Type := "AC"]

line.data.table[Type == "DC", Line := paste0(Line, "_DC")]

# clean up
line.data.table[,c("node.from.number", "node.to.number") := NULL]

setcolorder(line.data.table, c("Line", "Node From", "Node To", "Type", 
                               "Voltage.From", "Voltage.To", 
                               "Max Flow", "Min Flow", 
                               "ratingA", "ratingB", "ratingC",
                               "Resistance", "Reactance", "Status", "Length"))

# add to list to write out
reformatted.tables <- c(reformatted.tables, "line.data.table")


#------------------------------------------------------------------------------|
# generator.data.table ----
#------------------------------------------------------------------------------|

generator.data.table <- generator.data.table[,.(node.number,
                                                id, 
                                                `Max Capacity` = max.capacity.MW,
                                                `Min Stable Level` = min.output.MW,
                                                Status = status)]

# change to generator name
generator.data.table <- merge(generator.data.table, 
                              node.data.table[,.(node.number, Node)], 
                              by = "node.number", 
                              all.x = TRUE)

generator.data.table[,Generator := paste("GEN", Node, id, sep = "_")]

# clean up
generator.data.table[,c("node.number", "id") := NULL]

setcolorder(generator.data.table, c("Generator", "Node", "Max Capacity",  
                                    "Min Stable Level", "Status"))

# add to list to write out
reformatted.tables <- c(reformatted.tables, "generator.data.table")


#------------------------------------------------------------------------------|
# transformer.data.table ----
#------------------------------------------------------------------------------|

if (exists("transformer.data.table")) {
    transformer.data.table <- transformer.data.table[,.(Transformer = paste(node.from.number, 
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
    transformer.data.table <- merge(transformer.data.table, 
                                    node.data.table[,.(node.from.number = node.number,
                                                       `Node From` = Node,
                                                       Voltage.From = Voltage)],
                                    by = "node.from.number",
                                    all.x = TRUE)    
    
    transformer.data.table <- merge(transformer.data.table, 
                                    node.data.table[,.(node.to.number = node.number,
                                                       `Node To` = Node,
                                                       Voltage.To = Voltage)],
                                    by = "node.to.number",
                                    all.x = TRUE)
    
    # clean up
    transformer.data.table[,c("node.to.number", "node.from.number") := NULL]
    
    setcolorder(transformer.data.table, c("Transformer", "Node From", "Node To",
                                          "Voltage.From", "Voltage.To", "Rating", 
                                          "Resistance", "Reactance", "Status"))
    
    # add to list to write out
    reformatted.tables <- c(reformatted.tables, "transformer.data.table")
}

# clean up node
node.data.table[, node.number := NULL]
setorder(node.data.table, Node)

#------------------------------------------------------------------------------|

# for (tab.name in reformatted.tables) {
#     
#     write.csv(get(tab.name), 
#               file.path(output.dir, paste0(tab.name, ".csv")),
#               row.names = FALSE, 
#               quote = FALSE)
# }
# 
# # clean up
# rm(tab.name)
