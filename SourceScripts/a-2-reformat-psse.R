

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


#------------------------------------------------------------------------------|
# setup ----
#------------------------------------------------------------------------------|

# clean environment of skipped and empty tables, rename tables to use

node.data.table <- Bus.table
line.dc.data.table <- DC.line.table
transformer.data.table <- Transformer.table

if (exists('Load.table')) {
 load.data.table <- Load.table
}
if (exists('Branch.table')) {
  line.data.table <- Branch.table
}
if (exists('Generator.table')) {
  generator.data.table <- Generator.table
}
if (exists('Zone.table')) {
  zone.data.table <- Zone.table
}
if (exists('Area.interchange.table')){
  region.data.table <- Area.interchange.table
}
if (exists('Owner.table')) {
  owner.data.table <- Owner.table
}

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

gen.cols <- colnames(generator.data.table)
gen.owner.cols <- gen.cols[grepl("owner", gen.cols)]

if (length(gen.owner.cols) > 0) {
    
    # grab columns with ownership data to be merged with the full generator
    # table later
    gen.owners <- generator.data.table[,.SD, .SDcols = c("node.number", "id", 
                                                   gen.owner.cols)]
    

    
}

generator.data.table <- generator.data.table[,.(node.number,
                                                id, 
                                                `Max Capacity` = max.capacity.MW,
                                                `Min Stable Level` = min.output.MW,
                                                Status = status)]

# add ownership back in if it exists
if (length(gen.owner.cols) > 0) {
    
    generator.data.table <- merge(generator.data.table, gen.owners, 
                                  by = c("node.number", "id"),
                                  all.x = TRUE)
    
    # if owner.data.table exists, add real names for ownerships
    if (exists("owner.data.table")) {
        
        # need to merge for each owner.numberX column that exists. find 
        # out how many times we need to merge
        times.to.merge <- length(gen.owner.cols)/2
        
        # check this
        last.col <- gen.owner.cols[length(gen.owner.cols)]
        if (times.to.merge != as.numeric(substr(last.col, 
                                                nchar(last.col),
                                                nchar(last.col)))) {
            
            message(paste("Number of owner columns doesn't line up with.",
                          "owner column labels. Please check."))
        }
        
        for (i in seq_len(times.to.merge)) {
            
            # workaround to force owner.number to be numeric
            generator.data.table[, owner.number.temp := as.numeric(get(paste0("owner.number", i)))]
            generator.data.table[, (paste0("owner.number", i)) := NULL]
            generator.data.table[, (paste0("owner.number", i)) := owner.number.temp]
            generator.data.table[, owner.number.temp := NULL]
            
            # merge owner table in 
            generator.data.table <- merge(generator.data.table, 
                                            owner.data.table,
                                          by.x = paste0("owner.number", i),
                                          by.y = "owner.number",
                                          all.x = TRUE)
            
            # replace column names
            setnames(generator.data.table, 
                     "owner.name", 
                     paste0("Owner", i))
                        
            setnames(generator.data.table, 
                     paste0("owner.fraction", i), 
                     paste0("Owner Fraction", i))
            
            generator.data.table[,(paste0("owner.number", i)) := NULL]
        
            
        }
        # clean up
        rm(i)
        
    } # end exists("owner.data.table")
} # end length(gen.owner.cols)

# clean up
rm(gen.cols, gen.owner.cols)

# change to generator name
generator.data.table <- merge(generator.data.table, 
                              node.data.table[,.(node.number, Node)], 
                              by = "node.number", 
                              all.x = TRUE)

generator.data.table[,Generator := paste("GEN", Node, id, sep = "_")]

# clean up
generator.data.table[,c("node.number", "id") := NULL]

# reorder cols. ownership cols are optional and may not exist, so only reorder
# required columns
known.cols <- c("Generator", "Node", "Max Capacity", "Min Stable Level", 
                "Status")
all.cols <- colnames(generator.data.table)

setcolorder(generator.data.table, 
            c(known.cols, all.cols[!(all.cols %in% known.cols)]))

                                   
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
