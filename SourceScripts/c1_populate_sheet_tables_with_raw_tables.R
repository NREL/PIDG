# import data from PSSE .raw files into PLEXOS readable format

# for each subtable: initialize relevant .sheet table with any applicable 
# constants, populate this table, add table to full sheet table

# uses:

# node.file
# line.file
# generator.file
# transformer.file

#------------------------------------------------------------------------------|
# import data if needed ----
#------------------------------------------------------------------------------|

# either read in network data or procede

if (choose.input == "pre.parsed") {
    
    message("reading in pre-parsed network data")
    
    if (file.exists(file.path(inputfiles.dir, node.file))) {
        node.data.table <- fread(file.path(inputfiles.dir, node.file))
    } else {
        stop(sprintf("!!  %s does not exist", node.file))
    }
    
    if (file.exists(file.path(inputfiles.dir, line.file))) {
        line.data.table <- fread(file.path(inputfiles.dir, line.file))
    } else {
        stop(sprintf(" !!  %s does not exist", line.file))
    }

    if (file.exists(file.path(inputfiles.dir, generator.file))) {
        generator.data.table <- fread(file.path(inputfiles.dir, generator.file))
    } else {
        stop(sprintf("!!  %s does not exist", generator.file))
    }
    
    if (exists("transformer.file")) {
        if (file.exists(file.path(inputfiles.dir, transformer.file))) {
            transformer.data.table <- fread(file.path(inputfiles.dir, transformer.file))
        } else {
            stop(sprintf("!!  %s does not exist", transformer.file))
        }
    } else {
        message(sprintf(">>  transformer.file does not exist ... skipping"))
    }
    
    if (exists("load.file")) {
        if (file.exists(file.path(inputfiles.dir, load.file))) {
            load.data.table <- suppressWarnings(fread(file.path(inputfiles.dir, load.file))) 
            # for bumping load type col to character type in posoco
        } else {
            stop(sprintf("!!  %s does not exist", load.file))
        }
    } else {
        message(sprintf(">>  load.file does not exist ... skipping"))
    }
    
}


#------------------------------------------------------------------------------|
# nodes ----
#------------------------------------------------------------------------------|

message("arranging node data")

if (choose.input == "raw.psse") {
    # if there are input files to remap the nodes' regions and zones, remap them
    if (rename.regions) { 
      
      map.newregions <- fread(file.path(inputfiles.dir, map.newregion.file))
      
      node.data.table <- merge(node.data.table[,Region := NULL], 
                               map.newregions[,.(Node, Region)], 
                               by = "Node", 
                               all.x = TRUE)
    }
    
    if (rename.zones) {
    
      map.newzones <- fread(file.path(inputfiles.dir, map.newzone.file))
      
      node.data.table <- merge(node.data.table[,Zone := NULL], 
                               map.newzones[,.(Node, Zone)], 
                               by = "Node", 
                               all.x = TRUE)
    }
}

node.data.table[, Units := 1]

#------------------------------------------------------------------------------|
# Add nodes to .sheet tables ----
#------------------------------------------------------------------------------|

# add nodes to object .sheet
nodes.to.objects <- initialize_table(Objects.sheet, 
                                     nrow(node.data.table),
                                     list(class = "Node",
                                          name = node.data.table$Node,
                                          category = node.data.table$Region))

Objects.sheet <- merge_sheet_w_table(Objects.sheet, nodes.to.objects)

# add nodes to properties .sheet : Voltage, Units
nodes.to.properties <- node.data.table[,.(Node, Voltage, Units)]

add_to_properties_sheet(nodes.to.properties, 
                        object.class = 'Node', 
                        names.col = 'Node',
                        collection.name = 'Nodes')

# clean up
rm(nodes.to.objects, nodes.to.properties)

#------------------------------------------------------------------------------|
# Add regions to .sheet tables ----
#------------------------------------------------------------------------------|

# add regions to object .sheet
all.regions <- unique(node.data.table$Region)

regions.to.objects <- initialize_table(Objects.sheet, 
                                       length(all.regions),
                                       list(class = "Region",
                                            name = all.regions))

Objects.sheet <- merge_sheet_w_table(Objects.sheet, regions.to.objects)

# add node-region membership to memberships .sheet
regions.to.nodes.to.memberships <- initialize_table(Memberships.sheet, 
                                                    nrow(node.data.table), 
                                                    list(parent_class = "Node", 
                                                         child_class = "Region",
                                                         collection = "Region"))

regions.to.nodes.to.memberships[, parent_object := node.data.table$Node]
regions.to.nodes.to.memberships[, child_object := node.data.table$Region]

Memberships.sheet <- merge_sheet_w_table(Memberships.sheet, 
                                         regions.to.nodes.to.memberships)

# clean up
rm(all.regions, regions.to.objects, regions.to.nodes.to.memberships)


#------------------------------------------------------------------------------|
# Add zones to .sheet tables ----
#------------------------------------------------------------------------------|

# add zones to objects .sheet
all.zones <- unique(node.data.table$Zone)

zones.to.objects <- initialize_table(Objects.sheet, 
                                     length(all.zones),
                                     list(class = "Zone",
                                          name = all.zones))

Objects.sheet <- merge_sheet_w_table(Objects.sheet, zones.to.objects)

# add zone-region membership to memberships .sheet
zones.to.nodes.to.memberships <- initialize_table(Memberships.sheet, 
                                                  nrow(node.data.table), 
                                                  list(parent_class = "Node", 
                                                       child_class = "Zone", 
                                                       collection = "Zone"))

zones.to.nodes.to.memberships[, parent_object := node.data.table$Node]
zones.to.nodes.to.memberships[, child_object := node.data.table$Zone]

Memberships.sheet <- merge_sheet_w_table(Memberships.sheet, 
                                         zones.to.nodes.to.memberships)

# clean up
rm(all.zones, zones.to.objects, zones.to.nodes.to.memberships)


#------------------------------------------------------------------------------|
# lines ----
#------------------------------------------------------------------------------|

message("arranging line data")

# add line units. currently only avaiable for pre.parsed. maybe in the future
# can change automatically assign status to units 
if (!("Units" %in% colnames(line.data.table))) {
    line.data.table[, Units := 1]
}

# find regions from and to for line categorize
line.data.table <- merge(line.data.table,
                         node.data.table[,.(`Node From` = Node, 
                                            `Region.From` = Region)],
                         by = "Node From",
                         all.x = TRUE)

line.data.table <- merge(line.data.table,
                         node.data.table[,.(`Node To` = Node, 
                                            `Region.To` = Region)],
                         by = "Node To",
                         all.x = TRUE)

# add categories
if (!("Type" %in% colnames(line.data.table))) {
    
    line.data.table[is.na(Reactance) | Reactance == 0, Type := "DC"]
    line.data.table[!(is.na(Reactance) | Reactance == 0), Type := "AC"]
}

line.data.table[Region.From == Region.To, category := paste0(Type, "_", Region.From)]
line.data.table[Region.From != Region.To, category := paste0("Interstate_", Type)]

#------------------------------------------------------------------------------|
# Add lines to .sheet tables ----
#------------------------------------------------------------------------------|
  
# add lines to objects .sheet
lines.to.objects <- initialize_table(Objects.sheet, 
                                     nrow(line.data.table), 
                                     list(class = "Line"))

lines.to.objects[, name := line.data.table$Line] 
lines.to.objects[, category := line.data.table$category]

Objects.sheet <- merge_sheet_w_table(Objects.sheet, lines.to.objects)

# add lines to memberships .sheet
lines.to.nodes.to.memberships <- initialize_table(Memberships.sheet, 
                                                  nrow(line.data.table), 
                                                  list(parent_class = "Line", 
                                                       child_class = "Node"))

lines.to.nodes.to.memberships[, parent_object := line.data.table$Line]
lines.to.nodes.to.memberships[, `Node From` := line.data.table$`Node From`]
lines.to.nodes.to.memberships[, `Node To` := line.data.table$`Node To`]

# prepare for melting, then melt down to separate Node From and Node To
lines.to.nodes.to.memberships[, c("collection", "child_object") := NULL]

lines.to.nodes.to.memberships <- melt(lines.to.nodes.to.memberships, 
                                      measure.vars = c("Node From", "Node To"), 
                                      variable.name = "collection", 
                                      value.name = "child_object")

Memberships.sheet <- merge_sheet_w_table(Memberships.sheet, 
                                         lines.to.nodes.to.memberships)

# add lines to properties .sheet : pull relevant properties from line.data.table 
# and add them to the properties .sheet
lines.to.properties <- line.data.table[,.(Line, Units, `Max Flow`, `Min Flow`,
                                          Resistance,  Reactance)]

add_to_properties_sheet(lines.to.properties, 
                        object.class = 'Line', 
                        names.col = 'Line', 
                        collection.name = 'Lines')

# clean up
rm(lines.to.objects, lines.to.properties, lines.to.nodes.to.memberships)


#------------------------------------------------------------------------------|
# generators ----
#------------------------------------------------------------------------------|

message("arranging generator data")

generator.data.table[, Units := 1]

# add region
generator.data.table <- merge(generator.data.table, 
                              node.data.table[,.(Node, Region)],
                              by = "Node", 
                              all.x = TRUE)

# adjust gen cap if needed
# if (choose.input == "raw.psse") {
    # temporary!! adjust max capacity needed
    if (exists("adjust.max.cap")) {
        if(file.exists(file.path(inputfiles.dir, adjust.max.cap))) {
            
            message(sprintf("... adjusting max capacity of generators in %s", 
                            adjust.max.cap))
            
            new.cap <- fread(file.path(inputfiles.dir, adjust.max.cap))
            
            generator.data.table <- merge(generator.data.table, 
                                          new.cap,
                                          by = "Generator", 
                                          all.x = TRUE)
            
            generator.data.table[!is.na(new.capacity), 
                                 `Max Capacity` := new.capacity]
            generator.data.table[, new.capacity := NULL]
            
            rm(new.cap)
            
        } else {
            message(sprintf(">>  %s does not exist ... skipping", 
                            adjust.max.cap))
        }
            
    }
# }

#------------------------------------------------------------------------------|
# Add generators to .sheet tables ----
#------------------------------------------------------------------------------|

# add generators to objects .sheet, categorizing by region
gens.to.objects <- initialize_table(Objects.sheet, 
                                    nrow(generator.data.table), 
                                    list(class = "Generator", 
                                         name = generator.data.table$Generator,
                                         category = generator.data.table$Region))

Objects.sheet <- merge_sheet_w_table(Objects.sheet, gens.to.objects)

# add generator properties to properties .sheet
gens.to.properties <- generator.data.table[, .(Generator, Units,
                                               `Max Capacity`)]

add_to_properties_sheet(gens.to.properties, 
                        names.col = 'Generator',
                        object.class = 'Generator', 
                        collection.name = 'Generators')

if ("Min Stable Level" %in% generator.data.table) {
    msl.to.props <- generator.data.table[, .(Generator, `Min Stable Level`)]

    add_to_properties_sheet(msl.to.props)
}

# add generator-node membership to memberships .sheet
gens.to.memberships <- initialize_table(Memberships.sheet,
                                        nrow(generator.data.table), 
                                        list(parent_class = "Generator", 
                                             child_class = "Node", 
                                             collection = "Nodes"))

gens.to.memberships[, parent_object := generator.data.table$Generator]
gens.to.memberships[, child_object := generator.data.table$Node]

Memberships.sheet <- merge_sheet_w_table(Memberships.sheet, gens.to.memberships)

# clean up
rm(gens.to.objects, gens.to.properties, gens.to.memberships)


#------------------------------------------------------------------------------|
# transformers ----
#------------------------------------------------------------------------------|

if (exists("transformer.data.table")) {
    
    message("arranging transformer data")
    
    transformer.data.table[, Units := 1]
    
    # find regions from and to for line categorize
    transformer.data.table <- merge(transformer.data.table,
                                    node.data.table[,.(`Node From` = Node,
                                                       `Region.From` = Region)],
                                    by = "Node From",
                                    all.x = TRUE)
    
    transformer.data.table <- merge(transformer.data.table,
                                    node.data.table[,.(`Node To` = Node,
                                                       `Region.To` = Region)],
                                    by = "Node To",
                                    all.x = TRUE)
    
    # add category
    transformer.data.table[Region.From == Region.To, category := Region.From]
    transformer.data.table[Region.From != Region.To, category := "Interstate_tfmr"]
}

#------------------------------------------------------------------------------|
# Add transformers to .sheet tables ----
#------------------------------------------------------------------------------|

if (exists("transformer.data.table")) {
    
    # add transformers to objects .sheet
    transf.to.objects <- initialize_table(Objects.sheet, 
                                          nrow(transformer.data.table), 
                                          list(class = "Transformer",
                                               name = transformer.data.table$Transformer, 
                                               category = transformer.data.table$category))
    
    Objects.sheet <- merge_sheet_w_table(Objects.sheet, transf.to.objects)
    
    # add transformers to properties .sheet
    transf.to.properties <- transformer.data.table[,.(Transformer, Units, Rating, 
                                                      Resistance, Reactance)]
    
    add_to_properties_sheet(transf.to.properties, 
                            names.col = 'Transformer', 
                            object.class = 'Transformer', 
                            collection.name = 'Transformers')
    
    # add transformer-node membership to memberships .sheet
    transf.to.memberships <- initialize_table(Memberships.sheet, 
                                              nrow(transformer.data.table), 
                                              list(parent_class = "Transformer",
                                                   child_class = "Node"))
    
    transf.to.memberships[, parent_object := transformer.data.table$Transformer]
    transf.to.memberships[,`Node From` := transformer.data.table$`Node From`]
    transf.to.memberships[,`Node To` := transformer.data.table$`Node To`]
    
    # get rid of automatically generated columns for melting
    transf.to.memberships[,c("collection", "child_object") := NULL]
    
    transf.to.memberships <- melt(transf.to.memberships, 
                                  measure.vars = c("Node From", "Node To"),
                                  variable.name = "collection", 
                                  value.name = "child_object")
    
    Memberships.sheet  <- merge_sheet_w_table(Memberships.sheet, 
                                              transf.to.memberships)
    
    # clean up
    rm(transf.to.objects, transf.to.properties, transf.to.memberships)
}
