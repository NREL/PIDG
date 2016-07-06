# import data from PSSE .raw files into PLEXOS readable format

# for each subtable: initialize relevant .sheet table with any applicable 
# constants, populate this table, add table to full sheet table

# uses:

# node.file
# line.file
# generator.file
# transformer.file


#------------------------------------------------------------------------------|
# nodes ----
#------------------------------------------------------------------------------|

message("importing node data")

# read in 
node.data.table <- fread(node.file)

node.data.table[, Units := 1]

# check for dupes?


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
all.zones <- unique(node.data.table$ZoneName)

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

message("importing line data")

line.data.table <- fread(line.file)

line.data.table[, Units := 1]

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

message("importing generator data")

# get data
generator.data.table <- fread(generator.file)

# add region
generator.data.table <- merge(generator.data.table, 
                              node.data.table[,.(Node, Region)],
                              by = "Node", 
                              all.x = TRUE)

generator.data.table[, Units := 1]


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
                                               `Max Capacity`, 
                                               `Min Stable Level`)]

add_to_properties_sheet(gens.to.properties, 
                        names.col = 'Generator',
                        object.class = 'Generator', 
                        collection.name = 'Generators')

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

message("importing transformer data")

transformer.data.table <- fread(transformer.file)

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
transformer.data.table[Region.From == Region.From, category := Region.From]
transformer.data.table[Region.From != Region.From, category := "Interstate_tfmr"]


#------------------------------------------------------------------------------|
# Add transformers to .sheet tables ----
#------------------------------------------------------------------------------|

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