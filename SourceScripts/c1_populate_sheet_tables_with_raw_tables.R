# import data from PSSE .raw files into PLEXOS readable format

# for each subtable: initialize relevant .sheet table with any applicable 
# constants, populate this table, add table to full sheet table

# uses:
# rename.regions
# rename.zones
# map.newregion.file (optional)
# map.newzone.file (optional)
# unit.status.file

#------------------------------------------------------------------------------|
# Create node data table ----
#------------------------------------------------------------------------------|
# uses Bus.table

# start populating node.data.table with all information about nodes
node.data.table <- Bus.table[,.(BusNumber, BusName, Region, Zone, Voltage.kV)]
node.data.table[, BusName := paste0(BusNumber, "_", BusName, "_", Voltage.kV)]

# add region and zone names to node.data.table
node.data.table <- merge(node.data.table, 
  Area.interchange.table[, .(RegionName, Region)], by = "Region", all.x = TRUE)

node.data.table <- merge(node.data.table, Zone.table[, .(ZoneName, 
  Zone)], by = "Zone", all.x = TRUE)

# if there are input files to remap the nodes' regions and zones, remap them
if (rename.regions) { 
  
  map.newregions <- fread(file.path(inputfiles.dir, map.newregion.file))
  
  node.data.table <- merge(node.data.table[,RegionName := NULL], 
    map.newregions[,.(BusNumber, RegionName)], by = "BusNumber", all.x = TRUE)
  #node.data.table <- node.data.table[!
  #duplicated(node.data.table),] #commenting out b/c input file is clean
}

if (rename.zones) {

  map.newzones <- fread(file.path(inputfiles.dir, map.newzone.file))
  
  node.data.table <- merge(node.data.table[,ZoneName := NULL], 
    map.newzones[,.(BusNumber, ZoneName)], by = "BusNumber", all.x = TRUE)
  #node.data.table <- node.data.table[
  #!duplicated(node.data.table),] #commenting out b/c input file is clean
}

# correction for one SR CHHATTISGARH node. Looking at the network around this 
# node, it really looks like this node is supposed to be in AP/Telangana. 
# Changing it now.
node.data.table[BusName == "414019_LANCO_400" & ZoneName == "SR" &
    RegionName == "CHHATTISGARH", 
  RegionName := "ANDHRA PRADESH & TELANGANA"]


#------------------------------------------------------------------------------|
# Add nodes ----
#------------------------------------------------------------------------------|

# add nodes to object .sheet
nodes.to.objects <- initialize_table(Objects.prototype, nrow(node.data.table),
  c(class = "Node"))
nodes.to.objects[, name := node.data.table[,BusName]]
nodes.to.objects[, category := node.data.table[,RegionName]]

Objects.sheet <- merge_sheet_w_table(Objects.sheet, nodes.to.objects)

# add nodes to properties .sheet : first pull properties (Voltage and Units),
# format table correctly, then add to properties .sheet
nodes.to.properties <- node.data.table[,.(BusName, Voltage.kV)]
setnames(nodes.to.properties, 'Voltage.kV', 'Voltage')
nodes.to.properties[,Units := 1]

add_to_properties_sheet(nodes.to.properties, 'Node', names.col = 'BusName', 
  collection.name = 'Nodes')

# clean up
rm(nodes.to.objects, nodes.to.properties)


#------------------------------------------------------------------------------|
# Add regions ----
#------------------------------------------------------------------------------|

# add regions to object .sheet
all.regions <- unique(node.data.table[,RegionName])

regions.to.objects <- initialize_table(Objects.prototype, length(all.regions), 
  list(class = "Region"))
regions.to.objects[, name :=  all.regions]

Objects.sheet <- merge_sheet_w_table(Objects.sheet, regions.to.objects)

# add node-region membership to memberships .sheet
regions.to.nodes.to.memberships <- initialize_table(Memberships.prototype, 
  nrow(node.data.table), list(parent_class = "Node", child_class = "Region",
  collection = "Region"))
regions.to.nodes.to.memberships[, parent_object := node.data.table[,BusName]]
regions.to.nodes.to.memberships[, child_object := node.data.table[,RegionName]]

Memberships.sheet <- merge_sheet_w_table(Memberships.sheet, 
  regions.to.nodes.to.memberships)

# clean up
rm(all.regions, regions.to.objects, regions.to.nodes.to.memberships)


#------------------------------------------------------------------------------|
# Add zones ----
#------------------------------------------------------------------------------|

# add zones to objects .sheet
all.zones <- unique(node.data.table[,ZoneName])

zones.to.objects <- initialize_table(Objects.prototype, length(all.zones), 
  c(class = "Zone"))
zones.to.objects[, name :=  all.zones]

Objects.sheet <- merge_sheet_w_table(Objects.sheet, zones.to.objects)

# add zone-region membership to memberships .sheet
zones.to.nodes.to.memberships <- initialize_table(Memberships.prototype, 
  nrow(node.data.table), list(parent_class = "Node", child_class = "Zone", 
  collection = "Zone"))
zones.to.nodes.to.memberships[, parent_object := node.data.table[,BusName]]
zones.to.nodes.to.memberships[, child_object := node.data.table[,ZoneName]]

Memberships.sheet <- merge_sheet_w_table(Memberships.sheet, 
  zones.to.nodes.to.memberships)

# clean up
rm(all.zones, zones.to.objects, zones.to.nodes.to.memberships)


#------------------------------------------------------------------------------|
# Create line data table ----
#------------------------------------------------------------------------------|
# uses Branch.table

line.data.table <- Branch.table[,.(BranchFromBus, BranchToBus, ID, 
  Resistance.pu, Reactance.pu, RatingA, RatingB, RatingC, Status, Length)]
  
# add BranchFromBus attributes
line.data.table <- merge(line.data.table, node.data.table[, .(BusNumber, 
  BusName, RegionName, ZoneName, Voltage.kV)], by.x = 'BranchFromBus', 
  by.y = 'BusNumber')
setnames(line.data.table, c('BusName', 'RegionName', 'ZoneName', 'Voltage.kV'), 
  c('FromBusName', 'FromRegion', 'FromZone', 'FromKV'))
  
# add BranchToBus attributes
line.data.table <- merge(line.data.table, node.data.table[, .(BusNumber, 
  BusName, RegionName, ZoneName, Voltage.kV)], by.x = 'BranchToBus', 
  by.y = 'BusNumber')
setnames(line.data.table, c('BusName', 'RegionName', 'ZoneName', 'Voltage.kV'), 
  c('ToBusName', 'ToRegion', 'ToZone', 'ToKV'))

# add name, category, and that these are AC lines 
line.data.table[, name := paste0(BranchFromBus, "_", BranchToBus, "_", 
  ID, "_CKT")] 
line.data.table[FromRegion == ToRegion, category := paste0("AC_", FromRegion)]
line.data.table[FromRegion != ToRegion, category := "Interstate_AC"]
line.data.table[,ACorDC := 'AC']


#------------------------------------------------------------------------------|
# Add DC lines to line data table ----
#------------------------------------------------------------------------------|
# uses DC.line.table
                
dc.line.data.table <- DC.line.table[,.(BranchFromBus = FromBusNumber, 
  BranchToBus = ToBusNumber, ID = LineName, Resistance.pu, 
  RatingB = MaxFlow.MW)]
                  
# add BranchFromBus attributes
dc.line.data.table <- merge(dc.line.data.table, node.data.table[, .(BusNumber, 
  BusName, RegionName, ZoneName, Voltage.kV)], by.x = 'BranchFromBus', 
  by.y = 'BusNumber')
setnames(dc.line.data.table, c('BusName', 'RegionName', 'ZoneName', 
  'Voltage.kV'), c('FromBusName', 'FromRegion', 'FromZone', 'FromKV'))
                  
# add BranchToBus attributes
dc.line.data.table <- merge(dc.line.data.table, node.data.table[, .(BusNumber, 
  BusName, RegionName, ZoneName, Voltage.kV)], by.x = 'BranchToBus', 
  by.y = 'BusNumber')
setnames(dc.line.data.table, c('BusName', 'RegionName', 'ZoneName', 
  'Voltage.kV'), c('ToBusName', 'ToRegion', 'ToZone', 'ToKV'))
                
# add name, category, and that these are AC lines 
dc.line.data.table[, name := paste0(BranchFromBus, "_", BranchToBus, "_", 
  ID, "_CKT_DC")] 
dc.line.data.table[FromRegion == ToRegion, 
  category := paste0("DC_", FromRegion)]
dc.line.data.table[FromRegion != ToRegion, category := "Interstate_DC"]
dc.line.data.table[,ACorDC := 'DC']

# add to line.data.table
line.data.table <- rbind(line.data.table, dc.line.data.table, fill = TRUE)
                
# clean up
rm(dc.line.data.table)


#------------------------------------------------------------------------------|
# Add lines ----
#------------------------------------------------------------------------------|
  
# add lines to objects .sheet
lines.to.objects <- initialize_table(Objects.prototype, nrow(line.data.table), 
  list(class = "Line"))
lines.to.objects[, name := line.data.table[,name]] 
lines.to.objects[, category := line.data.table[,category]]

Objects.sheet <- merge_sheet_w_table(Objects.sheet, lines.to.objects)

# add lines to memberships .sheet
lines.to.nodes.to.memberships <- initialize_table(Memberships.prototype, 
  nrow(line.data.table), list(parent_class = "Line", child_class = "Node"))
lines.to.nodes.to.memberships[, parent_object := line.data.table[,name]]
lines.to.nodes.to.memberships[, `Node From` := line.data.table[,FromBusName]]
lines.to.nodes.to.memberships[, `Node To` := line.data.table[,ToBusName]]

lines.to.nodes.to.memberships <- melt(lines.to.nodes.to.memberships[, 
  c("collection", "child_object") := NULL], measure.vars = 
    c("Node From", "Node To"), variable.name = "collection", 
  value.name = "child_object")

Memberships.sheet <- merge_sheet_w_table(Memberships.sheet, 
  lines.to.nodes.to.memberships)

# check to see if lines have a RatingB... if not, use RatingC, then RatingA
line.data.table[ACorDC == 'AC' & RatingB == '0', RatingB := 
                  apply(line.data.table[ACorDC == 'AC' &
                                          RatingB == '0', 
                                        .(RatingA,RatingC)],1,max)]

# add lines to properties .sheet : pull relevant properties from line.data.table 
# and add them to the properties .sheet
# for now, AC and DC are done separately because they have different properties.
lines.to.properties.ac <- line.data.table[ACorDC == 'AC',.(name, Units = 1, 
  `Max Flow` = RatingB, `Min Flow` = -1 * RatingB,
  `Overload Max Rating` = RatingC, `Overload Min Rating` = -1 * RatingC,
  Resistance = Resistance.pu,  Reactance = Reactance.pu )]

add_to_properties_sheet(lines.to.properties.ac, 'Line', names.col = 'name', 
  collection.name = 'Lines')

lines.to.properties.dc <- line.data.table[ACorDC == 'DC',.(name, Units = 1, 
  `Max Flow` = RatingB, `Min Flow` = -1 * RatingB,
  Resistance = Resistance.pu)]

add_to_properties_sheet(lines.to.properties.dc, 'Line', names.col = 'name', 
  collection.name = 'Lines')

  # clean up
rm(lines.to.objects, lines.to.properties.ac, lines.to.properties.dc, 
  lines.to.nodes.to.memberships)


#------------------------------------------------------------------------------|
# Create generator data table ----
#------------------------------------------------------------------------------|
# uses Generator.table

generator.data.table <- Generator.table[,.(BusNumber, ID, MaxOutput.MW, 
  MinOutput.MW, Status)]

# add node's attributes to this table
generator.data.table <- merge(generator.data.table, node.data.table, 
  by = 'BusNumber')

generator.data.table[,Generator.Name := paste0("GEN_", BusName, "_", ID)]
generator.data.table[,Units := 1]

# **needs to be cleaned up in a better way: NLDC changed max capacity of these 
# two generators (initial capacities were 660 and 600 MW). See script d.
generator.data.table[Generator.Name %in% c("GEN_354013_GMR_400_1",
  "GEN_354013_GMR_400_2"), MaxOutput.MW := 685]


#------------------------------------------------------------------------------|
# Add generators ----
#------------------------------------------------------------------------------|
# uses units.turned.off.file

# add generators to objects .sheet, categorizing by region
gens.to.objects <- initialize_table(Objects.prototype, 
  nrow(generator.data.table), 
  list(class = "Generator", name = generator.data.table[,Generator.Name], 
  category = generator.data.table[,Region]))

Objects.sheet <- merge_sheet_w_table(Objects.sheet, gens.to.objects)

# add generator properties to properties .sheet
gens.to.properties <- generator.data.table[, .(Generator.Name, Units = 1,
  `Max Capacity` = MaxOutput.MW, `Min Stable Level` = MinOutput.MW)]

# create table with units that should be turned off, based on NLDC data
# this should go into c2 once the add properties function has an option
# to replace 
# units.off <- fread(file.path(inputfiles.dir, units.turned.off.file))[[1]]
# gens.to.properties[Generator.Name %in% units.off, Units := 0]

add_to_properties_sheet(gens.to.properties, names.col = 'Generator.Name', 
  object.class = 'Generator', collection.name = 'Generators')

# add generator-node membership to memberships .sheet
gens.to.memberships <- initialize_table(Memberships.prototype, 
  nrow(generator.data.table), list(parent_class = "Generator", 
    child_class = "Node", collection = "Nodes"))
gens.to.memberships[, parent_object := generator.data.table[, Generator.Name]]
gens.to.memberships[, child_object := generator.data.table[, BusName]]

Memberships.sheet <- merge_sheet_w_table(Memberships.sheet, gens.to.memberships)

# clean up
rm(gens.to.objects, gens.to.properties, gens.to.memberships)#, units.off)


#------------------------------------------------------------------------------|
# Create transformer.data.table ----
#------------------------------------------------------------------------------|
# uses Transformer.table.edit

transformer.data.table <- Transformer.table.edit[,.(FromBusNumber, ToBusNumber, 
  ID, Status, Resistance.pu, Reactance.pu, Rating.MW, OverloadRating.MW)]

# add FromBusNumber attributes
transformer.data.table <- merge(transformer.data.table, node.data.table[, 
  .(BusNumber, BusName, RegionName, ZoneName, Voltage.kV)], 
  by.x = 'FromBusNumber', by.y = 'BusNumber')
setnames(transformer.data.table, c('BusName', 'RegionName', 'ZoneName', 
  'Voltage.kV'), c('FromBusName', 'FromRegion', 'FromZone', 'FromKV'))
  
# add ToBusNumber attributes
transformer.data.table <- merge(transformer.data.table, node.data.table[, 
  .(BusNumber, BusName, RegionName, ZoneName, Voltage.kV)], 
  by.x = 'ToBusNumber', by.y = 'BusNumber')
setnames(transformer.data.table, c('BusName', 'RegionName', 'ZoneName', 
  'Voltage.kV'), c('ToBusName', 'ToRegion', 'ToZone', 'ToKV'))

# add name and category
transformer.data.table[, name := paste0(FromBusNumber, "_", ToBusNumber, "_", 
  ID, "_tfmr")] 
transformer.data.table[FromRegion == ToRegion, category := FromRegion]
transformer.data.table[FromRegion != ToRegion, category := "Interstate_tfmr"]


#------------------------------------------------------------------------------|
# Add transformers ----
#------------------------------------------------------------------------------|
# check to see if transformers have an Overload Rating if not, use Rating
transformer.data.table[OverloadRating.MW == '0', OverloadRating.MW := 
                         transformer.data.table[OverloadRating.MW == '0', 
                                                Rating.MW]]
# add transformers to objects .sheet
transf.to.objects <- initialize_table(Objects.prototype, 
  nrow(transformer.data.table), 
  list(class = "Transformer",
  name = transformer.data.table[,name], 
  category = transformer.data.table[,category]))

Objects.sheet <- merge_sheet_w_table(Objects.sheet, transf.to.objects)

# add transformers to properties .sheet
transf.to.properties <- transformer.data.table[,.(name, Units = 1, 
  Rating = Rating.MW, `Overload Rating` = OverloadRating.MW, 
  Resistance = Resistance.pu, Reactance = Reactance.pu)]

add_to_properties_sheet(transf.to.properties, names.col = 'name', 
  object.class = 'Transformer', collection.name = 'Transformers')

# add transformer-node membership to memberships .sheet
transf.to.memberships <- initialize_table(Memberships.prototype, 
  nrow(transformer.data.table), list(parent_class = "Transformer", 
    child_class = "Node"))
transf.to.memberships[,parent_object := transformer.data.table[,name]]
transf.to.memberships[,`Node From` := transformer.data.table[,FromBusName]]
transf.to.memberships[,`Node To` := transformer.data.table[,ToBusName]]

transf.to.memberships <- melt(transf.to.memberships[,c("collection", 
  "child_object") := NULL], measure.vars = c("Node From", "Node To"), 
  variable.name = "collection", value.name = "child_object")

Memberships.sheet  <- merge_sheet_w_table(Memberships.sheet, 
  transf.to.memberships)

# clean up
rm(transf.to.objects, transf.to.properties, transf.to.memberships)