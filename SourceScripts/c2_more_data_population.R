#uses (from master):
# map.gen.to.fuel.file
# map.region.to.load.RE.file
# map.vom.to.gens.file
# map.ramps.to.fuel.file
# import.model.base.file
# generator.property.file.list
# min.gen.file
# min.up.time.file
# min.down.time.file
# start.cost.file
# RE.gen.file
# interfaces.files.list
# generator.property.by.fuel.list

#------------------------------------------------------------------------------|
# add fuels and categorize generators by fuel ----
#------------------------------------------------------------------------------|

fuel.table <- fread(file.path(inputfiles.dir, map.gen.to.fuel.file))

# add fuels to generator.data.table
generator.data.table <- 
  merge(generator.data.table, fuel.table[, .(Generator.Name, Fuel)], 
        by = "Generator.Name", all.x = TRUE)

# add fuels to objects .sheet
all.fuels <- unique(fuel.table[, Fuel])

fuels.to.objects <- initialize_table(Objects.prototype, length(all.fuels), 
                                     list(class = "Fuel"))
fuels.to.objects[, name := all.fuels]

Objects.sheet <- merge_sheet_w_table(Objects.sheet, fuels.to.objects)

# add generator-fuels membership to memberships
fuels.to.gens.to.memberships <- 
  initialize_table(Memberships.prototype, nrow(generator.data.table), 
                   list(parent_class = "Generator", child_class = "Fuel", 
                        collection = "Fuels"))

fuels.to.gens.to.memberships[,parent_object := 
                               generator.data.table[,Generator.Name]]
fuels.to.gens.to.memberships[,child_object := generator.data.table[,Fuel]]

Memberships.sheet <- merge_sheet_w_table(Memberships.sheet, 
                                         fuels.to.gens.to.memberships)

# edit generators to categorize by fuel instead of region
Objects.sheet <- 
  merge(Objects.sheet, 
        generator.data.table[, .(Generator.Name, Fuel, class = 'Generator')], 
        by.x = c('class', 'name'), by.y = c('class', 'Generator.Name'), 
        all.x = T)
Objects.sheet[!is.na(Fuel),category := Fuel]
Objects.sheet[,Fuel := NULL]

# clean up
rm(fuel.table, all.fuels, fuels.to.objects, fuels.to.gens.to.memberships)


#------------------------------------------------------------------------------|
# add load (mapped by region by external file) and lpf ----
#------------------------------------------------------------------------------|
load.to.region.map <- 
  fread(file.path(inputfiles.dir, map.region.to.load.file))

# create data file object name column
load.to.region.map[,DataFile := paste0(load.to.region.map[,Region], 
                                     " Load File Object")]

# add load data file objects to objects .sheet 
# uses load.to.region.map
load.file.to.object <- 
  initialize_table(Objects.prototype, nrow(load.to.region.map), 
  list(class = "Data File", category = "Regional Load"))
load.file.to.object[, name := load.to.region.map[,DataFile]]

Objects.sheet <- merge_sheet_w_table(Objects.sheet, load.file.to.object)

# load file object to as regional load
# uses load.to.region.map
load.to.region.properties <- 
  initialize_table(Properties.prototype, nrow(load.to.region.map), 
                   list(parent_class = "System", child_class = "Region", 
                        collection = "Regions", parent_object = "System", 
                        band_id = 1, property = "Load", value = 0))
load.to.region.properties[,child_object := load.to.region.map[,Region]]
load.to.region.properties[, filename := 
                            paste0("{Object}",load.to.region.map[,DataFile])]

Properties.sheet <- merge_sheet_w_table(Properties.sheet, 
                                        load.to.region.properties)


# load to properties (attach filepath to object based on scenario)
# uses load.to.region.map

# loop through each column and add columns
load.scens <- colnames(load.to.region.map)
load.scens <- load.scens[!(load.scens %in% c('Region', 'DataFile'))]

for (name in load.scens) {
  # create small table to pass to add_to_properties_sheet
  cur.tab <- load.to.region.map[,.SD, .SDcols = c('DataFile', name)]
  cur.tab[,filename := 0]
  
  add_to_properties_sheet(cur.tab, names.col = 'DataFile', 
    object.class = 'Data File', collection.name = 'Data Files', 
    datafile.col = name, 
    scenario.name = ifelse(tolower(name) == 'base', NA, name))
}

# add any scenarios associated with load as objects
load.scens <- load.scens[tolower(load.scens) != 'base']
load.scens.to.objects <- 
  initialize_table(Objects.prototype, length(load.scens), 
  list(name = load.scens, class = "Scenario", category = "Load scenarios"))

Objects.sheet <- merge_sheet_w_table(Objects.sheet, load.scens.to.objects)


# add load participation factor to nodes
# uses Load.table, node.data.table

message('... Adding load participation factor from current raw file')

# original table has duplicates which aren't related to each other--only select
# one. Should refine later. 
# since there are NAs, must convert them to zero for the next step to work 
# correctly and for PLEXOS to read them in correctly
# remove any negative loads and replace them with zero
# convert NaNs to zero for PLEXOS to read them in correctly
load.part.fact.table <- Load.table[, .(BusNumber,ActivePower.MW)] 
# remove negative LPFs
if (any(load.part.fact.table[,ActivePower.MW < 0])) {
  message("Removing negative load participation factors... hope that is OK")
  load.part.fact.table[ActivePower.MW < 0, ActivePower.MW := 0] 
}
# if there are multiple LPFs for a given node, sum those
if (any(load.part.fact.table[,length(ActivePower.MW) > 1, by = "BusNumber"][,V1]
  )) {
  message(
  "Summing multiple load participation factors at same node... hope that is OK")
  load.part.fact.table <- 
  load.part.fact.table[,list(ActivePower.MW = sum(ActivePower.MW)), 
                       by = "BusNumber"]
}

# merge with nodes table
load.part.fact.table <- 
  merge(load.part.fact.table, 
        node.data.table[, .(BusNumber, BusName, RegionName)], 
        by = "BusNumber", all.y = TRUE)
load.part.fact.table[is.na(ActivePower.MW), ActivePower.MW := 0] 

load.part.fact.table[, LPF := prop.table(ActivePower.MW), by = "RegionName"]
load.part.fact.table[is.nan(LPF), LPF := 0] 

# add LPFs to properties .sheet
lpf.to.node.properties <- 
  load.part.fact.table[,.(BusName, `Load Participation Factor` = LPF)]

add_to_properties_sheet(lpf.to.node.properties, object.class = 'Node', 
                        names.col = 'BusName', collection.name = 'Nodes')

# clean up
rm(load.to.region.map, load.file.to.object, load.to.region.properties, 
  load.part.fact.table, lpf.to.node.properties, load.scens, cur.tab, 
  load.scens.to.objects)

#------------------------------------------------------------------------------|
# add RE generators ----
#------------------------------------------------------------------------------|
# these will be added to generator.data.table
if (add.RE.gens){
  # read in information about RE gens
  message(sprintf("... Adding properties for RE gens from  %s", RE.gen.file))
  
  RE.gens <- 
    fread(file.path(inputfiles.dir, RE.gen.file), colClasses = 'numeric')
  
  # 1. create nodes to put new RE on
  new.node.table <- unique(RE.gens, by = c('Node.Name', 'Category'))
  
  # add RE nodes to objects .sheet
  RE.nodes.to.objects <- 
    initialize_table(Objects.prototype, nrow(new.node.table), 
                     list(class = "Node", name = new.node.table[,Node.Name], 
                          category  = new.node.table[,Node.Region]))
  
  Objects.sheet <- merge_sheet_w_table(Objects.sheet, RE.nodes.to.objects)
  
  # add RE nodes to properties .sheet
  RE.nodes.to.properties <- new.node.table[,.(Node.Name, Voltage = Node.kV, 
                                              Units = 1)]
  
  add_to_properties_sheet(RE.nodes.to.properties, names.col = 'Node.Name', 
                          collection.name = 'Nodes', object.class = 'Node')
  
  # add RE node-region and node-zone membership to memberships .sheet
  RE.nodes.to.memberships.regions <- 
    initialize_table(Memberships.prototype, nrow(new.node.table), 
                     list(parent_class = "Node", 
                          parent_object = new.node.table[,Node.Name],
                          collection = "Region", child_class = "Region", 
                          child_object = new.node.table[, Node.Region]))
  
  Memberships.sheet <- merge_sheet_w_table(Memberships.sheet, 
                                           RE.nodes.to.memberships.regions)
  
  RE.nodes.to.memberships.zones <- 
    initialize_table(Memberships.prototype, nrow(new.node.table), 
                     list(parent_class = "Node", 
                          parent_object = new.node.table[,Node.Name], 
                          collection = "Zone", child_class = "Zone", 
                          child_object = new.node.table[,Node.Zone]))
  
  Memberships.sheet <- merge_sheet_w_table(Memberships.sheet, 
                                           RE.nodes.to.memberships.zones)
  
  # 2. create new lines with no congestion to connect new nodes to existing nodes
  RE.line.table <- new.node.table[,.(Node.From = Node.Name, 
                                     Node.To = Node.To.Connect, kV = Node.kV)]
  RE.line.table[, To.Node.Number := tstrsplit(Node.To, "_")[1]]
  RE.line.table[, Line.Name:= paste0(Node.From, "_", To.Node.Number, "_1_CKT")]
  RE.line.table[, Region := RE.gens[,Node.Region]]
  
  # add lines to objects .sheet 
  RE.lines.to.objects <- 
    initialize_table(Objects.prototype, nrow(RE.line.table), 
                     list(name = RE.line.table[, Line.Name], 
                          class = "Line", category = RE.line.table[,Region]))
  
  Objects.sheet <- merge_sheet_w_table(Objects.sheet, RE.lines.to.objects)
  
  # add new lines to properties .sheet
  RE.lines.to.properties <-
    RE.line.table[,.(Line.Name, Units = 1, `Max Flow` = 9999, 
                     `Min Flow` = -9999)]
  
  add_to_properties_sheet(RE.lines.to.properties, names.col = 'Line.Name', 
                          collection.name = 'Lines', object.class = 'Line')
  
  #  add RE Node To/Node From lines to memberships
  RE.lines.to.memberships.from <- 
    initialize_table(Memberships.prototype,nrow(new.node.table), 
                     list(parent_class = "Line", collection = "Node From", 
                          child_class = "Node"))
  RE.lines.to.memberships.from[, parent_object := RE.line.table[,Line.Name]]
  RE.lines.to.memberships.from[, child_object := RE.line.table[,Node.From]]
  
  RE.lines.to.memberships.to <- 
    initialize_table(Memberships.prototype, nrow(new.node.table), 
                     list(parent_class = "Line", collection = "Node To",
                          child_class = "Node"))
  RE.lines.to.memberships.to[, parent_object := RE.line.table[,Line.Name]]
  RE.lines.to.memberships.to[, child_object := RE.line.table[,Node.To]]
  
  Memberships.sheet <- merge_sheet_w_table(Memberships.sheet, 
                                           RE.lines.to.memberships.from)
  Memberships.sheet <- merge_sheet_w_table(Memberships.sheet, 
                                           RE.lines.to.memberships.to)
  
  # 3. add data file objects for rating of these gens
  rating.data.files.to.objects <- 
    initialize_table(Objects.prototype, nrow(RE.gens), 
                     list(class = "Data File", 
                          category = RE.gens[, paste0(Category, " Rating")]))
  rating.data.files.to.objects[,name := 
                                 paste(RE.gens[,gsub('GEN_','',Generator.Name)], 
                                       "Rating File Object")]
  
  Objects.sheet <- merge_sheet_w_table(Objects.sheet, 
                                       rating.data.files.to.objects)
  
  # add data files to properties (attach filepath to data file objects)
  rating.data.files.to.properties <- 
    initialize_table(Properties.prototype, nrow(RE.gens), 
                     list(parent_class = "System", parent_object = "System", 
                          collection = "Data Files", child_class = "Data File", 
                          property = "filename", band_id = 1, value = 0))
  rating.data.files.to.properties[,child_object := 
                                    paste(RE.gens[,gsub('GEN_','',Generator.Name)], 
                                          "Rating File Object")]
  rating.data.files.to.properties[, filename := RE.gens[,Data.File]]
  
  Properties.sheet <- merge_sheet_w_table(Properties.sheet, 
                                          rating.data.files.to.properties)
  
  # 4. (finally) add in RE gens
  # add RE gens to objects
  RE.gens.to.objects <- 
    initialize_table(Objects.prototype, nrow(RE.gens), 
                     list(class = "Generator", name = RE.gens[,Generator.Name], 
                          category = RE.gens[,paste(Category, Fuel)]))
  
  Objects.sheet <- merge_sheet_w_table(Objects.sheet, RE.gens.to.objects)
  
  # add RE gens to properties .sheet (Units, Max Capacity)
  RE.gens.to.properties <- RE.gens[,.(Generator.Name, Units = 1, 
                                      `Max Capacity` = Max.Capacity)]
  
  add_to_properties_sheet(RE.gens.to.properties, object.class = 'Generator', 
                          names.col = 'Generator.Name', 
                          collection.name = 'Generators')
  
  # add RE gens to properties .sheet (Rating and associated datafile)
  RE.gens.to.properties.rating <- 
    RE.gens[,.(Generator.Name, Rating = '0', 
               ratingfile = paste0("{Object}",
                                   gsub('GEN_','', Generator.Name), 
                                   " Rating File Object"))]
  
  add_to_properties_sheet(RE.gens.to.properties.rating, 
                          object.class = 'Generator', 
                          names.col = 'Generator.Name', 
                          collection.name = 'Generators', 
                          datafile.col = 'ratingfile')
  
  # add RE gen-fuel to memberships (connecting gens to fuel and nodes)
  RE.gens.to.memberships.nodes <- 
    initialize_table(Memberships.prototype, 
                     nrow(RE.gens), list(parent_class = "Generator", 
                                         collection = "Nodes", 
                                         child_class = "Node"))
  RE.gens.to.memberships.nodes[, parent_object := RE.gens[,Generator.Name]]
  RE.gens.to.memberships.nodes[, child_object := RE.gens[,Node.Name]]
  
  RE.gens.to.memberships.fuel <- 
    initialize_table(Memberships.prototype, 
                     nrow(RE.gens), list(parent_class = "Generator", 
                                         collection = "Fuels", 
                                         child_class = "Fuel"))
  RE.gens.to.memberships.fuel[, parent_object := RE.gens[,Generator.Name]]
  RE.gens.to.memberships.fuel[, child_object := RE.gens[,Fuel]]
  
  Memberships.sheet <- merge_sheet_w_table(Memberships.sheet, 
                                           RE.gens.to.memberships.nodes)
  Memberships.sheet <- merge_sheet_w_table(Memberships.sheet, 
                                           RE.gens.to.memberships.fuel)
  
  # add new nodes, gens, and lines to *.data.tables so can be accessed later
  # need to put each in right format before merging
  
  # nodes
  new.RE.nodes.data <- new.node.table[,.(BusName = Node.Name, 
                                         Voltage.kV = Node.kV, 
                                         RegionName = Node.Region, 
                                         ZoneName = Node.Zone)]
  
  node.data.table <- rbind(node.data.table, new.RE.nodes.data, fill = TRUE)
  
  # lines - pull table in right format, then add zones
  # doesn't fill in RatingB, ID, Status, ToKV, FromKV, category, ACorDC. should?
  # doesn't have resistance
  new.RE.lines.data <- RE.line.table[,.(name = Line.Name, ToRegion = Region, 
                                        FromRegion = Region, 
                                        BranchToBus = To.Node.Number, 
                                        FromBusName = Node.From, 
                                        ToBusName = Node.To)]
  new.RE.lines.data <- merge(new.RE.lines.data, 
                             new.RE.nodes.data[,.(BusName, ZoneName)], 
                             by.x = 'FromBusName', by.y = 'BusName', 
                             all.x = TRUE)
  new.RE.lines.data[,c('ToZone', 'FromZone') := ZoneName][,ZoneName := NULL]
  
  line.data.table <- rbind(line.data.table, new.RE.lines.data, fill = TRUE)
  
  # generators - can also add bus number, ID, an implied min cap of 0
  new.RE.gens.data <- RE.gens[,.(Generator.Name, BusName = Node.Name, 
                                 RegionName = Node.Region, ZoneName = Node.Zone, 
                                 Voltage.kV = Node.kV, 
                                 Fuel, MaxOutput.MW = Max.Capacity)]
  generator.data.table <- rbind(generator.data.table, new.RE.gens.data, 
                                fill = T)
  
  # clean up
  rm(RE.gens, new.node.table, RE.nodes.to.objects, RE.nodes.to.properties, 
     RE.nodes.to.memberships.regions,RE.nodes.to.memberships.zones,RE.line.table, 
     RE.lines.to.objects, RE.lines.to.properties, RE.lines.to.memberships.from, 
     RE.lines.to.memberships.to, rating.data.files.to.objects, 
     rating.data.files.to.properties, RE.gens.to.objects, RE.gens.to.properties, 
     RE.gens.to.properties.rating, new.RE.nodes.data, new.RE.lines.data, 
     new.RE.gens.data)
}else {
  message('... no RE gen info to be added... skipping')
}

#------------------------------------------------------------------------------|
# generator properties ----
#------------------------------------------------------------------------------|

# uses generator.property.by.fuel.list

for (elem in seq_along(generator.property.by.fuel.list)) {
  if (file.exists(file.path(inputfiles.dir,
                            generator.property.by.fuel.list[[elem]][1]))) {
    message(sprintf("... Adding properties from %s", 
                    generator.property.by.fuel.list[[elem]][1]))
    # read in table
    cur.table <- fread(file.path(inputfiles.dir,
                                 generator.property.by.fuel.list[[elem]][1]))
    
    # set up arguments for merge_property_by_fuel
    cur.map.fuel.args <- generator.property.by.fuel.list[[elem]][[2]]
    cur.map.fuel.args$input.table <- cur.table
    
    # merge properties fuel, produces table with list of generators in rows
    # and their properties in all other columns
    mapped.by.fuel <- do.call(merge_property_by_fuel, cur.map.fuel.args)
    
    # set up arguments for add_to_properties_sheet, using output of merge by fuel  
    cur.prop.sheet.args <- generator.property.by.fuel.list[[elem]][[3]]
    cur.prop.sheet.args$input.table <- mapped.by.fuel
    cur.prop.sheet.args$names.col <- 'Generator.Name'
    
    # add to properties sheet using input arguments and new table
    do.call(add_to_properties_sheet, cur.prop.sheet.args)
  } else {
    message(sprintf("... %s does not exist ... skipping", 
                    generator.property.by.fuel.list[[elem]][1]))
  }
}

# clean up
rm(cur.table, cur.map.fuel.args, cur.prop.sheet.args, mapped.by.fuel, elem)


# #----------------------------------------------------------------------------|
# # add generic generator properties (in scenarios)----
# #----------------------------------------------------------------------------|
# # uses generator.property.file.list
# 
# # reads from the list of vectors describing the hydro limit data. For each, 
# # first reads in the table, then submits it to add_to_properties sheet
# for (i in seq(generator.property.file.list)) {
#   
#   # read in file
#   cur.gen.file <- fread(file.path(inputfiles.dir, 
#     generator.property.file.list[[i]]['fl'])) 
# 
#   # add to properties sheet
#   add_to_properties_sheet(cur.gen.file, 
#     object.class = 'Generator',
#     collection.name = 'Generators',
#     scenario.name = generator.property.file.list[[i]]['scen'],
#     names.col = generator.property.file.list[[i]]['name'], 
#     pattern.col = generator.property.file.list[[i]]['ptrn'],
#     period.id = generator.property.file.list[[i]]['periodid']) 
# }
# 
# # clean up
# rm(cur.gen.file)

for (elem in seq_along(object.property.list)) {
  if (file.exists(file.path(inputfiles.dir,object.property.list[[elem]][1]))) {
    message(sprintf("... Adding properties from %s", 
                    object.property.list[[elem]][1]))
    # read in table
    cur.table <- fread(file.path(inputfiles.dir, 
                                 object.property.list[[elem]][1]))
    
    # read in args
    cur.args <- object.property.list[[elem]][[2]]
    cur.args$input.table <- cur.table
    
    # add to properties sheet using input arguments and new table
    do.call(add_to_properties_sheet, cur.args)
  } else {
    message(sprintf("... %s does not exist ... skipping", 
                    object.property.list[[elem]][1]))
  }
}

rm(elem, cur.table, cur.args)


#------------------------------------------------------------------------------|
# add start cost ----
#------------------------------------------------------------------------------|
if (file.exists(file.path(inputfiles.dir, start.cost.file))) {
  message(sprintf("... Adding start costs from %s", start.cost.file))
  #uses start.cost.file, fuels.to.gens
  start.cost <- fread(file.path(inputfiles.dir, start.cost.file))
  # the start cost file has cost by size for coal. this seperates those for 
  # merging, then binds it back together
  start.cost.na = start.cost[is.na(MaxOutput.MW), .(Fuel, `Start Cost`)]
  start.cost.thermal = start.cost[!is.na(MaxOutput.MW), .(Fuel, `Start Cost`, 
                                                          MaxOutput.MW)]
  start.cost.na <- merge(
    start.cost.na, 
    generator.data.table[,.(Generator.Name, Fuel, MaxOutput.MW)], 
    by = "Fuel", all.x = TRUE)
  
  generator.data.table[, MaxOutput.MW.group := ifelse(
    MaxOutput.MW <= 210, 210, ifelse(MaxOutput.MW <= 500, 500, 610)), 
    by=Generator.Name]
  
  setnames(start.cost.thermal, "MaxOutput.MW", "MaxOutput.MW.group")
  
  start.cost.thermal = merge(
    start.cost.thermal, 
    generator.data.table[,.(Generator.Name, Fuel, 
                            MaxOutput.MW.group, MaxOutput.MW)], 
    by = c("MaxOutput.MW.group", "Fuel"), all.x = TRUE)
  #not all combinations of cost and fuel.type exist, remove non-complete ones
  start.cost.thermal = start.cost.thermal[complete.cases(start.cost.thermal)]
  
  start.cost.thermal = select(start.cost.thermal, -MaxOutput.MW.group)
  
  start.cost = rbind(start.cost.thermal, start.cost.na)
  
  start.cost.to.properties <- initialize_table(
    Properties.prototype, nrow(start.cost), list(
      parent_class = "System", child_class = "Generator", 
      collection = "Generators", parent_object = "System", band_id = 1, 
      property = "Start Cost"))
  start.cost.to.properties[,child_object := start.cost[,Generator.Name]]
  start.cost.to.properties[,value := start.cost[,`Start Cost` * MaxOutput.MW]]
  
  Properties.sheet <- merge_sheet_w_table(Properties.sheet, 
                                          start.cost.to.properties)
  
} else {
  message(sprintf("... %s does not exist ... skipping start costs", 
                  start.cost.file))
}



#------------------------------------------------------------------------------|
# add interfaces from interface file list ----
#------------------------------------------------------------------------------|


# uses interfaces.files.list

for (i in seq(interfaces.files.list)) {
  if (file.exists(file.path(inputfiles.dir, interfaces.files.list[[i]][1]))) {
    message(
      sprintf("... Adding interfaces from %s", interfaces.files.list[[i]][1]))
    # read in files from interface files in this iteration
    interface.names <- fread(
      file.path(inputfiles.dir, interfaces.files.list[[i]]['names']))
    interface.properties <- fread(
      file.path(inputfiles.dir, interfaces.files.list[[i]]['properties']))
    interface.memberships <- fread(
      file.path(inputfiles.dir, interfaces.files.list[[i]]['memberships']))
    interface.coefficients <- fread(
      file.path(inputfiles.dir, interfaces.files.list[[i]]['flowcoefs']))
    
    # Add interfaces to objects sheet
    interfaces.to.objects <- initialize_table(
      Objects.sheet, nrow(interface.names), 
      list(class = "Interface", name = interface.names[,Interface.Name], 
           category = interface.names[,category]))
    
    Objects.sheet <- merge_sheet_w_table(Objects.sheet, interfaces.to.objects)
    
    # Add interface properties - changed to data.file. need to genericize, 
    # change to data file object so can put in 2014 and 2022 data files, etc 
    # add_to_properties_sheet(interface.properties, object.class = "Interface", 
    #   collection.name = "Interfaces", names.col = "Interface.Name")
    
    # add max flow data files
    
    interface.to.properties <- initialize_table(
      Properties.prototype, nrow(interface.names), list(
        parent_class = 'System', parent_object = 'System', 
        collection = 'Interfaces', child_class = 'Interface', band_id = 1))
    
    interface.to.properties[,property := 'Max Flow']
    interface.to.properties[,value := '0']
    interface.to.properties[,child_object := 
                              interface.properties[,Interface.Name]]
    interface.to.properties[,filename := interface.properties[,MaxFlow_datafile]]
    
    Properties.sheet <- merge_sheet_w_table(Properties.sheet, 
                                            interface.to.properties)
    
    # add min flow data files
    
    interface.to.properties <- initialize_table(
      Properties.prototype, nrow(interface.names), list(
        parent_class = 'System', parent_object = 'System', 
        collection = 'Interfaces', child_class = 'Interface', band_id = 1))
    
    interface.to.properties[,property := 'Min Flow']
    interface.to.properties[,value := '0']
    interface.to.properties[,child_object := 
                              interface.properties[,Interface.Name]]
    interface.to.properties[,filename := interface.properties[,MinFlow_datafile]]
    
    Properties.sheet <- merge_sheet_w_table(Properties.sheet, 
                                            interface.to.properties)
    
    # Add interface-line memberships
    interface.to.memberships <- initialize_table(
      Memberships.prototype, nrow(interface.memberships), list(
        parent_class = "Interface", child_class = "Line", collection = "Lines"))
    
    interface.to.memberships[,parent_object := 
                               interface.memberships[,Interface.Name]]
    interface.to.memberships[,child_object := interface.memberships[,Line.Name]]
    
    Memberships.sheet <- merge_sheet_w_table(Memberships.sheet, 
                                             interface.to.memberships)
    
    # Add flow coeffcienct to properties
    
    interface.coefficients.to.props <- initialize_table(
      Properties.prototype, nrow(interface.coefficients), list(
        parent_class = "Interface", child_class = "Line", collection = "Lines", 
        band_id = 1, property = "Flow Coefficient", value = -1))
    interface.coefficients.to.props[, parent_object := 
                                      interface.coefficients[, Interface.Name]]
    interface.coefficients.to.props[, child_object := 
                                      interface.coefficients[, Line.Name]]
    
    Properties.sheet <- merge_sheet_w_table(Properties.sheet, 
                                            interface.coefficients.to.props)
    
  } else {
    message(sprintf("... %s does not exist ... skipping", 
                    interfaces.files.list[[i]][1]))
  }
}