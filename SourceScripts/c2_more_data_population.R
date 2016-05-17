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
# RE.gen.file.list
# interfaces.files.list
# generator.property.by.fuel.list
# turn.off.except.in.scen.list

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

if (any(is.na(fuels.to.gens.to.memberships$child_object))) {
  warning ('There are generators without fuel definitions, deleting generatror-fuel membership entries')
  fuels.to.gens.to.memberships=fuels.to.gens.to.memberships[!is.na(child_object),]
}

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
                          object.class = 'Data File', 
                          collection.name = 'Data Files', 
                          datafile.col = name, 
                          scenario.name = ifelse(tolower(name) == 'base', 
                                                 NA, name))
}

# add any scenarios associated with load as objects
load.scens <- load.scens[tolower(load.scens) != 'base']
load.scens.to.objects <- 
  initialize_table(Objects.prototype, length(load.scens), 
                   list(name = load.scens, class = "Scenario",
                        category = "Load scenarios"))

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
if (add.RE.gens & exists("RE.gen.file.list")){
  
  for (item in RE.gen.file.list) {
      
    fname = item[1]
    scenname = item["scenario"]
    make.new.nodes = item["make.new.nodes"]

    if (file.exists(file.path(inputfiles.dir, fname))) {
      
    # read in information about RE gens
    message(sprintf("... Adding properties for RE gens from %s", fname))
    
    RE.gens <- 
      fread(file.path(inputfiles.dir, fname), colClasses = 'numeric')
    
    # create scenario if input file sceanrio is indicated in input_params*.R
    if(!is.na(scenname)){
      
      RE.gens[,Num.Units.Scn := Num.Units]
      RE.gens[,Num.Units := 0]
      
      if(!(scenname %in% Objects.sheet[,name])){
        # Add scenario to objects .sheet
        Scenario.to.objects <- 
          initialize_table(Objects.prototype, length(item[2]), 
                           list(name = scenname, 
                                class = "Scenario", category = "Generator status"))
        
        Objects.sheet <- merge_sheet_w_table(Objects.sheet, Scenario.to.objects)
      }
    }
  
    if(!is.na(make.new.nodes)){ 
        # 1. create nodes to put new RE on
        new.node.table <- unique(RE.gens, by = c('Node.Name', 'Category'))
        
        # add new RE nodes to objects .sheet
        RE.nodes.to.objects <- 
          initialize_table(Objects.prototype, nrow(new.node.table), 
                           list(class = "Node", name = new.node.table[,Node.Name], 
                                category  = new.node.table[,Node.Region]))
        
        Objects.sheet <- merge_sheet_w_table(Objects.sheet, RE.nodes.to.objects)
      
        # add new RE nodes to properties .sheet
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
        RE.line.table[, Region := new.node.table[,Node.Region]]
        
        # add lines to objects .sheet 
        RE.lines.to.objects <- 
          initialize_table(Objects.prototype, nrow(RE.line.table), 
                           list(name = RE.line.table[, Line.Name], 
                                class = "Line", category = RE.line.table[,Region]))
        
        Objects.sheet <- merge_sheet_w_table(Objects.sheet, RE.lines.to.objects)
        
        # add new lines to properties .sheet
        RE.lines.to.properties <-
          RE.line.table[,.(Line.Name, Units = 1, `Max Flow` = 99999, 
                           `Min Flow` = -99999)]
        
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
        
        # Reassigned Node.To.Connect with Node.Name if new nodes created
        RE.gens[,Node.To.Connect := Node.Name] 
        
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
      }
    
    # 4. (finally) add in RE gens
    # add RE gens to objects
    RE.gens.to.objects <- 
      initialize_table(Objects.prototype, nrow(RE.gens), 
                       list(class = "Generator", name = RE.gens[,Generator.Name], 
                            category = RE.gens[,Category]))
    
    Objects.sheet <- merge_sheet_w_table(Objects.sheet, RE.gens.to.objects)
    
    # add RE gens to properties .sheet (Units, Max Capacity)
    RE.gens.to.properties <- RE.gens[,.(Generator.Name, Units = Num.Units,
                                        `Max Capacity` = Max.Capacity)]
    
    add_to_properties_sheet(RE.gens.to.properties, object.class = 'Generator', 
                            names.col = 'Generator.Name', 
                            collection.name = 'Generators')
    
    # add RE gens to properties .sheet (Units) -- Scenario
    if(!is.na(scenname)){
      RE.gens.to.properties <- RE.gens[,.(Generator.Name, 
                                          Units = Num.Units.Scn)] 
      
      add_to_properties_sheet(RE.gens.to.properties, object.class = 'Generator', 
                              names.col = 'Generator.Name', 
                              collection.name = 'Generators',
                              scenario.name = scenname)
      
      RE.gens[,Num.Units := Num.Units.Scn]
    }
  
    # add RE gens to properties .sheet (Rating and associated datafile)
    RE.gens.to.properties.rating <- RE.gens[,.(Generator.Name, Rating = '0', 
                                                Data.File)]
    
    add_to_properties_sheet(RE.gens.to.properties.rating, 
                            object.class = 'Generator', 
                            names.col = 'Generator.Name', 
                            collection.name = 'Generators', 
                            datafile.col = 'Data.File')
    
    # add RE gen-fuel to memberships (connecting gens to fuel and nodes)
    RE.gens.to.memberships.nodes <- 
      initialize_table(Memberships.prototype, 
                       nrow(RE.gens), list(parent_class = "Generator", 
                                           collection = "Nodes", 
                                           child_class = "Node"))
    RE.gens.to.memberships.nodes[, parent_object := RE.gens[,Generator.Name]]
    RE.gens.to.memberships.nodes[, child_object := RE.gens[,Node.To.Connect]]
    
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
    

    
    # generators - can also add bus number, ID, an implied min cap of 0
    # must pull node's region and zone from node.data.table, in case new nodes
    # didn't get added
    new.RE.gens.data <- RE.gens[,.(Generator.Name, BusName = Node.To.Connect, 
                                   Fuel, MaxOutput.MW = Max.Capacity, 
                                   Units = Num.Units)]
    
    new.RE.gens.data <- merge(new.RE.gens.data, node.data.table, by = "BusName", 
                              all.x = T)

    generator.data.table <- rbind(generator.data.table, new.RE.gens.data, 
                                  fill = T)
    
    # clean up
    suppressWarnings({
      rm(RE.gens, new.node.table, RE.nodes.to.objects, RE.nodes.to.properties,
       RE.nodes.to.memberships.regions,RE.nodes.to.memberships.zones,RE.line.table,
       RE.lines.to.objects, RE.lines.to.properties, RE.lines.to.memberships.from,
       rating.data.files.to.properties, RE.gens.to.objects, RE.gens.to.properties,
       RE.gens.to.properties.rating, new.RE.nodes.data, new.RE.lines.data,
       new.RE.gens.data, node.info)})
  } else {
       message(sprintf("... %s does not exist ... skipping", fname))
    }
  }
  
} else {
  message('... no RE gen info to be added... skipping')
}

#------------------------------------------------------------------------------|
# generator properties by fuel----
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
    
    if ('scenario.name' %in% names(cur.prop.sheet.args)) {
        # for now, just add any scenario here that doesn't already exist
        # need to deal with categories later
        cur.scen <- cur.prop.sheet.args[['scenario.name']]
        if (!(cur.scen %in% Objects.sheet[,name])) {
            cur.scen.to.objects <- initialize_table(Objects.sheet, 1, 
                list(name = cur.scen, category = 'Object properties',
                    class = 'Scenario'))
              
            Objects.sheet <- merge_sheet_w_table(Objects.sheet, cur.scen.to.objects)
         
            # clean up
            rm(cur.scen.to.objects)
             
         }
      
        # clean up
        rm(cur.scen)
    }
    
  } else {
    message(sprintf("... %s does not exist ... skipping", 
                    generator.property.by.fuel.list[[elem]][1]))
  }
}

# clean up
rm(cur.table, cur.map.fuel.args, cur.prop.sheet.args, mapped.by.fuel, elem)

#----------------------------------------------------------------------------|
# add object properties by object ----
#----------------------------------------------------------------------------|
# uses generator.property.file.list

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
    
    # for now, just add any scenario here that doesn't already exist
    # need to deal with categories later
    if ('scenario.name' %in% names(cur.args)) { 
      cur.scen <- cur.args['scenario.name']
      if (!(cur.scen %in% Objects.sheet[,name])) {
        
        cur.scen.to.objects <- initialize_table(Objects.sheet, 1, 
          list(name = cur.scen, category = 'Object properties',
            class = 'Scenario'))
        
        Objects.sheet <- merge_sheet_w_table(Objects.sheet, cur.scen.to.objects)
      }
      
        # clean up
        # rm(cur.scen, cur.scen.to.objects)
    }
    
  } else {
    message(sprintf("... %s does not exist ... skipping", 
                    object.property.list[[elem]][1]))
  }
}


#------------------------------------------------------------------------------|
# turn off objects except in scenario ----
#------------------------------------------------------------------------------|
# uses turn.off.except.in.scen.list
if (exists('turn.off.except.in.scen.list')) {
  
  for (elem in seq_along(turn.off.except.in.scen.list)) {
    
    if (file.exists(file.path(inputfiles.dir,
                              turn.off.except.in.scen.list[[elem]][1]))) {
      message(sprintf(paste0("... Adding turning off objects from %s except",
                             " for in scenario '%s'"), 
                      turn.off.except.in.scen.list[[elem]][1],
                      turn.off.except.in.scen.list[[elem]][['scenario.name']]))
      
      # read in table
      cur.table <- fread(file.path(inputfiles.dir, 
                                   turn.off.except.in.scen.list[[elem]][1]))
      
      # turn off Units property of these objects
      cur.names <- turn.off.except.in.scen.list[[elem]][['names.col']]
      cur.class <- turn.off.except.in.scen.list[[elem]][['object.class']]
      cur.coll <- turn.off.except.in.scen.list[[elem]][['collection.name']]
      cur.scen <- turn.off.except.in.scen.list[[elem]][['scenario.name']]
      
      # turn off Units in bae
      cur.table[,Units := 0]
      
      add_to_properties_sheet(cur.table, names.col = cur.names, 
                              object.class = cur.class,
                              collection.name = cur.coll, overwrite = T)
      
      # turn on units in scenario. if a generator, pull units from 
      # generator.data.table
      # right now, code only supports maintaining multiple units for generators
      if (cur.class == 'Generator') {
        
        genunits <- generator.data.table[, .(Generator.Name, Units)]
        cur.table <- merge(cur.table[,Units := NULL], genunits, 
                           by.x = cur.names, by.y = 'Generator.Name')
        
      } else cur.table[,Units := 1]
      
      add_to_properties_sheet(cur.table, names.col = cur.names, 
                              object.class = cur.class, 
                              collection.name = cur.coll, 
                              scenario.name = cur.scen)
      
      # add scenario as an object
      cur.scen.to.obj <- 
        initialize_table(Objects.prototype, 1,list(class= 'Scenario', 
                                             name = cur.scen, 
                                             category = 'Generator status'))
      
      Objects.sheet <- merge_sheet_w_table(Objects.sheet, cur.scen.to.obj)
      
      # clean up
      rm(elem, cur.names, cur.class, cur.coll, cur.scen, cur.scen.to.obj)
      
    } else {
      message(sprintf("... %s does not exist ... skipping", 
                      turn.off.except.in.scen.list[[elem]][1]))
    }
  }
} else {
  message('... turn.off.except.in.scen.list does not exist ... skipping')
}

#------------------------------------------------------------------------------|
# add interfaces from interface file list ----
#------------------------------------------------------------------------------|


# uses interfaces.files.list
if(exists('interfaces.files.list')) {
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
      interface.to.properties[,filename := 
                                interface.properties[,MaxFlow_datafile]]
      
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
      interface.to.properties[,filename := 
                                interface.properties[,MinFlow_datafile]]
      
      Properties.sheet <- merge_sheet_w_table(Properties.sheet, 
                                              interface.to.properties)
      
      # Add interface-line memberships
      interface.to.memberships <- initialize_table(
        Memberships.prototype, nrow(interface.memberships), list(
          parent_class = "Interface", 
          child_class = "Line", collection = "Lines"))
      
      interface.to.memberships[,parent_object := 
                                 interface.memberships[,Interface.Name]]
      interface.to.memberships[,child_object := 
                                 interface.memberships[,Line.Name]]
      
      Memberships.sheet <- merge_sheet_w_table(Memberships.sheet, 
                                               interface.to.memberships)
      
      # Add flow coeffcienct to properties
      
      interface.coefficients.to.props <- initialize_table(
        Properties.prototype, nrow(interface.coefficients), list(
          parent_class = "Interface", 
          child_class = "Line", collection = "Lines", 
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
} else {
  message('... no interface files defined ... skipping')
}


