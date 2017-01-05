#uses (from master):
# TODO edit this

#------------------------------------------------------------------------------|
# add fuels and categorize generators by fuel ----
#------------------------------------------------------------------------------|

if (exists("map.gen.to.fuel.file")) {
    fuel.table <- fread(file.path(inputfiles.dir, map.gen.to.fuel.file))
    
    # add fuels to generator.data.table
    generator.data.table <- 
        merge(generator.data.table, fuel.table[, .(Generator, Fuel)], 
              by = "Generator", all.x = TRUE)
    
    # add fuels to objects .sheet
    all.fuels <- unique(fuel.table[, Fuel])
    
    fuels.to.objects <- initialize_table(Objects.sheet, length(all.fuels), 
                                         list(class = "Fuel"))
    fuels.to.objects[, name := all.fuels]
    
    Objects.sheet <- merge_sheet_w_table(Objects.sheet, fuels.to.objects)
    
    # add generator-fuels membership to memberships
    fuels.to.gens.to.memberships <- 
        initialize_table(Memberships.sheet, nrow(generator.data.table), 
                         list(parent_class = "Generator", child_class = "Fuel", 
                              collection = "Fuels"))
    
    fuels.to.gens.to.memberships[,parent_object := 
                                     generator.data.table[,Generator]]
    fuels.to.gens.to.memberships[,child_object := generator.data.table[,Fuel]]
    
    if (any(is.na(fuels.to.gens.to.memberships$child_object))) {
        warning (paste0('There are generators without fuel definitions, ",
                        "deleting generatror-fuel membership entries'))
        fuels.to.gens.to.memberships=fuels.to.gens.to.memberships[!is.na(child_object),]
    }
    
    Memberships.sheet <- merge_sheet_w_table(Memberships.sheet, 
                                             fuels.to.gens.to.memberships)
    
    # edit generators to categorize by fuel instead of region
    Objects.sheet <- 
        merge(Objects.sheet, 
              generator.data.table[, .(Generator, Fuel, class = 'Generator')], 
              by.x = c('class', 'name'), by.y = c('class', 'Generator'), 
              all.x = T)
    Objects.sheet[!is.na(Fuel),category := Fuel]
    Objects.sheet[,Fuel := NULL]
    
    # clean up
    rm(fuel.table, all.fuels, fuels.to.objects, fuels.to.gens.to.memberships)
} else {
    
    message(">>  map.gen.to.fuel.file does not exist ... skipping")
}
    
#------------------------------------------------------------------------------|
# add load (mapped by region by external file) and lpf ----
#------------------------------------------------------------------------------|

if (exists("map.region.to.load.file")) {
    
    load.to.region.map <- 
        fread(file.path(inputfiles.dir, map.region.to.load.file))
    
    # create data file object name column
    load.to.region.map[,DataFile := paste0(load.to.region.map[,Region], 
                                           " Load File Object")]
    
    # add load data file objects to objects .sheet 
    # uses load.to.region.map
    load.file.to.object <- 
        initialize_table(Objects.sheet, nrow(load.to.region.map), 
                         list(class = "Data File", category = "Regional Load"))
    load.file.to.object[, name := load.to.region.map[,DataFile]]
    
    Objects.sheet <- merge_sheet_w_table(Objects.sheet, load.file.to.object)
    
    # load file object to as regional load
    # uses load.to.region.map
    load.to.region.properties <- 
        initialize_table(Properties.sheet, nrow(load.to.region.map), 
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
        
        setnames(cur.tab, name, "filename")
        
        add_to_properties_sheet(cur.tab, names.col = 'DataFile', 
                                object.class = 'Data File', 
                                collection.name = 'Data Files', 
                                datafile.col = "filename", 
                                scenario.name = ifelse(tolower(name) == 'base', 
                                                       NA, name))
    }
    
    setnames(cur.tab, "filename_datafile", name) #hacky. fix this later
    
    # add any scenarios associated with load as objects
    load.scens <- load.scens[tolower(load.scens) != 'base']
    load.scens.to.objects <- 
        initialize_table(Objects.sheet, length(load.scens), 
                         list(name = load.scens, class = "Scenario",
                              category = "Load scenarios"))
    
    Objects.sheet <- merge_sheet_w_table(Objects.sheet, load.scens.to.objects)
    
    # clean up
    rm(load.to.region.map, load.file.to.object, load.to.region.properties, 
       load.scens, cur.tab, load.scens.to.objects)
    
} else {
    message(">>  map.region.to.load.file does not exist ... skipping")
}

if (exists("load.data.table")) {

    # add load participation factor to nodes
    # uses Load.table, node.data.table
    
    message('... Adding load participation factor from load.data.table')
    
    # original table has duplicates which aren't related to each other--only select
    # one. Should refine later. 
    # since there are NAs, must convert them to zero for the next step to work 
    # correctly and for PLEXOS to read them in correctly
    # remove any negative loads and replace them with zero
    # convert NaNs to zero for PLEXOS to read them in correctly
    
    load.part.fact.table <- load.data.table[, .(Node, Load, Status)] 
    
    # remove Status = 0 load
    load.part.fact.table[, Load := Load * Status]
    
    # remove negative LPFs
    if (any(load.part.fact.table[,Load < 0])) {
        message("Removing negative load participation factors... hope that is OK")
        load.part.fact.table[Load < 0, Load := 0] 
    }
    # if there are multiple LPFs for a given node, sum those
    if (any(load.part.fact.table[,length(Load) > 1, by = "Node"][,V1]
    )) {
        message(
            paste0("Summing multiple load participation factors at same node... ",
                   "hope that is OK"))
        load.part.fact.table <- 
            load.part.fact.table[,list(Load = sum(Load)), by = "Node"]
    }
    
    # merge with nodes table
    load.part.fact.table <-
        merge(load.part.fact.table, 
              node.data.table[, .(Node, Region)], 
              by = "Node", all.y = TRUE)
    load.part.fact.table[is.na(Load), Load := 0] 
    
    load.part.fact.table[, LPF := prop.table(Load), by = "Region"]
    load.part.fact.table[is.nan(LPF), LPF := 0] 
    
    # add LPFs to properties .sheet
    lpf.to.node.properties <- 
        load.part.fact.table[,.(Node, `Load Participation Factor` = LPF)]
    
    add_to_properties_sheet(lpf.to.node.properties, object.class = 'Node', 
                            names.col = 'Node', collection.name = 'Nodes')
    
    # clean up
    rm(lpf.to.node.properties, load.part.fact.table)
    
} else {
    message(">>  load.data.table does not exist ... skipping")
}
    
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
            
            # run a data check and save results to OutputFiles
            # check for missing Fuel, Number of Units, Max Capacity
            check.RE.fuel <- RE.gens[is.na(Fuel) | Fuel == "",]
            if(nrow(check.RE.fuel) > 0){ 
                warning(sprintf("At least one generator in %s is missing 'Fuel'",
                                fname))
            }
            
            check.RE.units <- RE.gens[is.na(Num.Units) | Num.Units == "",]
            if(nrow(check.RE.units) > 0){ 
                warning(sprintf("At least one generator in %s is missing 'Units'", 
                                fname))
            }
            
            check.RE.capacity <- RE.gens[is.na(Max.Capacity) | 
                                             Max.Capacity == "",]
            if(nrow(check.RE.capacity) > 0){ 
                warning(sprintf(
                    "At least one generator in %s is missing 'Max Capacity'",
                                fname))
            }
            
            # clean up
            rm(check.RE.fuel, check.RE.units, check.RE.capacity)
            
            # create scenario if input file scenario in input_params*.R
            if(!is.na(scenname)){
                
                RE.gens[,Num.Units.Scn := Num.Units]
                RE.gens[,Num.Units := 0]
                
                if(!(scenname %in% Objects.sheet[,name])){
                    # Add scenario to objects .sheet
                    Scenario.to.objects <- 
                        initialize_table(Objects.sheet, length(item[2]), 
                                         list(name = scenname, 
                                              class = "Scenario", 
                                              category = "Generator status"))
                    
                    Objects.sheet <- merge_sheet_w_table(Objects.sheet, 
                                                         Scenario.to.objects)
                }
            }
            
            if(!is.na(make.new.nodes)){ 
                # 1. create nodes to put new RE on
                new.node.table <- unique(RE.gens, by = c('Node', 'Category'))
                
                # add new RE nodes to objects .sheet
                RE.nodes.to.objects <- 
                    initialize_table(Objects.sheet, 
                                     nrow(new.node.table), 
                                     list(class = "Node", 
                                          name = new.node.table[,Node], 
                                          category  = new.node.table[,Node.Region]))
                
                Objects.sheet <- merge_sheet_w_table(Objects.sheet, 
                                                     RE.nodes.to.objects)
                
                # add new RE nodes to properties .sheet
                RE.nodes.to.properties <- new.node.table[,.(Node, 
                                                            Voltage = Node.kV, 
                                                            Units = 1)]
                
                add_to_properties_sheet(RE.nodes.to.properties, 
                                        names.col = 'Node', 
                                        collection.name = 'Nodes', 
                                        object.class = 'Node')
                
                # add RE node-region and node-zone membership to memberships .sheet
                RE.nodes.to.memberships.regions <- 
                    initialize_table(Memberships.sheet, 
                                     nrow(new.node.table), 
                                     list(parent_class = "Node", 
                                          parent_object = new.node.table[,Node],
                                          collection = "Region", 
                                          child_class = "Region", 
                                          child_object = new.node.table[, Node.Region]))
                
                Memberships.sheet <- merge_sheet_w_table(Memberships.sheet, 
                                                         RE.nodes.to.memberships.regions)
                
                RE.nodes.to.memberships.zones <- 
                    initialize_table(Memberships.sheet, 
                                     nrow(new.node.table), 
                                     list(parent_class = "Node", 
                                          parent_object = new.node.table[,Node], 
                                          collection = "Zone", child_class = "Zone", 
                                          child_object = new.node.table[,Node.Zone]))
                
                Memberships.sheet <- merge_sheet_w_table(Memberships.sheet, 
                                                         RE.nodes.to.memberships.zones)
                
                # 2. create new lines with no congestion to connect new nodes to existing nodes
                RE.line.table <- new.node.table[,.(Node.From = Node, 
                                                   Node.To = Node.To.Connect, 
                                                   kV = Node.kV)]
                RE.line.table[, To.Node.Number := tstrsplit(Node.To, "_")[1]]
                RE.line.table[, Line:= paste0(Node.From, "_", 
                                              To.Node.Number, 
                                              "_1_CKT")]
                RE.line.table[, Region := new.node.table[,Node.Region]]
                
                # add lines to objects .sheet 
                RE.lines.to.objects <- 
                    initialize_table(Objects.sheet, 
                                     nrow(RE.line.table), 
                                     list(name = RE.line.table[, Line], 
                                          class = "Line", 
                                          category = RE.line.table[,Region]))
                
                Objects.sheet <- merge_sheet_w_table(Objects.sheet, RE.lines.to.objects)
                
                # add new lines to properties .sheet
                RE.lines.to.properties <-
                    RE.line.table[,.(Line, Units = 1, `Max Flow` = 10^30, 
                                     `Min Flow` = -10^30)]
                
                add_to_properties_sheet(RE.lines.to.properties, names.col = 'Line', 
                                        collection.name = 'Lines', object.class = 'Line')
                
                #  add RE Node To/Node From lines to memberships
                RE.lines.to.memberships.from <- 
                    initialize_table(Memberships.sheet,nrow(new.node.table), 
                                     list(parent_class = "Line", collection = "Node From", 
                                          child_class = "Node"))
                RE.lines.to.memberships.from[, parent_object := RE.line.table[,Line]]
                RE.lines.to.memberships.from[, child_object := RE.line.table[,Node.From]]
                
                RE.lines.to.memberships.to <- 
                    initialize_table(Memberships.sheet, nrow(new.node.table), 
                                     list(parent_class = "Line", collection = "Node To",
                                          child_class = "Node"))
                RE.lines.to.memberships.to[, parent_object := RE.line.table[,Line]]
                RE.lines.to.memberships.to[, child_object := RE.line.table[,Node.To]]
                
                Memberships.sheet <- merge_sheet_w_table(Memberships.sheet, 
                                                         RE.lines.to.memberships.from)
                Memberships.sheet <- merge_sheet_w_table(Memberships.sheet, 
                                                         RE.lines.to.memberships.to)
                
                # Reassigned Node.To.Connect with Node if new nodes created
                RE.gens[,Node.To.Connect := Node] 
                
                # add new nodes, gens, and lines to *.data.tables so can be accessed later
                # need to put each in right format before merging
                
                # nodes
                new.RE.nodes.data <- new.node.table[,.(Node = Node.Name, 
                                                       Voltage = Node.kV, 
                                                       Region = Node.Region, 
                                                       Zone = Node.Zone)]
                
                node.data.table <- rbind(node.data.table, new.RE.nodes.data, fill = TRUE)
                
                # lines - pull table in right format, then add zones
                # doesn't fill in RatingB, ID, Status, ToKV, FromKV, category, ACorDC. should?
                # doesn't have resistance
                new.RE.lines.data <- RE.line.table[,.(Line = Line.Name, Region.To = Region, 
                                                      Region.From = Region, 
                                                      `Node From` = Node.From, 
                                                      `Node To` = Node.To)]
                new.RE.lines.data <- merge(new.RE.lines.data, 
                                           new.RE.nodes.data[,.(Node, Zone)], 
                                           by.x = 'Node From', by.y = 'Node', 
                                           all.x = TRUE)
                new.RE.lines.data[,c('Zone To', 'Zone From') := Zone][,Zone := NULL]
                
                line.data.table <- rbind(line.data.table, new.RE.lines.data, fill = TRUE)
            }
            
            # 4. (finally) add in RE gens
            # add RE gens to objects
            RE.gens.to.objects <- 
                initialize_table(Objects.sheet, nrow(RE.gens), 
                                 list(class = "Generator", name = RE.gens[,Generator.Name], 
                                      category = RE.gens[,Category]))
            
            Objects.sheet <- merge_sheet_w_table(Objects.sheet, RE.gens.to.objects)
            
            # add RE gens to properties .sheet (Units, Max Capacity)
            RE.gens.to.properties <- RE.gens[,.(Generator = Generator.Name, 
                                                Units = Num.Units,
                                                `Max Capacity` = Max.Capacity)]
            
            add_to_properties_sheet(RE.gens.to.properties, 
                                    object.class = 'Generator', 
                                    names.col = 'Generator', 
                                    collection.name = 'Generators')
            
            # add RE gens to properties .sheet (Units) -- Scenario
            if(!is.na(scenname)){
                RE.gens.to.properties <- RE.gens[,.(Generator = Generator.Name, 
                                                    Units = Num.Units.Scn)] 
                
                add_to_properties_sheet(RE.gens.to.properties, 
                                        object.class = 'Generator', 
                                        names.col = 'Generator', 
                                        collection.name = 'Generators',
                                        scenario.name = scenname)
                
                RE.gens[,Num.Units := Num.Units.Scn]
            }
            
            # add RE gens to properties .sheet (Rating and associated datafile)
            # note: it is better to add rating separately, but leaving in the option
            # for backwards compatability reasons
            if ("Rating" %in% colnames(RE.gens)) {
                
                RE.gens.to.properties.rating <- RE.gens[,.(Generator = Generator.Name, 
                                                           Rating)]
                
                add_to_properties_sheet(RE.gens.to.properties.rating, 
                                        object.class = 'Generator', 
                                        names.col = 'Generator', 
                                        collection.name = 'Generators', 
                                        datafile.col = 'Rating'
                )
                rm(RE.gens.to.properties.rating)
            }
            
            # if fuel objects don't exist, add them
            missing.fuels = RE.gens[,unique(Fuel)]
            existing.fuels = generator.data.table[,unique(Fuel)]
            
            missing.fuels = missing.fuels[!(missing.fuels %in% existing.fuels)]
            
            if (length(missing.fuels) > 0) {
                new.fuels.to.objects <- initialize_table(Objects.sheet, 
                                                         length(missing.fuels), 
                                                         list(class = "Fuel", 
                                                              name = missing.fuels))
                
                Objects.sheet <- merge_sheet_w_table(Objects.sheet, new.fuels.to.objects)
                
                rm(new.fuels.to.objects)
            }
            
            # add RE gen-fuel to memberships (connecting gens to fuel and nodes)
            RE.gens.to.memberships.nodes <- 
                initialize_table(Memberships.sheet, 
                                 nrow(RE.gens), list(parent_class = "Generator", 
                                                     collection = "Nodes", 
                                                     child_class = "Node"))
            
            RE.gens.to.memberships.nodes[, parent_object := RE.gens[,Generator.Name]]
            RE.gens.to.memberships.nodes[, child_object := RE.gens[,Node.To.Connect]]
            
            RE.gens.to.memberships.fuel <- 
                initialize_table(Memberships.sheet, 
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
            new.RE.gens.data <- RE.gens[,.(Generator = Generator.Name, 
                                           Node = Node.To.Connect, 
                                           Fuel, 
                                           `Max Capacity` = Max.Capacity, 
                                           Units = Num.Units)]
            
            new.RE.gens.data <- merge(new.RE.gens.data, 
                                      node.data.table[,.(Node, Region, Zone)], 
                                      by = "Node", 
                                      all.x = T)
            
            generator.data.table <- rbind(generator.data.table, new.RE.gens.data, 
                                          fill = T)
            
            # clean up
            suppressWarnings({
                rm(RE.gens, new.node.table, RE.nodes.to.objects, 
                   RE.nodes.to.properties, RE.nodes.to.memberships.regions,
                   RE.nodes.to.memberships.zones, RE.line.table, 
                   RE.lines.to.objects, RE.lines.to.properties, 
                   RE.lines.to.memberships.from, rating.data.files.to.properties, 
                   RE.gens.to.objects, RE.gens.to.properties,
                   new.RE.nodes.data, new.RE.lines.data,
                   new.RE.gens.data, node.info, existing.fuels, missing.fuels)})
        } else {
            message(sprintf(">>  %s does not exist ... skipping", fname))
        }
    }
    
} else {
    
    message('>>  no RE gen info to be added... skipping')
}

#------------------------------------------------------------------------------|
# generator properties by fuel----
#------------------------------------------------------------------------------|

# uses generator.property.by.fuel.list
if (exists("generator.property.by.fuel.list")) {
    
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
            
            # set up args for add_to_properties_sheet, using output of merge by fuel  
            cur.prop.sheet.args <- generator.property.by.fuel.list[[elem]][[3]]
            cur.prop.sheet.args$input.table <- mapped.by.fuel
            cur.prop.sheet.args$names.col <- 'Generator'
            
            # add to properties sheet using input arguments and new table
            do.call(add_to_properties_sheet, cur.prop.sheet.args)
            
            if ('scenario.name' %in% names(cur.prop.sheet.args)) {
                # for now, just add any scenario here that doesn't already exist
                # need to deal with categories later
                cur.scen <- cur.prop.sheet.args[['scenario.name']]
                if (!(cur.scen %in% Objects.sheet[,name])) {
                    cur.scen.to.objects <- initialize_table(Objects.sheet, 
                                                            1, 
                                                            list(name = cur.scen, 
                                                                 category = 'Object properties',
                                                                 class = 'Scenario'))
                    
                    Objects.sheet <- merge_sheet_w_table(Objects.sheet, 
                                                         cur.scen.to.objects)
                    
                    # clean up
                    rm(cur.scen.to.objects)
                    
                }
                
                # clean up
                rm(cur.scen)
            }
            
        } else {
            message(sprintf(">>  %s does not exist ... skipping", 
                            generator.property.by.fuel.list[[elem]][1]))
        }
    }
    
    # clean up
    rm(cur.table, cur.map.fuel.args, cur.prop.sheet.args, mapped.by.fuel, elem)
}

#----------------------------------------------------------------------------|
# add object properties by object ----
#----------------------------------------------------------------------------|
# uses generator.property.file.list

if (exists("object.property.list")) {
    
    for (elem in seq_along(object.property.list)) {
        
        if (file.exists(file.path(inputfiles.dir,object.property.list[[elem]][1]))) {
            message(sprintf("... Adding properties from %s", 
                            object.property.list[[elem]][1]))
            # read in table
            cur.table <- fread(file.path(inputfiles.dir, 
                                         object.property.list[[elem]][1]))
            
            # read in args
            if (length(object.property.list[[elem]]) > 1) {
                
                cur.args <- object.property.list[[elem]][[2]]  
            } else {
                
                cur.args <- list()
            }
        
            cur.args$input.table <- cur.table
            
            # add to properties sheet using input arguments and new table
            do.call(add_to_properties_sheet, cur.args)
            
            # for now, just add any scenario here that doesn't already exist
            # need to deal with categories later
            if ('scenario.name' %in% names(cur.args)) { 
                
                cur.scen <- cur.args['scenario.name']
                if (!(cur.scen %in% Objects.sheet[,name])) {
                    
                    cur.scen.to.objects <- initialize_table(Objects.sheet, 1, 
                                                            list(name = cur.scen, 
                                                                 category = 'Object properties',
                                                                 class = 'Scenario'))
                    
                    Objects.sheet <- merge_sheet_w_table(Objects.sheet, 
                                                         cur.scen.to.objects)
                }
                
                # clean up
                # rm(cur.scen, cur.scen.to.objects)
            }
            
        } else {
            message(sprintf(">>  %s does not exist ... skipping", 
                            object.property.list[[elem]][1]))
        }
    }
} else {
    message(">>  object.property.list does not exist ... skipping")
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
            
            add_to_properties_sheet(cur.table, 
                                    names.col = cur.names, 
                                    object.class = cur.class,
                                    collection.name = cur.coll, 
                                    overwrite = T)
            
            # turn on units in scenario. if a generator, pull units from 
            # generator.data.table
            # right now, code only supports maintaining multiple units for generators
            if (cur.class == 'Generator') {
                
                genunits <- generator.data.table[, .(Generator, Units)]
                cur.table <- merge(cur.table[,Units := NULL], genunits, 
                                   by.x = cur.names, by.y = 'Generator')
                
            } else cur.table[,Units := 1]
            
            add_to_properties_sheet(cur.table, 
                                    names.col = cur.names, 
                                    object.class = cur.class, 
                                    collection.name = cur.coll, 
                                    scenario.name = cur.scen)
            
            # add scenario as an object
            cur.scen.to.obj <- 
                initialize_table(Objects.sheet, 1,list(class= 'Scenario', 
                                                       name = cur.scen, 
                                                       category = 'Generator status'))
            
            Objects.sheet <- merge_sheet_w_table(Objects.sheet, cur.scen.to.obj)
            
            # clean up
            rm(elem, cur.names, cur.class, cur.coll, cur.scen, cur.scen.to.obj)
            
        } else {
            message(sprintf(">>  %s does not exist ... skipping", 
                            turn.off.except.in.scen.list[[elem]][1]))
        }
    }
} else {
    message('>>  turn.off.except.in.scen.list does not exist ... skipping')
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
            
            # add min and max flow datafile pointers 
            add_to_properties_sheet(interface.properties, object.class = "Interface", 
                                    names.col = "Interface.Name", 
                                    collection.name = "Interfaces", 
                                    datafile.col = c("Min Flow", "Max Flow"))
            
            
            # Add interface-line memberships
            interface.to.memberships <- initialize_table(
                Memberships.sheet, nrow(interface.memberships), list(
                    parent_class = "Interface", 
                    child_class = "Line", collection = "Lines"))
            
            interface.to.memberships[,parent_object := 
                                         interface.memberships[,Interface.Name]]
            interface.to.memberships[,child_object := 
                                         interface.memberships[,Line]]
            
            Memberships.sheet <- merge_sheet_w_table(Memberships.sheet, 
                                                     interface.to.memberships)
            
            # Add flow coeffcienct to properties
            
            interface.coefficients.to.props <- initialize_table(
                Properties.sheet, nrow(interface.coefficients), list(
                    parent_class = "Interface", 
                    child_class = "Line", collection = "Lines", 
                    band_id = 1, property = "Flow Coefficient", value = -1))
            interface.coefficients.to.props[, parent_object := 
                                                interface.coefficients[, Interface.Name]]
            interface.coefficients.to.props[, child_object := 
                                                interface.coefficients[, Line]]
            
            Properties.sheet <- merge_sheet_w_table(Properties.sheet, 
                                                    interface.coefficients.to.props)
            
        } else {
            message(sprintf(">>  %s does not exist ... skipping", 
                            interfaces.files.list[[i]][1]))
        }
    }
} else {
    message('>>  no interface files defined ... skipping')
}

#------------------------------------------------------------------------------|
# add reserves ----
#------------------------------------------------------------------------------|

# read in files from reserve.files specified in input_params

if(exists('reserve.files')) {
    if(file.exists(file.path(inputfiles.dir,reserve.files$reserves))){
      # read reserves file
      message(sprintf("...Adding reserves from %s", reserve.files$reserves))
      reserves <- fread(file.path(inputfiles.dir, reserve.files$reserves))
      
      # add reserves to objects sheet
      reserve.to.objects <- initialize_table(Objects.sheet, nrow(reserves), 
                                             list(class = "Reserve"))
      
      reserve.to.objects[, name := reserves[,Reserve]]
      
      Objects.sheet <- merge_sheet_w_table(Objects.sheet, reserve.to.objects)
      
      # add reserve properties to properties .sheet
      reserve.properties <- reserves[,!c("Is Enabled","Scenario"),with = F]
      
      add_to_properties_sheet(reserve.properties, object.class = 'Reserve', 
                              names.col = 'Reserve', 
                              collection.name = 'Reserves')
      
      # add scenario on 'Is Enabled' property
      reserve.scenarios <- unique(reserves[,Scenario])
      for(i in reserve.scenarios){
        
        reserve.enabled <- reserves[Scenario == i,.(Reserve,`Is Enabled`)]
        
        add_to_properties_sheet(reserve.enabled, object.class = 'Reserve', 
                                names.col = 'Reserve', 
                                collection.name = 'Reserves',
                                scenario.name = i)
      }
      
      # turn off reserve when scenario not selected
      reserve.scenario.off <- reserves[,.(Reserve,`Is Enabled` = 0)]
  
      add_to_properties_sheet(reserve.scenario.off, object.class = 'Reserve',
                              names.col = 'Reserve',
                              collection.name = 'Reserves')
      
      # add reserve scenarios to objects .sheet
      reserve.scenario.to.objects <- initialize_table(Objects.sheet, 
                                                      length(reserve.scenarios),
                                                      list(class = "Scenario",
                                                           category = "Reserves"))
      
      reserve.scenario.to.objects[, name := reserve.scenarios]
      
      Objects.sheet <- merge_sheet_w_table(Objects.sheet,reserve.scenario.to.objects)
    }else {
      message(sprintf('>>  file %s not found ... skipping', 
                      reserve.files$reserves))
    }
    
    # add reserve generators 
    if(file.exists(file.path(inputfiles.dir,reserve.files$reserve.generators))){
      # read reserve generators file
      message(sprintf("...Adding reserves from %s", reserve.files$reserve.generators))
      reserve.generators <- fread(file.path(inputfiles.dir,
                                            reserve.files$reserve.generators))
      
      # add reserve-generator memberships to memberships.sheet
      reserve.to.gens.to.memberships <- 
        initialize_table(Memberships.sheet, nrow(reserve.generators), 
                         list(parent_class = "Reserve", child_class = "Generator", 
                              collection = "Generators"))
      
      reserve.to.gens.to.memberships[,parent_object := 
                                       reserve.generators[,Reserve]]
      
      reserve.to.gens.to.memberships[,child_object := reserve.generators[,Generator]]
      
      Memberships.sheet <- merge_sheet_w_table(Memberships.sheet, 
                                               reserve.to.gens.to.memberships)
      
    }else {
      message(sprintf('>>  file %s not found ... skipping', 
                      reserve.files$reserve.generators))
    }
  
}else {
    message('>>  no reserves files defined ... skipping')
}