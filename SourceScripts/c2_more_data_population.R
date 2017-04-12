#uses (from master):
# TODO edit this

#------------------------------------------------------------------------------|
# test generic object/property adder ----
#------------------------------------------------------------------------------|

if (exists("objects.list")) {
  
    for (elem in seq_along(objects.list)) {
        
        data.path <- objects.list[[elem]][1]
        
        cur.data <- read_data(data.path, sep = ",")
        
        if (is.data.table(cur.data)) {
        
            message(sprintf("... Adding objects/properties from %s", data.path))
          
            # do some cleaning/checking
            check_colname_cap(cur.data)
            
            if ("notes" %in% names(cur.data)) cur.data[, notes := NULL]
            
            # if any memberships, add those (there may be dupes in objects or
            # properties here)
            memb.cols <- names(cur.data)
            memb.cols <- c(memb.cols[1], memb.cols[grepl("_", memb.cols)])
            
            if (length(memb.cols) > 1) {
                
                # check_for_dupes(cur.data, memb.cols)
                
                import_memberships(cur.data[,memb.cols, with = FALSE])
            }
            
            # make sure there are no duplicates in the non-membership columns
            if (length(memb.cols) > 1) cur.data[, (memb.cols[-1]) := NULL]
            
            cur.data <- unique(cur.data) # this could be an efficiency thing
            # check_for_dupes(cur.data, names(cur.data[1]))
            
            # add objects
            obj.cols <- c(names(cur.data)[1], 
                          if ("category" %in% names(cur.data)) "category")
            
            import_objects(unique(cur.data[,obj.cols, with = FALSE]))
            
            # add properties (and args)
            excluded.cols <- c("notes", "category")
            
            excluded.cols <- excluded.cols[excluded.cols %in% names(cur.data)]
            
            if (length(excluded.cols) > 0) {
                cur.data <- cur.data[,!excluded.cols, with = FALSE]
            }
            
            # read in args if given
            if (length(objects.list[[elem]]) > 1) {
                
                cur.args <- objects.list[[elem]][[2]]
                
            } else {
                
                cur.args <- list()
            }
            
            cur.args$input.table <- cur.data
            
            # add to properties sheet using input arguments
            do.call(import_properties, cur.args)
            
        } # end if (is.data.table(cur.data))
        
    }
    
    # clean up
    rm(data.path, cur.data, excluded.cols, memb.cols, cur.args, elem)

} else {
    
    message(">>  objects.list does not exist ... skipping")
}


#------------------------------------------------------------------------------|
# test generic membership adder ----
#------------------------------------------------------------------------------|

if (exists("memberships.list")) {
    
    for (fname in memberships.list) {
        
        cur.dt <- read_data(fname)
        
        if (is.data.table(cur.dt)) {
            
            message(sprintf("... Adding memberships from %s", fname))
            
            # do some cleaning/checking
            check_for_dupes(cur.dt, names(cur.dt))
            check_colname_cap(cur.dt)
            
            # add memberships
            import_memberships(cur.dt)
            
        }
    }
    
    # clean up
    rm(cur.dt, fname)
    
} else {
    
    message(">>  memberships.list does not exist ... skipping")
}


#------------------------------------------------------------------------------|
# add fuels and categorize generators by fuel ----
#------------------------------------------------------------------------------|

if (exists("map.gen.to.fuel.file")) {
    
    fuel.table <- read_data(map.gen.to.fuel.file)
    
    if (is.data.table(fuel.table)) {
        
        # add fuels to generator.data.table
        generator.data.table <- 
            merge(generator.data.table, fuel.table[, .(Generator, Fuel)], 
                  by = "Generator", all.x = TRUE)
        
        # add fuels to objects .sheet
        all.fuels <- unique(fuel.table[, .(Fuel)])
        
        import_objects(all.fuels)
        
        # add generator-fuels membership to memberships
        fuels.to.gens.to.membs <- fuel.table[,.(Generator, Fuels_Fuel = Fuel)]
        
        import_memberships(fuels.to.gens.to.membs)
        
        # edit generators to categorize by fuel instead of region (edit later)
        Objects.sheet <- 
            merge(Objects.sheet, 
                  generator.data.table[, .(Generator, Fuel, class = 'Generator')], 
                  by.x = c('class', 'name'), by.y = c('class', 'Generator'), 
                  all.x = T)
        Objects.sheet[!is.na(Fuel),category := Fuel]
        Objects.sheet[,Fuel := NULL]
        
        # clean up
        rm(fuel.table, all.fuels, fuels.to.gens.to.membs)
        
    } # end if (is.data.table(fuel.table))
    
} else {
    
    message(">>  map.gen.to.fuel.file does not exist ... skipping")
}
    
#------------------------------------------------------------------------------|
# add load (mapped by region by external file) and lpf ----
#------------------------------------------------------------------------------|

if (exists("map.region.to.load.file")) {
    
    load.to.region.map <- read_data(map.region.to.load.file)
    
    if (is.data.table(load.to.region.map)) {
        # create data file object name column
        load.to.region.map[,DataFile := paste0(load.to.region.map[,Region], 
                                               " Load File Object")]
        
        # add load data file objects to objects .sheet 
        # uses load.to.region.map
        load.file.to.object <- load.to.region.map[, .(`Data File` = DataFile, 
                                                      category = "Regional Load")]

        import_objects(load.file.to.object)
        
        # load file object to as regional load
        # uses load.to.region.map
        load.to.region.props <- load.to.region.map[, .(Region, 
                                                       Load = paste0("{Object}", DataFile))]
        
        import_properties(load.to.region.props, datafile.col = "Load")
        
        # load to properties (attach filepath to object based on scenario)
        # uses load.to.region.map
        
        # loop through each column and add columns
        load.scens <- colnames(load.to.region.map)
        load.scens <- load.scens[!(load.scens %in% c('Region', 'DataFile'))]
        
        for (name in load.scens) {
            # create small table to pass to import_properties
            cur.tab <- load.to.region.map[,.SD, .SDcols = c('DataFile', name)]
            
            setnames(cur.tab, name, "filename")
            
            import_properties(cur.tab, names.col = 'DataFile', 
                                    object.class = 'Data File', 
                                    collection.name = 'Data Files', 
                                    datafile.col = "filename", 
                                    scenario.name = ifelse(tolower(name) == 'base', 
                                                           NA, name))
        }
        
        setnames(cur.tab, "filename_datafile", name) #hacky. fix this later
        
        # add any scenarios associated with load as objects
        load.scens <- load.scens[tolower(load.scens) != 'base']
        
        if (length(load.scens)) {
            add_scenarios(load.scens, category = "Load scenarios")
        }

        # clean up
        rm(load.to.region.map, load.file.to.object, load.to.region.props, 
           load.scens, cur.tab)
        
    } # end if (is.data.table(load.to.region.map))
    
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
    
    if ("Status" %in% names(load.data.table)) {
        
        # remove Status = 0 load
        load.data.table[,Load := Load * Status]
    }
    
    load.part.fact.table <- load.data.table[, .(Node, Load)] 
    
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
    
    import_properties(lpf.to.node.properties, object.class = 'Node', 
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
if (exists("RE.gen.file.list") && add.RE.gens){
    
    for (item in RE.gen.file.list) {
        
        fname = item[1]
        scenname = item["scenario"]
        make.new.nodes = item["make.new.nodes"]
        
        RE.gens <- read_data(fname, colClasses = 'numeric')
        
        if (is.data.table(RE.gens)) {
            
            # read in information about RE gens
            message(sprintf("... Adding properties for RE gens from %s", fname))

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
                
                add_scenarios(scenname, category = "Generator status")
                
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
                
                import_properties(RE.nodes.to.properties, 
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
                
                import_properties(RE.lines.to.properties, names.col = 'Line', 
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
            RE.gens.to.objects <- RE.gens[,.(Generator = Generator.Name, 
                                             category = Category)]

            import_objects(RE.gens.to.objects)
            
            # add RE gens to properties .sheet (Units, Max Capacity)
            RE.gens.to.properties <- RE.gens[,.(Generator = Generator.Name, 
                                                Units = Num.Units,
                                                `Max Capacity` = Max.Capacity)]
            
            import_properties(RE.gens.to.properties, 
                                    object.class = 'Generator', 
                                    names.col = 'Generator', 
                                    collection.name = 'Generators')
            
            # add RE gens to properties .sheet (Units) -- Scenario
            if(!is.na(scenname)){
                RE.gens.to.properties <- RE.gens[,.(Generator = Generator.Name, 
                                                    Units = Num.Units.Scn)] 
                
                import_properties(RE.gens.to.properties, 
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
                
                import_properties(RE.gens.to.properties.rating, 
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
                
                missing.fuels <- data.table(Fuel = missing.fuels)
                
                import_objects(missing.fuels) 
                    
            }
            
            # add RE gen-fuel to memberships (connecting gens to fuel and nodes)
            RE.gens.to.membs <- RE.gens[, .(Generator = Generator.Name, 
                                            Nodes_Node = Node.To.Connect, 
                                            Fuels_Fuel = Fuel)]
            
            import_memberships(RE.gens.to.membs)
            
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
                   RE.nodes.to.properties, 
                   RE.gens.to.membs,
                   RE.nodes.to.memberships.regions,
                   RE.nodes.to.memberships.zones, RE.line.table, 
                   RE.lines.to.objects, RE.lines.to.properties, 
                   RE.lines.to.memberships.from, rating.data.files.to.properties, 
                   RE.gens.to.objects, RE.gens.to.properties,
                   new.RE.nodes.data, new.RE.lines.data,
                   new.RE.gens.data, node.info, existing.fuels, missing.fuels)})
            
        } # end if (is.data.table(RE.gens))
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
        
        cur.data <- read_data(generator.property.by.fuel.list[[elem]][[1]])
        
        if (is.data.table(cur.data)) {
            
            message(sprintf("... Adding properties from %s", 
                            generator.property.by.fuel.list[[elem]][1]))
            
            # set up arguments for merge_property_by_fuel
            cur.map.fuel.args <- generator.property.by.fuel.list[[elem]][[2]]
            cur.map.fuel.args$input.table <- cur.data
            
            # merge properties fuel, produces table with list of generators in rows
            # and their properties in all other columns
            mapped.by.fuel <- do.call(merge_property_by_fuel, cur.map.fuel.args)
            
            # set up args for import_properties, using output of merge by fuel  
            cur.prop.sheet.args <- generator.property.by.fuel.list[[elem]][[3]]
            cur.prop.sheet.args$input.table <- mapped.by.fuel
            cur.prop.sheet.args$names.col <- 'Generator'
            
            # add to properties sheet using input arguments and new table
            do.call(import_properties, cur.prop.sheet.args)
            
            if ('scenario.name' %in% names(cur.prop.sheet.args)) {
                # for now, just add any scenario here that doesn't already exist
                # need to deal with categories later
                
                add_scenarios(cur.prop.sheet.args[['scenario.name']], 
                              category = "Object properties")
                
            }
            
        } # end if (is.data.table(cur.data))
    }
    
    # clean up
    rm(cur.data, cur.map.fuel.args, cur.prop.sheet.args, mapped.by.fuel, elem)
}

#----------------------------------------------------------------------------|
# add object properties by object ----
#----------------------------------------------------------------------------|
# uses generator.property.file.list

if (exists("object.property.list")) {
    
    for (elem in seq_along(object.property.list)) {
        
        cur.data <- read_data(object.property.list[[elem]][[1]])
        
        if (is.data.table(cur.data)) {
            
            message(sprintf("... Adding properties from %s", 
                            object.property.list[[elem]][1]))
            
            # read in args
            if (length(object.property.list[[elem]]) > 1) {
                
                cur.args <- object.property.list[[elem]][[2]]  
            } else {
                
                cur.args <- list()
            }
        
            cur.args$input.table <- cur.data
            
            # clean, add to properties sheet using input arguments and new table
            check_colname_cap(cur.data)
            
            do.call(import_properties, cur.args)
            
            # add any scenario here that doesn't already exist
            # need to deal with categories later
            if ('scenario.name' %in% names(cur.args)) { 
                
                add_scenarios(cur.args['scenario.name'], 
                              category = "Object properties")
                
            }
            
            # clean up
            rm(cur.data)
            
        } # end if (is.data.table(cur.data))
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
        
        cur.data <- read_data(turn.off.except.in.scen.list[[elem]][[1]])
        
        if (is.data.table(cur.data)) {
            
            message(sprintf(paste0("... Adding turning off objects from %s except",
                                   " for in scenario '%s'"), 
                            turn.off.except.in.scen.list[[elem]][1],
                            turn.off.except.in.scen.list[[elem]][['scenario.name']]))
            
            # turn off Units property of these objects
            cur.names <- turn.off.except.in.scen.list[[elem]][['names.col']]
            cur.class <- turn.off.except.in.scen.list[[elem]][['object.class']]
            cur.coll <- turn.off.except.in.scen.list[[elem]][['collection.name']]
            cur.scen <- turn.off.except.in.scen.list[[elem]][['scenario.name']]
            
            # turn off Units in bae
            cur.data[,Units := 0]
            
            import_properties(cur.data, 
                                    names.col = cur.names, 
                                    object.class = cur.class,
                                    collection.name = cur.coll, 
                                    overwrite = T)
            
            # turn on units in scenario. if a generator, pull units from 
            # generator.data.table
            # right now, code only supports maintaining multiple units for generators
            if (cur.class == 'Generator') {
                
                genunits <- generator.data.table[, .(Generator, Units)]
                cur.data <- merge(cur.data[,Units := NULL], genunits, 
                                   by.x = cur.names, by.y = 'Generator')
                
            } else cur.data[,Units := 1]
            
            import_properties(cur.data, 
                                    names.col = cur.names, 
                                    object.class = cur.class, 
                                    collection.name = cur.coll, 
                                    scenario.name = cur.scen)
            
            # add scenario as an object
            add_scenarios(cur.scen, category = "Generator status")
            
            # clean up
            rm(elem, cur.names, cur.class, cur.coll, cur.scen)
            
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
            interface.names <- read_data(interfaces.files.list[[i]]['names'])
            interface.properties <- read_data(interfaces.files.list[[i]]['properties'])
            interface.memberships <- read_data(interfaces.files.list[[i]]['memberships'])
            interface.coefficients <- read_data(interfaces.files.list[[i]]['flowcoefs'])
            
            # Add interfaces to objects sheet
            import_objects(interface.names[, .(Interface = Interface.Name, 
                                               category)])
                
            # Add interface properties - changed to data.file. need to genericize, 
            # change to data file object so can put in 2014 and 2022 data files, etc 
            # import_properties(interface.properties, object.class = "Interface", 
            #   collection.name = "Interfaces", names.col = "Interface.Name")
            
            # add min and max flow datafile pointers 
            import_properties(interface.properties, object.class = "Interface", 
                                    names.col = "Interface.Name", 
                                    collection.name = "Interfaces", 
                                    datafile.col = c("Min Flow", "Max Flow"))
            
            
            # Add interface-line memberships
            interface.to.membs <- interface.memberships[,.(Interface = Interface.Name, 
                                                           Lines_Line = Line)] 
            
            import_memberships(interface.to.membs)
            
            # Add flow coeffcienct to properties
            interface.coefficients.to.props <- 
                interface.coefficients[, .(Line, 
                                           Interface = Interface.Name, 
                                           `Flow Coefficient`)]
            
            import_properties(interface.coefficients.to.props, 
                                    parent.col = "Interface")
            
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
    
    reserves <- read_data(reserve.files$reserves)
    
    if (is.data.table(reserves)) {
        
        # read reserves file
        message(sprintf("... Adding reserves from %s", reserve.files$reserves))
        
        # add reserves to objects sheet
        import_objects(reserves[, .(Reserve)])

        # fix scenario name so is always lowercase
        check_colname_cap(reserves)
        
        # add scenario on 'Is Enabled' property
        if ("scenario" %in% names(reserves)) {
            
            reserve.scenarios <- unique(reserves[,scenario])
            
            for(i in reserve.scenarios){
                
                reserve.enabled <- reserves[scenario == i,.(Reserve,`Is Enabled`)]
                
                import_properties(reserve.enabled, object.class = 'Reserve', 
                                        names.col = 'Reserve', 
                                        collection.name = 'Reserves',
                                        scenario.name = i)
            }
            
            # turn off reserve when scenario not selected
            reserve.scenario.off <- reserves[,.(Reserve,`Is Enabled` = 0)]
            
            import_properties(reserve.scenario.off, 
                                    object.class = 'Reserve',
                                    names.col = 'Reserve',
                                    collection.name = 'Reserves')
            
            # add reserve scenarios to objects.sheet
            add_scenarios(reserve.scenarios, category = "Reserves")
            
            # clean up
            rm(reserve.scenarios, reserve.enabled, reserve.scenario.off)
            
        } else if ("Is Enabled" %in% names(reserves)) {
            
            # add `Is Enabled` property without a scenario
            import_properties(reserves[,.(Reserve, `Is Enabled`)])
            
        } else {
            
            # give `Is Enabled` anyway, set to 1
            message(sprintf("'Is Enabled' property not given in %s ... setting to 1",
                            reserve.files$reserves))
        }
        
        # add reserve properties to properties .sheet
        excluded.cols <- c("Is Enabled","scenario")
        excluded.cols <- excluded.cols[excluded.cols %in% names(reserves)]
        
        reserve.properties <- reserves[,!excluded.cols, with = F]
        
        import_properties(reserve.properties, object.class = 'Reserve', 
                                names.col = 'Reserve', 
                                collection.name = 'Reserves')
        
        # clean up
        rm(excluded.cols, reserve.properties, reserves)
        
    } # end if (is.data.table(reserves))
    
    # add reserve generators 
    if (length(reserve.files$reserve.generators) > 0 &&
        file.exists(file.path(inputfiles.dir, reserve.files$reserve.generators))){
        
        # read reserve generators file
        message(sprintf("... Adding reserves from %s", 
                        reserve.files$reserve.generators))
        
        reserve.generators <- read_data(reserve.files$reserve.generators)
        
        # add reserve-generator memberships to memberships.sheet
        reserve.to.gens.to.membs <- reserve.generators[,.(Reserve, 
                                                          Generators_Generator = Generator)]

        import_memberships(reserve.to.gens.to.membs)
        
        # add reserve-generator properties to properties.sheet
        cnames <- colnames(reserve.generators)
        
        if (length(cnames[!(cnames %in% c("Reserve", "Generator", "notes"))]) > 0) {
            
            if ("notes" %in% cnames) {
                reserve.generators[,notes := NULL]
            }
            
            import_properties(reserve.generators, 
                                    names.col = "Generator",
                                    parent.col = "Reserve")
        }
        
        # clean up
        rm(reserve.generators, reserve.to.gens.to.membs, cnames)
        
    } else {
        
        if (length(reserve.files$reserve.regions) > 0) {
            message(sprintf('>> file %s not found ... skipping',
                            reserve.files$reserve.generators))
        }
    }
    
    # add reserve regions
    if (length(reserve.files$reserve.regions) > 0 &&
        file.exists(file.path(inputfiles.dir, reserve.files$reserve.regions))){
        
        # read reserve regions file
        message(sprintf("... Adding reserves from %s", 
                        reserve.files$reserve.regions))
        
        reserve.regions <- read_data(reserve.files$reserve.regions)
        
        # add reserve-region memberships to memberships.sheet
        reserve.to.regs.to.membs <- reserve.regions[,.(Reserve, 
                                                       Regions_Region = Region)]
        
        import_memberships(reserve.to.regs.to.membs)
        
        # add reserve.region properties to properties .sheet
        import_properties(reserve.regions, object.class = 'Region',
                                parent.col = 'Reserve',
                                names.col = 'Region', 
                                collection.name = 'Regions')
        
        # clean up
        rm(reserve.regions, reserve.to.regs.to.membs)
        
    } else {
        if (length(reserve.files$reserve.regions) > 0) {
            message(sprintf('>> file %s not found ... skipping',
                            reserve.files$reserve.regions))
        }
    }
    
    # clean up
    rm(reserve.files)
    
}else {
    message('>>  no reserves files defined ... skipping')
}