#------------------------------------------------------------------------------|
# ADD OBJECTS ----
#------------------------------------------------------------------------------|

#------------------------------------------------------------------------------|
# test generic object/property adder ----
#------------------------------------------------------------------------------|

if (exists("objects.list")) {
  
    for (elem in seq_along(objects.list)) {
        
        data.path <- objects.list[[elem]][[1]]
        
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
                
                # get all args but the first (a little gynmastics to account 
                # for the args being in a separate list or not)
                cur.args <- objects.list[[elem]][-1]
                
                if (is.list(cur.args) & all(is.null(names(cur.args)))) {
                    cur.args <- cur.args[[1]]
                }
                
            } else {
                
                cur.args <- list()
            }
            
            # add another element and coerce to a list
            suppressWarnings(cur.args$input.table <- cur.data)
            
            # add to properties sheet using input arguments
            do.call(import_properties, cur.args)
            
            rm(excluded.cols, memb.cols, cur.args)
            
        } # end if (is.data.table(cur.data))

    }
    
    # clean up
    rm(data.path, cur.data, elem)

} else {
    
    message(">>  objects.list does not exist ... skipping")
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
        
        setnames(cur.tab, "datafile_filename", name) #hacky. fix this later
        
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
              # node.data.table[, .(Node, Region)], 
              Memberships.sheet[parent_class == "Node" & collection == "Region", 
                                .(Node = parent_object, Region = child_object)],
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
            # check for Number of Units, Max Capacity
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
            rm(check.RE.capacity)
            
            # create scenario if input file scenario in input_params*.R
            if(!is.na(scenname)){
                
                RE.gens[,Num.Units.Scn := Num.Units]
                RE.gens[,Num.Units := 0]
                
                add_scenarios(scenname, category = "Generator status")
                
            }
            
            if ("Commit" %in% names(RE.gens)) {
                import_properties(RE.gens[,.(Generator = Generator.Name, Commit)])
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
                # new.RE.nodes.data <- new.node.table[,.(Node = Node.Name, 
                #                                        Voltage = Node.kV, 
                #                                        Region = Node.Region, 
                #                                        Zone = Node.Zone)]
                # 
                # node.data.table <- rbind(node.data.table, new.RE.nodes.data, fill = TRUE)
                
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
            
            # add RE gen-fuel to memberships (connecting gens to fuel and nodes)
            RE.gens.to.membs <- RE.gens[, .(Generator = Generator.Name, 
                                            Nodes_Node = Node.To.Connect, 
                                            Fuels_Fuel = Fuel)]
            
            import_memberships(RE.gens.to.membs)
            
            # add fuel objects
            RE.fuels <- RE.gens[,unique(Fuel)]
            
            RE.fuels <- RE.fuels[!(RE.fuels %in% Objects.sheet[class == "Fuel", name])]
            
            if (length(RE.fuels) > 0) import_objects(data.table(Fuel = RE.fuels))
                
            # generators - can also add bus number, ID, an implied min cap of 0
            # must pull node's region and zone from node.data.table, in case new nodes
            # didn't get added
            new.RE.gens.data <- RE.gens[,.(Generator = Generator.Name, 
                                           Node = Node.To.Connect, 
                                           Fuel, 
                                           `Max Capacity` = Max.Capacity, 
                                           Units = Num.Units)]
            
            # new.RE.gens.data <- merge(new.RE.gens.data, 
            #                           node.data.table[,.(Node, Region, Zone)], 
            #                           by = "Node", 
            #                           all.x = T)
            
            # generator.data.table <- rbind(generator.data.table, new.RE.gens.data, 
                                          # fill = T)
            
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
                   new.RE.gens.data, node.info)})
            
        } # end if (is.data.table(RE.gens))
    }
    
} else {
    
    message('>>  no RE gen info to be added... skipping')
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
    if (length(reserve.files$reserve.generators) > 0){
        
        reserve.generators <- read_data(reserve.files$reserve.generators)
        
        if (is.data.table(reserve.generators)) {
            
            # read reserve generators file
            message(sprintf("... Adding reserves from %s", 
                            reserve.files$reserve.generators))
            
            
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
        }
        
    }
    
    # add reserve regions
    if (length(reserve.files$reserve.regions) > 0){
        
        reserve.regions <- read_data(reserve.files$reserve.regions)
        
        if (is.data.table(reserve.regions)) {
            
            # read reserve regions file
            message(sprintf("... Adding reserves from %s", 
                            reserve.files$reserve.regions))
            
            
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
            
        } 
        
        # clean up
        rm(reserve.files)
    }
    
} else {
    
    message('>>  no reserves files defined ... skipping')
}

#------------------------------------------------------------------------------|
# Import Generic Files ----
# -----------------------------------------------------------------------------|

#uses generic.import.files

all.sheets <- c("Objects", "Categories", "Memberships", "Attributes", 
                "Properties", "Reports")

import_and_merge <- function(imported.tab, sheet.name) {
    
    cur.tab <- import_table_generic(imported.tab, sheet.name)
    
    if (!is.null(cur.tab)) {
        
        assign(paste0(sheet.name, ".sheet"), 
               merge_sheet_w_table(get(paste0(sheet.name,".sheet")), cur.tab), 
               envir = .GlobalEnv)
    } 
}

#import and merge all generic import files
if (exists('generic.import.files')) {
    
    invisible(lapply(generic.import.files, function (x) {
        
        # read in data, change blanks to NA, and import into .sheet tables
        imported.file <- read_data(x[[1]],
                                   fill = TRUE, 
                                   header = FALSE, 
                                   strip.white = TRUE, 
                                   fix.db.colnames = FALSE)
        
        if (is.data.table(imported.file)) {
            
            message(sprintf("... importing from %s", x[[1]]))
            
            # hacky: if a table type is given, assume that table needs 
            # formatting (put header as first col, add begin and end tags) 
            if (length(x) > 1) {
                
                type <- x[[2]]
                
                # add headers as first row (useful for reading from db)
                if (names(imported.file)[1] != "V1") {
                    imported.file <- rbind(as.list(names(imported.file)), 
                                           imported.file)
                    
                    # change names to V1:Vn
                    setnames(imported.file, 
                             names(imported.file), 
                             paste0("V", 1:length(imported.file)))
                }
                
                # add / BEGIN and / END tags
                imported.file <- rbindlist(list(list(V1 = "/ BEGIN", 
                                                     V2 = type), 
                                                imported.file, 
                                                list(V1 = "/ END", 
                                                     V2 = type)), 
                                           fill = TRUE)
                
            }
            
            for (j in seq_len(ncol(imported.file)))
                set(imported.file, which(imported.file[[j]] == ""), j, NA)
            
            lapply(all.sheets, function(y) import_and_merge(imported.file, y))
            
        } # end if (is.data.table(imported.file)) 
    }))
} else { message('>>  no generic import files defined ... skipping') }

rm(import_and_merge, all.sheets)

#------------------------------------------------------------------------------|
# Import Compact Generic Files ----
#------------------------------------------------------------------------------|
# uses compact.generic.import.files
# loop through compact generic input files and read in tables

if (exists('compact.generic.import.files')) {
    
    for (i in seq_along(compact.generic.import.files)) {
        
        cur.data <- read_data(compact.generic.import.files[[i]][[1]])
        
        if (is.data.table(cur.data)) {
            message(sprintf("... importing from %s", 
                            compact.generic.import.files[[i]][1]))
            
            cur.obj.type <- compact.generic.import.files[[i]][2]
            
            # read in file, add appropriate sections to object, attib, memb .sheet tables
            import_table_compact(cur.data, cur.obj.type)
            
        } # end if (is.data.table(cur.data))
    }
    
    # clean up
    if (exists("cur.data")) rm(cur.data)
    if (exists("cur.obj.type")) rm(cur.obj.type)
    
} else { message('>>  no compact generic import files defined ... skipping')}

#------------------------------------------------------------------------------|
# User-defined Constraint Import                                      ----
# -----------------------------------------------------------------------------|
# 
if (exists('constraint.import.files')) {
    
    for (i in seq_along(constraint.import.files)) {
        
        cur.data <- read_data(constraint.import.files[[i]][[1]])
        
        if (is.data.table(cur.data)) {
            
            message(sprintf("... importing constraint from  %s", 
                            constraint.import.files[[i]][1]))
            
            import_constraint(cur.data)
            
        } else {
            
            message(sprintf(">>  %s does not exist ... skipping", 
                            constraint.import.files[[i]][1]))
            
        }
        
    }
    
} else { message('>>  no constraint import files defined ... skipping')}

#------------------------------------------------------------------------------|
# Create generator.data.table ----
#------------------------------------------------------------------------------|

generator.data.table <- unique(Objects.sheet[class == "Generator",
                                             .(Generator = name, category)])

generator.data.table <- merge(generator.data.table, 
                              Memberships.sheet[parent_class == "Generator" &
                                                    child_class == "Fuel" &
                                                    collection == "Fuels",
                                                .(Generator = parent_object,
                                                  Fuel = child_object)],
                              all.x = TRUE)

generator.data.table <- merge(generator.data.table,
                              Properties.sheet[property == "Units" & 
                                                   child_class == "Generator" & 
                                                   is.na(scenario),
                                               .(Generator = child_object,
                                                 Units = as.numeric(value))],
                              all.x = TRUE)

generator.data.table <- merge(generator.data.table,
                              Properties.sheet[property == "Max Capacity" & 
                                                   child_class == "Generator",
                                               .(Generator = child_object,
                                                 `Max Capacity` = as.numeric(value))],
                              all.x = TRUE)
    
#------------------------------------------------------------------------------|
# ADD PROPERTIES AND MEMBERSHIPS ----
#------------------------------------------------------------------------------|

#------------------------------------------------------------------------------|
# test generic membership adder ----
#------------------------------------------------------------------------------|

if (exists("memberships.list")) {
    
    for (elem in memberships.list) {
      
        fname <- elem[[1]] 
        cur.dt <- read_data(fname)
        
        if (is.data.table(cur.dt)) {
            
            message(sprintf("... Adding memberships from %s", fname))
            
            # do some cleaning/checking
            check_for_dupes(cur.dt, names(cur.dt))
            check_colname_cap(cur.dt)
            
            # import memberships
            memb.cols <- names(cur.dt)
            memb.cols <- c(memb.cols[1], memb.cols[grepl("_", memb.cols)])
            
            if (length(memb.cols) > 1) {
                import_memberships(cur.dt[,memb.cols, with = FALSE])
            }
            
            # if there are property cols, import those, too
            non.prop.cols <- c(memb.cols, "notes")
            prop.cols <- names(cur.dt)[!(names(cur.dt) %in% non.prop.cols)] 
            
            if (length(prop.cols) > 0) {
                
                # read in other args if given
                if (length(elem) > 1) {
                    # get all args but the first (a little gynmastics to account 
                    # for the args being in a separate list or not)
                    cur.args <- elem[-1]
                    
                    if (is.list(cur.args) & all(is.null(names(cur.args)))) {
                        cur.args <- cur.args[[1]]
                    }
                } else {
                    cur.args <- list()
                }
                
                # set up and import properties. untested with multiple children
                suppressWarnings(cur.args$input.table <- cur.dt)
                cur.args$parent.col <- memb.cols[1]
                cur.args$names.col <- memb.cols[-1]
                cur.args$object.class <- tstrsplit(memb.cols[-1], "_")[[2]]
                cur.args$collection.name <- tstrsplit(memb.cols[-1], "_")[[1]]

                do.call(import_properties, cur.args)
                
            }
            
        }
    }
    
    # clean up
    rm(cur.dt, fname)
    
} else {
    
    message(">>  memberships.list does not exist ... skipping")
}

#------------------------------------------------------------------------------|
# generator properties by fuel----
#------------------------------------------------------------------------------|

# uses generator.property.by.fuel.list
if (exists("generator.property.by.fuel.list")) {
    
    for (elem in seq_along(generator.property.by.fuel.list)) {

        cur.data <- read_data(generator.property.by.fuel.list[[elem]][[1]])
        
        check_colname_cap(cur.data)
        
        if (is.data.table(cur.data)) {
            
            message(sprintf("... Adding properties from %s", 
                            generator.property.by.fuel.list[[elem]][1]))
            
            # shifting from doing this by fuel to by category
            if ("Fuel" %in% names(cur.data)) {
                setnames(cur.data, "Fuel", "category")
                message(sprintf(paste0("in merge_property_by_fuel, please change",
                                      " 'fuel' to 'category' in %s. ",
                                      "I'll do this for you for now."),
                                generator.property.by.fuel.list[[elem]][1]))
            }
            
            # set up arguments for merge_property_by_fuel
            if ("fuel.map.args" %in% names(generator.property.by.fuel.list[[elem]])) {
                cur.map.fuel.args <- as.list(generator.property.by.fuel.list[[elem]][["fuel.map.args"]])
            } else {
                cur.map.fuel.args <- list()
            }
            
            if ("prop.cols" %in% names(cur.map.fuel.args)) {
                message(sprintf(paste0("use of prop.cols is deprecated in ",
                                       "merge_property_by_fuel. I'll ",
                                       "auto-populate prop.cols. for the future,",
                                       " please remove prop.cols from input ",
                                       "params when you pass in %s"),
                                generator.property.by.fuel.list[[elem]][1]))
                
                cur.map.fuel.args <- cur.map.fuel.args[names(cur.map.fuel.args) != "prop.cols"]
            }
            
            if ("notes" %in% names(cur.data)) cur.data[,notes := NULL]
            
            cur.map.fuel.args$input.table <- cur.data
            
            # merge properties fuel, produces table with list of generators in rows
            # and their properties in all other columns
            mapped.by.fuel <- do.call(merge_property_by_fuel, cur.map.fuel.args)
            
            # set up args for import_properties, using output of merge by fuel  
            if ("add.to.prop.args" %in% names(generator.property.by.fuel.list[[elem]])) {
                cur.prop.sheet.args <- as.list(generator.property.by.fuel.list[[elem]][["add.to.prop.args"]])
            } else {
                cur.prop.sheet.args <- list()
            }
            
            cur.prop.sheet.args$input.table <- mapped.by.fuel
            
            # add to properties sheet using input arguments and new table
            do.call(import_properties, cur.prop.sheet.args)
            
            if ('scenario.name' %in% names(cur.prop.sheet.args)) {
                # for now, just add any scenario here that doesn't already exist
                # need to deal with categories later
                
                add_scenarios(cur.prop.sheet.args[['scenario.name']], 
                              category = ifelse('scenario.cat' %in% names(cur.prop.sheet.args), 
                                                cur.prop.sheet.args[['scenario.cat']], 
                                                NA))
                
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
            
            # read in args if given
            if (length(object.property.list[[elem]]) > 1) {
                
                # get all args but the first (a little gynmastics to account 
                # for the args being in a separate list or not)
                cur.args <- object.property.list[[elem]][-1]
                
                if (is.list(cur.args) & all(is.null(names(cur.args)))) {
                    cur.args <- cur.args[[1]]
                }
                
            } else {
                
                cur.args <- list()
            }
            
            # add another element and coerce to a list
            suppressWarnings(cur.args$input.table <- cur.data)
            
            # clean, add to properties sheet using input arguments and new table
            check_colname_cap(cur.data)
            
            do.call(import_properties, cur.args)
            
            # add any scenario here that doesn't already exist
            # need to deal with categories later
            if ('scenario.name' %in% names(cur.args)) { 
                
                add_scenarios(cur.args['scenario.name'], 
                              category = ifelse('scenario.cat' %in% names(cur.args), 
                                                cur.args['scenario.cat'], 
                                                NA))
                
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
# Import model interleave                                       ----
#------------------------------------------------------------------------------|

if (exists("interleave.models.list")) {
    # go through all files in this list
    for (item in interleave.models.list) {
        cur.fname = item[[1]]
        cur.template.fuel.name = item[["template.fuel"]]
        cur.template.object.name = item[["template.object"]]
        cur.interleave = item[["interleave"]]
        
        # correct interleave
        if (is.null(cur.interleave)) cur.interleave <- FALSE
        
        # make sure interleave file and template file both exist 
        if (all(
            file.exists(file.path(inputfiles.dir, cur.fname)) & 
            file.exists(file.path(inputfiles.dir, cur.template.fuel.name)) &
            ifelse(!is.null(cur.template.object.name[1]), 
                   file.exists(file.path(inputfiles.dir, cur.template.object.name)),
                   TRUE) # only check for cur.template.obj if it exists
        )) {
            
            if (is.null(cur.template.object.name[1])) {
                message(sprintf("... interleaving models in %s, using template in %s",
                                cur.fname, cur.template.fuel.name))
            } else {
                message(sprintf("... interleaving models in %s, using templates in %s and %s",
                                cur.fname, cur.template.fuel.name, 
                                paste0(cur.template.object.name, collapse = ", ")))
            }
            
            
            # do actual work  
            # parse this file
            cur.tab = read_data(cur.fname)
            cur.template.fuel = read_data(cur.template.fuel.name)
            
            # if cur.template.object exists, grab it, handling as a list since
            # could be any number of templates
            if (!is.null(cur.template.object.name[1])) (
                cur.template.object = lapply(cur.template.object.name, 
                                             function(x) read_data(x))
            )
            
            # need to make a datafile object for each property to be passed down
            # it will have filepointers to the datafile to be passed, in 
            # scenarios.
            # this datafile object will be attached with no scenario to that 
            # property of all applicable objects
            # NOTE tihs means that this formulation does not currently support
            #   one scenario where a property is passed to all generators and
            #   another scenario where the same property is passed to only half
            #   the generators (since the datafile object will be attached to
            #   all of the generators)
            
            # ---- first, process templates
            
            # if something exists in the "property" row, then process it. 
            # 1 - change names of datafile objects to include property
            # 2 - create name to property vector to map back to properties later
            #     (inside function)
            # 3 - if the object exists but its property doesn't, then add that 
            #     property back to the object
            
            if ("Attribute" %in% cur.template.fuel$Fuel) {
                
                # if this exists, add the propname to the colname to be able
                # to uniquely identify datafile objects with different 
                # properties, and keep track of what properties need to be added
                # later. The colon will be an indicator later about whether
                # names should be split
                
                # column names to set names
                props <- unlist(cur.template.fuel[Fuel == "Attribute"])
                props["Fuel"] <- ""
                props[props != ""] <- paste0(": ", props[props != ""])
                
                setnames(cur.template.fuel, 
                         paste0("Pass ", names(props), props))
                setnames(cur.template.fuel, "Pass Fuel", "Fuel") # hacky but... 
                
                # grab table of properties to set
                dfo.props <- cur.template.fuel[Fuel == "Attribute"]
                dfo.props <- melt(dfo.props, 
                                  measure.vars = colnames(dfo.props),
                                  variable.name = "name",
                                  value.name = "attribute")
                
                # get rid of NAs, blanks, the fuel column 
                dfo.props <- dfo.props[!is.na(attribute) & 
                                           !(attribute %in% c("", "Attribute"))]
                
                # separate names and properties and format to be added to 
                # attributes sheet
                dfo.props[,c("attribute", "value") := tstrsplit(attribute, 
                                                                "=| = | =|= ")]
                
                dfo.props[,class := "Data File"]
                
                # dfo props will be used later, after making sure all df objects
                # exist, to add attributes
                
                # remove attribute row from cur.template.fuel
                cur.template.fuel <- cur.template.fuel[Fuel != "Attribute"]
                
                
            } else {
                
                # set names with no property specification
                prop.cols <- names(cur.template.fuel)
                prop.cols <- prop.cols[prop.cols != "Fuel"]
                
                setnames(cur.template.fuel, prop.cols, paste("Pass", prop.cols))
            }
            
            # check if any of these datafile objets aren't in objects sheet. 
            # if they aren't, add them
            all.propnames <- names(cur.template.fuel)
            all.propnames <- all.propnames[all.propnames != "Fuel"]
            
            missing.propnames = all.propnames[
                !(all.propnames %in% Objects.sheet[class == "Data File", name])]
            
            if (length(missing.propnames) > 0) {
                dfobj.to.obects = initialize_table(Objects.sheet, 
                                                   length(missing.propnames), 
                                                   list(class = "Data File",
                                                        name = missing.propnames, 
                                                        category = "Pass properties"))
                
                Objects.sheet <- merge_sheet_w_table(Objects.sheet, dfobj.to.obects)
                
                rm(dfobj.to.obects)
            }
            
            # add specified properties to datafile objects, if they were 
            # specified
            if (exists("dfo.props") && nrow(dfo.props) > 0) {
                
                Attributes.sheet <- merge_sheet_w_table(Attributes.sheet, 
                                                        dfo.props)
            }
            
            # change blanks to NAs (easier to handle) and check that template 
            # doesn't have more than one file pointer per col
            if (length(which(cur.template.fuel == "")) > 0) {
                for (j in seq_len(ncol(cur.template.fuel))) {
                    set(cur.template.fuel,which(cur.template.fuel[[j]] == ""),j,NA)    
                }
            }
            
            if (cur.template.fuel[, any(sapply(.SD, function(x) length(unique(na.omit(x))) > 1)), 
                                  .SDcols = -1]) {
                
                message(sprintf(paste(">>  all filepointers in template %s are",
                                      "not identical. this will not be read correctly ... skipping"),
                                cur.template.fuel.name))
                
                cur.template.fuel <- NA
            }
            
            # same, but for object templates if they exist
            if (!is.null(cur.template.object.name)) {
                for (i in seq_along(cur.template.object)) {
                    
                    # change blanks to NA if there are any 
                    for (j in seq_len(ncol(cur.template.object[[i]]))) {
                        if (length(which(cur.template.object[[i]][[j]] == "") > 0)) {
                            set(i,which(cur.template.object[[i]][[j]] == ""),j,NA)    
                        }
                    }
                    
                    if (cur.template.object[[i]][,
                                                 any(sapply(.SD, function(x) length(unique(na.omit(x))) > 1)), .SDcols = -1]) {
                        
                        message(sprintf(paste(">>  all filepointers in template %s are",
                                              "not identical. this will not be read correctly ... skipping"),
                                        cur.template.object.name[i]))
                        
                        cur.template.object[[i]] <- NA
                        
                        break()
                    }
                    
                    # add datafile objects
                    
                    # set names with no property specification
                    cur.prop.cols <- names(cur.template.object[[i]])
                    cur.prop.cols <- cur.prop.cols[-1] 
                    
                    # setnames(cur.template.object[[i]], cur.prop.cols, 
                    #          paste("Pass", cur.prop.cols))
                    
                    # check for existence of datafile objects, add them if don't already exist
                    # check if any of these datafile objets aren't in objects sheet. 
                    # if they aren't, add them
                    cur.all.propnames <- paste("Pass", cur.prop.cols)
                    
                    missing.propnames = cur.all.propnames[
                        !(cur.all.propnames %in% Objects.sheet[class == "Data File", name])]
                    
                    if (length(missing.propnames) > 0) {
                        dfobj.to.obects = initialize_table(Objects.sheet, 
                                                           length(missing.propnames), 
                                                           list(class = "Data File",
                                                                name = missing.propnames, 
                                                                category = "Pass properties"))
                        
                        Objects.sheet <- merge_sheet_w_table(Objects.sheet, dfobj.to.obects)
                        
                        rm(dfobj.to.obects)
                    } 
                    
                }
                rm(i,j)
            }
            
            # ---- second, interleave models using templates
            
            for (i in 1:nrow(cur.tab)) {
                # pass to function that will add filepointers to datafile 
                # objects under the right scenario and add datafile objects to 
                # properties (with no scenario) if they aren't already there
                # NOTE this will overwrite data in these properties that is
                #   already defined
                make_interleave_pointers(
                    parent.model = cur.tab[i, parent.model],
                    child.model = cur.tab[i, child.model],
                    filepointer.scenario = cur.tab[i, filepointer.scenario],
                    datafileobj.scenario = cur.tab[i, datafileobj.scenario],
                    template.fuel = cur.template.fuel,
                    template.object = ifelse(exists("cur.template.object"), 
                                             cur.template.object, NA),
                    interleave = cur.interleave)
            }
            
            
            # rm(cur.fname, cur.template.fuel, cur.tab, 
            # all.propnames, missing.propnames, cur.template.fuel.name,
            # cur.template.object.name)
            # 
            # if (exists("cur.template.object)) rm(cur.template.object)
            
        } else {
            if (is.null(cur.template.object.name)) {
                message(sprintf(">>  %s or %s does not exist ... skipping", 
                                cur.fname, cur.template.fuel.name))
            } else 
                message(sprintf(">>  %s or %s or %s does not exist ... skipping", 
                                cur.fname, cur.template.fuel.name, 
                                paste0(cur.template.object.name, collapse = ", ")))
        }
    }
} else {
    message('>>  interleave.models.list does not exist ... skipping')
}
