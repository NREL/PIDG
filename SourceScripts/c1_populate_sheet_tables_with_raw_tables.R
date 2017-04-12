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
    
    if (exists("node.file")) {
        node.data.table <- read_data(node.file)
    } else {
        message(sprintf(">>  node.file does not exist ... skipping"))
    }
    
    if (exists("line.file")) {
        line.data.table <- read_data(line.file)
    } else {
        message(sprintf(">>  line.file does not exist ... skipping"))
    }
    
    if (exists("generator.file")) {
        generator.data.table <- read_data(generator.file)
    } else {
        message(sprintf(">>  generator.file does not exist ... skipping"))
    }
    
    if (exists("transformer.file")) {
        transformer.data.table <- read_data(transformer.file)
    } else {
        message(sprintf(">>  transformer.file does not exist ... skipping"))
    }
    
    if (exists("load.file")) {
        load.data.table <- suppressWarnings(read_data(load.file)) 
        # for bumping load type col to character type in posoco
    } else {
        message(sprintf(">>  load.file does not exist ... skipping"))
    }
    
}

if (exists("node.data.table") && is.data.table(node.data.table)) {
    
    #--------------------------------------------------------------------------|
    # nodes ----
    #--------------------------------------------------------------------------|
    
    message("arranging node data")
    
    # check for object dupes and capitalization errors in 'category', 'notes' 
    check_for_dupes(node.data.table, "Node")
    check_colname_cap(node.data.table)
    
    # if needed, remap regions and/or zones
    if (choose.input == "raw.psse") {
        # if there are input files to remap the nodes' regions and zones, remap
        if (rename.regions) { 
          
          map.newregions <- read_data(map.newregion.file)
          
          if (is.data.table(map.newrgions)) {
              node.data.table <- merge(node.data.table[,Region := NULL], 
                                   map.newregions[,.(Node, Region)], 
                                   by = "Node", 
                                   all.x = TRUE)
          }
        }
        
        if (rename.zones) {
        
          map.newzones <- read_data(map.newzone.file)
          
          if (is.data.table(map.newzones)) {
              node.data.table <- merge(node.data.table[,Zone := NULL], 
                                   map.newzones[,.(Node, Zone)], 
                                   by = "Node", 
                                   all.x = TRUE)
          }
        }
    }
    
    # the Units property is required
    if (!("Units" %in% colnames(node.data.table))) {
        message("No Units specified for nodes ... setting Units = 1 for all")
        node.data.table[, Units := 1]    
    }
    
    # use category column if it exists; otherwise, categorize by region
    if (!("category" %in% colnames(node.data.table))){
        if ("Region" %in% colnames(node.data.table)) {
            node.data.table[, category := Region]   
        } else {
            node.data.table[, category := NA]
        }
    }
    
    # make sure blanks are turned into NAs 
    node.data.table[category %in% c("", " "), category := NA]
    
    
    #--------------------------------------------------------------------------|
    # Add nodes to .sheet tables ----
    #--------------------------------------------------------------------------|
    
    # add nodes to object .sheet
    
    import_objects(node.data.table)
    
    # add node properties
    # TODO: for now, there is some membership data included in the node.data.tab 
    # for now, specifically exclude that. in the future, should separated or 
    # differentiate membership and proprerty data
    
    # what cols should not be considered properties? ('Owner' is relic of PSSE)
    excluded.cols <- c("notes", "category", "Region", "Zone", "Owner")
    excluded.cols <- excluded.cols[excluded.cols %in% names(node.data.table)]
    
    nodes.to.properties <- node.data.table[,!excluded.cols, with = FALSE]
    
    import_properties(nodes.to.properties, names.col = "Node")
    
    # clean up
    rm(nodes.to.properties, excluded.cols)
    
    
    #--------------------------------------------------------------------------|
    # Add regions to .sheet tables ----
    #--------------------------------------------------------------------------|
    
    if ("Region" %in% names(node.data.table) && 
        node.data.table[!is.na(Region) & !(Region %in% c("", " ")), .N] > 0) {
        
        message("... adding Regions")
        
        # add regions to object .sheet
        node.regions <- node.data.table[!is.na(Region) & 
                                            !(Region %in% c("", " "))]
        all.regions <- node.regions[,.(Region = unique(Region))]
        
        import_objects(all.regions)
        
        # add node-region membership to memberships .sheet
        import_memberships(node.regions[,.(Node, Region_Region = Region)])
        
        # clean up
        rm(node.regions, all.regions)
        
    } else {
        stop(paste("At least one region is required. Please add a",
                   "Region column to node.file"))
    }
    
    #--------------------------------------------------------------------------|
    # Add zones to .sheet tables ----
    #--------------------------------------------------------------------------|
    
    if ("Zone" %in% names(node.data.table) && 
        node.data.table[!is.na(Zone) & !(Zone %in% c("", " ")), .N] > 0) {
        
        message("... adding Zones")
        
        # add zones to objects .sheet
        node.zones <- node.data.table[!is.na(Zone) & !(Zone %in% c("", " "))]
        all.zones <- node.zones[,.(Zone = unique(Zone))]
        
        import_objects(all.zones)
        
        # add zone-region membership to memberships .sheet
        import_memberships(node.zones[,.(Node, Zone_Zone = Zone)])
        
        # clean up
        rm(node.zones, all.zones)
    }
    
}
# if (exists("node.data.table") && is.data.table(node.data.table)) 
    

if (exists("line.data.table") && is.data.table(line.data.table)) {
    
    #--------------------------------------------------------------------------|
    # lines ----
    #--------------------------------------------------------------------------|
    
    message("arranging line data")
    
    # check for object dupes and capitalization errors in 'category', 'notes' 
    check_for_dupes(line.data.table, "Line")
    check_colname_cap(line.data.table)
    
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
    
    # the Units property is required
    if (!("Units" %in% colnames(line.data.table))) {
        message("No Units specified for lines ... setting Units = 1 for all")
        line.data.table[, Units := 1]    
    }
    
    # use category column if it exists; otherwise, categorize by region, ac/dc
    if (!("category" %in% colnames(line.data.table))) {
        
        if ("Reactance" %in% colnames(line.data.table)) {
            # category is AC/DC AND region
            
            # categorize by AC/DC (check what happens w/ Reactance = 0 -- 
            # should be AC?)
            line.data.table[is.na(Reactance) | Reactance == 0, Type := "DC"]
            line.data.table[!(is.na(Reactance) | Reactance == 0), Type := "AC"]
            
            # add category
            line.data.table[Region.From == Region.To, 
                            category := paste0(Type, "_", Region.From)]
            
            line.data.table[Region.From != Region.To, 
                            category := paste0("Interregion_", Type)]
        
            line.data.table[, Type := NULL]
            
        } else {
            # category is just region
            line.data.table[Region.From == Region.To, 
                            category := paste0(Region.From)]
            
            line.data.table[Region.From != Region.To, 
                            category := paste0("Interregion")]
        }
            
        # clean up
        line.data.table[,c("Region.From", "Region.To") := NULL]
        
    }
    
    # make sure blanks are turned into NAs 
    line.data.table[category %in% c("", " "), category := NA]
    
    
    #--------------------------------------------------------------------------|
    # Add lines to .sheet tables ----
    #--------------------------------------------------------------------------|
      
    # add lines to objects .sheet
    
    import_objects(line.data.table, object.col = "Line")
    
    # add lines to memberships .sheet
    import_memberships(line.data.table[, .(Line,
                                           `Node From_Node` = `Node From`,
                                           `Node To_Node` = `Node To`)])
    
    # add lines properties
    
    # what columns should not be considered properties? (everything after 
    # 'Node To' is relic from PSSE parsing)
    excluded.cols <- c("notes", "category", "Node From", "Node To", 
                       "Voltage.From", "Voltage.To", "ratingA", "ratingB", 
                       "ratingC", "rateA", "rateB", "rateC", "Status", "Length",
                       "Region.From", "Region.To")
    
    excluded.cols <- excluded.cols[excluded.cols %in% names(line.data.table)]
    
    lines.to.properties <- line.data.table[,!excluded.cols, with = FALSE]
    
    import_properties(lines.to.properties, names.col = "Line")
    
    # clean up
    rm(excluded.cols, lines.to.properties)

} # end if (exists("line.data.table")  && is.data.table(line.data.table))

if (exists("generator.data.table") && is.data.table(generator.data.table)) {

    #--------------------------------------------------------------------------|
    # generators ----
    #--------------------------------------------------------------------------|
    
    message("arranging generator data")
    
    # check for object dupes and capitalization errors in 'category', 'notes' 
    # only allow duplicates in generator if Node Participation exists (TODO)
    if ("Generation Participation Factor" %in% names(generator.data.table)) {
        check_for_dupes(generator.data.table, c("Generator", "Node"))
    } else {
        check_for_dupes(generator.data.table, "Generator")
    }
    
    check_colname_cap(generator.data.table)
    
    # add region
    generator.data.table <- merge(generator.data.table, 
                                  node.data.table[,.(Node, Region)],
                                  by = "Node", 
                                  all.x = TRUE)
    
    if (!("Units" %in% colnames(generator.data.table))) {
        message(paste("No Units specified for generators ... setting",
                      "Units = 1 for all"))
        generator.data.table[, Units := 1]    
    }
    
    # use category column if it exists; otherwise, categorize by region
    if (!("category" %in% colnames(generator.data.table))){
        generator.data.table[, category := Region]   
    }
    
    # make sure blanks are turned into NAs 
    generator.data.table[category %in% c("", " "), category := NA]
    
    
    #--------------------------------------------------------------------------|
    # temp until decide what to do with this: adjust max cap ----
    #--------------------------------------------------------------------------|
    
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
    
    
    #--------------------------------------------------------------------------|
    # Add generators to .sheet tables ----
    #--------------------------------------------------------------------------|
    
    # because might have dupes, take unique values
    gen.object <- unique(generator.data.table, by = c("Generator", "category"))
    
    # add generators to objects .sheet, categorizing by region
    import_objects(gen.object, object.col = "Generator")
    
    # add generator-node membership to memberships .sheet
    import_memberships(generator.data.table[,.(Generator, Nodes_Node = Node)])
    
    # add generator properties
    
    # special case: if there is a 'Generation Participation Factor' column, 
    # split off that column with add add it to Generator.Nodes objects
    if ("Generation Participation Factor" %in% names(generator.data.table)) {
        
        # add Generation Participation Factor to Generator.Nodes Nodes
        gen.nodes.props <- generator.data.table[,.(Node, Generator,
                                                   `Generation Participation Factor`)]
            
        import_properties(gen.nodes.props, parent.col = "Generator")
        
        # get rid of dupes so can add other properties if they exist
        gen.props <- generator.data.table[,!c("Node", 
                                              "Generation Participation Factor"), 
                                          with = FALSE]
        
        if (uniqueN(gen.props) != gen.props[, uniqueN(Generator)]) {
            stop(paste0("At least one generator in generator.data.table is ",
                        "repeated and does not have identical information in ",
                        "each non-Node/non-GPF entry. Please check."))
        }
        
        gen.props <- unique(gen.props)
        
        # clean up
        rm(gen.nodes.props)
    } else {
        
        gen.props <- generator.data.table
    }
    
    # special case: add fuels and start fuels if they exist: fix this later
    fuel.cols <- names(generator.data.table)
    fuel.cols <- fuel.cols[fuel.cols %in% c("Fuel", "Start Fuel")]
    
    if (length(fuel.cols > 0)) {
        
        # add fuel objects 
        fuels <- unique(c(sapply(fuel.cols, 
                                 function(x) generator.data.table[,get(x)])))
        
        fuels <- fuels[!(fuels %in% c(NA, "", " "))]
        
        import_objects(data.table(Fuel = fuels))
        
        # format properly, then add memberships
        fuel.membs <- generator.data.table[, c("Generator", fuel.cols), 
                                           with = F]
        
        setnames(fuel.membs, fuel.cols, paste0(fuel.cols, "s_Fuel"))
    
        import_memberships(fuel.membs)    
        
        # clean up
        rm(fuels,fuel.membs)
        
    }
    
    # what columns should not be considered properties? (everything after 
    # 'Node' is relic from PSSE parsing)
    excluded.cols <- c("notes", "category", "Region", 
                       "Node", "Status", "Fuel", "Start Fuel", 
                       grep("Owner", names(gen.props), value = TRUE))
    
    excluded.cols <- excluded.cols[excluded.cols %in% names(gen.props)]
    
    gens.to.properties <- gen.props[,!excluded.cols, with = FALSE]
    
    import_properties(gens.to.properties, names.col = "Generator")
    
    # clean up
    rm(gen.object, gens.to.properties, excluded.cols, gen.props, fuel.cols)

} # end if (exists("generator.data.table") && is.data.table(generator.data.table))


if (exists("transformer.data.table") && is.data.table(transformer.data.table)) {
    #--------------------------------------------------------------------------|
    # transformers ----
    #--------------------------------------------------------------------------|

    message("arranging transformer data")
    
    # check for object dupes and capitalization errors in 'category', 'notes' 
    check_for_dupes(transformer.data.table, "Transformer")
    check_colname_cap(transformer.data.table)
    
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
    
    # the Units property is required
    if (!("Units" %in% colnames(transformer.data.table))) {
        
        message(paste("No Units specified for transformers ... setting",
                      "Units = 1 for all"))
        transformer.data.table[, Units := 1]    
    }
    
    # use category column if it exists; otherwise, categorize by region
    if (!("category" %in% colnames(transformer.data.table))){
        
        transformer.data.table[Region.From == Region.To, 
                               category := Region.From]
        
        transformer.data.table[Region.From != Region.To, 
                               category := "Interregion_tfmr"]
    }
    
    # make sure blanks are turned into NAs 
    transformer.data.table[category %in% c("", " "), category := NA]

    
    #--------------------------------------------------------------------------|
    # Add transformers to .sheet tables ----
    #--------------------------------------------------------------------------|

    # add transformers to objects .sheet
    
    import_objects(transformer.data.table, object.col = "Transformer")

    # add transformer-node membership to memberships .sheet
    import_memberships(transformer.data.table[, .(Transformer,
                                                  `Node From_Node` = `Node From`,
                                                  `Node To_Node` = `Node To`)])

    # add transformers to properties .sheet

    # what columns should not be considered properties? (everything after
    # 'Node To' is relic from PSSE parsing)
    excluded.cols <- c("notes", "category", "Node From", "Node To", 
                       "Voltage.From", "Voltage.To", "Status", 
                       "Region.From", "Region.To")
    
    excluded.cols <- excluded.cols[excluded.cols %in% 
                                       names(transformer.data.table)]
    
    transf.to.properties <- transformer.data.table[,!excluded.cols, 
                                                   with = FALSE]
    
    import_properties(transf.to.properties, names.col = "Transformer")
    
    # clean up
    rm(transf.to.properties)

} # end if (exists("transformer.data.table") && is.data.table(transformer.data.table))

