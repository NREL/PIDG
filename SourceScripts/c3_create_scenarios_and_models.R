#create other models/scenarios

#uses:
# load.file.path
# import.horizon.mayJune.file
# import.model.baseAgTx.file
# import.model.mayJune.file
# import.model.mayJuneAgTx.file
# enforced.interstate.lines.file
# isolated.nodes.to.remove.file
# compact.generic.import.files
# constraint.import.files

#make components of scenarios

#------------------------------------------------------------------------------|
# [[Tx configuration]] Aggregate transmission scenario ----
# -----------------------------------------------------------------------------|

if (exists("make.aggtx.scenario") && make.aggtx.scenario == TRUE) {
    
    #turn on "Aggregate Transmission" property for each region and assign a 
    #reference node to each region
    
    #scenario to objects
    scenario.agTx.to.objects <- initialize_table(Objects.sheet, 1, list(
        class = "Scenario", name = "Aggregate transmission in all regions", 
        category = "Transmission configuration"))
    Objects.sheet <- merge_sheet_w_table(Objects.sheet, scenario.agTx.to.objects)
    
    #set aggregate Tx to 1 in each region
    #uses regions.to.objects
    all.regions <- unique(node.data.table[,Region])
    
    agTx.to.properties <- initialize_table(Properties.sheet, 
                                           length(all.regions), 
                                           list(parent_class = "System",
                                                child_class = "Region", 
                                                collection = "Regions", 
                                                parent_object = "System", 
                                                band_id = 1, 
                                                property = "Aggregate Transmission", 
                                                value = -1,
                                                scenario = "{Object}Aggregate transmission in all regions"))
    
    agTx.to.properties[, child_object := all.regions]
    
    Properties.sheet <- merge_sheet_w_table(Properties.sheet, agTx.to.properties)
    
    # add one node as reference node to every region
    # uses node.data.table, remap.reference.nodes, map.ref.node.file
    
    # if there is an external file and this option is turned on, grab it and 
    # note and regions that aren't included
    if (exists('remap.reference.nodes')) {
        
        if (remap.reference.nodes == TRUE) {
            
            external.refnode <- read_data(map.ref.node.file)
            
            if (is.data.table(external.refnode)) {
                
                # keep track of what regions aren't in this file to assign ref node to them
                other.regions <- node.data.table[,unique(Region)]
                other.regions <- other.regions[!(other.regions %in% 
                                                     external.refnode[,unique(Region)])]
                
            }} else {
                message(paste('... remap.reference.nodes is FALSE.',
                              'Assigning first node in each region as reference node'))
                other.regions <- node.data.table[,unique(Region)]
            }
    } else {
        message(paste('... remap.reference.nodes doesn\'t exist.',
                      'Assigning first node in each region as reference node'))
        other.regions <- node.data.table[,unique(Region)]
    }
    
    if (!exists("other.regions")) {
        other.regions <- character()
    }
    
    # for any missing regions, grab a reference node and output full table, 
    # then combine all refnode tables so that all info is contained in 
    # ref.node.region.table
    if (length(other.regions) > 0) {
        
        message(sprintf('... Assigning reference node as first node in %s', 
                        paste0(other.regions, collapse = ', ')))
        
        ref.node.region.table <- node.data.table[Region %in% other.regions, 
                                                 .(Region = Region, 
                                                   `Region.Reference Node` = Node)]
        
        ref.node.region.table <- ref.node.region.table[!duplicated(Region),]
        
        if (exists('external.refnode')) {
            ref.node.region.table <- merge(ref.node.region.table, external.refnode, 
                                           by = c('Region', 'Region.Reference Node'), all = T)
        } 
    } else {
        ref.node.region.table <- external.refnode
    }
    
    agTx.refnode.region.to.memberships <- initialize_table(Memberships.sheet, 
                                                           nrow(ref.node.region.table), 
                                                           list(parent_class = "Region", 
                                                                child_class = "Node", 
                                                                collection = "Reference Node"))
    
    agTx.refnode.region.to.memberships[, parent_object := 
                                           ref.node.region.table[,Region]]
    agTx.refnode.region.to.memberships[, child_object := 
                                           ref.node.region.table[,`Region.Reference Node`]]
    
    Memberships.sheet <- merge_sheet_w_table(Memberships.sheet, 
                                             agTx.refnode.region.to.memberships)
    
    # clean up
    rm(ref.node.region.table, agTx.refnode.region.to.memberships, other.regions, 
       scenario.agTx.to.objects, agTx.to.properties)
}

#------------------------------------------------------------------------------|
# [[Tx onfiguration]] Add scen. tag to interfaces ----
# -----------------------------------------------------------------------------|

if (exists("make.interface.scenario") && make.interface.scenario == TRUE) {
    # ---- ZONAL INTERFACES
    interface.names <- Objects.sheet[class == "Interface" & 
                                         category == 'Zonal interfaces', name]
    
    # add dummy min up, min down, start costs to objects
    interf.scen.to.objects <- initialize_table(Objects.sheet, 
                                               1, 
                                               list(class = "Scenario", 
                                                    name = "Include zonal interfaces", 
                                                    category = "Transmission configuration"))
    Objects.sheet <- merge_sheet_w_table(Objects.sheet, interf.scen.to.objects)
    
    # add scenario tag to these properties
    # uses interface.names
    interf.off.to.properties <- initialize_table(Properties.sheet, 
                                                 length(interface.names), 
                                                 list(parent_class = "System", 
                                                      parent_object = "System",
                                                      collection = "Interfaces", 
                                                      child_class = "Interface", 
                                                      band_id = 1,    
                                                      child_object = interface.names, 
                                                      property = "Units", value = 0))
    
    interf.scen.to.properties <- initialize_table(Properties.sheet, 
                                                  length(interface.names), 
                                                  list(parent_class = "System", 
                                                       parent_object = "System", 
                                                       collection = "Interfaces", 
                                                       child_class = "Interface", 
                                                       band_id = 1, 
                                                       child_object = interface.names, 
                                                       property = "Units", 
                                                       value = 1, 
                                                       scenario = "{Object}Include zonal interfaces"))
    
    Properties.sheet <- merge_sheet_w_table(Properties.sheet, 
                                            interf.scen.to.properties)
    Properties.sheet <- merge_sheet_w_table(Properties.sheet, 
                                            interf.off.to.properties)
    
    # ---- REGIONAL INTERFACES
    interface.names <- Objects.sheet[class == "Interface" & 
                                         category == 'Regional interfaces', name]
    
    # add dummy min up, min down, start costs to objects
    interf.scen.to.objects <- initialize_table(Objects.sheet, 
                                               1, 
                                               list(class = "Scenario", 
                                                    name = "Include regional interfaces", 
                                                    category = "Transmission configuration"))
    
    Objects.sheet <- merge_sheet_w_table(Objects.sheet, interf.scen.to.objects)
    
    # add scenario tag to these properties
    # uses interface.names
    interf.off.to.properties <- initialize_table(Properties.sheet, 
                                                 length(interface.names), 
                                                 list(parent_class = "System", 
                                                      parent_object = "System", 
                                                      collection = "Interfaces", 
                                                      child_class = "Interface",
                                                      band_id = 1,
                                                      child_object = interface.names, 
                                                      property = "Units", 
                                                      value = 0))
    
    interf.scen.to.properties <- initialize_table(Properties.sheet, 
                                                  length(interface.names), 
                                                  list(parent_class = "System", 
                                                       parent_object = "System",
                                                       collection = "Interfaces", 
                                                       child_class = "Interface", 
                                                       band_id = 1, 
                                                       child_object = interface.names, 
                                                       property = "Units",
                                                       value = 1, scenario = "{Object}Include regional interfaces"))
    
    Properties.sheet <- merge_sheet_w_table(Properties.sheet, 
                                            interf.scen.to.properties)
    Properties.sheet <- merge_sheet_w_table(Properties.sheet, 
                                            interf.off.to.properties)
}
    
# #------------------------------------------------------------------------------|
# # [[Tx configuration]] Don't enforce intERstate lines ----
# # -----------------------------------------------------------------------------|
# # scneario to objects
# scenario.no.intrastate.lines <- initialize_table(Objects.sheet, 
#                                                  1, 
#                                                  list(class = "Scenario", 
#                                                       name = "Turn off interstate lines", 
#                                                       category = "Transmission configuration"))
# Objects.sheet <- merge_sheet_w_table(Objects.sheet, 
#                                      scenario.no.intrastate.lines)
# 
# # scenario to properties
# # uses line.data.table
# interstate.lines <- line.data.table[grepl("Interstate", category), Line]
# scenario.no.inters.lines.to.propterties <- initialize_table(Properties.sheet, 
#                                                             length(interstate.lines), 
#                                                             list(parent_class = "System", 
#                                                                  child_class = "Line", 
#                                                                  parent_object = "System", 
#                                                                  band_id = 1, 
#                                                                  collection = "Lines"))
# 
# scenario.no.inters.lines.to.propterties[,child_object := interstate.lines]
# scenario.no.inters.lines.to.propterties[,property := "Enforce Limits"]
# scenario.no.inters.lines.to.propterties[,value := "0"]
# scenario.no.inters.lines.to.propterties[,scenario := "{Object}Turn off interstate lines"]
# 
# Properties.sheet <- merge_sheet_w_table(Properties.sheet, 
#                                         scenario.no.inters.lines.to.propterties)


# #------------------------------------------------------------------------------|
# # [[Tx configuration]] Don't enforce intRAstate lines ----
# # -----------------------------------------------------------------------------|
# # scneario to objects
# scenario.no.intrastate.lines <- initialize_table(Objects.sheet, 
#                                                  1, 
#                                                  list(class = "Scenario", 
#                                                       name = "For PsN - don't enforce intrastate lines", 
#                                                       category = "Transmission configuration"))
# Objects.sheet <- merge_sheet_w_table(Objects.sheet, 
#                                      scenario.no.intrastate.lines)
# 
# # scenario to properties
# # uses line.data.table
# intrastate.lines <- line.data.table[!grepl("Interstate", category), Line]
# scenario.no.intras.lines.to.propterties <- initialize_table(Properties.sheet, 
#                                                             length(intrastate.lines), 
#                                                             list(parent_class = "System", 
#                                                                  child_class = "Line", 
#                                                                  parent_object = "System", 
#                                                                  band_id = 1, 
#                                                                  collection = "Lines"))
# 
# scenario.no.intras.lines.to.propterties[,child_object := intrastate.lines]
# scenario.no.intras.lines.to.propterties[,property := "Enforce Limits"]
# scenario.no.intras.lines.to.propterties[,value := "0"]
# scenario.no.intras.lines.to.propterties[, scenario := "{Object}For PsN - don't enforce intrastate lines"]
# 
# Properties.sheet <- merge_sheet_w_table(Properties.sheet, 
#                                         scenario.no.intras.lines.to.propterties)


if (choose.input == "raw.psse") {

    #--------------------------------------------------------------------------|
    # [[Add standard data]] Add standard ratings to lines ----
    # -------------------------------------------------------------------------|
    #scenario to objects
    scenario.line.MW.std.to.objects <- initialize_table(Objects.sheet, 
                                                        1, 
                                                        list(class = "Scenario", 
                                                             name = "Add Standard Line Flow Lims",
                                                             category = "Add standard data"))
    Objects.sheet <- merge_sheet_w_table(Objects.sheet,
                                         scenario.line.MW.std.to.objects)
    
    # add standard flow limits to lines with ratings of zero
    # uses line.data.table
    zero.flow.lines <- line.data.table[`Max Flow` == "0"]
    
    # defines stadards with [name = kV level] = [element = MW flow limit]
    standard.flow.lims <- c("132" = "80", "220" = "200", "400" = "870", 
                            "765" = "2200",
                            # these are from looking at most commnon flow limits on these lines
                            "11" = "30", "33" = "33", "66" = "28", "100" = "80", 
                            "110" = "80", "230" = "200",
                            # and these ones are rough guesses
                            "0.6" = "10", "69" = "30", "115" = "80", 
                            "22.9" = "20", "34.5" = "34.5", "13.8" = "30", 
                            "138" = "150")
    
    max.flow.correction <- initialize_table(Properties.sheet,
                                            nrow(zero.flow.lines),
                                            list(parent_class = "System", 
                                                 child_class = "Line",
                                                 collection = "Lines", 
                                                 parent_object =  "System",
                                                 child_object = zero.flow.lines[,Line], 
                                                 band_id = 1,
                                                 scenario = "{Object}Add Standard Line Flow Lims"))
    
    invisible(lapply(names(standard.flow.lims), 
                     function(kV.level) {
                         max.flow.correction[(child_object %in% zero.flow.lines[Voltage.From == kV.level,Line]),
                                             c("property", "value") := list("Max Flow", standard.flow.lims[[kV.level]] )]  }))
    
    min.flow.correction <- initialize_table(Properties.sheet,
                                            nrow(zero.flow.lines), 
                                            list(parent_class = "System", 
                                                 child_class = "Line",
                                                 collection = "Lines", 
                                                 parent_object =  "System",
                                                 child_object = zero.flow.lines[,Line], 
                                                 band_id = 1,
                                                 scenario = "{Object}Add Standard Line Flow Lims"))
    
    invisible(lapply(names(standard.flow.lims), 
                     function(kV.level) {min.flow.correction[(child_object %in% zero.flow.lines[Voltage.From == kV.level,Line]), 
                                                             c("property", "value") := list("Min Flow", paste0("-",standard.flow.lims[[kV.level]]) )]  }))
    
    Properties.sheet <- merge_sheet_w_table(Properties.sheet, max.flow.correction)
    Properties.sheet <- merge_sheet_w_table(Properties.sheet, min.flow.correction)
    
} # end if (choose.input == "raw.psse")

if (choose.input == "raw.psse") {
    
    #--------------------------------------------------------------------------|
    # [[Add standard data]] Add standard ratings to transformers ----
    # -------------------------------------------------------------------------|
    #scenario to objects
    scenario.tfmr.MW.std.to.objects <- initialize_table(Objects.sheet, 
                                                        1, 
                                                        list(class = "Scenario", 
                                                             name = "Add Standard Tfmr Ratings",
                                                             category = "Add standard data"))
    
    Objects.sheet <- merge_sheet_w_table(Objects.sheet,
                                         scenario.tfmr.MW.std.to.objects)
    
    # add standard flow limits to lines with ratings of zero
    # uses transformer.data.table
    
    zero.flow.tfmrs <- transformer.data.table[Rating == 0]
    
    # defines stadards with [name = low kV (kV.To)] = [element = standard rating]
    standard.flow.tfmr.lims <- c("220" = "315", "132" = "100", "110" = "100",
                                 "66" = "100", "69" = "100", "138" = "100", 
                                 "13.8" = "100")
    
    tfmr.rating.correction <- initialize_table(Properties.sheet,
                                               nrow(zero.flow.tfmrs), 
                                               list(parent_class = "System",
                                                    child_class = "Transformer", 
                                                    collection = "Transformers",
                                                    parent_object =  "System", 
                                                    child_object = zero.flow.tfmrs[,Transformer],
                                                    band_id = 1, 
                                                    scenario = "{Object}Add Standard Tfmr Ratings"))
    
    invisible(lapply(names(standard.flow.tfmr.lims), 
                     function(kV.level) {tfmr.rating.correction[(child_object %in% zero.flow.tfmrs[Voltage.To == kV.level,Transformer]), 
                                                                c("property", "value") := list("Rating", standard.flow.tfmr.lims[[kV.level]])]  }))
    
    Properties.sheet <- merge_sheet_w_table(Properties.sheet,
                                            tfmr.rating.correction)
    
} # end if (choose.input == "raw.psse")


#------------------------------------------------------------------------------|
# [[Scenario archive for other configurations]] Set line reactance to zero ----
# -----------------------------------------------------------------------------|
# hopefully, this forces model to run transport instead of DCOPF

if (exists("make.dcline.scenario") && make.dcline.scenario == TRUE) {
    # scneario to objects
    scenario.dc.lines <- initialize_table(Objects.sheet, 
                                          1, 
                                          list(class = "Scenario", 
                                               name = "Make all lines DC", 
                                               category = "Scenario archive for other configurations"))
    
    Objects.sheet <- merge_sheet_w_table(Objects.sheet, scenario.dc.lines)
    
    # scenario to properties
    # uses line.data.table
    # create table of only AC lines to use
    ac.lines <- line.data.table[grepl("\\_AC$|^AC\\_", category)]
    scenario.dc.lines.to.properties <- initialize_table(Properties.sheet, 
                                                        nrow(ac.lines), 
                                                        list(parent_class = "System", 
                                                             child_class = "Line",
                                                             parent_object = "System", 
                                                             band_id = 1, 
                                                             collection = "Lines"))
    
    scenario.dc.lines.to.properties[,child_object := ac.lines[,Line]]
    scenario.dc.lines.to.properties[,property := "Reactance"]
    scenario.dc.lines.to.properties[,value := "0"]
    scenario.dc.lines.to.properties[,scenario := "{Object}Make all lines DC"]
    
    Properties.sheet <- merge_sheet_w_table(Properties.sheet, 
                                            scenario.dc.lines.to.properties)
}

#------------------------------------------------------------------------------|
# [[Scenario archive for other configs]] Lines to enforce for natnl study ----
# -----------------------------------------------------------------------------|
if (exists('enforced.interstate.lines.file')) {
    
    interstate.to.enf <- read_data(enforced.interstate.lines.file)
    
    if (is.data.table(interstate.to.enf)) {
        
        message(sprintf("... enforcing lines from  %s", 
                        enforced.interstate.lines.file))
        
        # scneario to objects
        scenario.interstate.lines <- 
            initialize_table(Objects.sheet, 1, 
                             list(class = "Scenario", 
                                  name = "For PsN - fewer interstate lines to enforce", 
                                  category = "Scenario archive for other configurations"))
        Objects.sheet <- merge_sheet_w_table(Objects.sheet, 
                                             scenario.interstate.lines)
        
        # scenario to properties
        # uses line.data.table
        scenario.enf.interstate.lines.to.propterties <- 
            initialize_table(Properties.sheet, nrow(interstate.to.enf), 
                             list(parent_class = "System", child_class = "Line", 
                                  parent_object = "System", band_id = 1, 
                                  collection = "Lines"))
        scenario.enf.interstate.lines.to.propterties[,child_object := 
                                                         interstate.to.enf[,Line.Name]]
        scenario.enf.interstate.lines.to.propterties[,property := "Enforce Limits"]
        scenario.enf.interstate.lines.to.propterties[,value := "2"] # always
        scenario.enf.interstate.lines.to.propterties[,scenario := 
                                                         "{Object}For PsN - fewer interstate lines to enforce"]
        
        Properties.sheet <- merge_sheet_w_table(Properties.sheet, 
                                                scenario.enf.interstate.lines.to.propterties)
        
    } # end if (is.data.table(interstate.to.enf))
    
} else {
    message(">>  enforced.interstate.lines.file does not exist ... skipping")
}

#------------------------------------------------------------------------------|
# [[Scen arx for other configs]] Rmve isolated nodes, recalc LPF for others ----
# -----------------------------------------------------------------------------|
if (exists('isolated.nodes.to.remove.args.list')) {
    for (i in seq_along(isolated.nodes.to.remove.args.list)) {
        # pull element from the list
        isolated.nodes.to.remove.args = isolated.nodes.to.remove.args.list[[i]]
        
        # get file, scenario, and category names
        isolated.nodes.to.remove.file = isolated.nodes.to.remove.args[1]
        cur.scenario = isolated.nodes.to.remove.args["scenario"]
        cur.category = isolated.nodes.to.remove.args["category"]
        
        isolated.nodes.to.remove <- read_data(isolated.nodes.to.remove.file)
        
        if (is.data.table(isolated.nodes.to.remove)) {
            
            if (is.null(cur.scenario))
                cur.scenario = NA
            
            if (is.null(cur.category))
                cur.category = NA
            
            message(sprintf("... removing isolated nodes from  %s in scenario %s in category %s", 
                            isolated.nodes.to.remove.file,
                            cur.scenario,
                            cur.category))
            
            if (!is.na(cur.scenario)) {
                # scenario to objects
                scenario.remove.isolated <- 
                    initialize_table(Objects.sheet, 1, 
                                     list(class = "Scenario", 
                                          name = cur.scenario, 
                                          category = cur.category))
                
                Objects.sheet <- merge_sheet_w_table(Objects.sheet, 
                                                     scenario.remove.isolated)
            }
            
            # scenario to properties
            # uses isolated.nodes.to.remove.file
            # read in isolated nodes to remove file and change it to a veector
            isolated.nodes.to.remove[,Units:="0"]
            
            if(!is.na(cur.scenario)){
                import_properties(isolated.nodes.to.remove, names.col = "Node.Name", 
                                        object.class = "Node", collection.name =  "Nodes",
                                        scenario.name = cur.scenario)
            } else {
                import_properties(isolated.nodes.to.remove, names.col = "Node.Name", 
                                        object.class = "Node", collection.name =  "Nodes",
                                        overwrite = TRUE)
            }
            
            # recalculate relevant LPFs for other nodes 
            # pull node LPFs in base case (no scenario) from properties sheet for all 
            # nodes except the ones to be removed
            redo.lpfs.to.properties <- 
                Properties.sheet[property == "Load Participation Factor" & 
                                     !(child_object %in% isolated.nodes.to.remove$Node.Name) &
                                     is.na(scenario), 
                                 .(Node = child_object, value)]
            
            # add region for calculating LPF
            redo.lpfs.to.properties <-
                merge(redo.lpfs.to.properties, node.data.table[,.(Node, Region)], 
                      by = "Node")
            
            # recalculate LPF
            redo.lpfs.to.properties[,`Load Participation Factor` := prop.table(as.numeric(value)), 
                                    by = "Region"]
            redo.lpfs.to.properties <- redo.lpfs.to.properties[value != `Load Participation Factor`]
            
            # for nodes with LPFs that have changed, assign the new LPFs to the nodes
            # and attach the scenario
            redo.lpfs.to.properties[, c("value", "Region") := NULL]
            
            if(!is.na(cur.scenario)){
                import_properties(redo.lpfs.to.properties, names.col = "Node", 
                                        object.class = "Node", collection.name =  "Nodes",
                                        scenario.name = cur.scenario)
            } else {
                import_properties(redo.lpfs.to.properties, names.col = "Node", 
                                        object.class = "Node", collection.name =  "Nodes",
                                        overwrite = TRUE)
            }
            
        } # end if (is.data.table(isolated.nodes.to.remove))
        
        # clean up
        rm(cur.category, cur.scenario, redo.lpfs.to.properties, 
           isolated.nodes.to.remove.args, isolated.nodes.to.remove, 
           scenario.remove.isolated)
        
    }
} else {
    message(">>  isolated.nodes.to.remove.file does not exist ... skipping")
}