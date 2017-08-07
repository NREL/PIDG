#create other models/scenarios

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
        cur.category = isolated.nodes.to.remove.args["scenario.cat"]
        
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
                merge(redo.lpfs.to.properties, Memberships.sheet[parent_class == "Node" & collection == "Region",
                                                                 .(Node = parent_object, Region = child_object)], 
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
            
            rm(redo.lpfs.to.properties, isolated.nodes.to.remove.args, 
               scenario.remove.isolated)
            
        } # end if (is.data.table(isolated.nodes.to.remove))
        
        # clean up
        rm(cur.category, cur.scenario, isolated.nodes.to.remove)
        
    }
} else {
    message(">>  isolated.nodes.to.remove.file does not exist ... skipping")
}


