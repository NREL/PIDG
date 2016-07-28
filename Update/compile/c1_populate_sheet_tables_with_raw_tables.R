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
    
    if (file.exists(file.path(inputfiles.dir, node.file))) {
        node.data.table <- fread(file.path(inputfiles.dir, node.file))
    } else {
        stop(sprintf("!!  %s does not exist", node.file))
    }
    
    if (file.exists(file.path(inputfiles.dir, line.file))) {
        line.data.table <- fread(file.path(inputfiles.dir, line.file))
    } else {
        stop(sprintf(" !!  %s does not exist", line.file))
    }

    if (file.exists(file.path(inputfiles.dir, generator.file))) {
        generator.data.table <- fread(file.path(inputfiles.dir, generator.file))
    } else {
        stop(sprintf("!!  %s does not exist", generator.file))
    }
    
    if (exists("transformer.file")) {
        if (file.exists(file.path(inputfiles.dir, transformer.file))) {
            transformer.data.table <- fread(file.path(inputfiles.dir, transformer.file))
        } else {
            stop(sprintf("!!  %se does not exist", transformer.file))
        }
    } else {
        message(sprintf(">>  %s does not exist ... skipping", transformer.file))
    }
    
}


#------------------------------------------------------------------------------|
# nodes ----
#------------------------------------------------------------------------------|

message("arranging node data")

node.data.table[, Units := 1]

# run data checks on nodes and save in OutputFiles

# a. Regions and zones
regions.zones <- unique(node.data.table[,.(Zone, Region)])

write.csv(regions.zones, file = file.path(outputfiles.dir,
                                          "DataCheck/data.check_node.freq.csv"))
# b. number of nodes by zone and region
node.freq <- as.data.table(xtabs(~ Zone + Region, data = node.data.table))
setnames(node.freq,"N","Number of Nodes")

write.csv(node.freq, file = file.path(outputfiles.dir,
                                      "DataCheck/data.check_node.freq.csv"))

# c. check that no nodes have missing region *** add zone check - contingent on zones existing in database
node.missing.region <- node.data.table[Region == "" | is.na(Region),Node]

if(length(node.missing.region) > 0){
  stop("The following nodes do not have a region: ", 
       paste(node.missing.region, collapse = ", "))
}

# add list of objects that have issues - nodes, lines, gens, etc...

# d. node voltage by region
node.kV.plot <- ggplot(data = node.data.table) + 
  geom_bar(aes(x = factor(Voltage), fill = factor(Voltage))) +
  facet_wrap(~ Region, scales = "free") +
  labs(x = "Node voltage", y = "Number of nodes") + 
  scale_fill_discrete(name="Node voltage")

pdf(file.path(outputfiles.dir,"DataCheck/node.voltage.by.state.pdf"),
    width = 12, height = 8)
plot(node.kV.plot)
dev.off()

# clean up 
rm(regions.zones,node.freq,node.missing.region)


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
all.zones <- unique(node.data.table$Zone)

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

message("arranging line data")

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

# run data check and save results in OutputFiles
# a. summary table - voltage, reactance, resistance, max flow, min flow
numeric.cols <- sapply(line.data.table,is.numeric)

line.summary <- describe(line.data.table[,numeric.cols, with = F])

write.csv(line.summary, file = file.path(outputfiles.dir,
                                         "DataCheck/line.summary.csv"))

# b. boxplots of line reactance by region
tfrm.reactance.plot <- ggplot(data = line.data.table[!is.na(Reactance)]) + 
  geom_boxplot(aes(x = factor(1), y = Reactance)) + 
  facet_wrap(~ category, scales = "free") +
  theme(axis.text.x=element_blank(),
        axis.title.x=element_blank(), 
        axis.title.y = element_blank(),
        axis.ticks.x = element_blank())

pdf(file.path(outputfiles.dir,"DataCheck/line.reactance.by.state.pdf"), 
    width = 12, height = 8)
plot(tfrm.reactance.plot)
dev.off()

# c. boxplots of line resistance by region
tfrm.resistance.plot <- ggplot(data = line.data.table) + 
  geom_boxplot(aes(x = factor(1), y = Resistance)) + 
  facet_wrap(~ category, scales = "free") +
  theme(axis.text.x=element_blank(),
        axis.title.x=element_blank(), 
        axis.title.y = element_blank(),
        axis.ticks.x = element_blank())

pdf(file.path(outputfiles.dir,"DataCheck/line.resistance.by.state.pdf"), 
    width = 12, height = 8)
plot(tfrm.resistance.plot)
dev.off()


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

message("arranging generator data")

generator.data.table[, Units := 1]

# add region
generator.data.table <- merge(generator.data.table, 
                              node.data.table[,.(Node, Region)],
                              by = "Node", 
                              all.x = TRUE)

# run data checks and save results in OutputFiles
# a. Generator properties summarized by region
numeric.cols <- which(sapply(generator.data.table, is.numeric))
numeric.cols <- c(names(numeric.cols),"Region")

suppressWarnings(
generator.region.summary <- describeBy(generator.data.table[,numeric.cols, with = F],
                                       group = "Region", mat = T)
)

write.csv(generator.region.summary,
          file = file.path(outputfiles.dir,
                           "DataCheck/generator.summary.by.region.csv"))

# b. Plot summary of generator properties by region
gen.capacity.plot <- ggplot(data = generator.data.table) +
  geom_boxplot(aes(x = factor(0), y = `Max Capacity`)) +
  facet_wrap(~ Region, scales = "free") +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
  
pdf(file.path(outputfiles.dir,"DataCheck/gen.capacity.by.state.pdf"), 
    width = 12, height = 8)
plot(gen.capacity.plot)
dev.off()


# adjust gen cap if needed
if (choose.input == "raw.psse") {
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
}

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

message("arranging transformer data")

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
transformer.data.table[Region.From == Region.To, category := Region.From]
transformer.data.table[Region.From != Region.To, category := "Interstate_tfmr"]

# run data check on transformers and save results in OutputFiles
# a. Transformer properties 
numeric.cols <- which(sapply(transformer.data.table,is.numeric))
numeric.cols <- c(names(numeric.cols),"category")

suppressWarnings(
transformer.summary <- describeBy(transformer.data.table[,numeric.cols, with = F],
                                  group = "category", mat = T)
)

write.csv(transformer.summary, 
          file = file.path(outputfiles.dir,"DataCheck/transformer.summary.csv"))

# b. boxplots of transformer reactance by region
tfrm.reactance.plot <- ggplot(data = transformer.data.table) + 
  geom_boxplot(aes(x = factor(1), y = Reactance)) + 
  facet_wrap(~ category, scales = "free") +
  theme(axis.text.x=element_blank(),
        axis.title.x=element_blank(), 
        axis.title.y = element_blank(),
        axis.ticks.x = element_blank())

pdf(file.path(outputfiles.dir,"DataCheck/transformer.reactance.by.state.pdf"), 
    width = 12, height = 8)
plot(tfrm.reactance.plot)
dev.off()

# c. boxplots of transformer resistance by region
tfrm.resistance.plot <- ggplot(data = transformer.data.table) + 
  geom_boxplot(aes(x = factor(1), y = Resistance)) + 
  facet_wrap(~ category, scales = "free") +
  theme(axis.text.x=element_blank(),
        axis.title.x=element_blank(), 
        axis.title.y = element_blank(),
        axis.ticks.x = element_blank())

pdf(file.path(outputfiles.dir,"DataCheck/transformer.resistance.by.state.pdf"), 
    width = 12, height = 8)
plot(tfrm.resistance.plot)
dev.off()


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