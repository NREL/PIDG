#------------------------------------------------------------------------------|
# Functions for creating and merging tables for data population ----
#------------------------------------------------------------------------------|

###initiallize_table
# initializes new .sheets table. Requires number of rows and fills in with 
# constants whatever informtion is provided by the user (can be NULL). Returns 
# created data.table. 
# This is useful for initializing tables with constants in several columns so 
# code can be condensed to one line
# Note: cols.list should be a list to allow data in different columns to be of 
# different types. If cols.vec is a list, nothing bad will happen, but all 
# numbers will be coerced into characters and that will throw a warning 
# (but not error) later.
initialize_table <- function (prototype.table, nrows, cols.list) {
  # create dummy column to initialize table with proper number of rows
  new.table <- data.table(init.col = character(nrows))
  col.names <- colnames(prototype.table)
  for (col.name in names(cols.list)) {
    new.table[, eval(col.name) := cols.list[[col.name]]]
  }
  for (col.name in col.names[!(col.names %in% names(cols.list))]) {
    new.table[, eval(col.name) := NA_character_]
  }
  new.table[, init.col := NULL]
  return(new.table)
}
# colorder doesn't matter b/c will be taken care of in merging step


###merge_sheet_w_table
# Takes a .sheet table and another data.table, merges them, and returns the 
# populated .sheet table. Will preserve the order of the columns, which is
# important when these tables are being read into Plexos.
# Note: thiscon
merge_sheet_w_table <- function(sheet.table, table.to.merge) {
  sheet.cols <- colnames(sheet.table) 
  
  # convert all columns to character so that they will be same data type 
  # as .sheet table
  table.to.merge <- table.to.merge[, lapply(.SD, as.character)] 
  
  sheet.table <- merge(sheet.table, table.to.merge, 
    by = colnames(table.to.merge), all = TRUE)
  
  setcolorder(sheet.table, sheet.cols)
  return(sheet.table)
}


#------------------------------------------------------------------------------|
# Functions to standardize and generalize data import ----
#------------------------------------------------------------------------------|

###import_table
# Takes a .csv file consisting of Objects, Categories, Memberships, Attributes, 
# Properties, and/or Reports tables delimited by "/ BEGIN, [name]" and 
# "/ END, [name]" lines, as well as a character specifying which table should be 
# output and returns a data.table with that information. 
# Not all tables are needed. Column names for tables with data must be spelled 
# and capitalized exactly as they are in .sheets tables. Order does not matter. 
# Any column called "notes" will be deleted during import, so comments can be 
# stored in the .csv there
import_table_generic <- function(imported.csv.table, sheet.name) {
  if (!(sheet.name %in% c("Objects", "Categories", "Memberships", "Attributes", 
    "Properties", "Reports"))) {
    print("Please specify one of the following tables: 
    Objects, Categories, Memberships, Attributes, Properties, Reports")} else {
      #set up indices
    begin.index <- which(imported.csv.table[,toupper(V1)] == "/ BEGIN" & 
        imported.csv.table[,toupper(V2)] == toupper(sheet.name)) + 2
    end.index <- which(imported.csv.table[,toupper(V1)] == "/ END" & 
        imported.csv.table[,toupper(V2)] == toupper(sheet.name)) - 1
    colnames.index <- begin.index - 1
    if (length(colnames.index) == 0 || colnames.index >= end.index) {
      return(NULL)}
      #set up table
    requested.table <- imported.csv.table[begin.index:end.index,]
    setnames(requested.table, as.character(imported.csv.table[colnames.index,]))
    requested.table <- requested.table[,which(sapply(requested.table, 
      function(x) !all(x=="") & !all(is.na(x)) )),with=F]
    if ("notes" %in% colnames(requested.table)) {
      requested.table[,notes := NULL]}
    return(requested.table)
 }
} 

###import_table_compact
# Takes two arguments: data.table of information that has been read in from an
# external file and a string describing the objects contained in that file
# (currently, 'model' or 'horizon,' not case sensitive)
# 
# REQUIRED FILE FORMAT: 
# 
# first column is 'names', first row of that is 'category' (could have text or 
# not), other columns either names of objects or 'notes' for explanation of 
# any numbers. notes columns will be ignored.
# other than that, 'names' col is divided into chucks starting with these tags.
# data should start the row after these tags. order of tagged chunks does not
# matter.
# /START ATTRIBUTES
# /START MEMBERSHIPS
# /START SCENARIOS
# 
# data chunk should consist of: (attributes) plexos attribute name in 'names' 
# column, desired value in column corresponding to object name. (memberships)
# name of object class/collection* which is the child of object on column. name 
# of child object is in column. (scenarios) scenarios that should be attached
# to models listed under models. extra, blank rows will be ignored.
# 
# *will only for for non-scenario memberships where object class == collection
import_table_compact <- function(input.table, object.type) {
  
  # can only handle models and horizons now. yell if user selected smthing else
  if (!(tolower(object.type) %in% c('model', 'horizon', 'production',
    'transmission', 'performance', 'mt schedule', 'st schedule'))) {
    warning(paste0("In compact.generic.import.files, an incorrect object type",
      " was selected. Please choose from (not case sensitive): model, ",
      "horizon, production, transmission, performance, mt schedule, ", 
      "st schedule"))
  }
  
  # set object.type to properly capitalized version (Model, Horizon, etc)
  # this is used to fill class cols later
  object.type <- paste0(toupper(substr(object.type, 1, 1)), 
    tolower(substr(object.type, 2, nchar(object.type))))
  
  # treat special case of MT or ST schedule
  if (grepl('schedule', tolower(object.type))) {
    object.type.parts <- strsplit(object.type, ' ')[[1]]
    object.type <- paste0(toupper(object.type.parts[1]), ' ', 
      toupper(substr(object.type.parts[2], 1, 1)), 
      tolower(substr(object.type.parts[2], 2, nchar(object.type.parts[2]))))
  }

  # ---- SET UP DATA FOR USE LATER
  
  # pull all object names, which are stored as column names (except first col)
  all.objects <- colnames(input.table)
  all.objects <- all.objects[all.objects != 'names' & all.objects != 'notes']
  
  # remove notes columns
  input.table <- input.table[,.SD, .SDcols = c('names', all.objects)]
  
  # grab /START of attributes, memberships, and scenarios and put them in 
  # ordered vector for index comparison later. also grab total numrows
  start.attrib <- input.table[names == '/START ATTRIBUTES', which = TRUE]
  start.memb <- input.table[names == '/START MEMBERSHIPS', which = TRUE]
  start.scen <- input.table[names == '/START SCENARIOS', which = TRUE]
  all.start.ind <- sort(c(start.attrib, start.memb, start.scen))
  
  nrows <- input.table[,.N]
  
  
  # ---- ADD TO OBJECTS SHEET
  
  # grab category information and put these all together in objects.sheet
  objcat <- input.table[names == 'category', .SD, .SDcols = all.objects]
  
  # transpose to be in long form so can be put into initialize_table
  objcat <- melt(objcat, measure.vars = all.objects, 
    variable.name = 'name', value.name = 'category')
    
  # create objects table and merge with Objects.sheet
  to.object.sheet <- initialize_table(Objects.prototype, nrow(objcat), 
    list(class = object.type, name = objcat$name, category = objcat$category))
  
  Objects.sheet <<- merge_sheet_w_table(Objects.sheet, to.object.sheet)
  
  
  # ---- ADD TO ATTRIBUTE SHEET (if there is a /START ATTRIBUTES tag)

  if (length(start.attrib) > 0) {
    # find where attribs section ends, either next /START index or the last row
    last.row.ind <- if (any(all.start.ind > start.attrib)) {
      # get first ind larger than start.attib, end of attribs is previous row
      all.start.ind[all.start.ind > start.attrib][1] - 1 } else nrows
  
  attribs.raw <- input.table[(start.attrib + 1) : last.row.ind]
  
  # melt to be in better format
  attribs.raw <- melt(attribs.raw, id.vars = 'names', variable.name = 'name')
  
  # remove blanks
  attribs.raw <- attribs.raw[value != ""]
  
  to.attrib.sheet <- initialize_table(Attributes.prototype, nrow(attribs.raw), 
    list(name = attribs.raw$name, class = object.type, 
      attribute = attribs.raw$names, value = attribs.raw$value))
  
  Attributes.sheet <<- merge_sheet_w_table(Attributes.sheet, to.attrib.sheet)
  
  }
  
  
  # ADD TO (non-scenario) MEMBERSHIPS SHEET
  
  if (length(start.memb) > 0) {
    # find where membs section ends, either next /START index or the last row
    last.row.ind <- if (any(all.start.ind > start.memb)) {
      # get first ind larger that start.memb, end of membs is previous row
      all.start.ind[all.start.ind > start.memb][1] - 1 } else nrows
  
  membs.raw <- input.table[(start.memb + 1) : last.row.ind]
  
  # melt to be in better format
  membs.raw <- melt(membs.raw, id.vars = 'names', variable.name = 'name', 
    value.name = 'child')
  
  # remove blanks
  membs.raw <- membs.raw[child != ""]
  
  to.memb.sheet <- initialize_table(Memberships.prototype, nrow(membs.raw), 
    list(parent_class = object.type, parent_object = membs.raw$name, 
      collection = membs.raw$names, child_class = membs.raw$names, 
      child_object = membs.raw$child))
  
  Memberships.sheet <<- merge_sheet_w_table(Memberships.sheet, to.memb.sheet)
  
  }
  
  # ----- ADD TO (scenario) MEMBERSHIPS SHEET
  
  if (length(start.scen) > 0) {
    # find where membs section ends, either next /START index or the last row
    last.row.ind <- if (any(all.start.ind > start.scen)) {
      # get first ind larger that start.memb, end of membs is previous row
      all.start.ind[all.start.ind > start.scen][1] - 1 } else nrows
  
  scens.raw <- input.table[(start.scen + 1) : last.row.ind]
  
  # melt to be in better format
  scens.raw <- melt(scens.raw, id.vars = 'names', variable.name = 'name', 
    value.name = 'child')
  
  # remove blanks
  scens.raw <- scens.raw[child != ""]
  
  to.scen.sheet <- initialize_table(Memberships.prototype, nrow(scens.raw), 
    list(parent_class = object.type, parent_object = scens.raw$name, 
      collection = 'Scenarios', child_class = 'Scenario', 
      child_object = scens.raw$child))
  
  Memberships.sheet <<- merge_sheet_w_table(Memberships.sheet, to.scen.sheet)
  
  }
}


##merge_property_by_fuel
# Takes an input table containing properties of generators indexed by fuel, 
# name of property column, Plexos name of property. Merges this with 
# gen.names,table, then returns a two column data.table, which contains
# Generator.Name and a column with the property in question whose column name
# is the Plexos name for that property.
# 
# If property.name is NULL, take property name to be the name of the column
# Note: Name of fuel column must be "Fuel"
# 
# NOTE: should clean up standard internal tables (gen.names.table, 
# fuels.to.gens, etc). Including change MaxOutput.MW to Max Capacity
merge_property_by_fuel <- function(input.table, prop.cols, 
  mult.by.max.cap = FALSE, mult.by.num.units = FALSE, 
  cap.band.col = NA, band.col = NA, memo.col = NA) {
  
  all.cols <- colnames(input.table)
  
  non.prop.cols <- c("Fuel", cap.band.col, band.col, memo.col)
  
  # make sure Fuel column exists before merging
  if ( !("Fuel" %in% colnames(input.table))) {
    stop(paste0("There is no 'Fuel' column in the input table. ", 
    "Cannot merge this table. Property name is: ", prop.cols, "."))
  }
  
  #This caused errors... need to determine how to insert memos into PLEXOS
  #if (!is.na(memo.col)) prop.cols <- c(prop.cols, memo.col)
  
  # split property by max capacity if needed
  if (is.na(cap.band.col)) {
    
    # if cap.band.col is NA, then don't need to split properties by max
    # capacity, so can do a regular merge with generator.data.table
    generator.data.table <- merge(generator.data.table, 
      input.table[,.SD, .SDcols = c("Fuel", prop.cols)], by = "Fuel")

  } else {
    
    # is a cap.band.col is give, use that to split up property distribution
    # when merging
    
    # create vectors of breaks for each fuel type (with min == 0 and 
    # max == max capacity in generator.data.table)
    maxes <- generator.data.table[,.(maxcap = max(MaxOutput.MW)), by = 'Fuel']
    all.fuels <- input.table[,unique(Fuel)]
    
    fuel.breaks <- list()
    for (fuel in all.fuels) {
     unique.breaks <- input.table[Fuel == fuel, unique(get(cap.band.col))]
     
     if (!is.na(unique.breaks[1]))
       # remove maxcap if it's less than any of the supplied breaks
        if (any(maxes[Fuel == fuel, maxcap <= unique.breaks]))
          maxes[Fuel == fuel, maxcap := NA] 

     unique.breaks <- c(-1, unique.breaks, maxes[Fuel == fuel, maxcap])
     unique.breaks <- sort(na.omit(unique.breaks))
 
     fuel.breaks[[fuel]] <- unique.breaks
    }
    
    # now that we have breaks for each fuel, add column to generator.data.table 
    # and input.table that sorts gens, so can merge by that and fuel
    for (fuel in all.fuels) {
      # add capacity even to NA cols in input.table, so they get sorted right
      input.table[Fuel == fuel & is.na(get(cap.band.col)), 
        (cap.band.col) := maxes[Fuel == fuel, as.integer(maxcap)]]
      
      # add breaks col
      input.table[Fuel == fuel, breaks.col := cut(get(cap.band.col), 
                                breaks = fuel.breaks[[fuel]])]
      generator.data.table[Fuel == fuel, breaks.col := cut(MaxOutput.MW, 
        breaks = fuel.breaks[[fuel]])]
    }
    
    # finally, merge input.table with generator.data.table
    generator.data.table <- merge(generator.data.table, 
      input.table[,.SD, .SDcols = c("Fuel", prop.cols, "breaks.col")], 
      by = c("Fuel", "breaks.col"), all.x = T)
  
  }

  
  # if this property should be multiplied by max capacity, do it
  if (mult.by.max.cap) {
    for (colname in prop.cols) {
      generator.data.table[,c(colname) := get(colname) * MaxOutput.MW]
    }
  }
  
  # if this property should be multiplied by number of units, do it
  if (mult.by.num.units) {
    for (colname in prop.cols) {
      generator.data.table[,c(colname) := get(colname) * Units]
    }
  }
  
  if (!is.na(band.col)){
    generator.data.table = merge(generator.data.table,
                                 input.table[,.SD,.SDcols = c('Fuel',band.col)],
                                 by='Fuel')
  }
  
  return.cols = c('Generator.Name',prop.cols,band.col)
  # return generator + property
  return(generator.data.table[,.SD, .SDcols = return.cols[!is.na(return.cols)]])
}


##add_to_properties_sheet
# Shortcut for creating addition to Properties.Sheet table, given inputs of a 
# certain form.
# 
# Takes a table of (col1) Object Name and (cols 2-n) property values (where 
# plexos property name are the column names), name of object class and 
# collection, melts this into long form, then merges with Properties.sheet
# 
# NOTE: doesn't handle scenarios or filepaths yet
add_to_properties_sheet <- function(input.table, object.class, names.col, 
  collection.name, scenario.name = NA, pattern.col = NA, period.id = NA, 
  datafile.col = NA, date_from.col = NA, overwrite = FALSE, band.col = NA, memo.col = NA) {
  
  # get all property column names (everything but object names column and 
  # pattern column, if applicable)
  all.cols <- colnames(input.table)
  
  non.prop.cols <- c(names.col, pattern.col, period.id, datafile.col, 
                     date_from.col,band.col, memo.col)
  
  prop.cols <- all.cols[!(all.cols %in% non.prop.cols)] 

  # melt input table. results in table with 3 columns: (names.col), 
  # property, value
  input.table <- melt(input.table, measure.vars = prop.cols, 
    variable.name = "property")
  # remove missing values
  input.table <- input.table[!(is.na(value) | is.na(property))]
  
  # create properties table with these properties
  props.tab <- initialize_table(Properties.prototype, nrow(input.table), 
    list(
      parent_class = "System", 
      parent_object = "System", 
      collection = collection.name, 
      child_class = object.class, 
      child_object = input.table[, .SD, .SDcols = names.col], 
      property = input.table[, property], 
      value = input.table[, value], 
      band_id = ifelse(is.na(band.col),1,list(input.table[,get(band.col)]))))

  # add pattern column if specified
  if (!is.na(pattern.col)) props.tab[, pattern := input.table[, 
      .SD, .SDcols = pattern.col] ] 
  
  # add datafile column if specified - only useful if putting in one property
  # at a time (because all will be associated with the datafile)
  if (!is.na(datafile.col)) props.tab[, filename := input.table[, 
      .SD, .SDcols = datafile.col] ] 
  
  #adding a date_from col if specified
  if (!is.na(date_from.col)) props.tab[, date_from := input.table[, .SD, .SDcols = date_from.col] ] 
  
  # add period type id column if specified
  if (!is.na(period.id)) props.tab[, period_type_id := period.id]
  
  # add scenario name if specified
  if (!is.na(scenario.name)) {
    props.tab[,scenario := paste0("{Object}", scenario.name)] }
  
  # add memo column if specified
  if (!is.na(memo.col)) props.tab[, memo := input.table[,get(memo.col)]]
  
  # merge with Properties.sheet
  if (overwrite == FALSE) {
    # merge
    Properties.sheet <<- merge_sheet_w_table(Properties.sheet, props.tab)
 
  } else {
    # merge everything but the value column, allow new data to overwrite old 
    # value col
    
    sheet.cols <- colnames(Properties.sheet) 
    props.tab.cols <- colnames(props.tab)

    # convert all columns to character so that they will be same data type 
    # as .sheet table
    props.tab <- props.tab[, lapply(.SD, as.character)] 
    
    Properties.sheet <- merge(Properties.sheet, props.tab, 
      by = props.tab.cols[props.tab.cols != 'value'], all = TRUE)
    
    # this should give two value columns. where data exists in value.y, use it
    Properties.sheet[!is.na(value.y), value.x := value.y]
    
    # delete duplicate columns
    Properties.sheet[,value := value.x][,c('value.x', 'value.y') := NULL]
    
    setcolorder(Properties.sheet, sheet.cols)
    
    # reassign Properties.sheet
    Properties.sheet <<- Properties.sheet

  }
}

##import_constraint
#shortcut for importing a table of constraints (all of the same type) and 
# adding objects, categories, attributes, memberships, and properties to sheets.
# constraint.table must have the format:
#   obj.col= column defining objects for which to define constraints
#   constraint.col = column declaring constraint names
#   category.col = column declaring constraint categories
#   prop.col = column declaring the property to which the constraint applies
#   sense.col = sense value defining the inequality (-1='<=')
# data files should be saved as : DataFiles\Constraint_Category\Constraint_name.csv
#  with one file for every defined constraint
import_constraint = function(constraint.table,obj.col = 'generator.name',constraint.type = 'RHS Month',collection='Generators',
                             child_class='Generator',constraint.col = 'constraint',category.col = 'category',prop.col = 'property',
                             sense.col = 'sense', scenario.col='scenario') {
  if (constraint.type =='RHS Month') period_type_id = 3
  else if (constraint.type == 'RHS') period_type_id = 0
  else error(paste0('import_constraint() not yet defined for ',constraint.type))
  
  new.object.table = data.table(expand.grid(name=unique(constraint.table[,get(constraint.col)]),class = c('Constraint','Data File')),description=NA,key='name')
  setkeyv(constraint.table,constraint.col)
  new.object.table[constraint.table,category:=get(category.col)]
  
  scenario.list = NA
  if (scenario.col %in% names(constraint.table)) {
    scenario.list=unique(constraint.table[,.SD,.SDcols=c(scenario.col,category.col)])
    new.object.table = rbind(new.object.table,data.table(expand.grid(name=scenario.list[,get(scenario.col)],category=scenario.list[,get(category.col)],class = c('Scenario')),description=NA))
  } else {
    constraint.table[,eval(scenario.col):=NA]
  }
  
  new.category.table = constraint.table[!duplicated(constraint.table[,.(get(category.col))]),
                                        .(class=c('Constraint','Data File'),category=get(category.col))]
  
  new.attribute.table = constraint.table[!duplicated(constraint.table[,.(get(constraint.col))]),
                                         .(name=get(constraint.col),class='Data File', attribute='Enabled', value=-1)]
  new.membership.table = constraint.table[!duplicated(constraint.table[,.(get(obj.col))]),
                                          .(parent_class='Constraint',child_class,collection,parent_object=get(constraint.col),
                                            child_object=get(obj.col))]
  
  constraint.table[!is.na(get(scenario.col)),eval(scenario.col):=paste0('{Object}',get(scenario.col))]
  
  new.properties.table = constraint.table[!duplicated(constraint.table[,.(get(obj.col),get(constraint.col))]), .(parent_class='Constraint',child_class,collection,parent_object=get(constraint.col),child_object=get(obj.col),property=get(prop.col),band_id=1,value=1,filename=NA,period_type_id=NA,scenario=get(scenario.col))]
  
  sense.props = constraint.table[!duplicated(constraint.table[,.(get(constraint.col))]),.(parent_class='System',child_class='Constraint',collection='Constraints',parent_object='System',child_object=get(constraint.col),property='Sense',band_id=1,value=get(sense.col),filename=NA,period_type_id=NA,scenario=get(scenario.col))]
  
  constraint.props = constraint.table[!duplicated(constraint.table[,.(get(constraint.col))]),.(parent_class='System',child_class='Constraint',collection='Constraints',parent_object='System',child_object=get(constraint.col),property=constraint.type,band_id=1,value=0,filename=paste0('{Object}',get(constraint.col)),period_type_id,scenario=get(scenario.col))]
  
  datafile.props = constraint.table[!duplicated(constraint.table[,.(get(constraint.col))]),.(parent_class='System',child_class='Data File',collection='Data Files',parent_object='System',child_object=get(constraint.col),property='Filename',band_id=1,value=0,filename=paste('DataFiles',gsub(' ','_',x=get(category.col)),paste0(gsub(' ','_',x=get(constraint.col)),'.csv'),sep='\\'),period_type_id=NA,scenario=get(scenario.col))]
  
  new.properties.table = rbind(new.properties.table,sense.props,constraint.props,datafile.props)
  
  Objects.sheet <<- merge_sheet_w_table(Objects.sheet,new.object.table)
  Categories.sheet <<- merge_sheet_w_table(Categories.sheet,new.category.table)
  Attributes.sheet <<- merge_sheet_w_table(Attributes.sheet,new.attribute.table)
  Memberships.sheet <<- merge_sheet_w_table(Memberships.sheet,new.membership.table)
  Properties.sheet <<- merge_sheet_w_table(Properties.sheet,new.properties.table)
}


