#clean data to allow the final export to import to Plexos and run with no errors

# uses 
#   units.to.delete.file

#------------------------------------------------------------------------------|
# Data cleaning ----
#------------------------------------------------------------------------------|
###some min stable levels are less than zero, which Plexos can't handle. 
###Adjust them to zero.
Properties.sheet[property=="Min Stable Level" & as.numeric(value) < 0, 
  value := "0"] 
#remove quotes if end up changing numeric col types back to numeric
#This doesn't matter now since we are reading in min gen files


###One fuel type is blank, which Plexos can't handle. Change blanks to BLANK.
###10/28: have now removed all blanks. Commenting this out.
#Objects.sheet[class == "Fuel" & (is.na(name) | name == ""), name := "BLANK"]
#Objects.sheet[class == "Generator" & (is.na(category) | category == ""), 
#  category := "BLANK"]
#Memberships.sheet[parent_class == "Generator" & 
#  child_class == "Fuel" & collection == "Fuels" & (is.na(child_object) | 
# child_object == ""), child_object := "BLANK"] 
#Properties.sheet[parent_class == "System" & 
#  child_class == "Fuel" & collection == "Fuels" & (is.na(child_object) | 
# child_object == ""), child_object := "BLANK"]
#Properties.sheet[parent_class == "Generator" & 
#  child_class == "Fuel" & collection == "Fuels" & (is.na(child_object) | 
#  child_object == ""), child_object := "BLANK"]
#Categories.sheet[class == "Generator" & (category == "" | 
#  is.na(category)), category := "BLANK"]

###Two lines importing into Delhi are congesting all Delhi imports, possible
###because of a combination of low MW rating and extremely low 
###resistance/reactance values. <-- only in AgTx scenarios
# Scenario that decrease congestion enough for the model to be able to serve all 
# load in Delhi is defined in c3 and not automatically attached to a model.
### This scen changes the resistance/reactance values of those two lines to the 
###resistance/reactance values of similar lines that have the same MW rating and 
###are in a similar region.

###one region doesn't have any nodes. PLEXOS won't run if that's the case. 
###Remove Region objects with no nodes. NOTE: Looks like this isn't necessary 
###anymore with cleaner NLDC node-to-region mapping
#regions.list <- unique(Objects.sheet[get("class") == "Region", name])
#regions.in.memberships <- unique(Memberships.sheet[parent_class == "Node" & 
#child_class == "Region", child_object])
#regions.w.no.node <- regions.list[!(regions.list %in% regions.in.memberships)]
#if (length(regions.w.no.node > 0)) {Objects.sheet <- Objects.sheet[name != 
#regions.w.no.node]}

###when importing raw load participation factor from .raw file, there were 
### repaired infeasibilities at every step. Not sure 
###exactly what's going on, but replacing LPF < 0 with 0 in c2.
#Properties.sheet[property == "Load Participation Factor" & as.numeric(value) < 
#0, value := "0"]
##NOTE: this correction happens before Load Participation Factor is calculated, 
##to total regional load participation factors can add up to 1. 
###WILL BE FIXED AFTER DP's NLDC MEETING: turn off rogue nuke plant


#------------------------------------------------------------------------------|
# Specific adjustments to PPS/e data ----
#------------------------------------------------------------------------------|

# rogue nuke (w max capacity given as 9999 MW in original .raw file) shoud be 
# turned off (Mohit, NLDC meeting 10/29)
Properties.sheet[child_object == "GEN_132005_RAPS_A2_220_1" & 
    property == "Units", value := "0"]

# There are two generators for which NLDC changed the max capacity, in 
# spreadsheet "Google Drive\GreeningTheGrid-LBNL-NREL\Data\PLEXOS database 
# characteristics\Obsolete\Generators_for_PLEXOS_import_05_10_2015_for 
# NLDC_input_david_checking.xlsx" 
# This correnction is also in c1, in the creation of the gen.names.table, 
# because calculation of start costs and min gen levels depends on max capacity
Properties.sheet[parent_object == "System" & property == "Max Capacity" &
  (child_object=="GEN_354013_GMR_400_1" | child_object=="GEN_354013_GMR_400_2"),
  value := "685"]

# This line has a different rating than what's given in the PSS/e onpeak file
# 11/20: this is no longer necessary b/c this is a zero flow line (see below)
#Properties.sheet[child_object == "317001_337006_1_CKT" & property == "Max Flow", 
#  value := "2316"]
#Properties.sheet[child_object == "317001_337006_1_CKT" & property == "Min Flow", 
#  value := "-2316"]

# Some nodes are assigned to the wrong region (they serve the region they are
# assigned to, but are physically located elsewere)
## 1/7 removing this for now. If we reassign all of these nodes that are 
## electrically located in one place and physically located in a different 
## place, this should happen all at once and earlier in the script, or this 
## code should recategorize the node and all lines associated with it, etc.
# Memberships.sheet[parent_class == "Node" & child_class == "Region" & 
#     parent_object == "142046_NARELA_BB_220", child_object := "DELHI"]

# The one node that is assigned to be in Chhattisgarh and also the SR should be
# changed until further notice. It will make things more difficult when putting
# wheeling charges on inter-zonal lines, for example. Will check back in on this
# CORRECTED IN SCRIPT C1 so as to update line categorization (if matters) etc

# retire plants from the "units_retired" file. This means: delete them 
# completely from the database, since they will not 
to.delete <- fread(file.path("../InputFiles/", units.to.delete.file))
Objects.sheet <- Objects.sheet[!(name %in% to.delete[,Generator.Name])]
Properties.sheet <- Properties.sheet[!(child_object %in% 
    to.delete[,Generator.Name])]
Memberships.sheet <- Memberships.sheet[!(child_object %in% 
    to.delete[,Generator.Name]) & 
    !(parent_object %in% to.delete[,Generator.Name])]

# also need to retire RE plants in the PSSE file, since we are replacing them 
# with our own. For now, doing it here, but if the new RE gens ever get added 
# to gen.names.table, this must be done elsewhere.
re.to.delete <- generator.data.table[Fuel %in% c("WIND", "SOLAR-PV") & 
  !is.na(BusNumber), Generator.Name] 

Objects.sheet <- Objects.sheet[!(name %in% re.to.delete)]
Properties.sheet <- Properties.sheet[!(child_object %in% re.to.delete)]
Memberships.sheet <- Memberships.sheet[!(child_object %in% re.to.delete) & 
    !(parent_object %in% re.to.delete)]

# add standard flow limits to lines with ratings of zero
# do this in a scenario (in script d)

# add standard flow limits to transformers with ratings of zero
# do this in a scenario (in script d)

# there is one Chattisgarh node that is assigned to SR. This will aggregate 
# transmission incorrectly. This fix is made in c3 when creating 
# region-specific scenarios
# Maybe when fix this, include error check that remove these, warns user, or 
# asks user to choose which zone a conflicted state is in 

# all RAJASTHA nodes are assigned to NR. DP told Ella to put them in WR. This
# taken into account with WR regional scenarios (in script c3), but should 
# probably be fixed in the input data in the long run.

#------------------------------------------------------------------------------|
# Error checking: final database ----
#------------------------------------------------------------------------------|

# make sure there are no missing properties
if (any(Properties.sheet[,value == "" | is.na(value)])) {
  print("WARNING: the following property value(s) are missing:")
  print(Properties.sheet[value == "" | is.na(value),
    .(child_object, property, value, scenario)])
}

# make sure no region has no nodes
all.regions <- Objects.sheet[class == "Region",name]
regions.w.nodes <- Memberships.sheet[parent_class == "Node" & collection == 
    "Region",child_object]
if (!all(all.regions %in% regions.w.nodes)) {
  print("WARNING: the following region(s) have no nodes:")
  print(all.regions[!(all.regions %in% regions.w.nodes)])
}

# make sure no object name has more than 50 characters 
if (any(Objects.sheet[,nchar(name) > 50])) {
  print("WARNING: the following object(s) have names with > 50 characters:")
  print(Objects.sheet[nchar(name) > 50])
}

# make sure min stable level is less than zero
if (any(Properties.sheet[,property == "Min Stable Level" & value < 0])) {
  print("WARNING: the following generator(s) have negative min stable levels:")
  print(Properties.sheet[property == "Min Stable Level" & value < 0])
}

#------------------------------------------------------------------------------|
# Alphabetize all categories ----
#------------------------------------------------------------------------------|

cat.by.class <- unique(Objects.sheet[!is.na(category),.(class, category)])

# order categories by class and category
setorder(cat.by.class, class, category)

# add rank of each category by class
cat.by.class[,rank := 1:.N, by = 'class']

# add this to categories .sheet so categories will be alphabetized
cat.to.categories <- initialize_table(Categories.prototype, nrow(cat.by.class), 
  list(class = cat.by.class$class, category = cat.by.class$category, 
  rank = cat.by.class$rank))
  
Categories.sheet <- merge_sheet_w_table(Categories.sheet, cat.to.categories)

# clean up
rm(cat.by.class, cat.to.categories)