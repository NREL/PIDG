**********
### OVERVIEW OF CODE STRUCTURE
**********
__Introduction__

Plexos can read in any Excel file that has the following structure: one workbook with 6 worksheets named Objects, Categories, Memberships, Attributes, Properties, and Reports, each with specified columns. These worksheets can hold all the information needed to construct and personalize a working model in Plexos. This set of scripts reads in data describing a plexos network, either based off a PSSE file or a set of other csvs (format defined below) and creates 6 data.tables, each corresponding to one required worksheet (referred to as .sheet tables throughout these comments and the code). Once these tables are created, the scripts compile all input data, format the data to correspond with Plexos's excel input file format, and add the data chunks to the .sheet tables. Lastly, the fully populated .sheet tables are exported into one Excel workbook, which can be imported directly into Plexos. 


__To run__

Load required variables into environment. Basic required variables are: 

* path to directory containing master PSSE2PLEXOS script (**create\_plexos\_db\_from\_raw\_master\_script.R**), defined as variable `master.script.dir`
* path to input parameters file, defined as variable `input.params`
* path to directory containing all input files  (input parameters file should refer to input files relative to this directory), defined as variable `inputfiles.dir`
* path to directory where output excel file should be saved (will be created if does not exist), defined as variable `outputfiles.dir`
* name of output excel file, defined as variable `output.wb.name`

Then, run **create\_plexos\_db\_from\_raw\_master\_script.R**. This will sequentially run the scripts in the directory *SourceScripts* and write the output into an excel file.


#### Basic structure of *SourceScripts*

* TODO: REDO THIS **a_import_raw.R:** reads in and parses the .raw file, based on expected PSSE version-specific table structure. Currently based on documentation for PSSE v31. If intending to use a .raw file from a different version of PSSE, this script should be modified to ensure that columns are named correctly. This is the only script that is dependent on the version of PSSE being used.
* **b_create_sheet_tables.R:** This script creates empty .sheet tables (Objects.sheet, Categories.sheet, Memberships.sheet, Attributes.sheet, Properties.sheet, and Reports.sheet), as well as prototypes of these tables to be used in the initialize_table function (see below).
* **c1_populate_sheet_tables_with_raw_tables.R:** populates .sheets tables using data from the .raw file created in script (a). It also creates node.data.table, generator.data.table, line.data.table, and transformer.data.table, which contain information about each type of object to be referenced later in the scripts when information is needed about these objects. 
* **c2_more_data_population.R:** populates .sheet tables with information in other .csv input files. 
* **c3_create_scenarios_and_models.R:** defines new scenarios and models.
* **d_data_cleanup.R:** database cleaning and some data checks. Ideally the database-specific corrections will be moved to database-specific input files later.
* **e_export_to_excel.R:** gathers fully-populated .sheet tables and exports them as separate sheets in output Excel workbook, which can be imported directly into Plexos.


#### Guide to input parameter file

This file defines parameters that will be used to read in data and create a Plexos database. This works by defining certain variables as a combination of file pointers and other parameters, which the scripts will use to pull in and process data. This is a list of the different variables that can be defined in the input parameters file, with the required format for associated files.

Note: all file pointers should be relative to whatever input files directory (`inputfiles.dir`) is defined to be.

* `choose.input`: character, set to "raw.psse" or "pre.parsed". If `choose.input == "raw.psse"`, the scripts will expect to read in a psse file, so the variable `raw.file.path` is required. If `choose.input` == "pre.parsed", the scripts will expect to read in csvs to define the base network, so the files `node.file`, `line.file`, and `generator.file` are required.
	* to define network data with PSSE file:
		* `raw.file.path`: character, set to path to PSSE file. Only used if `choose.input == "raw.psse"`. **Format:** PSSE file should be in v31 format.
	* alternate way of defining network data:
		* `node.file`: character, path to csv that defines node data, required. **Format:** requires columns "Node", "Region". Listed nodes and regions will be created, and nodes will be categorized by region. Optional columns are "Zone" and any other Node property (frequently: Voltage, Unit, etc). If "Zone" exists, Zones will be created and attached to Nodes. If "Units" does not exist, all Nodes will be created with Units == 1. A "notes" column may exist, which will not be used. A "category" column may exist and will be used to categorize nodes (blanks or NA values mean the node will not be categorized); otherwise, nodes will be categorized by region. Any other column (but "Ownership" for now) will be treated as a property of the Node and added accordingly. Blanks or values of `NA` in any column but "Node" will be ignored. Node names should not be repeated.
		* `line.file`: character, path to csv that defines line data, required. **Format:** requires columns "Line", "Node From", and "Node To". A "category" column will be used to categorize lines (NA values or blanks will not be categorized); otherwise, lines will be categorized by their region or (if a reactance column exists) region and AC or DC (nonexistent reactace or--for now--Reactance == 0). A "notes" column will be ignored. Any other column will be treated as a property. Current exception is some unused columns which result from PSSE parsing, but this will be fixed soon. Line names should not be repeated.
		* `generator.file`: character, path to csv that defines generator data, required. **Format:** requires columns "Generator" and "Node". A "category" column will be used to categorize generators (NA values or blanks are taken to mean no category); otherwise, generators will be categorized by region. A "notes" column will be ignored. Generator names should not be repeated. Exception: if the column "Generation Participation Factor" exists, it will be assumed that a generator might be connected to multiple nodes and that the values in that column should be added to Nodes via the Generator.Nodes collection. All other property columns will be treated as Generator properties. In that case, Generator-Node pairs should not be repeated. When a Generator is repeated, any property, category, or note data must be identical between entries. 

The database is built starting with a PSSE .raw file. Other input .csv are used to supplement and build off of the PSSE database. See "REQUIRED INPUT FILES" (documentation not complete as of 4/10/16) section for more detail.


This needs to be filled in more, leaving sketches for now. See *functions.R* for more details on required arguments for these functions.

**Functions for basic interaction with .sheet tables**

* **initialize_table:** create empty table in the format of a .sheet table, to be populated with data
* **merge_sheet_w_table:** merge populated table with existing .sheet table (always use this to add to .sheet table because it preserved column order and class)


**Functions to import generic objects**

These functions take .csv files defined as input parameters and read them in. Any file in the *generic.import.files* list will be read in by **import_table_generic** and any file in the *compact.generic.import.files* list will be read in by **import_table_compact.**

* **import_table_generic:** read in any information in the raw form of the output excel file
* **import_table_compact:** more compact, readable form of **import_table_generic.** Can only import objects of one type of per file.


**Functions to add properties to objects**

These functions read in inputfiles of specific formats and add properties contained in those files to objects that already exist in the database, with various options for customization. Any file in the *object.property.list* list will be read in by **add_to_properties_sheet** and any file in the *generator.property.by.fuel.list* list will be read in by **merge_property_by_fuel.**

* **add_to_properties_sheet:** assigns properties to specific objects. takes any table of the form [colum with names of objects, arbitrary number of other columns with names that are exactly the Plexos property name] and add those properties to Properties.sheet. must also pass this function the name of the column that contains object names, the type of object (Generator, Line, etc), and the collection of that object (Generators, Lines, Interface.Lines, etc).
    * Other options:
        * set "overwrite" to TRUE if want it to overwrite existing properties instead of simply adding new properties
        * if the table has columns with information on pattern (timeslice) or band, the names of those can be passed in to the function as well, and it will add them to Properties.sheet appropriately
		* if properties will be defined by a datafile, pass in those columns (any number, still with names exactly equal to Plexos property names) with the filepointer as the columns' values, and pass in the name(s) of those columns as a character vector using the optional argument datafile.col. This function will set that value of the specified properties to zero and the filename to the value of the column.
        * character strings corresponding to a scenario or period type id that should be associated with these properties can also be passed in
		* if these properties belong in a collection that is the child of an object that isn't System (ex: Interface.Lines), identify the appropriate collection like normal (ex: Lines) and include a column in the input csv with the parent objects. The name of the column must be the parent object class. Include the optional parameter "parent.col" when running the function.

* **merge_property_by_fuel:** assigns properties to generators, but by fuel type instead of object name. Requires a column named Fuel. All other columns will be taken to be Plexos properties (again, names must be exact matches to Plexos properties). It will spit out a table in the right format to be read in by **add_to_properties_sheet.** In these scripts, results are automatically fed to the **add_to_properties_sheet** function. 
    * Other options:
        * set "mult.by.max.cap" to TRUE if value in table should be multiplied by the max capacity of the generator before setting the property (ex: useful for things like min gen, where input file will say that coal units can turn down to 70% of max capacity)
		* set "cap.band.col" to the name of a banded column if have a property that depends on fuel and size. This will merge by fuel type and size, based on the breaks defined in that column (each property is given to generators with capacities *less* than or equal to the break listed)
		* set "mult.by.num.units" to TRUE if value in table should be multiplied by the number of units of that generator
	* Input object:
		* input should be a list of lists. Each sublist corresponds to one file to be read in, where the first element is the file path, the second is a list *fuel.map.args*, containing arguments to **merge_property_by_fuel**, and the third is a list *add.to.prop.args*, containing arguments to add_to_properties_sheet. 
		* *add.to.prop.args* can have any argument that **add_to_properties_sheet** takes. If one is ''scenario'' and that scenario does not exist, it will be created.


**Other inputs**

These all refer to variables in input_params

* raw.file.path (filepath): path to .raw (PSSE) file
* map.gen.to.fuel.file (filepath): path to file that maps all generators to a fuel type (needs two colums: Generator.Name and Fuel)
* map.region.to.load.file (filepath): path to file that matches regions with a pointer to a load file. One column must be called Region. Others will be treated as load scenarios. Any column called "Base" will be added with no scenario. Data file columns called anything else will be added with a scenario tag that is equal to the column name.
* rename.regions (logical) / map.newregion.file (filepath)  and rename.zones (logical) / map.newzone.file (filepath): should the regions and zones assigned to nodes be mapped by an external file, rather than left as defaults from PSSE?
* add.RE.gens (logical) / RE.gen.file.list (list or vector or filepaths): should new generators be added to the database? if yes, do it. RE.gen.file.list should be a list of vectors. Each vector must contain the path to the RE gen input file as its first element. RE gen input file required columns: Generator.Name [name of generator to be created], Fuel [as of 5/16, all fuels must also exist in map_fuels_to_gens, or else fuel object won't be created], Max.Capacity, Num.Units, Category [of generator], Rating [contains pointer to rating datafile; rating property will be set to zero and point to this file], Node.To.Connect [existing node in network to connect to]. Other named elements are optional: 
    * *scenario* : if scenario is defined, generators will be added, but Units property will be set to how it's defined in the input file only in specified scenario, and 0 otherwise. Scenario will be created if it doesn't already exist. 
    * *make.new.nodes* : if this element exists, new nodes will also be created. New generators will be attached to those nodes, and 0-resistance, high thermal limit lines will be created between the new node and Node.To.Connect. If this exists, additional columns in the input file are required: Node.Name, Node.Region, Node.Zone, Node.kV. see format of RE.gen.file.
* turn.off.except.in.scen.list (list): list of generators which should have Units set to 0 (will turn them off) in the base case but to 1 or whatever Units was in scenario passed in with filename
* delete.original.RE (logical): should generators that are originally assigned the Fuel WIND or SOLAR-PV be eradicated from the database?
* units.to.delete.file (filepath): names of objects that should be completely eradiated from the database
* remap.reference.nodes (logical) / map.ref.node.file (filepath): should reference nodes for regions be assigned based on external file? if set to FALSE or it map.ref.node.file doesn't have all regions, any region without a reference node with have the reference node assigned arbitrarily  
* interfaces.files.list (list): does specific things with interfaces. should be improved. these input files are the outputs of the india db's interface creation scripts
    * list should contain any number of character vectors with the named elements: names, properties, memberships, flowcoefs, which each are pointers to input files. these input files should be formatted with the following column:
	    * names: Interface.Name, category
		* properties: Interface.Name, Max Flow, Min Flow [the last two should be filepointers to max, min flow data]
		* memberships: Interface.Name, Line.Name [to define what line is in each interface]
		* flowcoefs: Interface.Name, Line.Name, Flow Coefficient
* isolated.nodes.to.remove.args.list (list): list of vectors. first element in each vector is filepath to one-column (Node.Name) .csv with list of nodes to turn off. second and third elements are optional names arguments (scenario and category). This csv will be read in, listed nodes' Units will be set to zero, and the LPF of all other nodes will be recalculated. If scenario (and scenario category, optional) is specified, these changes will be tagged with that scenario name; otherwise, they will replace information in the base case. 
* interleave.models.list: takes two models, interleaves them, and, using a template, sets up the filepointers that pass information between the models
    * the list contains character vector elements. In each vector, the first element is the path to an input file defining which models should be interleaved, and rest are named elements. These can be template.fuel, template.object, and interleave. The two template.* files should contain filepointers to template files These should be formatted as follows:
		* first element: csv that defines interleaved models: three columns: parent.model, child.model, scenario.name (child.model will be added to the Interleaved collection of the parent model; the filepointers in the child.model, which point to outputs of the parent.model, will be added under the given scenario; the scenario will be added to the child.model unless otherwise specified). any number of parent/child model pairs can be in the same file; these will all be added using the same template.
		* template.fuel: for now, this only works with the format for marge_property_by_fuel (one Fuel column, arbitraty number of property columns). All property columns should contain templates for filepointers between models, and the string "[DA MODEL]" should go where the name of the parent model would.
		    * optional: add a row with the word "Attribute" (case sensitivie) in the "Fuel" column. In this row, define what attribute (not property) that datafile object should have, of the form "Upscaling Method = 0" (LSH should be Plexos property, exact match, RHS should be value to be set for that attribute, and there must be an `=` between them. Do not use a `:` (this is used elsewhere to split strings). This will append a useful message to the datafile object name (meaning one property can be passed two ways using two datafile objects) and set that property for the datafile object. As of now, this does not allow for multiple attributes to be set. Also, there is no way to set two datafile objects of the same property but with different attributes to the same fuel category. This would likely throw an error in plexos, anyway, but if there are duplicates, all will be overridden by whichever appears first in the table.
		* template.object: similar to template.fuel, but instead of Fuel, the first column should be entitled the class of objects listed in the first column (note that this means that only one object type per csv can be defined)
		    * NOTE: this is now functional with at least one file (untested with more), but adding the Attribute row in these by-object templates will NOT work for now
		* interleave: logical. TRUE means models will be interleaved, FALSE means filepointers will be set up but RT models will not be added to the "Interleave" collection of the DA models (so that they can be run sequentially)
		


**********
###REQUIRED INPUT FILES - everything below here needs to be updated
**********

__Directory Structure__
The script _create_plexos_db_from_raw_master_script.R_ should be in a directory that also contains folders nsmed "SourceScripts", "InputFiles", and "OutputFiles". This "master" script requires that the scripts it class (function definitions and other sub-scripts) be stored in the "SourceScripts" directory and that all input .raw and .csv files be stored in "InputFiles". The excel workbook created by the scripts will be automatically written to the "OutputFiles" folder.

__Code Structure__
_create_plexos_db_from_raw_master_script.R_ is the only script that needs to be modified and run by the user, as it will call all other scripts to read in, process, and output data. Each script in SourceScripts can be run independently, but, as most depend on variables created earlier in the sequence, they must be run sequentially. The master file also contains variable definitions used to specify names of input and output files; see "REQUIRED INPUT FILES" for more detail. Before running, the working directory must be set to the directory of the master file. Below is a list of the purpose of each sub-script in the "SourceScripts" directory.

* (a) _a_import_raw.R:_ This script reads in the .raw file and breaks it into tables based on the .raw delimiter "0 /". Columns of each of these tables are then manually renamed based on documentation for PSSE v31. If intending to use a .raw file from a different version of PSSE, this script should be modified to ensure that columns are named correctly. This is the only script that is dependent on the version of PSSE being used, although the names of the columns (but not their order) are used in later scripts. 
  * Note: Lines are given 3 ratings (A, B, and C) in the PSS/e .raw files. These scripts assume that RatingA is a technical limit (not useful for this application), Rating B is a thermal limit, and Rating C is the overload limit.
* (b) _b_create_sheet_tables.R:_ This script creates empty .sheet tables (Objects.sheet, Categories.sheet, Memberships.sheet, Attributes.sheet, Properties.sheet, and Reports.sheet), as well as prototypes of these tables to be used in the initialize_table function (see below).
* _(functions) functions.R:_ This script defines three functions that are used throughout the rest of the scripts:  
	1. _initialize_table_ takes a prototype of a .sheet table and creates an empty table with the same columns as that prototype, however many rows are specified by the user, and fills specified columns with any constants provided by the user. The resultant table can then be filled in with the relevant data. 
	2. _import_table imports_ a specified .sheet table from a .csv file.
	3. _merge_sheet_w_table_ merges a data.table to a .sheet table, preserving the column order in the .sheet table. 
In every step of data population throughout the remainder of the scripts, a table is initialized using initialize_table or import_table. If needed, more data (from .raw or .csv files) is added. Then, the table is merged to the appropriate .sheet table using merge_sheet_w_table.
* (c1) _c1_populate_sheet_tables_with_raw_tables.R:_ This script formats and populates .sheets tables using tables from the .raw file created in script (a).
* (c2) _c2_more_data_population.R:_ This script continues to populate the .sheet tables by integrating data in other .csv input files into .sheet tables.
* (c3) _c3_create_scenarios_and_models.R:_ Any new scenarios or models (other than the Base model) associated with those scenarios are defined here, by reading in definition tables from input .csv files.
* (d) _d_data_cleanup.R:_ If there are steps required to clean the database to ensure that it can be read by Plexos and run out of the box, they are coded here.   
* (e) _e_export_to_excel.R:_ This final scripts exports populated .sheet tables as separate sheets in an Excel workbook, which can be imported directly into Plexos.


**********
###REQUIRED INPUT FILES
**********

####Input parameters 

* _raw.file.path:_ Filepath to .raw PSSE file to be imported. This .raw file makes up the core of the database, and is supplemented by other .csv input files. Required format: PSSE version 31 
* _map.gen.to.fuel.file:_ Filepath to a .csv file that maps generator identifier (Generator.Name) to fuel type. Required columns: Generator.Name, Fuel
* _map.region.to.load.file:_ Filepath to a .csv file that maps region names to the location of corresponding load data files, relative to where this model will be run in Plexos. These are used to create data file objects in Plexos. Required columns: RegionName, Loadfile
* _map.fuel.price.to.fuel.file:_ Filepath to .csv file that maps fuel type to cost of power generation ($/MWh), which is read into Plexos's Generator.Fuels "Offer Price" property. Required columns: Fuel, Price
* _map.ramps.to.fuel.file:_ Filepath to a .csv file that maps fuel type to max (and min) ramp rates. Required columns: Fuel, maxRamp

* _rename.regions:_ Should nodes be regrouped in regions that are different from regions (areas) defined in the PSSE database? Required format: logical
* Optional: _map.newregion.file:_ If rename.regions is true, filepath to .csv that maps nodes to new regions. Required columns: BusNumber, RegionName

* _rename.zones:_ Should nodes be remapped to zones that are different from zones defined in the PSSE database? Required format: logical
* Optional: _map.newzone.file:_ If rename.zones is true, filepath to .csv that maps nodes to new zones. Required columns: BusNumber, ZoneName

* _hydro.energy.limits.file:_ Filepath to a .csv file that maps hydro generators to monthly energy limit constrints. Required columns: Generator.Name, month (in format "Mxx"), monthly.limit.GWh 
* _hydro.cf.limits.file:_ Filepath to a .csv file that maps hydro generators to monthly capacity factor limit constrints. Required columns: Generator.Name, month (in format "Mxx"), monthly.limit.GWh 
* _min.gen.file:_ Filepath to a .csv file that maps generator min gens by fuel. Required columns: Fuel, MinStableLevel
* _unit.status.file:_ Filepath to a .csv file that maps generators to their status. 1 means to be Commissioned, 2 means commissioned but switched off in basecase, 3 means to be Deleted. The sheet from the NLDC used '3' for units that might have been recently retired or are not operational. The 1's should not be included in the 2014 validation,so we might need to out a commission date of 2015 on these and just assume they are built before our 2022 case (good assumption) Required columns: Generator.Name, status

* _generic.import.files:_ Character vector of any number of filepaths to any files of the form of _import.model.base.file_. For now, this is used to import objects, attributes, and memberships of any horizons or models that a user wants to define. However, these files could contain any data to be imported. equired format: must be made up of at least one of the following tables: Objects, Memberships, Attributes, Properties, Reports, Categories. Line immediately before the beginning of a table must start with (for example) '/ BEGIN', 'Objects' and line immediately after the end of a table must start with (for example) '/ END','OBJECTS'. It is not necessary to include all tables in this file, but if a table is present, it must have columns names as they are spelled and capitalized in .sheet tables.



####Output parameters

* _output.wb.name:_ Name of output excel file. Required format: character

* _copy.workbook.elsewhere:_ Should a copy of the output excel file be made to another location on the computer? Required format: logical
* Optional: _copy.destination:_ If copy.workbook.elsewhere is true, filepath to location where excel workbook should be copied to.


**********
###MISC NOTES AND QUESTIONS
**********

######PSSE qs
- What is the actual difference between the different .raw files? Some have more lines than others?
- does BusTypeCode matter at all? 
- what do we care about phase angle at any point? this info is in the PSSE database, at least for the timeperiod in the power flow case
- load table - using the "Active Power" column. Is that right?
- 547 nodes have duplicates in PSSE load table, and the few that I looked at were a factor of 5-10 apart. For now, just using the first entry. 
- 31 nodes in PSSE load table have negative load. Why? For now, correcting that to zero in the R scripts. None of them are duplicates. 
- Only 3969 nodes are in the PSSE Load table at all (including the 31 negatives) (4480 including duplicates).  Why so many buses with no load? Is that intentional?
- why do some generators have negative min stable levels? switching them to zero in the script so Plexos doesn't yell.
- using max and min output columns for generator max cap and min stable level, but there are a lot of columns. Are any of the others useful?
- Most min stable levels are zero, which is probably not right. Should we put in numbers from the SCADA data? Generic by-fuel numbers? (Coal == 55%?)
- there are three ratings per line in the PSSE file. PLEXOS read them as max/min <- rating1, overload max/min <- rating2, and appeared to ignore rating3. So far, the scripts do the same thing. What should we actually do?
- do we need to do anything with the area interchange in Area.interchange.table?
- transformers have so many options! Should probably double check that we're using the right ones. 
- same with DC lines
- since transformers can have two or three windings in PSSE, there is code (which is currently commented out because it is slow) to separate the two to two different tables (this is because the .raw formatting is different between them). Will we ever have three-winding transformers? If no, we can delete this code. If yes, we need to keep it or think of a better way to separate transformer types.
- some lines have max/min flows of 0 (even if the line is turned on). Why?

- not doing anything with fixed or switched shunt. Should we be?
- not doing anything with PSSE's Owner column at the moment (other than reading it in)

######generic data cleaning qs/comments
- need to finish cleaning up technology/fuel mapping: Ranjit's questions from spreadsheet
- need to finish cleaning up technology/fuel mapping: some generators are duplicated in unclear ways in nodes_gens_all_india. R script currently deletes generators (BusNumber_ID) duplicates.
- similarly, some about 10 node names are duplicated in the "Node - Region.Zone" tab of nodes_gens_all_india. Also, there are more nodes in that spreadsheet than in the PSSE database (6757 nodes in PSSE. Initially, it looks like there are 7102 unique nodes in the spreadsheet, but there are really only 6834 unique bus numbers and 6220 unique bus names. When R merges the PSSE node table with the "Node - Region.Zone" sheet, there are 7034 nodes. Removing duplicates brings the number back down to 6757. What's going on here? Same thing happens with zones.) R script currently deletes BusNumber duplicates. Glancing through spreadsheet, it looks like only one of these duplicated nodes is assigned to two different regions. This is node 522015, which is assigned to both Assam and Nagaland (both in NER). The R scripts assign this node to Assam, simply because it comes first.
- swtiched from 2011 load files to 2014 load files. Two questions: 1) there is no region corresponding to Telangana in the nodes_gens_all_india (i.e. no Telangana region in Plexos), even though it is now a separate state. It looks like Telangana was part of Andhra Pradesh until 2014. What should we do? For now, I added a new .csv called DemandHourly2014-AndhraPradesh_Telangana_SUM.csv, which is the sum of the loads in the two separate files. This is currently assigned as the load of Andhra Pradesh. 2) There are no load files for out-of-country regions (BHUTAN, BHUTAN-NER, BANGLADESH, NEPAL), so I assigned a file with zero load to all of them.
- final decision on unit status for gens, lines, and transformers?
- the following command (run from a bash terminal, like GitBash) will change the currency units from $ to PHP
 ```sed -ie 's/\$/PHP/' OutputFiles/ph_2014_V0_3.xml```

######aesthetics q
- Right now, the generators are named something similar to their PSSE names. This can be changed very easily by modifying what is assigned to the "name" column when creating the generator objects.
- what should line names be? fullBusName_fullBusName is too long for plexos, so they are currently named BusNumber_busNumber_ID
- only generators, nodes, and scenarios are currently categorized, but could categorize lines, etc.
- Should nodes be categorized differently (i.e. by region or zone?)
- question about error-correcting code cleanups (see OneNote AE 9/16)
- how is the current process of creating scenarios/models/horizonz/etc?
- should we personalize report, ST schedule, etc, more?

######other
- what kind of solvers will everyone have access to? these scripts specify the solver as Xpress-MP, but that can change
- what units are we using?

######other data
- final decision on how to cost out generation? heat rate + fuel price + startup/shutdown + VOM? Only offer price? Should there be an offer price curve?
- need to come up with better ramp constraints and min gen constraints
- what exactly does MVA base do? is the resistance of each HVDC line calculated correctly?




