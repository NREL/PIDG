## PLEXOS Input Data Generator (PIDG)

### Introduction

PLEXOS can read in any Excel file that has the following structure: one workbook with 6 worksheets named Objects, Categories, Memberships, Attributes, Properties, and Reports, each containing specific columns. These worksheets can hold almost all the information needed to construct and personalize a working model in PLEXOS. The PLEXOS Input Data Generator (PIDG) can take data stored in csv files or a postgreSQL database and compile it into an appropriately-formatted Excel workbook. This allows the input data to be scripted, reproducible, and version-controlled.

Soon, there may be a command line interface that would enable the automated import of a PIDG-produced Excel file into PLEXOS. For now, this step requires human intervention through PLEXOS's import wizard. When working with a large database, it is often useful to uncheck the "Check Data" box in the second step of the import wizard to cut down import time (note: this is only tested for importing into a fresh database, not for importing into an existing database). Finally, there are two types of data that cannot be imported using PLEXOS's import wizard: anything in the "Settings" menu (ex: unit changes) and toggles for writing specific flat files (it is possible to import the option to write all or no flat files, but not to select which to write out separately from selecting what properties will be reported).

This README describes how to run PIDG, all options for types of data to import, and required data formats.

### How to run PIDG

PIDG is run by **PIDG/driver.R** and all pointers to data and options for reading data are set in a separate input parameters file. To run PIDG, set up in the input parameters file (all options for this defined below) and run **PIDG/driver.R** (required arguments defined below). **PIDG/driver.R** will read in data referred to in the input parameter file and run PIDG's core scripts to process, compile, check, and output that data.

The file structure usually as follows. All of these will be project-specific and should be storage outside of the PIDG repository. An example of this setup can be found in **PIDG/example**.

* a file called **run_PIDG.R** which loads the arguments in the *PIDG/driver.R parameters* section below and calls **PIDG/driver.R**
* an **input_params.R** file that specifies what data to use by defining variables described below in the *Input parameter file options* section
* a directory called **inputfiles** 
* a directory called **outputfiles**

### PIDG/driver.R parameters

**PIDG/driver.R** can be run either through the command line with the following arguments (in argname=argvalue form) or by running another R script which includes defining the following variables and then sourcing **PIDG/driver.R**.

* `pidg.dir`: required. path to directory of the PIDG repo. like any of the following path inputs, this can be an absolute path or, if calling from another script that sets the working directory, a path relative to the working directory
* `input.params`: required. path to input parameter file. 
* `inputfiles.dir`: optional (required if input parameters file contains pointers to csv files). directory in which all input csv files are stored. paths in the input parameter file will be interpreted as relative to `inputfiles.dir`
* `inputfiles.db`: optional (required if input parameter file contains SQL queries). list with the following named elements: `drv`, `host`, `dbname`, `user`, `password`, all arguments of RPostgreSQL's function `dbConnect()`. this is used to open a connection to a postgreSQL database 
* `outputfiles.dir`: optional. directory where the output workbook and data check outputs will be saved. if does not exist, will be set to directory where `input.params` lives
* `output.wb.name`: optional. name (including ".xlsx") of workbook to be saved. if not set, workbook will automatically be saved as "pidg\_export\_[*current\_datetime*].xlsx"
* `export.db`: optional. logical, setting whether Excel workbook will be saved. setting this to `FALSE` can be useful for testing if the stage of writing the Excel file takes a while. if not set, will be set to `TRUE`
* `data.check.plots`: optional. logical, setting whether, when PIDG is performing its final data checking, pdf plots should be exported. setting this to `FALSE` can be useful if writing the pdf plot takes a while. if not set, will be set to `TRUE`

### Input parameter file options

The input parameter file tells PIDG where to look for data and how to process it. Data can be stored in a PSS/E (version 31) .raw file, .csv files, or in a postgresql database. Pointers to data are defined in the input parameters file in one of several lists, and which list data is read in in will determine how PIDG will treat the data. This is a list of possible variables that can be defined as input parameters. Note: below, "data pointer" means either a path to a csv file, relative to the variable `inputfiles.dir` or a SQL query (beginning with "SELECT") that will be sent to the postgresql database defined by `inputfiles.db`. Unless otherwise specified, if a variable is undefined, it will be ignored.

switches:
* `choose.input`: character, can to "pre.parsed" or "raw.psse". Defaults to "pre.parsed" if undefined. Set to "pre.parsed" unless data should be read from a PSS/E .raw file.
* `plexos.version`: numeric, can be 6 or 7. Defaults to 7 if undefined. This determine the column names in the Properties tab of the final Excel workbook, since these are different between PLEXOS 6.xx and 7.xx.

basic functionality:
* `raw.file.path`: character, optional. filepointer to the PSS/E .raw file.
* `objects.list`: list. Import objects, their categories, any memberships with child objects, and any properties.
* `memberships.list`: list. Import memberships between objects and any properties of memberships.
* `object.property.list`: list. Import properties of objects only.
* `generic.import.files`: list. Import anything.

convenience functions:
* `generator.property.by.fuel.list`: list. Rather than importing object-specific properties, import generator properties by their category (optionally multiplying by max capacity of the generators and/or number of units)
* `interleave.models.list`: list. More easily enable running DA-RT (etc) sequence by created filepointers between objects for certain properies, either for generator category or for specific objects (CHECK THIS ONE). Optionally, set models to run interleaved.
* `compact.generic.import.files`: list. Convenience format for importing models and horizons. 
* `reserve.files` - MAYBE. Semi-convenient format for importing reserves. 
* `interfaces.files.list` - MAYBE - need to update this. Semi-convenient format for importing interfaces. 
* `constraint.import.files`: Convenience format for importing constraints. 
* `turn.off.except.in.scen.list` - MAYBE. Convenience format for setting Units = 0 in the base data and Units = 1 in a scenario for any object (?).
* `map.region.to.load.file`: soon to be deprecated - MAYBE: Convenience format for creating load data file object with multiple load scenarios for each region.
* `isolated.nodes.to.remove.args.list` - MAYBE. Convenience format that creates a scenario to turn off isolated nodes and recalculate load participation factor. 
* `units.to.delete.files` - MAYBE. Convenience format to remove objects entirely from the database.

soon to be deprecated:
* `node.file`: charcter, optional. data pointer to path that defines nodes. soon to be deprecated.
* `line.file`: charcter, optional. data pointer to path that defines lines. soon to be deprecated.
* `generator.file`: charcter, optional. data pointer to path that defines generators. soon to be deprecated.
* `transmformer.file`: charcter, optional. data pointer to path that defines transformers. soon to be deprecated.
* `load.file`: charcter, optional. data pointer to path that defines load by node. soon to be deprecated. CHECK for LPFs HERE


### Basic PIDG code structure and troubleshooting

Once all variables are read in and checked as described in the **"How to run PIDG"** and **"PIDG/driver.R"** sections, PIDG's core scripts are run. This is a basic description of the process PIDG uses, for help with troubleshooting if something goes wrong. 

Basic information of PIDG is as follows:

1. **prepare environment** (**PIDG/driver.R**): read in arguments and necessary functions and source `input.params`
2. _**optional: parse specifically formatted input data** (**a-1**, **a-2**): if a PSS/E file has been given, parse it_
3. **create blank \*.sheet tables** (**b**): as described in the introduction, the final Excel workbook will contain six sheets, Objects, Categories, Memberships, Attributes, Properties, and Reports. Create a blank data.table to correspond to each sheet, with correct column names (i.e. `Objects.sheet`, `Categories.sheet`, `Memberships.sheet`, `Attributes.sheet`, `Properties.sheet`, and `Reports.sheet`)
4. **populate \*.sheet tables** (**c1**, **c2**, **c3**, **d**): the core of PIDG's functionality: go through each possible variable that could exist in `input.params`. if it does exist, read in data, process it, then add it to whichever of these table(s) is appropriate. the processing of each variable happens independently of the processing of each other variable
5. **check data** (**e**): once all `input.params` input variables are dealt with, run checks on the data. export summaries of various parts of the database and flag potential issues. these issues are sorted into "fatal warnings" (known to cause failures in importing to PLEXOS or when running PLEXOS) and "warnings" (which may not cause failures but may be grounds for a second look at input data) 
6. **export data** (**f**): if `export.wb == TRUE`, save the workbook

Because of the modularity of step 4, if something goes wrong, it is usually helpful to use progress messages to identify what data PIDG is trying to process in when the error happens and which variable that data corresponds to. Then, find within the PIDG scripts where that variable is processed, add a `stop()` or `message()` or two and rerun.

It is also important to heed "fatal warnings." Most of these are known to cause issues and PLEXOS's error messages related to some of these problems are somewhat enigmatic, meaning it is hard to track down what's going on if they are allowed persist.

**********
**********

## STOP HERE. Everything below this is old and probably no longer relevant.

#### Guide to input parameter file

This file defines parameters that will be used to read in data and create a Plexos database. This works by defining certain variables as a combination of file pointers and other parameters, which the scripts will use to pull in and process data. This is a list of the different variables that can be defined in the input parameters file, with the required format for associated files.

Note: all file pointers should be relative to whatever input files directory (`inputfiles.dir`) is defined to be.

* `choose.input`: character, set to "raw.psse" or "pre.parsed". If `choose.input == "raw.psse"`, the scripts will expect to read in a psse file, so the variable `raw.file.path` is required. If `choose.input` == "pre.parsed", the scripts will expect to read in csvs to define the base network, so the files `node.file`, `line.file`, and `generator.file` are required and `transformer.file` is optional.
	* to define network data with PSSE file:
		* `raw.file.path`: character, set to path to PSSE file. Only used if `choose.input == "raw.psse"`. **Format:** PSSE file should be in v31 format.
	* alternate way of defining network data:
		* `node.file`: character, path to csv that defines node data, required. **Format:** requires columns "Node" (should be first column), "Region". Listed nodes and regions will be created. Optional columns are "Zone" and any other Node property (frequently: Voltage, Unit, etc). If "Zone" exists, Zones will be created and attached to Nodes. If "Units" does not exist, all Nodes will be created with Units == 1. A "notes" column may exist and will not be used. A "category" column may exist and will be used to categorize nodes (blanks or NA values mean the node will not be categorized); otherwise, nodes will be categorized by region. Any other column (but "Owner" for now) will be treated as a property of the Node and added accordingly. Blanks or values of `NA` in any column but "Node" will be ignored. Node names should not be repeated.
		* `line.file`: character, path to csv that defines line data, required. **Format:** requires columns "Line" (should be first column), "Node From", and "Node To". A "category" column will be used to categorize lines (NA values or blanks will not be categorized); otherwise, lines will be categorized by their region or (if a reactance column exists) region and AC or DC (nonexistent reactace or--for now--Reactance == 0). A "notes" column will be ignored. If "Units" does not exist, all Lines will be created with Units == 1. Any other column will be treated as a property. Current exception is some unused columns which result from PSSE parsing, but this will be fixed soon (currently "Voltage.From", "Voltage.To", "ratingA", "ratingB", "ratingC", "rateA", "rateB", "rateC", "Status", "Length"). Blanks or values of `NA` in any column but "Line" will be ignored. Line names should not be repeated and columns.
		* `generator.file`: character, path to csv that defines generator data, required. **Format:** requires columns "Generator" (should be first column) and "Node". A "category" column will be used to categorize generators (NA values or blanks are taken to mean no category); otherwise, generators will be categorized by region. If "Units" does not exist, all Generators will be created with Units == 1. A "notes" column will be ignored. Blanks or values of `NA` in any column but "Generator" will be ignored. Generator names should not be repeated. Exception: if the column "Generation Participation Factor" exists, it will be assumed that a generator might be connected to multiple nodes and that the values in that column should be added to Nodes via the Generator.Nodes collection. All other property columns will be treated as Generator properties. In that case, Generator-Node pairs should not be repeated. When a Generator is repeated, any property, category, or note data must be identical between entries. All other columns will be treated as properties. Current exception is some unused columns which result from PSSE parsing, but this will be fixed soon (currently "Status", "Owner[anything]").
		* `transformer.file` (this is optional): character, path to csv that defines transformer data, required. **Format:** requires columns "Transformer" (should be first column), "Node From", and "Node To". A "category" column will be used to categorize transformers (NA values or blanks will not be categorized); otherwise, transformers will be categorized by their region(s). If "Units" does not exist, all Transformers will be created with Units == 1. A "notes" column will be ignored. Any other column will be treated as a property. Current exception is some unused columns which result from PSSE parsing, but this will be fixed soon (currently "Voltage.From", "Voltage.To", "Status"). Blanks or values of `NA` in any column but "Transformer" will be ignored. Transformer names should not be repeated.

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
### REQUIRED INPUT FILES
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


**********
#### miscellany

###### PSSE notes
- does BusTypeCode matter at all? 
- what do we care about phase angle at any point? this info is in the PSSE database, at least for the timeperiod in the power flow case
- load table - using the "Active Power" column. Is that right?
- 31 nodes in PSSE load table have negative load. Why? For now, correcting that to zero in the R scripts. None of them are duplicates. 
- why do some generators have negative min stable levels? switching them to zero in the script so Plexos doesn't yell.
- using max and min output columns for generator max cap and min stable level, but there are a lot of columns. Are any of the others useful?
- which line rating?
- transformers have so many options! Should probably double check that we're using the right ones. 
- same with DC lines
- since transformers can have two or three windings in PSSE, there is code (which is currently commented out because it is slow) to separate the two to two different tables (this is because the .raw formatting is different between them). Will we ever have three-winding transformers? If no, we can delete this code. If yes, we need to keep it or think of a better way to separate transformer types.
- some lines have max/min flows of 0 (even if the line is turned on). Why?
- not doing anything with fixed or switched shunt. Should we be?
- not doing anything with PSSE's Owner column at the moment (other than reading it in)

###### other notes

- the following command (run from a bash terminal, like GitBash) will change the currency units from $ to PHP
 ```sed -ie 's/\$/PHP/' OutputFiles/ph_2014_V0_3.xml```
