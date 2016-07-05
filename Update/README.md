## README for update to PSSE2PLEXOS

There are currently four scripts in this folder

`a-parse-psse.R`
* parse psse file, export all possible tables into some user-defined folder. also export some metadata about the data that was parsed (what tables do and don't exist, how much data is in each, etc)

`b-call-input-process.R`
* optional - call input processing scripts on data. could also exogenously 
    create data. ideally, if processing using scripts, these should report
    all changes to the psse file and each input processer should have its 
    own report-type readme (but these are optional)
* after running this script or exogenously creating data, should have a 
    folder with all inputs + an input_params file

`c-call-input-checks.R`
* call a set of optional input data checks. this will require certain 
    naming conventions and a path to the input data folder

`d-compile.R`
* compiles checked input data into excel format to be imported into plexos
    

### envisaged usage
1. call `a-parse-psse.R` once
    * pass: location of psse file, desired location of outputs 
    * return: output dump into specified folder
2. cycle through `b-call-input-process.R` and `c-call-input-checks.R` until 
    data is ready.
    * ??
3. tweak input_params.
    * manual in input_params
4. call `d-compile.R`
    * pass: location of cleaned input data, location of input_params, 
        desired location of output workbook
    * return: excel workbook in specified location