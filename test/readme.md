# Test the created network with python networkx package

## To run:
1. run the PSSE2PLEXOS import tool in R
2. After PSSE2PLEXOS has successfully created ```node.data.table```, ```line.data.table``` and ```transformer.data.table``` run the following two lines (in R) to create your network data .csv files.
 1. ```write.table(node.data.table[,.(BusNumber)],file='PSSE2PLEXOS/test/buses.csv',sep=',', row.names = F, col.names = F)```
 2. ```write.table(rbind(line.data.table[,.(from=BranchFromBus,to=BranchToBus)],transformer.data.table[,.(from=FromBusNumber,to=ToBusNumber)]),file='PSSE2PLEXOS/test/network.csv',sep=',',row.names=F,col.names = F)```
3. Go to a terminal and run ```ipython notebook```  from the 'test' directory
4. Analyze your network in the ipython notebook
