The Package downloads data and performs aggregations of information and Index calculation for ***Practice Level Prescription*** Data in England. The package also allows for the inclusion of territorial and health performance data, such as practice addresses and Quality Outcomes Frameworks (QOF).

**Aggregation** of prescription information is available at the following levels:

 - England
 - Clinical Commissioning Group (CCG)
 - GP practice

**Index** information relates to the number of items prescribed per 1000 registered patients.


Automatic RMarkdown Reports can be produced by GP and CCG.

**!!!!!!!!!Important**: notice that the default version of PrescRiption package functions provides a sample of available prescriptions data of about *500.000* rows. For the use of complete data select \emph{sample = FALSE} in the *monthlyDataImport()* and/or* monthlyDataDownload()* functions. Be aware that the data are very heavy!

Need a feature? Write us!
