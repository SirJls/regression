# The granddaddy of Supervised Artificial Intelligence - Regression

*** The retail market dataset ***

## Description

This is a Haskell implementation of the Regression exercise from the book "Data Smart" by John W. Foreman.
This implementation uses the provided RetailMart dataset from the book.

This implementation is able to produce the following two regressions models by generating two [GNUplot](http://www.gnuplot.info/) scripts:
* A linear regression model & ROC curve
* A logistic regression model & ROC curve

All the meta-data and the actual data points to be plotted are generated in a dataset belonging used by the script to generate the plot.


## Dependencies

### Software
* bed-and-breakfast    <---- matrix library
* statistics           <---- Only used for calculating the F Test P Value
* filepath             <---- file paths 
* decimal-arithmetic   <---- To avoid floating point round errors 
* bytestring           <---- Used to read in the dataset as a bytestream
* optparse-applicative <---- Applicatives to parse command line arguments 
* vector >= 0.11       <---- High speed optimized vector library
* cassava >= 0.5       <---- CSV parser
* genetic-algorithm    <---- Our own continuous genetic algorithm library (Custom)

### Tooling
* Cabal
	- Linux, use your favorite distro to install, or build cabal from [source](http://www.haskell.org/cabal/download.html)
	- Windows, install the [Haskell platform](https://www.haskell.org/platform/) or get a [pre-built binary](www.haskell.org/cabal/download.html)
* [GNUplot](https://sourceforge.net/projects/gnuplot/files/gnuplot/5.2.8/) version >= 5.2 (required to generate the plots)


## How to execute the program

In order to generate a binary and generate the plots, execute the following steps:
1. Clone the [genetic-algorithm](https://github.com/SirJls/genetic-algorithm) project and place it at the same folder level as this project.
2. Cd into the regression project folder `cd regression` and excute the command `cabal new-configure && cabal new-build` (this will pull in all dependencies)
3. (Optional) if you get a `cabal: Could not resolve dependencies:`, then execute `cabal new-update` and then retry step 2
3. We now have a compiled target in the `dist-newstyle` folder and can generate the datasets and plots using `cabal new-run regression -- --generate data/TrainingSet.csv data/TestSet.csv <YourFileName>
4. (Optional) if for some reason (Microsoft windows) this does not work, locate the binary (called regression) inside the `dist-newstyle` and execute `./regression(.exe) --generate data/TrainingSet.csv data/TestSet.csv <YourFileName>
4. After the run, we now have two GNUplot scripts, which can create graph(s) by using GNUplot:
	- `gnuplot -persist <name>_linear_model.plot`
	- `gnuplot -persist <name>_logistic_model.plot`
	- These scripts, will use the <name>_linear_model.dat & <name>_logistic_model.dat, which contain the data points and meta-data, used by the plots, as well as the reports of the data, which was also visible in the retailmart spreadsheets in the form of a commented table.

