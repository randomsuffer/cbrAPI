## cbrAPI

**cbrAPI** is an R package that allows to download and read dbf-files with different forms of accounting reports for a certain time period, as well as find information about the banks on Central Bank of Russia site.

The goal of this project is to make available to users all over the world the unique data of the accounting statements of Russian banks, which is collected by the Central Bank of Russian Federation. Data is represented as archived dbf files (database files). The package provides the opportunity to download these archives from the Central Bank's website, read them and load into R. You can download any of the five forms of the accounting reports (101, 102, 123, 134, 135) for any given time period or over the entire available. You can also get detailed information on any Bank, knowing his registration number to find out the name, address, is it registered it in the Deposit Insurance Agency, is it went bankrupt or not.

#### Installation:

```r
library(devtools)
install_github("randomsuffer/cbrAPI")
library(cbrAPI)
```

All files on [Central Bank of Russia site](http://cbr.ru) are packed in zip and rar archives, so you'll need unrar software to work

#### For Windows
Download unrar tools from [RARLAB site](http://www.rarlab.com/rar/unrarw32.exe) and extract them to C:\\Windows.

#### For OS X
Using Terminal enter
```
ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
brew install unrar
```
but I should say, that the package can do that itself :)

#### For Unix
Install 7zip using
```
sudo apt-get install p7zip-rar
```

## Known issues
Package now is in it's beta version. Here're some things I'll fix in near future:
1. Check unrar tools in .onLoad() rather than every time the function is invoked.
2. Build vignettes.
3. Check latest available date of report before download.
