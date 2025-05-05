# Tidy Buddy

**Tidy Buddy** is designed to assist users in generating data in a tidy format, making it easier to prepare datasets for various analytical tasks. While this app is particularly useful for [IGGYPOPseq](https://github.com/ZenanXing/Construct-Validation-for-IGGYPOPseq) users without coding background by helping create the input file `SampleInfo.tsv`, its functionality extends to generating any tidy format data suitable for a variety of research application, such as [BioCurve Analyzer](https://github.com/ZenanXing/Biocurve-Analyzer) :)  

#### Key Features

- Dynamic Sample Entry: Easily specify the number and format of input tables and variables. The application utilized [`rhandsontable`](https://github.com/jrowen/rhandsontable?tab=readme-ov-file), providing an Excel-like interface that enables convenient features, such as dragging to copy cell values.
- Interactive Data Preview: Preview the generated dataset to verify that the data meets your requirements before downloading.
- Diverse File Export: Download the generated data in a variety of formats, ready for immediate use in your analysis workflow.

## Getting Started

Tidy Buddy can be used both locally and online. The app can be installed following the instructions below and it is also hosted on Shinyapps.io:https://zenanx.shinyapps.io/tidy-buddy/.  

### Installation

To use Tidy Buddy locally, you can follow the steps.  

  1. Install R and RStudio IDE. The app has been tested with R 4.4.1 and RStudio version 2024.04.2+764.  

  2. Clone or download the Tidy Buddy from the GitHub. You can either [clone the repository](https://docs.github.com/en/repositories/creating-and-managing-repositories/cloning-a-repository) using git or download the app as a ZIP file.  
  In addition, these R packages should also be installed by from CRAN or GitHub using the code below.  

      ```
      install.packages("shiny", "rhandsontable", "bslib", "tidyverse", "openxlsx")
      ```

  3. Run the shiny app using the following code in RStudio.

      ```
      shiny.runApp()
      ```

### How It Works

The breif tutorial for the app is available [here](./Tutorial.pdf). We have included datasets from our previously published paper as examples, which you can download for reference.

### Help

If you need any help or support related to this app, feel free to contact us at zxing001@ucr.edu, and the issues can also be reported on https://github.com/ZenanXing/Tidy-Buddy/issues.  
  
**Enjoy using the app!** 
  
[(Back to top)](#tidy-buddy)

## License & DOI

This project is licensed under the GNU General Public License, version 3 (GPLv3) - see the LICENSE.md file for details, and the DOI for the app is [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.15331613.svg)](https://doi.org/10.5281/zenodo.15331613)
.  
  
[(Back to top)](#tidy-buddy)

## Citations

If you use the Tidy Buddy, please cite our paper and the related papers listed below.

- **Our paper:**  
*Xing Z, Eckhardt J, Vaidya AS, Cutler SR. [BioCurve Analyzer: a web-based shiny app for analyzing biological response curves](https://rdcu.be/ejBEp). Plant Methods 2025; 21: 1–9*

- **IGGYPOPseq example:**  
*Dvir G, Xing Z, Beldman I, Rivera A, Wheeldon I, Cutler SR. Synthesis of large single-transcript pathways from oligonucleotide pools: design of STARBURST, an autobioluminescent reporter (manuscript in preparation)*  

- **Dose-response data example:**  
*Vaidya AS, Helander JDM, Peterson FC, Elzinga D, Dejonghe W, Kaundal A, et al. [Dynamic control of plant water use using designed ABA receptor agonists](https://www.science.org/doi/10.1126/science.aaw8848?url_ver=Z39.88-2003&rfr_id=ori:rid:crossref.org&rfr_dat=cr_pub%20%200pubmed). Science. 2019;366:eaaw8848.*  
  
[(Back to top)](#tidy-buddy)