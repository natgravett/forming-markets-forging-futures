# forming-markets-forging-futures
Code associated with article "Forming markets, forging futures: Public procurement and lead markets for European low-emission steel" by Natalie Gravett-Foyn & Carl Dalhammar, IIIEE, Lund University


You will find two R files related to the paper.

These have been created by NGF and then cleaned with ChatGPT (Lund University version), to make the code more concise and ensure there are section separators and labels to improve readability. 

Please read the methodology in the article for more information.

Contact natalie.gravett-foyn@iiiee.lu.se for queries or discussion.



gs_gpp_docanalysis.R - R file related to the qualitative and quantitative document analysis

gs_gpp_tedanalysis.R - R file related to the TED Contract Award Notice analysis



**ABSTRACT**

Steel is a strategic material for Europe, embedded in critical sectors such as construction, transport and defence, yet responsible for approximately 5% of EU CO₂ emissions. While technological pathways for low-emission steel are advancing, their large-scale deployment depends on credible demand signals capable of supporting early market formation and reducing investment uncertainty. In response, EU industrial policy increasingly positions public procurement as a strategic demand-side instrument for industrial decarbonisation and lead market creation. However, the extent to which these ambitions are translated into national policy frameworks and operational procurement practice remains unclear.
This paper examines how public procurement is framed and operationalised as a lead market instrument for low-emission steel across multiple governance levels within the European Union. Adopting a multi-level governance approach, the study conceptualises the governance chain through three interconnected levels: 1) the EU as policy signaller; 2) Member States as policy messengers; and 3) public buyers as implementers. The analysis combines quantitative and qualitative policy document analysis with Contract Award Notice (CAN) analysis from the EU Tenders Electronic Daily (TED) database, focusing on Sweden, Germany and the Netherlands.
The results show that procurement is increasingly framed strategically within EU industrial policy discourse, particularly in relation to decarbonisation, competitiveness and industrial resilience. However, substantial governance translation gaps emerge across governance levels, and operational uptake of sustainability-oriented procurement approaches remains comparatively limited. The findings suggest that the primary bottleneck in developing lead markets for low-emission steel lies less in the absence of policy ambition than in uneven governance translation, limited institutional capacity and weak implementation infrastructures.


**Data Sources: Procurement Data**


Procurement data were obtained from the European Union's Tenders Electronic Daily (TED) Open Data portal:

https://data.europa.eu/data/datasets/ted-csv?locale=en

The analysis uses Contract Award Notice (CAN) data covering the period 2010–2023. The original TED datasets were downloaded as CSV files and subsequently filtered to focus on procurement activity within steel-relevant sectors (construction, transport and defence). Further processing steps are documented in the accompanying R scripts.


**R PACKAGES**

Document Analysis (`gs_gpp_docanalysis.R`)

The document analysis script relies on the following packages:

```r
library(readtext)
library(pdftools)
library(quanteda)
library(quanteda.textstats)
library(stringr)
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(ggwordcloud)
library(fmsb)
library(viridis)
```

Package purposes:

| Package            | Purpose                           |
| ------------------ | --------------------------------- |
| readtext           | Import policy documents           |
| pdftools           | Extract text from PDF files       |
| quanteda           | Corpus creation and text analysis |
| quanteda.textstats | Frequency and keyness analysis    |
| stringr            | Text processing and cleaning      |
| dplyr              | Data manipulation                 |
| tidyr              | Data reshaping                    |
| readr              | Reading and exporting data        |
| ggplot2            | Visualisation                     |
| ggwordcloud        | Word cloud visualisations         |
| fmsb               | Radar/spider charts               |
| viridis            | Colour scales                     |

---

TED Procurement Analysis (`gs_gpp_tedanalysis.R`)

The TED procurement analysis script relies on the following packages:

```r
library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(lubridate)
library(ggplot2)
library(viridis)
```

Package purposes:

| Package   | Purpose                                |
| --------- | -------------------------------------- |
| dplyr     | Filtering and aggregation of TED data  |
| tidyr     | Reshaping procurement datasets         |
| readr     | Importing TED datasets                 |
| stringr   | Keyword matching and text processing   |
| lubridate | Date handling and time-series analysis |
| ggplot2   | Visualisation                          |
| viridis   | Colour scales                          |

```
```
