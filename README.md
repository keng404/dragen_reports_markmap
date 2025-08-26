# dragen_reports_markmap
Create concept mapping of metrics within DRAGEN HTML report to initial data source file.

This is an unofficial tool to help visualize how (i.e. what files and how fields within the HTML report are computed) the HTML report is formed.

[DRAGEN reports](https://help.dragen.illumina.com/product-guides/dragen-v4.4/dragen-reports) can be bundled as a part of an Illumina BaseSpace or Illumina Connected Analytics pipeline.

[![Ask DeepWiki](https://deepwiki.com/badge.svg)](https://deepwiki.com/keng404/dragen_reports_markmap)


# The Flow

- 1) ```dragen_reports.json_mapper.R``` 
    - Take JSON configuration files from DRAGEN reports from the **manifests** directory and create 1 JSON file per report type
- 2) ```json_finalize_names.R```
    - Pre-process the JSON file and rename fields as needed
- 3) ```json_to_markdown.R```
    - Convert JSON file to a md (Markdown) file
- 4) run Markmap JS
    - via ```keng404/markmap:0.18.11``` docker image, ingest Markdown file
    and generate interactive mindmap plot in HTML file

# DISCLAIMER

TO THE GREATEST EXTENT PERMITTED BY APPLICABLE LAW, THIS WEBSITE AND ITS CONTENT, INCLUDING ALL SOFTWARE, SOFTWARE CODE, SITE-RELATED SERVICES, AND DATA, ARE PROVIDED "AS IS," WITH ALL FAULTS, WITH NO REPRESENTATIONS OR WARRANTIES OF ANY KIND, EITHER EXPRESS OR IMPLIED, INCLUDING, BUT NOT LIMITED TO, ANY WARRANTIES OF MERCHANTABILITY, SATISFACTORY QUALITY, NON-INFRINGEMENT OR FITNESS FOR A PARTICULAR PURPOSE. ALL WARRANTIES ARE REJECTED AND DISCLAIMED. YOU ASSUME TOTAL RESPONSIBILITY AND RISK FOR YOUR USE OF THE FOREGOING. ILLUMINA IS NOT OBLIGATED TO PROVIDE ANY SUPPORT FOR ANY OF THE FOREGOING, AND ANY SUPPORT ILLUMINA DOES PROVIDE IS SIMILARLY PROVIDED WITHOUT REPRESENTATION OR WARRANTY OF ANY KIND. NO ORAL OR WRITTEN INFORMATION OR ADVICE SHALL CREATE A REPRESENTATION OR WARRANTY OF ANY KIND. ANY REFERENCES TO SPECIFIC PRODUCTS OR SERVICES ON THE WEBSITES DO NOT CONSTITUTE OR IMPLY A RECOMMENDATION OR ENDORSEMENT BY ILLUMINA.
