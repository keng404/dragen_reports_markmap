# dragen_reports_markmap
Create concept mapping of metrics within DRAGEN HTML report to initial data source file.

This is an unofficial tool to help visualize how (i.e. what files and how fields within the HTML report are computed) the HTML report is formed.

[DRAGEN reports](https://help.dragen.illumina.com/product-guides/dragen-v4.4/dragen-reports) can be bundled as a part of an Illumina BaseSpace or Illumina Connected Analytics pipeline.


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
