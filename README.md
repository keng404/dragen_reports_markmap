# dragen_reports_markmap
Create concept mapping of metrics within DRAGEN HTML report to initial data source file


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