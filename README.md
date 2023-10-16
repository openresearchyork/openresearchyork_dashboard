# Open Access at York Dashboard

The shiny app created by the Open Research Team (University of York) showcases open access practices at the University of York (UK) in an [interactive dashboard](https://shiny.york.ac.uk/open_research_at_york/).

Open Access data from Scopus is based on unpaywall.org. The scopus export file must contain the fields 'Open Access', 'Year', 'Correspondence Address', 'Source title', 'DOI' and 'Document Type'. The Transformative Agreement and York Open Access Fund data was collected by the Open Research team (University of York) and cross-checked with Scopus data to include the Publication type and correspondence address as a proxy for corresponding author affiliation. The current version can be replaced as long as the file name and column names remain the same. The R script combines scopus export data from all files starting with 'scopusUoY'. Hence, when new scopus data is added (e.g. for a new year), the old Scopus file doesn't need to be replaced. Instead, a new file starting with 'scopusUoY' can be added, containing only the data for the new year. 

When any new data is uploaded, the intermediate file 'TAYOAF_OAformat.csv' has to be updated. Deleting the file prompts the script to create a new version automatically. As the script is run every 30 min on the shiny server of the University of York (see link above), any updates might take up to half an hour to appear.

Please let us know how you are using the visualisations and data (lib-open-research@york.ac.uk).
