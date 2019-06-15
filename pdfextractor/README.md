# PDF Extractor

pdf_extractor_web_v2.R

This script was developed by the Evans School Policy Analysis & Research Group (EPAR) to retrieve PDFs from any number of pages of Google.com Results based on a list of search queries. The initial use case was looking for regulations related to digital credit in Sub-Saharan Africa and Asia.

The input is a csv of search queries: 

* Example input csv: web_input_search_terms.csv

The outputs are a directory (folder) with all of the extracted PDFs, a directory with the extracted PDFs where duplicates have been removed, and a spreadsheet with the most relevant PDFs sorted to the top, hereon referred to as the "top pdfs ledger".

* Directory with all PDFs: web_results
* Directory with PDFs where duplicates have been removed: unique_results
* Example top pdfs ledger: pdfs_extracted.xlsx

Note 1: In the directory with all PDFs, the file names include a prefix for which query obtained that result. The query number corresponds with the query order provided in the input csv of search queries. 

Note 2: If you do not change the names for the folders/directories in the user input section at the top of the R script, existing folders/directories with that file name will be overwritten (and files within them deleted). So rename those folders or copy your initial results into a different location if you plan to run this script multiple times! 

It takes approximately 3.33 minutes (3 minutes and 20 seconds) for one page of results to download on a lab computer. So if you have 5 search queries and are planning to download the first 10 pages of results, it will take 5*10*3.33=166.66 minutes or 2.7 hours for the script to run. Close as many other programs you have running to increase the speed. However, the speed may still differ from computer to computer. 

Tip: If you have a long list of search queries, the R script may be blocked by Google at some point. Making too many requests from the server can cause Google to think this the script is conducting suspicious activity. If this happens to you, we recommend downloading the (free) tool Windscribe (https://windscribe.com/) and encrypt your browsing activity.

Last updated June 14, 2019