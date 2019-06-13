# Word Counts

This script was developed by the Evans School Policy Analysis & Research Group (EPAR) to report how frequently groups of keywords appear in a group of documents, normalized to the length of each document. The initial use case was looking for discussion of specific areas (e.g. self help groups) by specifying keywords for each area (e.g. self help groups, shgs, women's empowerment collective, wecs).

The inputs are a csv file that contains keywords and the "buckets"/groups they fit into, from hereon referenced as "dictionary csv", as well as a directory of documents to conduct word counts in (in doc or docx format). 

The output is a csv file with the raw count of words within each bucket and the counts normalized to the number of characters in the document (a proxy for the length of documents).

* Example input dictionary csv: dict_input_counts.csv
* Example output: words_counts_example_output.xlsx

Tip: If you want to apply this tool on PDFs, newer versions of Microsoft Word have some built-in optical character recognition (OCR) capabilities and can automatically convert PDFs to Word documents. Simply right-click on the PDF document and click "Open With" Microsoft Word. 

Last updated June 10, 2019
