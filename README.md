# publication_analyzer
This program (R shiny web app) analyzes publications found in catalogues of two large Russian libraries. 
The goal is to display a time chart of found publication based on the search in publication title.
It also allows downloads and comparison of datasets. 

**Current hosted version is available at** https://scherbakovdmitri.shinyapps.io/Publication_analyzer/

The program has two modes.
1) Top part of page - allows to choose a library and enter a search term (for example a name). It returns the found publication with a time chart and a table of most occuring authors where search term is found (in the Title).
![Screen Shot 2021-09-13 at 6 08 36 PM](https://user-images.githubusercontent.com/65131820/133109253-79a1afa4-273e-4f63-9718-4dfaf42635df.png)


For example, we are looking for publications which have the name of the writer Chernyshevksy. Here we clearly see three publication peaks which coincide with birthday anniversary of Chernushevsky (100, 125, 150 years since his birth year - 1828).

2) The comparison part, second half of page - allows to compare up to 3 datasets previously saved as CSV to see dynamics in publication activity.
As example I compare publications mentioning Chernyshevsky with the ones mentioning Dostoevsky - we can see the growing popularity of the latter in the last several decades:
![Screen Shot 2021-09-13 at 6 21 48 PM](https://user-images.githubusercontent.com/65131820/133111327-9838aeca-a7c1-46bf-ac90-5cf7d91883fe.png)


