Relationship between crime and CCTV presence in Baltimore
=========================================================

Overview
--------
The goal of this script is to create a simple plot relating
number of arrests and prescence of CCTV cameria in Baltimore
city. This is part of a class exercise for the [CMSC 702](http://www.cbcb.umd.edu/~hcorrada/CMSC702/)
at [University of Maryland](http://umd.edu/) taught by [Héctor Corrada Bravo](http://www.cbcb.umd.edu/~hcorrada/).

Note that the [GDAL](http://www.gdal.org/) library is needed in order to 
install the rgdal binding.

Data
----
The following datasets from [data.baltimorecity.gov](http://data.baltimorecity.gov/)
were used:

* [BPD Arrests](https://data.baltimorecity.gov/Crime/BPD-Arrests/3i3v-ibrt)
* [CCTV Locations](https://data.baltimorecity.gov/Crime/CCTV-Locations/hdyb-27ak)
* [Baltimore City Line - Shape](https://data.baltimorecity.gov/Geographic/Baltimore-City-Line-Shape/936n-hugy)
* [Neighborhoods](https://data.baltimorecity.gov/Geographic/Neighborhoods/5cni-ybar)

References
----------
* http://www.r-bloggers.com/visualizing-baltimore-with-r-and-ggplot2-crime-data/

Screenshot
----------
![screenshot](https://github.com/khughitt/baltimore_ggplot2/blob/master/result.png)

