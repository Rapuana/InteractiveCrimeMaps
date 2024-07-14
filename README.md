# Interactive Crime Map project

Auther: Samuel Dudley

## Introduction

Initially this project is part of a technical test. The project currently visualises Crimes across Brisbane LGA grouped by suburb.

Notes:

* The LGA data is sourced from QGSO from 2016
* The Boundary Data for the suburb plot is sourced from absmapsdata package also from 2016
* The offences data is sourced from Queensland Government and is sourced from 2023-06-30 to 2024-06-30.

The difference in times for the map data and the offences data causes some differences in the Meshblocks used to map the offences data to a suburb.

This could have been avoided by simply calling the API by the Suburb, for all suburbs in Brisbane, however that would have over 180 times more API calls.
