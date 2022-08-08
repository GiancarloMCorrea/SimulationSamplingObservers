# Simulation framework: Collected data by onboard observers

This is the code used in Correa et al. (XXXX) to evaluate the impacts 
of onboard observers sampling strategies on collected data. This code 
can be applied to any stock and fishery worldwide. 

There are six main R files in this repository:

-   `runSimulationParallel.R`: run the simulation framework in parallel (Windows users)
-   `1_parameters.R`: contain the parameters of the simulation. This is the file that the user needs to modify to apply this code to other case studies. 
-   `2_spatial_variability.R`: simulates the Gaussian random field for fish abundance
-   `3_population_fishery_model.R`: simulates the dynamics of the population and fishery
-   `4_sampling.R`: evaluate different sampling strategies by onboard observers
-   `analyze_results.R`: analyze the simulated data and produces figures and tables

Feel free to modify any part of this repository to apply it to specific case studies. 

The most important file is `1_parameters.R`, which contains the parameters for this simulation framework. It is divided in four sections:

Simulation parameters
---------------------

### Population model

-   `maxAge`: age plus grop.
-   `minAge`: minimum age. Should always be 0.
-   `maxLen`: maximum length (cm)
-   `minLen`: minimum length (cm)
-   `lenBin`: length bin size (cm)
-   `meanRec`: mean recruitment (number of fish)
-   `sigmaR`: variability of recruitment
-   `femFrac`: fraction of females in the population
-   `A1_par`: reference age
-   `Linf`: asymptotic length (cm)
-   `K_par`: growth rate (1/year)
-   `M_par`: natural mortality (1/year)
-   `L1_par`: mean length at age `A1_par`
-   `SD1`: standard deviation of lengths at `minAge`
-   `SD2`: standard deviation of lengths at `maxAge`
-   `par_a`: parameter length-weight relationship
-   `par_b`: parameter length-weight relationship




