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
