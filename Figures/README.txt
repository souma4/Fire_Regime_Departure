This a folder that contains all figures. 
ave_sev_map are maps of the average fire severity for each mask analysed
bps_map shows the BioPhysical Setting map for each mask
freq_bar shows fire frequency distributions for historical and contemporary periods over every mask
freq_sev_each shows fire severity classifications made over every mask. This is using all three LANDFIRE classifcations for fire severity
freq_sev_lm2h shows fire severity classifications, combining low and mixed severity, similar to Mallek et al 2013 and Williams 2023
sev_density_burned are fire severity distributions between historical and contemporary periods. This is what I eventually want to use in my analysis. I am selecting
      a class via the LANDFIRE BPS information, then drawing a number from a uniform distribution between the thresholds for that given class, estimating CBI.
      this is also performing the analysis under the logic of "if a location burned in the modern record, what would the fire severity be if it burned historically"
      that is why burned in the name, the fire severity incidences and information is coming from pixels that burned in the modern day
sev_density_est. This is the same as sev_density_burned HOWEVER this is using estimated historical fire frequencies instead of modern fire frequency. This is 
     attempting to model fire severity distributions as a function of where burns occured in the past. There doesn't appear to be a large difference in this compared
     to sev_density_burned, however is good to look at because fires may be burning in different locations today than historically.
sev_violin. These are just violin plots of sev_density_burned. Same information, packaged differently
final_figures, these are the figures used in the poster. Major big picture figures
