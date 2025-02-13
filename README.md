# taperNOR
Taper models for spruce, pine and birch in Norway
and helper functions

The taper models are based on Endre Hansen, Johannes Rahlf, Rasmus Astrup & Terje Gobakken (2023) Taper, volume, and bark thickness models for spruce, pine, and birch in Norway, Scandinavian Journal of Forest Research, DOI: 10.1080/02827581.2023.2243821 (https://www.tandfonline.com/doi/full/10.1080/02827581.2023.2243821?scroll=top&needAccess=true&role=tab) and correction, DOI: 10.1080/02827581.2024.2358467 (https://www.tandfonline.com/doi/full/10.1080/02827581.2024.2358467).

## Abstract
Taper models, which describe the shape of tree stems, are central to estimating stem volume. Literature provides both taper- and volume models for the three main species in Norway, Norway spruce, Scots pine, and birch. These models, however, were mainly developed using approaches established over 50 years ago, and without consistency between taper and volume. We tested eleven equations for taper and six equations for bark thickness. The models were fitted and evaluated using a large dataset covering all forested regions in Norway. The selected models were converted into volume functions using numerical integration, providing both with- and without-bark volumes and compared to the volume functions in operational use. Taper models resulted in root mean squared error (RMSE) of 7.2, 7.9, and 9.0 mm for spruce, pine, and birch respectively. Bark thickness models resulted in RMSE of 2.5, 6.1, and 4.1 mm, for spruce, pine, and birch respectively. Validation of volume models with bark resulted in RMSE of 12.7%, 13.0%, and 19.7% for spruce, pine, and birch respectively. Additional variables, tree age, site index, elevation, and live crown proportion, were tested without resulting in any strong increase in predictive power. 

## Installation

```R
devtools::install_git("https://github.com/SmartForest-no/taperNOR")
```
