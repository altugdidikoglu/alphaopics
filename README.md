![Screenshot](logo.png)

# Species and Photoreceptor-Specific Light Exposure Measurement in Mammals
--------------------------------------------------------------------------------

**Light serves as one of the primary environmental cues for regulating various physiological and behavioral processes. Disruptions in circadian rhythms resulting from mistimed or inadequate light exposure have been linked to a range of health problems across mammalian species. As a consequence, ensuring appropriate lighting is crucial for indoor housed mammals. Lux is the most commonly used metric for measuring lighting, yet it is a weighted spectral function based on human perceived brightness and is unsuitable for capturing non-image forming effects of light or for measuring light exposure across different species. Photoreceptor-specific (α-opic) measurements of lighting have been proposed as a more suitable units of quantifying light in humans. Since the number of photoreceptor types, photopigment spectral sensitivities, and eye anatomy differ across mammalian species, we have developed a method for measuring light exposure in a species-specific manner based on photoreceptor activation.**

--------------------------------------------------------------------------------

*The package was prepared by Time Vision Behaviour Lab in the University of Manchester. This project has received funding from the Wellcome Trust Investigator Award (210684/Z/18/Z) to [Robert Lucas](https://research.manchester.ac.uk/en/persons/robert.lucas). The package is maintained by [Altug Didikoglu](https://research.manchester.ac.uk/en/persons/altug.didikoglu).*

--------------------------------------------------------------------------------

# alphaopics

R package to calculate mammal species and photopigment specific light exposures

## Installation

**Install with GitHub**

To install this package from GitHub, make sure that the `devtools` library is installed.

```
install.packages("devtools")
library(devtools)
```

Use `install_github` to install using `devtools`.

```
install_github("altugdidikoglu/alphaopics")
```



This repository includes R scripts to calculate animal light exposure and required reference data
1. **functions.R** includes functions:
* *govardovskii*

<sub>Calculate Govardovskii nomogram as Govardovskii et al. 2000. (Vis Neurosci. 2000 Jul-Aug;17(4):509-28. doi: 10.1017/s0952523800174036)</sub>

* *photonenergy*

<sub>Calculates the energy of a photon at a given wavelengths(nm)</sub>

* *harmonisetrans*

<sub>Extrapolate/adjust species specific prereceptoral filtering data</sub>

* *harmonisesens*

<sub>Extrapolate/adjust curve to match the wavelength range for species specific action spectra</sub>

* *generateaopicactionspec*

<sub>Generate a govardovski nomogram with corrections prereceptoral filtering</sub>

These functions are called by *calculateEDI* function which calculates EDI, total irradiance, and effective photon according to given spectral power distribution, wavelength range, species name, opsin type, sensitivity curve (from data or given lambda max), prereceptoral filtering (from data or given normalised transmission data)

2. **data** includes required spectral sensitivity and transmission data and standards

3. **addspecieswizard.R** includes code to add new sensitivity or transmission to data

4. **App.R** includes Shiny package to convert functions to a web page

Online app: [Animal α-opic light exposure calculator](https://altugdidikoglu.shinyapps.io/alphaopics/)
