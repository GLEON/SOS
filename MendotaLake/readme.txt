Data all from https://lter.limnology.wisc.edu/

Mendota Lake Comments & Quirks
Sources of data and manipulations conducted. 


LakeMendotaTS.csv
- datetime: Daily time (00:00 hours) from 7/26/2012 to 10/29/2013. 

- Volume: Static volume; Take from : http://limnology.wisc.edu/lake_information/mendota/mendota.html

- FlowIn: Daily discharge summed from GSGS station 05427948 (Pheasant Branch Creek),05427930 (Dorn Creek at Cty Hwy), and 05427910 (Six Mile Creek at Cty Hwy), and 05427850 (Yahara River at State Hwy 113)

- FlowOut: Daily discharge based on USGS 05428500 gauge station at Yahara River, East Main St, Madison, WI.

- Rain: 

- HypoTemp: Hypo defined as bottom Temp. Daily average of depths are available at https://lter.limnology.wisc.edu/dataset/north-temperate-lakes-lter-high-frequency-water-temperature-data-lake-mendota-buoy-2006-curr. 

- EpiTemp: Daily average of depths are available at https://lter.limnology.wisc.edu/dataset/north-temperate-lakes-lter-high-frequency-water-temperature-data-lake-mendota-buoy-2006-curr. 


- TP: Value of total unfiltered phosphorus (‘totpuf’) based on average from all sampled at 1-5m depths on each sampling date (https://lter.limnology.wisc.edu/dataset/north-temperate-lakes-lter-physical-limnology-primary-study-lakes-1981-current). 

- Chla: Value of chl-a based on average from depths 0-2 on each sampling date (https://lter.limnology.wisc.edu/data/filter/5734). 

- SwDOC: Surface water discharge are assumed to zero, therefore zero were filled for SwDOC 

- Secchi: 


MendotaParameterInputsTrout.txt
#General Parameters
- LakePerimeter:  http://limnology.wisc.edu/lake_information/mendota/mendota.html

- LakeDepth:  http://limnology.wisc.edu/lake_information/mendota/mendota.html

- LakeArea:  http://limnology.wisc.edu/lake_information/mendota/mendota.html

--DOC_init: Observed data on July 10, 2012
- POC_init: Observed data on July 10, 2012

#Sedimentation Parameters
- BurialFactor: Set to 0.01
#SWGW Parameters
- PropCanopy: Calculated by Ian McCullough u
- PropWetlands: Calculated by Ian McCullough
- AerialLoad: 1 gPOC/m/d, based on Hanson et al. L&O 2014, set same as trout
- WetlandLoad: 1 gPOC/m/d, based on Hanson et al. L&O 2014,set same as trout
- DOC_gw: set to 0 based on our assumption that gw is ignorable
- PropGW: set to 0 
- DOC_precip: 2 g/m3, based Hanson et al. L&O 2014, set same as trout

- Residence time: 6.2 years,Thomas D. Brock, A Eutrophic Lake, Lake Mendota, Wisconsin

- DO: Surface 0m