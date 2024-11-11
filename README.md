# Premature mortality and substance use
- Ask 1: What proportion of premature deaths, given a cutoff point e.g. 55, are due to drugs and alcohol.  
- Ask 2: Years of life lost (YLL) as a measure of premature mortality 
*NB: ask Chioma for alcohol-specific deaths data by age and maybe LA.* 
- Ask 3: Add IMD to all of this - preferably by lower levels check feasibility first 

## 1. Proportion of deaths

### Number of deaths associated with drug use 

The ONS classifies death related to drug poisoning according ICD-10 codes. Certain ICD-10 codes classify a death as a "drug misuse death"[^1]. Each of these requires a specific substance (e.g. heroin) or substance category (e.g. opioids) to be indicated either in the ICD-10 code or on the death certificate. 

There are deaths each year where the ONS holds no information on the substance(s) involved[1^]:

| Year of death registration | All drug poisonings | Number of deaths without substance information | Percentage without substance information |
| :------------------------: | :-----------------: | :--------------------------------------------: | :---------------------------------------: |
|            2023            |       5,448         |                     1,245                      |                  22.9                     |
|            2022            |       4,907         |                     1,239                      |                  25.2                     |
|            2021            |       4,859         |                     1,219                      |                  25.1                     |
|            2020            |       4,561         |                     1,050                      |                  23.0                     |



Some of these will be classified as related to drug misuse where an ICD-10 code indicates mental and behavioural disorders due to drug use (excluding alcohol and tobacco) without a specific substance (e.g. F19 "multiple drug use and use of other psychoactive substances"). But others, broadly those coded as accidental/intentional self-poisonings or self-poisonings of unknown intent, will not be classfified as related to drug misuse unless a controlled drug under Misuse of Drugs Act 1971 was mentioned on the death record. 

The data linkage between ONS mortality and NDTMS allows some of those deaths to be identified indirectly as related to drug misuse where the person that died had had contact with the drug treatment system within a year of their date of death. 




[^1]: The criteria for this classification are described in `Box 2` of the `Definition` tab of the latest relese of *Deaths related to drug poisoning, England and Wales* available [here](https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/datasets/deathsrelatedtodrugpoisoningenglandandwalesreferencetable).
[^1]: This is the first four rows of `Table 11` in the same release.
---

## 2. Years of life lost due to substance use  

Years of life lost (YLL) is a measure of the impact of premature mortality, helpfully defined by Public Health England [here](https://fingertips.phe.org.uk/static-reports/health-profile-for-england/definitions-regional.html#years-of-life-lost-yll)

Chudasama et al. (2022) investigated five methods for estimating YLL[^1]. The first two methods are feasible with the available data for YLL from drug use and alcohol specific deaths. Only the drug-related YLL could be segmented by geographical estimates of deprivation.

All five methods are detailed in the supplementary PDF [here](https://ars.els-cdn.com/content/image/1-s2.0-S0895435622001639-mmc1.pdf).

### Mortality data
- Deaths related to drug misuse as defined by the ONS[^2].
- Additional drugs related to drugs misuse that the ONS had insufficient data to classify, but can be inferred from the data linkage of ONS data with NDTMS data by [Better Outcomes through Linked data (BOLD)](https://www.gov.uk/government/publications/ministry-of-justice-better-outcomes-through-linked-data-bold) [^2].
- Alcohol specific deaths

[^1]: Chudasama, Y.V., Khunti, K., Gillies, C.L., Dhalwani, N.N., Davies, M.J., Yates, T., & Zaccardi, F. (2022). Estimates of years of life lost depended on the method used: tutorial and comparative investigation. Journal of Clinical Epidemiology, 150, pp. 42-50. Available at: https://doi.org/10.1016/j.jclinepi.2022.06.012 [Accessed 6 Nov. 2024].
[^2]: *Not publicly available*