by Borhan Mostafizi 
updated May 27, 2025

The purpose of the program is to find the highest-performing protein biomarkers based on AUC values. Next, using the most functional proteins, graphs for each cancer are created to show the shift in the stage of detection for cancer cases. The main purpose of this software is to emphasize the effective possibility of detecting cancers using protein biomarkers. 

File Purposes

- Data
  - cancer.table.xlsx: A SEER dataset for various cancer cases. It also includes the level of presence of the 39 most common protein biomarkers in and around the cancer. This dataset was used to find the best-performing biomarker.
  - 20200113_external_train_sens.xlsx: This dataset was used to create the graphs.
 
- R
  - ProteinBiomarkerauc.R: This code generates the AUC values of 39 protein biomarkers for each of the 8 most common cancers and their stages. The AUC value considers the protein's specificity and sensitivity relative to the cancer and its stage.
  - CancerTableGenerator.R: This code generates two graphs. The first utilizes the protein with the largest AUC value and creates a graph that highlights the shift in detection of cancer cases. The second graph highlights the mortality from the newly-shifted cancer cases.
  - SensitivityToMortalityReductionConverter.R: This program models the relationship between biomarker sensitivity and mortality reduction using a recursive stage interception algorithm. Given user-defined sensitivity and cancer type, it simulates cancer detection across stages, calculates how many late-stage cancers (III/IV) are shifted to early stages (I/II), and estimates overall mortality reduction by scaling with a surrogate endpoint factor (p). The tool also performs optimization to find the sensitivity value that maximizes mortality benefit and includes a histogram of mortality reduction simulations. 
 
- Output
  - auc.txt: This is the output of 'proteinbiomarkerauc.R'. It's the AUC values of each protein biomarker for each cancer type and stage.
  - BreastCancerGraphs/ColonCancerGraphs/EsophagealCancerGraphs/LiverCancerGraphs/LungCancerGraphs/OvarianCancerGraphs/PancreaticCancerGraphs/StomachCancerGraphs: It's the generated cancer graphs for each of the cancers researched. 
