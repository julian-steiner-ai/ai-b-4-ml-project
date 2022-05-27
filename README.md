# KI-B-4 - Machine Learning Project

Repository for PStA in Machine Learning (KI-B-4)

## Dataset

Source of dataset: [kaggle](https://www.kaggle.com/datasets/kukuroo3/body-signal-of-smoking).

### Information

This dataset is a collection of basic health biological signal data. The goal is to determine the presence or absence of smoking through bio-signals.

### Columns

- ID : index
- gender
- age : 5-years gap
- height(cm)
- weight(kg)
- waist(cm) : Waist circumference length
- eyesight(left)
- eyesight(right)
- hearing(left)
- hearing(right)
- systolic : Blood pressure
- relaxation : Blood pressure
- fasting blood sugar
- Cholesterol : total
- triglyceride
- HDL : cholesterol type
- LDL : cholesterol type
- hemoglobin
- Urine protein
- serum creatinine
- AST : glutamic oxaloacetic transaminase type
- ALT : glutamic oxaloacetic transaminase type
- Gtp : γ-GTP
- oral : Oral Examination status
- dental caries
- tartar : tartar status
- smoking

### Source

Some post-processing and filtering has done from the raw data.

[Link](https://www.data.go.kr/data/15007122/fileData.do)

## Use Case

The use case for our trained machine learning model is for health insurance agencies to determine (after a regular check up on them) wether or not a potential costumer has lied about them smoking on their insurance application.