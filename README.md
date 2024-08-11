# IMR Analysis

## Introduction
The health care industry in the US is one of the largest areas of spending in the country.
For 2023, the national health expenditure is expected to top 4.7 trillion dollars, or
~14,000 per person. The US has a complicated payer system in which individuals
largely rely on private insurance. Unfortunately, many patients have claims denied
which requires extra resources to be assigned to fight these decisions.
The state of California permits review of an insurance company’s denied, delayed, or
modified service to a patient’s health care plan. This is managed by the California
Department of Managed Health Care (DMHC) via an Independent Medical Review
(IMR) board. This process occurs when a patients contest a denied, delayed, or
modified service by their health care plan. The IMR is carried out by independent
physicians with no affiliation to the insurance company. During this process, the patient
submits a request for review, and the independent reviewer carefully assesses the
medical records, information provided by the patient, their physician, and the insurance
company. Subsequently, the reviewer issues a binding decision that is applicable to both
parties involved.

## Problem Statement
How can we develop an accurate and reliable classification model to predict the
outcome of the IMR’s decision with the goal of identifying potential biases or disparities
in the review process, and ultimately improving the fairness and efficiency of the IMR
system for resolving denied, delayed, or modified health care services by insurance
companies?

## Data Overview
The data was obtained from the California Department of Managed Health Care
(DMHC) spanning from 2001 to 2023, which includes over 34,000 IMR decisions. The
dataset consisted of both a structured and unstructured portion. The structured data
provided information about each claim, such as the report year, diagnosis, treatment
details, ruling outcome, gender, and days in the review process. On the other hand, the
unstructured data consisted of extensive explanations provided by physicians for each
case, averaging around 300 words each, totaling approximately 10 million words across
the dataset.
