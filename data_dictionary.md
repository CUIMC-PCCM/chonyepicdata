# Data Dictionary

A structured reference for all standard fields  pulled by from Columbia Clinical Data Navigators for Pediatric data from the Epic EHR.
Use this document to understand variable names, data types, allowed values, and relationships.
Data are stored in individual files. Files most commonly use pipe-delimeted format, i.e. using the `|` ("pipe") symbol, although this is not strictly necessary.
Whenever possible data should be pulled using these nomenclatures and formats.

---

##  1. <a name='DatasetOverview'></a>📂 Dataset Overview
- **Dataset Name:**  
- **Description:**  
- **Source:**  
- **Last Updated:**  
- **Maintainer:**  

---

<!-- vscode-markdown-toc -->
* 1. [📂 Dataset Overview](#DatasetOverview)
* 2. [📝 Change Log](#ChangeLog)
* 3. [🧬 Report types](#Reporttypes)
	* 3.1. [Hospital encounters](#Hospitalencounters)
	* 3.2. [Discharge disposition](#Dischargedisposition)
	* 3.3. [Death date](#Deathdate)
	* 3.4. [ADT (Admit/Discharge/Transfer)](#ADTAdmitDischargeTransfer)

<!-- vscode-markdown-toc-config
	numbering=true
	autoSave=true
	/vscode-markdown-toc-config -->
<!-- /vscode-markdown-toc -->

---

##  2. <a name='ChangeLog'></a>📝 Change Log
| Date | Change | Author |
|------|---------|--------|
| 2026-01-05 | Initial version created | Andy Geneslaw |

---

##  3. <a name='Reporttypes'></a>🧬 Report types

###  3.1. <a name='Hospitalencounters'></a>Hospital encounters

These files contain data regarding hospital encounters. Each encounter is a single interaction with the healthcare system. An encounter can be one-time, such as a phone call. An encounter can also span multiple dates, such as an inpatient hospitalization. Thus an inpatient hospital encounter spans the hospital admission date to the hospital discharge date. (A patient who dies is technically discharged on the date of death.)

| Field Name | Type | Description | Allowed Values / Format | Example |
|-----------|------|-------------|--------------------------|---------|
| `MRN` | numeric | Medical record number - an identifier unique to each patient. This is consistent across all encoutners. | positive integer | `xxxxxxxxxx` |
| `PAT_ENC_CSN_ID` | numeric | 	A unique serial number for this encounter. This number is unique across all patients and encounters in the system. One patient MRN will have multiple PAT_ENC_CSN_ID, one for each encounter. | positive integer | `xxxxxxxxxx` |
| `PATIENT_NAME` | string | Patient name | Last Name, First Name | `Smith, John` |
| `BIRTH_DATE` | date | Date of birth | `YYYY-MM-DD` | `2020-12-01` |
| `Sex` | string | Sex assigned at birth | Male, Female, Other | `Female` |
| `AGE_AT_ADMIT` | numeric | Age in years when the encounter began | positive integer | `14` |
| `ETHNICITY` | string | Patient ethnicity as recorded in the EHR. (In future versions this should be removed and entered into a different file.) | `HISPANIC OR LATINO OR SPANISH ORIGIN; NOT HISPANIC OR LATINO OR SPANISH ORIGIN; DECLINED; OTHER` |  |
| `ZIP_CODE` | string | Zip code | 5-character string| `10021` |
| `ADMISSION_DATE` | datetime | Date & time of encounter start / inpatient admission | `YYYY-MM-DD HH:MM:SS` | `2021-06-14 07:28:00.000` |
| `DISCHARGE_DATE` | datetime | Date & time of encounter stop / inpatient discharge | `YYYY-MM-DD HH:MM:SS` | `2021-06-14 07:28:00.000` |

---

###  3.2. <a name='Dischargedisposition'></a>Discharge disposition

What was the patient's destination at the end of the encounter? It may make more sense for this to be included in the **Encounters** file, but sometimes it is separated.

| Field Name | Type | Description | Allowed Values / Format | Example |
|-----------|------|-------------|--------------------------|---------|
| `PAT_ENC_CSN_ID` | numeric | 	A unique serial number for this encounter. This number is unique across all patients and encounters in the system. One patient MRN will have multiple PAT_ENC_CSN_ID, one for each encounter. | positive integer | `xxxxxxxxxx` ||
| `HOSPITAL_DISCHARGE_DATE` | datetime | Date & time of encounter stop / inpatient discharge. Should be identical to `DISCHARGE_DATE` from the **Encounters** file | `YYYY-MM-DD HH:MM:SS` | `2021-06-14 07:28:00.000` 
| `DISCHARGE_DISPOSITION` | string | Patient destination at the end of the inpatient hospitalization. If the patient died will list 'expired' | string | `Home or Self Care`; `Expired`; etc. |

---

###  3.3. <a name='Deathdate'></a>Death date

Date of patient's death, if relevant.

| Field Name | Type | Description | Allowed Values / Format | Example |
|------------|------|-------------|-------------------------|---------|
| `MRN` | numeric | Medical record number - an identifier unique to each patient. This is consistent across all encoutners. | positive integer | `xxxxxxxxxx` |
| `DEATH_DATE` | datetime | Date & time of death. If the  patient did not die, this value will be `NULL` | `YYYY-MM-DD HH:MM:SS`; or `NULL` | |


---

###  3.4. <a name='ADTAdmitDischargeTransfer'></a>ADT (Admit/Discharge/Transfer)

Record of patient movement throughout the hospital, encompassing admissions, discharges, and transfers. Some transfers are virtual, meaning the patient's department or location is listed as changing due to a technicality even though the patient didn't actually physically move. An example of this is when a portable radiology study is performed. The patient is sometimes listed as "transferring" to the Radiology department, even though they stay in their room. Regardless, radiology is not a primary admitting inpatient location so this can be ignored.

Depending on the use case, transfers to operative locations (OR, interventional radiology, cardiac cath lab) may be considered transfers. For example, if the purpose of an analysis is length of stay in the ICU versus the floor:

**ICU &rarr; OR &rarr; ICU** does not change ICU LOS, because the patient never "left" the ICU, they just stopped in the OR for a procedure.

whereas

**general inpatient unit &rarr; OR &rarr; ICU** changes the LOS on the floor and in the ICU. The general inpatient unit duration ends when the patient goes to the OR. And the ICU LOS clock begins when the patient enters the ICU.

There are several different types of events, defined in the `EVENT_TYPE` field:

* Admission             
* Census       
* Discharge
* Hospital Outpatient  
* Leave of Absence Out 
* Leave of Absence Return
* Patient Update      
* Transfer In      
* Transfer Out  

Only Admission, Discharge, Transfer In, and Transfer Out are really relevant. The others have more to do with daily updates and census counts. It is unclear what Leave of Absence really is but it is exceedingly rare.

| Field Name | Type | Description | Allowed Values / Format / Example |
|------------|------|-------------|-----------------------------------|
| `MRN` | numeric | Medical record number - an identifier unique to each patient. This is consistent across all encoutners. | `1234567890` |
| `PAT_ENC_CSN_ID` | numeric | 	A unique serial number for this encounter. This number is unique across all patients and encounters in the system. One patient MRN will have multiple PAT_ENC_CSN_ID, one for each encounter. | `1234567890` |
| `EVENT_ID` | numeric | A unique serial number for the ADT event. This is seldom used.  `123456789` |
| `EVENT_TYPE` | string | Type of event, i.e. admit, transfer, etc. | (see above on valid types) |
| `EFECTIVE_TIME` | datetime | Date/time when the ADT event occurred. | `YYYY-MM-DD HH:MM:SS` |
| `DEPARTMENT_ID` | numeric | Unique serial number defining the different location departments. | `123456789` |
| `DEPARTMENT_NAME` | string | Name of the location as recorded in the EHR | `MSCH 9 TOWER` |
| `PAT_CLASS` | string | Type of patient | `Emergency`; `Inpatient`; etc |
| `BED_LABEL` | string | Actual room location as recorded in the EHR. Generally follows the format `xxyy-zz` where `xx` is the floor, `yy` is the room number, and `zz` is the bed space. For single-digit floors, this follows the format `xyy-zz` this means the floor is a single digit (e.g. room 837-01 is the 8th floor, room 37, bed 01.)| `837-01` |
| `PATIENT_SERVICE` | string | Hospital service or team responsible for the patient. This is input from a dropdown by the ordering provider in the EHR, and is not verified to be correct. (i.e. the ordering provider might note a patient is a surgical patient when they are actually an ICU patient).  | `Pediatric - Critical Care` |


---

### ICD10 Diagnosis codes

These are diagnosis codes entered into the system in a variety of ways. All of these codes are ICD10 Clinical Modification (CM) Diagnosis Codes (as opposed to procedure codes). They can be entered in the Problem List, as an Admission diagnosis, or Billed. Billed diagnoses are supposed to be entered as A Primary Diagnosis means that it was listed as Primary (in any way), or Billed as the first diagnosis on a given day. The first Billed diagnosis is supposed to be the proximate cause for a given charge. This is only as good as the person entering the bill, however.

Depending on the dataset these may be limited to diagnoses billed by a critical care attending physician, or by any physician. 

These may also be constrained to a given hospital encounter, versus across all time.

These are other locations to pull these data from. Notably there are encounter level diagnoses, and discharge diagnoses.

| Field Name | Type | Description | Allowed Values / Format / Example |
|------------|------|-------------|-----------------------------------|
| `MRN` | numeric | (**NOT ALWAYS PRESENT**) Medical record number - an identifier unique to each patient. This is consistent across all encoutners. | `1234567890` |
| `PAT_ENC_CSN_ID` | numeric | 	A unique serial number for this encounter. This number is unique across all patients and encounters in the system. One patient MRN will have multiple PAT_ENC_CSN_ID, one for each encounter. | `1234567890` |
| `ICD10_CODE` or `ICD10` | string | ICD10-CM diagnosis code. Format with a period. These can be classified into diagnosis groups according to [HCUP Clinical Classification Software](https://hcup-us.ahrq.gov/toolssoftware/ccsr/dxccsr.jsp). The R package [pccc](https://cran.r-project.org/web/packages/pccc/index.html) also may be very useful for classifying complex chronic conditions. | `R63.30` |
| `DX_NAME` | string | Description of `ICD10_CODE` field. | `Feeding difficulties, unspecified` |
| `DX_DATE` | datetime | Date/time when the the ICD10 code was entered. | `YYYY-MM-DD HH:MM:SS` |
| `PRIMARY_DX_YN` | factor | Value is `Y` if this code was listed as a primary diagnosis, or as the first Bill diagnosis. Otherwisw `N`  | `Y` or `N` |
| `DX_TYPE` | string | Where in the patient record did the ICD10 code originate? | `Admission`; `Billed 1`; `Billed 2`; `Billed 3`; `Billed 4`; `Billed 5`; `Billed 6`; `Hospital Problem List` |
