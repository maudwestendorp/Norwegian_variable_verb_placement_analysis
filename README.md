Analysis code and data of "Variable verb placement in Norwegian: deviations from asymmetric V2"
---------------------
July 1, 2021

### Author:

- Maud Westendorp, UiT The Arctic University of Norway

### About the project:
This repo contains analysis and data files associated with the following publication:
Maud Westendorp. 2021. "[Variable verb second in Norwegian main and embedded clauses](https://doi.org/10.5617/nals.9423)". *Nordic Atlas of Language Structures (NALS) Journal*
6(1): 1–48.

It is part of the project "[Nordic Word Order Database](https://tekstlab.uio.no/nwd)". [A repo for the larger project](https://github.com/maudwestendorp/NWD) is also on this Github.

### Analysis files:

- 01NV2_preprocessing
- 02NV2_mixed_model_analyses

### Data files:

- NO2.csv

## Code book:
no2-dataframe:
- Informant: unique participant number (e.g., "T200", "KO04", "NOR002")
- InformantGroup: groupID (i.e., A - D)
- Northern: does the participant speak a Northern-Norwegian dialect? ("yes"/"no")
- Version: version of the experiment (i.e., 1 - 3)
- UniqueNumb: unique itemID (e.g., "2311", "2753")
- SentenceID: new IDs for grouping together items with same verb/adverb combination in EV2-condition (e.g., "ass.oft1", "int.nev2")
- Type: what for does the response have?   
     "Read": read aloud sentences from text,  
     "Produce": transformed responses based on read-aloud text,  
     "Spoken": responses in spoken-mode experiment, after on spoken dialect elicitation 
- Subcondition: exact condition in the experiment (e.g., "ObjQshort", "BridgeV", "V3adv")
- Adverb: adverb used in elicitation (i.e., "ikke", "aldri", "alltid", "ofte" or "none")
- Background.sentence: background sentence in experiment (e.g., "Jeg setter meg ofte lengst bak i bussen.")
- Response: exact response (e.g., "Han Ole sier at han sett sæ ofte lengst bak i bussen.")
- WordOrder: word order in response (e.g., "VA": verb > adverb)
- Comment: comments from annotator on responses
