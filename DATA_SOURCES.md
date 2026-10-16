# Data sources

No data is distributed with this repository. This file lists each input, its
license, what the pipeline uses it for, and the path the code expects.

| Source | Used for | License / terms | Included? | Expected path |
|---|---|---|---|---|
| Professional profile data (public profiles, obtained through a commercial data provider) | Education and employment histories | Provider terms; concerns identifiable individuals | No | `data/LinkedIn people profiles.csv`, plus supplementary pulls under `output/tables/*_collected.csv` |
| Company records (same provider) | Employer industry, size, type | Provider terms | No | `data/LinkedIn company information.csv`, `output/tables/companies_pull_collected.csv` |
| ProQuest Dissertations & Theses, via TDM Studio | Identity and field verification, PhD year, subject terms | ProQuest TDM license; not redistributable | No | `data/extended.csv/extended.csv` |
| NeuroTree / Academic Family Tree export | Advisor lineage, dissertation bridge, publications, NIH grants | CC BY 3.0 | No (download script provided) | `data/neurotree/` via `data/fetch_neurotree.R` |
| Research Organization Registry (ROR) | Institution resolution and country | CC0 | No (download script provided) | `data/ror/` via `data/fetch_ror.R` |
| research.com neuroscience university rankings (2026) | Program-prestige covariate | Publisher terms | No | `data/research_com_neuroscience_university_rankings_2026.csv` |
| US News neuroscience rankings (2020) | Program-prestige covariate | Publisher terms | No | `data/usnews_neuro_rankings_2020.csv` |
| UCOT-Exp2 coding key | Sector cross-check taxonomy | CC BY (OSF project 5cguj) | **Yes** | `data/dictionaries/ucot_exp2_coding_key.csv` |

## Supplementary profile acquisition

Some neuroscientists' profiles cannot be found by searching profile content,
because their education entries carry no degree or field text. These people
were located by identity instead: names from NeuroTree and ProQuest records
were resolved to profiles and then verified against institution and
graduation year. Matches confirmed only by name were then checked against the
fetched education years, and profiles that contradicted the seed record were
removed. The acquisition scripts are not included here. The pipeline reads
their outputs through stub-guarded file targets, so it builds without them.

## Attribution obligations

- **NeuroTree:** any published finding using NeuroTree-derived data must
  credit academictree.org and cite
  [10.1038/s41467-018-07034-y](https://doi.org/10.1038/s41467-018-07034-y) or
  [10.5281/zenodo.4247418](https://doi.org/10.5281/zenodo.4247418).
- **UCOT-Exp2:** Stayart et al. (2020), *F1000Research* 9:8,
  [10.12688/f1000research.21641.1](https://doi.org/10.12688/f1000research.21641.1).
- **ROR:** CC0; no attribution required, though [ror.org](https://ror.org) is
  acknowledged.

## Privacy

Results will be published only as aggregates, with small cells suppressed.
No profile data, names, profile URLs or person-level tables will be
released. If you believe you can be identified in a published aggregate,
please open an issue on this repository.
