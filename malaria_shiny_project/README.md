# Malaria Elimination Evidence Explorer (Shiny + Leaflet)

This project is a Shiny application intended to be embedded in a larger website. It combines a WHO European Region country-year malaria case compilation with an Azerbaijan-focused intervention/financing timeline.

## Included files

- `app.R` — Shiny app with interactive choropleth, year slider, case-count/rate/insurance indicator selector, Azerbaijan intervention timeline, country detail panel, and data-quality/source tabs.
- `prepare_data.R` — data checks, provenance/schema safeguards, optional population-rate join, and QA report generation.
- `data/who_europe_malaria_indigenous_cases_2000_2015_audited.csv` — country-year case data with preserved source values, map-ready values, QA flags, provenance fields, and optional population-rate fields.
- `data/azerbaijan_malaria_interventions_financing_2000_2015_audited.csv` — documented intervention and finance timeline with explicit funding-measure limitations.
- `data/population_denominators.csv` — blank template for cited annual population denominators.
- `data/health_insurance_coverage.csv` — blank template for comparable country-year insurance coverage.

## Run locally

1. Install R and RStudio.
2. Unzip this project, open the project folder in RStudio, and set it as the working directory.
3. Install the required packages:

```r
install.packages(c(
  "shiny", "leaflet", "sf", "dplyr", "readr", "stringr",
  "ggplot2", "DT", "scales", "htmltools", "rnaturalearth",
  "rnaturalearthdata"
))
```

4. Run the data preparation script:

```r
source("prepare_data.R")
```

5. Start the app:

```r
shiny::runApp()
```

## Optional population rates

Add one row per ISO3-year to `data/population_denominators.csv` with these columns:

- `iso3` — ISO3 country code, e.g. `AZE`
- `country`
- `year`
- `population` — numeric population denominator
- `population_source_citation`
- `population_source_url`
- `source_edition`
- `retrieval_date` — date you retrieved the source, not a guessed historical date
- `verification_status`
- `notes`

The rate is calculated as `map_value_cases / population * 100000` only when both a displayable case value and a positive population denominator exist. Do not calculate a rate from an unresolved case count.

## Optional health-insurance coverage

Add one row per ISO3-year to `data/health_insurance_coverage.csv` with these columns:

- `iso3`, `country`, `year`, `coverage_percent`
- `indicator_name` — precisely name the measure (e.g. population covered by a defined insurance scheme)
- `population_definition` — who is included in the denominator
- `insurance_type`
- `source_citation`, `source_url`, `source_edition`, `retrieval_date`, `verification_status`, `notes`

Use a consistent indicator and definition across countries and years. Insurance coverage is displayed as a separate health-system indicator; it is not a proxy for free diagnosis, access to antimalarial treatment, bed-net distribution, or malaria intervention coverage. If comparable data are unavailable, leave the template empty and state the limitation.

## Palette and map interpretation

The requested palette is used for the six positive-value bins: `#c2168c`, `#f187d0`, `#0c0434`, `#8669a2`, `#890f63`, `#312c54`. Zero values and missing/unavailable values are shown separately, and known unresolved case conflicts use a distinct warning color. Bins are fixed across years to support comparisons.

## Important accuracy safeguards

- `source_reported_cases` preserves the supplied source value.
- `map_value_cases` is the display-ready value and may be blank when a known discrepancy remains unresolved (Azerbaijan 2012 is flagged this way).
- Missing values are not converted to zero.
- The supplied case compilation is not independently verified in full against one dated WHO release. Verify the country-year series against a single archived data vintage before presenting it as definitive.
- The Azerbaijan intervention records are source-documented, not independently verified implementation audits. Funding commitments/contributions are not annual expenditure unless the source explicitly says so.
- The current intervention dataset is Azerbaijan-focused. Do not imply that other countries had no interventions just because their records are absent.
- Natural Earth boundaries are loaded at runtime through `rnaturalearth`. Record package/data versions, inspect unmatched ISO3 codes, and document the geographic scope of the WHO European Region used in the analysis.
- The timeline is descriptive. Temporal association between interventions and declining cases does not prove causation.

## Embedding

Deploy the app to a Shiny-capable host (for example, Posit Connect or Shiny Server), then embed the deployed app URL in your website using the host's supported iframe/embed method. Check the host's authentication, iframe, and content-security settings. Avoid embedding a local `runApp()` URL in a public website.

## Source-by-source validation and explicit missing values

The project now includes four validation/audit files:

- `data/source_validation_register.csv` — what each source was checked for, what it supports, and what it cannot establish.
- `data/case_source_validation.csv` — row-level review status for each country-year case record.
- `data/intervention_source_validation.csv` — event-level review notes for each Azerbaijan intervention/financing record.
- `data/validation_run_summary.csv` — row counts, duplicate-key check, and availability of optional datasets.

### Validation findings

- **WHO indigenous-case series:** the supplied dataset points to the legacy WHO GHO/Athena interface. WHO now states that this legacy interface is retired. Selected Azerbaijan values can be corroborated from indexed WHO GHO records, but the exact historical data vintage used for the full 880-row compilation is not recorded and the complete series has not been independently revalidated against one dated extract. Do not describe the whole country-year series as fully verified.
- **Azerbaijan 2012:** the supplied WHO compilation lists 3 indigenous cases, while Mammadov et al. (2016) reports 3 total cases, of which 2 were indigenous. The source-reported value is preserved, but `map_value_cases` is `NA` pending resolution.
- **Azerbaijan interventions:** the cited paper supports the Eni US$760,000 contribution agreed in 1999 for a three-year plan, 50,000 LLINs distributed during 2009–2012, Global Fund support from 2009, electronic surveillance since 2009, 233 laboratories capable of diagnosis, and over 10,000 people reached by primaquine mass treatment in seven districts in 2011. Some dates are not explicit in the paper; those fields are left `NA` rather than inferred. The IRS record was revised to reflect the reported scale-up to 1.25 million people in the 2005 policy narrative; treat the year as contextual and verify against original programme records if exact annual timing is essential.
- **WHO 2015 milestone:** WHO reports that the European Region had zero indigenous cases in 2015. This supports the regional milestone, not every country-year value in the dataset.
- **Population and insurance:** their CSVs are intentionally header-only until comparable sourced observations are available. No rates or insurance percentages are inferred.

### Missing-value convention

Empty cells in populated data files are now serialized as the literal `NA`. The supplied table also contained duplicate country-name aliases for Czechia/Czech Republic and Türkiye/Turkey, all with missing case values; these were consolidated to one row per ISO3-year. The app now stops if duplicate ISO3-year rows remain. The R code treats empty strings, `NA`, and `NaN` as missing values. Do not replace missing values with zero: zero means a source-reported zero, while `NA` means no usable observation or an unresolved value. The header-only population and insurance templates contain no observations; do not add artificial all-`NA` country-year rows.

The app reads the validation register in the **Data quality & sources** tab. Missing end dates in the intervention timeline are displayed as point events rather than being expanded into an invented multi-year interval.

### Sources reviewed

1. WHO, *World Malaria Report 2015 — summary* (regional zero-indigenous-case milestone): https://www.who.int/publications/i/item/WHO-HTM-GMP-2016.2
2. Mammadov S, Gasimov E, Kurdova-Mintcheva R, Wongsrichanalai C. *Elimination of Plasmodium vivax Malaria in Azerbaijan*. American Journal of Tropical Medicine and Hygiene. 2016;95(6 Suppl):78–86. https://pmc.ncbi.nlm.nih.gov/articles/PMC5201226/
3. WHO Global Health Observatory legacy indicator for indigenous malaria cases (legacy interface now retired): https://apps.who.int/gho/athena/data/GHO/MALARIA_INDIG_STATUS%2CMALARIA_INDIG.html?ead=&filter=COUNTRY%3A%2A&profile=ztable

