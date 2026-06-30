# Image-Based Plant Morphometrics

Tools I built to extract **quantitative morphological measurements from images of plant structures** — automating trait measurement that would otherwise be slow, subjective manual work. Two domains, one theme: turning photographs into reproducible numbers.

> **▶ Try it immediately — sample data included.**
> - **Pappus (in-browser, no install):** open [`pappus-analyzer.html`](pappus-analyzer.html) in a browser, click *Drop image or click to upload*, choose [`seeds/examples/pappus_sample.png`](seeds/examples/pappus_sample.png), then **Process** → **Measure** to get area / perimeter / circularity / Feret morphometrics.
> - **Roots (Python):** `python roots/rsml_analyzer.py roots/examples/root_sample_t1.rsml` prints a full root-architecture report; `root_sample_t1.rsml` (early) and `root_sample_t2.rsml` (later) are a two-time-point pair.
>
> All bundled samples are **synthetic** (no real photos, traces, GPS or PII).

## 1. Root architecture (`roots/`)

A pipeline for analyzing **root-system architecture** from scanned/photographed roots, built around **RSML** (Root System Markup Language) — the standard format for traced root systems.

- **Python** — batch processing and analysis of traced roots:
  - `rsml_batch_processor.py` — process many root images/traces in bulk
  - `rsml_analyzer.py` — extract architectural metrics from RSML
  - `rsml_viewer.py` — visualize traced root systems
  - `compare_smartroot_rsml.py` — compare/validate traces (e.g. against SmartRoot output)
- **R / Shiny** — an interactive **root-alignment app** for curating and aligning traced roots:
  - `root_alignment_app.R`, `root_alignment_utils.R`, `run_root_alignment_app.R`

*Built to support a desert-plant root-trait experiment.*

## 2. Seed / pappus morphometrics (`seeds/`)

- `pappus_analyzer.py` — extracts **pappus morphology** (the feathery wind-dispersal structure on Asteraceae seeds) from seed images, producing dispersal-relevant trait measurements. Built for a PhD project on dispersal-trait variation (anemochory/epizoochory trade-offs).

## Why it matters

Morphological traits (root length/branching, pappus size/structure) drive ecological function but are tedious and error-prone to measure by hand. These tools make the measurement **automated, reproducible, and scalable** across hundreds of images — and demonstrate practical computer-vision / image-analysis engineering applied to real biological data.

## My role

Designed and built all of these — the Python image-analysis and batch-processing pipelines, and the interactive R/Shiny curation app.

## Repository structure

```
image-morphometrics/
├── roots/   RSML root-architecture pipeline (Python) + alignment app (R/Shiny)
└── seeds/   pappus / seed morphometrics (Python)
```

## Data policy

**No real images, traces, or measurement data** are included — those are `.gitignore`d. The only data shipped are small **synthetic** demo samples (`seeds/examples/pappus_sample.png`, `roots/examples/*.rsml`) so the tools can be tried immediately; point the scripts at your own local image/RSML folder for real work.

---

*Part of the research-software portfolio of Dr. Efrat Dener — plant ecologist & quantitative/computational researcher.*

<!-- TO FINALIZE (Efrat): a couple of before/after example images (a photo → traced/measured output) would make this land hard — use any non-sensitive sample. -->
