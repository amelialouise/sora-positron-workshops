# Positron + AI Workshops for SORA members

Resources and tools for managing registration and participation in the **State of Oregon Research Academy (SORA)** workshop series on AI-assisted data science with Positron.

> SORA is a community-run organization for State of Oregon and Oregon University System employees interested in research and analysis. There are no membership fees, all employees at all levels are welcome to join.
>
> 🔗 [oregonresearch.org/membership](https://oregonresearch.org/membership/) | [LinkedIn Group](https://www.linkedin.com/groups/14414150/)

---

## Cohort Builder App

The Workshop Cohort Builder is hosted on Posit Connect and available here:

> **🚀 [Launch the Workshop Cohort Builder v2.0→](https://connect.posit.cloud/amelia/content/019d799e-805a-87aa-0cb2-76b74d341670?utm_source=publisher-positron)**

Upload a registration CSV to visualize availability, generate optimal cohorts, and export assignments. No data is stored on the server, ever! All processing happens in the browser session like magic. Seriously, try it out. 

---

## ⚠️ Data & Privacy Policy for Contributors

**This is a public repository. Never commit registration data of any kind.**

Registration CSVs contain participant names, email addresses, and agency affiliations. Committing this data to a public repo would expose state employees' information publicly and violate SORA's commitment to participant privacy.

### Rules for SORA Executive Committee members

1. **Never add real registration files to this repo** — not in `data/`, not anywhere.
2. **Never paste registration data into code, comments, or documentation.**
3. All processed cohort output CSVs must be saved to **SORA's private Google Drive**, not committed here.
4. The `data/` folder in this repo is for test data only. Sample files use fake names and placeholder emails (see `data/sample_registrations.csv`).
5. If you're unsure whether something is safe to commit, **don't commit it**. Instead, ask an EC member first.

### How to handle registration data in practice

```
Registration CSV received
        │
        ▼
Upload to Cohort Builder app (hosted on Posit Connect)
        │
        ▼
Download cohort assignment CSV from the app
        │
        ▼
Save to SORA Google Drive (private, EC access only)
        │
        ▼
Share cohort assignments directly with Posit presenters
```

The `.gitignore` in this repo excludes `data/registrations/` as a safeguard, but **git is not a substitute for good judgment**. Any files in unexpected locations won't be caught automatically!

---

## Repository Contents

| Path | Description |
|------|-------------|
| `app.R` | Shiny app source; edit here, redeploy to Posit Connect |
| `docs/FAQ.md` | General FAQ to support workshop participants |
| `docs/Positron_Workshop.pdf` | Posit's official workshop overview and contact info |
| `data/` | Fake sample CSV for testing the app locally |

---

## Developing the App Locally

The app lives in `app.R`. To run it locally for development:

```r
# Install dependencies
install.packages(c("shiny", "bslib", "dplyr", "tidyr", "DT", "ggplot2"))

# Launch
shiny::runApp("app")
```

Use the sample CSV in `data/sample_registrations.csv` to test with non-sensitive data.

### Deploying Updates to Posit Connect

1. Open `app.R` in Positron or RStudio.
2. Use the **Posit Publisher** extension to push updates to Posit Connect.
3. After deploying, verify the live link above still works and update it if the URL changed.

---

## Workshop FAQ

Common questions from participants and agency staff are in [`docs/FAQ.md`](docs/FAQ.md).

Topics covered: ITI/EIS approval status, agency-level AI use policies, SORA's role and scope, and who to contact at Posit for technical governance questions.

---

## Support & Contact

| Need | Where to go |
|------|-------------|
| Agency IT/AI approval questions | Your agency's EIS contact or [Posit](https://posit.co/contact/) |
| Workshop scheduling for your org | Contact the Posit account team (see PDF) |
| SORA mailing list | [listsmart.osl.state.or.us](http://listsmart.osl.state.or.us/mailman/listinfo/sora) |
| General SORA info | [oregonresearch.org](https://oregonresearch.org) |

---

## Contributing

This repository is maintained by SORA Executive Committee volunteers. To suggest changes to the FAQ or app, open a pull request or file an issue. Please review the data policy above before contributing.
