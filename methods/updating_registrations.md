# Updating the registrations data

This walks a SORA member through converting the Microsoft Forms survey export
(hosted in Google Drive) into the `data/registrations.csv` file that the
Positron Workshop builder app reads. You only need to do this when new
registrations have come in.

## What you'll end up with

A cleaned `data/registrations.csv` with these columns:
`level,name,email,availability,comments` — which then gets uploaded to the
shared team Google Drive so the builder app can ingest it.

## One-time setup

You only do this the first time you run the pipeline on a given machine.

1. **Install R packages** (from an R console):
   ```r
   install.packages(c("readxl", "dplyr", "stringr", "googledrive"))
   ```

2. **Mirror the Forms export into your own Google Drive.**
   The Microsoft Forms responses get dropped into a shared Drive folder as an
   `.xlsx`. You need your own copy because the Google API authenticates *as
   you* and you need read permission on the file.
   - Open the file in Drive
   - Right-click → **Make a copy**
   - The copy lands in your *My Drive*. Grab its share link — the file ID is
     the long string after `/d/` in the URL.

3. **Authenticate googledrive.** In an *interactive* R console (RStudio
   Console, or `R` in a terminal — **not** `Rscript`):
   ```r
   library(googledrive)
   drive_auth(scopes = "https://www.googleapis.com/auth/drive.readonly")
   ```
   A browser tab opens. Sign in with the Google account that owns the copied
   file. **Tick every permission checkbox** on the consent screen before
   clicking Continue — if any are unchecked you'll end up authenticated but
   without Drive read access, and later steps will 403.

   Verify it worked:
   ```r
   drive_get(googledrive::as_id("<your file ID>"))
   ```
   You should see a one-row tibble with the file name.

## Every time registrations change

From the repo root, in a terminal:

```bash
Rscript methods/prep_registrations.R '<share URL or file ID>'
```

Examples:

```bash
# share URL
Rscript methods/prep_registrations.R 'https://docs.google.com/spreadsheets/d/1YiWwRN-WnUtjGkYnw0DP8A5a8dxsV-XK/edit?usp=sharing'

# bare file ID
Rscript methods/prep_registrations.R 1YiWwRN-WnUtjGkYnw0DP8A5a8dxsV-XK

# local xlsx if you already downloaded it
Rscript methods/prep_registrations.R 'data/registrations/somefile.xlsx'
```

This writes `data/registrations.csv` by default. Pass a second argument to
write somewhere else.

Then **upload the generated `data/registrations.csv` to the shared team Drive
folder** so the builder app can ingest it. Do not commit it to the repo —
`.gitignore` already excludes `data/registrations*` because it contains PII.

## Troubleshooting

**`403 Forbidden — insufficient authentication scopes`**
Your cached OAuth token doesn't have Drive read scope. In interactive R:
```r
cache_dir <- gargle::gargle_oauth_cache()
file.remove(list.files(cache_dir, full.names = TRUE, recursive = TRUE))
googledrive::drive_auth(scopes = "https://www.googleapis.com/auth/drive.readonly")
# tick every box on the consent screen
```

**Multiple tokens cached, script picks the wrong one**
Check with `gargle::gargle_oauth_sitrep()`. The script pins to
`drive.readonly` scope, so keep a token with that scope and delete others, or
edit the scope in `methods/prep_registrations.R` to match what you have.

**`Error in drive_auth(): Can't get Google credentials` when running Rscript**
You haven't done the interactive auth step yet. Rscript is non-interactive —
it can't pop a browser, it can only reuse a token that was cached during a
prior interactive session. Run `drive_auth()` in an R console first.

**Permission denied on the original shared file (not your copy)**
The file's owner needs to grant you view access, or — simpler — make your own
copy as described in setup step 2. The API authenticates as you, so you need
direct access; being in a shared folder isn't always enough.
