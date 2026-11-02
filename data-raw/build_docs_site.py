# Builds the documentation site in docs/ (plain HTML, works offline and on GitHub Pages).
# Run from the package folder:  python data-raw/build_docs_site.py
import re, json, html, os
ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
DOCS = os.path.join(ROOT, 'docs')
GH = 'https://github.com/TinaLubamba/HybriFlow'

def code(s, lang='r'): return '<pre><code>' + html.escape(s.strip('\n')) + '</code></pre>'
def tip(title, body): return f'<div class="callout"><div class="t">{title}</div>{body}</div>'
def warn(title, body): return f'<div class="callout warn"><div class="t">{title}</div>{body}</div>'
def table(head, rows):
    return '<table><thead><tr>' + ''.join(f'<th>{h}</th>' for h in head) + '</tr></thead><tbody>' + \
        ''.join('<tr>' + ''.join(f'<td>{c}</td>' for c in r) + '</tr>' for r in rows) + '</tbody></table>'

PAGES = []

# ------------------------------------------------------------------ Installation (home)
PAGES.append(('index.html', 'Installation', f'''
<h1>Installation</h1>
<p>HybriFlow is an R package. It takes the Excel probe files and PDF reports from the HybriSpot platform and builds a quality-controlled, searchable sample database and, for longitudinal studies, a follow-up analysis. This page shows how to install it.</p>
{tip('Independent tool', '<p>HybriFlow is an independent open-source tool. It is not developed, endorsed or supported by the manufacturer of the HybriSpot platform.</p>')}
<h2 id="prerequisites">Prerequisites</h2>
<ul>
<li>A computer running Windows 10 or 11 (developed and tested on Windows; other systems have not been tested)</li>
<li><strong>R</strong>, version 4.1.0 or newer, from <a href="https://cran.r-project.org/">cran.r-project.org</a></li>
<li><strong>RStudio</strong> (recommended, because it gives the folder-selection window), from <a href="https://posit.co/download/rstudio-desktop/">posit.co</a></li>
<li>An internet connection for the first installation, to download the packages HybriFlow needs</li>
</ul>
<p>To check your R version, type this in the R console:</p>
{code('R.version.string')}
<h2 id="install-github">1. Install from GitHub</h2>
{code('install.packages("remotes")' + chr(10) + 'remotes::install_github("TinaLubamba/HybriFlow")')}
<p>This also installs every package HybriFlow needs.</p>
<h2 id="install-zip">2. Or install from a downloaded copy</h2>
<p>Download the repository as a zip file from <a href="{GH}">the project page</a> (Code, then Download ZIP) and unzip it. Install the packages HybriFlow needs first, then install HybriFlow from the unzipped folder:</p>
{code('install.packages(c("dplyr", "DT", "ggplot2", "htmltools", "htmlwidgets", "openxlsx",' + chr(10) + '                   "pdftools", "purrr", "readxl", "scales", "stringr", "tibble",' + chr(10) + '                   "tidyr", "zip", "rstudioapi"))' + chr(10) + chr(10) + 'install.packages("C:/path/to/HybriFlow", repos = NULL, type = "source")')}
<p>Replace <code>C:/path/to/HybriFlow</code> with the folder that contains the file <code>DESCRIPTION</code>. Use forward slashes.</p>
{warn('If Windows blocks the installation', '<p>Some computers show a message such as <em>"An Application Control policy has blocked this file"</em>. This is a Windows security setting, not a problem with HybriFlow. Use the base-R command above (<code>install.packages(..., repos = NULL, type = "source")</code>) instead of <code>devtools::install()</code>.</p>')}
<h2 id="check">3. Check that it works</h2>
{code('library(HybriFlow)' + chr(10) + 'packageVersion("HybriFlow")')}
<p>If a version number appears, the package is installed. Continue with <a href="usage.html">Usage</a>.</p>
<h2 id="update">Update or remove</h2>
{code('remove.packages("HybriFlow")   # remove' + chr(10) + '# then install again as above to update')}
<div class="pager"><span></span><a href="usage.html">Usage &rarr;</a></div>
'''))

# ------------------------------------------------------------------ Usage
PAGES.append(('usage.html', 'Usage', '''
<h1>Usage</h1>
<p>You do not need to open or edit any script. Load the package and run one function.</p>
<h2 id="quick-start">Quick start</h2>
''' + code('library(HybriFlow)' + chr(10) + 'run_hybriflow()') + '''
<p>Two pop-up windows ask for what the pipeline needs.</p>
<h3 id="popup-folder">1. Choose the data folder</h3>
<p>Pick the parent folder that holds all the run sub-folders (the Excel files and the PDF reports, in any sub-folder).</p>
<figure><img src="images/popup-folder.png" alt="Pop-up 1: select the data folder"><figcaption>Pop-up 1: "Select your data folder". Outside RStudio you are asked to type the folder path instead.</figcaption></figure>
<h3 id="popup-repeats">2. Choose how repeated samples are handled</h3>
<p>This choice affects the QC-passed database only. The main database always keeps every record.</p>
<figure><img src="images/popup-repeats.png" alt="Pop-up 2: repeat handling"><figcaption>Pop-up 2: "How should repeated samples be handled?"</figcaption></figure>
''' + table(['Choice', 'What is kept in the QC-passed database'], [
    ['<code>"all"</code>', 'Every record, including all repeats'],
    ['<code>"original"</code>', 'Only the original sample (repeat 0)'],
    ['<code>"latest"</code>', 'Only the latest repeat of each sample'],
    ['<code>"specific"</code>', 'Only the repeat number you choose (set with <code>specific_repeat</code>)']]) + '''
<h2 id="what-happens">What happens during a run</h2>
<p>The pipeline reads every Excel and PDF file, links them by Sample ID, runs the quality checks, builds the database and writes the results to a <code>HybriFlow_Output</code> folder in your current working folder, with a zipped copy next to it. Progress messages appear in the console, and the HTML report opens in your browser when it finishes.</p>
<figure><img src="images/output-folder.png" alt="The output folder"><figcaption>The output folder when the run is finished.</figcaption></figure>
''' + tip('The first run takes longest', '<p>The first run reads every file (about 13 minutes for 2744 files in our test). The cache is on by default, so later runs only read new or changed files. Use <code>force_rebuild = TRUE</code> to read everything again.</p>') + '''
<h2 id="arguments">Choosing options</h2>
<p>To skip the pop-ups, or to change a setting, pass arguments. All of them are listed on the <a href="parameters.html">Parameters</a> page.</p>
''' + code('''run_hybriflow(
  data_folder     = "D:/hybriflow_runs",      # parent folder with all run sub-folders
  output_folder   = "D:/hybriflow_results",
  repeat_handling = "latest",                 # "all", "original", "latest", "specific"
  followup        = followup_settings(visits_required = 3)
)''') + '''
<h2 id="followup">Follow-up analysis</h2>
<p>For studies in which each woman is followed over several visits, HybriFlow judges every baseline genotype separately:</p>
<ul>
<li><strong>Persisted:</strong> detected at baseline and at every follow-up visit.</li>
<li><strong>Cleared:</strong> not detected at one or more follow-up visits, even if it is detected again later.</li>
<li><strong>Acquired:</strong> absent at baseline but detected at any follow-up visit.</li>
</ul>
<p>Whether at least one genotype persisted (P), cleared (C) and was acquired (A) gives six mutually exclusive categories:</p>
''' + table(['Category', 'P', 'C', 'A'], [
    ['Persist only', 'yes', '', ''], ['Clear only', '', 'yes', ''], ['Persist + clear', 'yes', 'yes', ''],
    ['Persist + acquire', 'yes', '', 'yes'], ['Clear + acquire', '', 'yes', 'yes'], ['Persist + clear + acquire', 'yes', 'yes', 'yes']]) + '''
<p>"Persist any", "clear any" and "acquire any" are reported as overlapping summaries. Persist + clear can only occur in women with two or more baseline genotypes.</p>
<h3 id="visits">Changing the study design</h3>
<p>The number of study visits (baseline plus follow-ups) is a setting, so a cohort with a different design needs no code changes:</p>
''' + code('''run_hybriflow(followup = followup_settings(visits_required = 7))   # baseline + 6 follow-ups
run_hybriflow(followup = followup_settings(run = FALSE))           # skip the follow-up analysis''') + '''
<h2 id="sample-ids">Sample ID format</h2>
<p><code>&lt;13-digit sample number&gt;_&lt;visit code / date&gt;</code>, for example <code>100-00-0001-0001_1-004-0_20240729</code>. The visit year and date are read from the part after the first underscore, and repeats carry a suffix such as <code>-1</code>.</p>
''' + warn('Label sample IDs in this format', '<p>Samples whose visit date cannot be read cannot be placed in visit order in the follow-up analysis, so those women are left out. Laboratories should label every sample ID in the standard format, including the visit date.</p>') + '''
<h2 id="requirements">What the files must look like</h2>
<ul>
<li>Sample files are recognised by a <code>Target</code> column and need the columns <code>Sample ID, Target, Position, TH, LTH, Value, Result</code>.</li>
<li>PDF reports are matched by the ID after <code>report_</code> or <code>report-</code> in the file name, or by the <code>ID SAMPLE</code> printed inside the PDF.</li>
<li>A genotype is POSITIVE only if all its probe positions are positive. Disagreeing positions are flagged as discordant, which is a review flag and not a QC failure.</li>
</ul>
<div class="pager"><a href="index.html">&larr; Installation</a><a href="parameters.html">Parameters &rarr;</a></div>
'''))

# ------------------------------------------------------------------ Parameters
PAGES.append(('parameters.html', 'Parameters', '''
<h1>Parameters</h1>
<h2 id="run-hybriflow"><code>run_hybriflow()</code></h2>
''' + table(['Argument', 'Meaning', 'Default'], [
 ['<code>data_folder</code>', 'Folder containing the run sub-folders (searched including sub-folders). If not given, a folder chooser (RStudio) or a prompt is used.', 'pop-up'],
 ['<code>output_folder</code>', 'Where the results are written. Files made by the previous run are removed from it first; other files are never touched.', '<code>HybriFlow_Output</code> in the working folder'],
 ['<code>repeat_handling</code>', '<code>"all"</code>, <code>"original"</code>, <code>"latest"</code> or <code>"specific"</code> (QC-passed database only). If not given, a menu is shown in interactive sessions and <code>"latest"</code> is used otherwise.', 'menu'],
 ['<code>specific_repeat</code>', 'Repeat number to keep when <code>repeat_handling = "specific"</code> (0 = original, 1 = first repeat, ...).', '<code>NA</code>'],
 ['<code>followup</code>', 'Follow-up settings, made with <code>followup_settings()</code>.', '<code>followup_settings()</code>'],
 ['<code>use_cache</code>', 'Save extracted data so later runs read only new or changed files.', '<code>TRUE</code>'],
 ['<code>cache_dir</code>', 'Folder for the cache.', '<code>HybriFlow_Cache</code> next to the output folder'],
 ['<code>force_rebuild</code>', 'With the cache on, ignore it and re-read every file.', '<code>FALSE</code>'],
 ['<code>split_by_year</code>', 'With the cache on, also save one database file per visit year.', '<code>TRUE</code>'],
 ['<code>selected_years</code>', 'Character vector of years; exports and plots use only those years. <code>NULL</code> uses all years.', '<code>NULL</code> (all)'],
 ['<code>open_report</code>', 'Open the HTML report in the browser when finished.', '<code>interactive()</code>'],
 ['<code>zip_output</code>', 'Zip the output folder into <code>HybriFlow_Output.zip</code> next to it.', '<code>TRUE</code>'],
 ['<code>resource_report</code>', 'Print memory and disk use at the end.', '<code>FALSE</code>']]) + '''
<p>The function returns, invisibly, a list with <code>final_database</code>, <code>qc_passed</code>, <code>followup</code>, <code>run_summary</code> and <code>paths</code> (the files written).</p>
<h2 id="followup-settings"><code>followup_settings()</code></h2>
''' + table(['Argument', 'Meaning', 'Default'], [
 ['<code>run</code>', '<code>TRUE</code> runs the follow-up analysis; <code>FALSE</code> builds the sample database only.', '<code>TRUE</code>'],
 ['<code>visits_required</code>', 'Number of visits per woman in the study (baseline plus follow-ups). A whole number, 2 or more.', '<code>3</code>'],
 ['<code>clear_last_k</code>', 'Used only for the <code>ended_HPV_negative</code> column. 1 = HPV-negative at the last visit; 2 = at the last two visits.', '<code>1</code>'],
 ['<code>qc_only</code>', 'Use only samples whose <code>Overall_QC_Status</code> is PASS.', '<code>TRUE</code>']]) + '''
<p>Persisted and cleared are fixed definitions (see <a href="usage.html#followup">Usage</a>), not settings.</p>
<h2 id="help">Help inside R</h2>
''' + code('?run_hybriflow' + chr(10) + '?followup_settings') + '''
<div class="pager"><a href="usage.html">&larr; Usage</a><a href="output.html">Output &rarr;</a></div>
'''))

# ------------------------------------------------------------------ Output
PAGES.append(('output.html', 'Output', '''
<h1>Output</h1>
<p>Everything is written to the output folder (<code>HybriFlow_Output</code> by default). Keep the files in the folder together, because the buttons and links in the HTML pages open the other files by their names.</p>
<h2 id="files">Files</h2>
''' + table(['File', 'What it contains'], [
 ['<code>final_database.html</code> and the folder <code>final_database_files</code>', 'The interactive database: one row per sample, searchable, sortable and filterable, with buttons to export (Copy, CSV, Excel, Print). The page needs the <code>final_database_files</code> folder beside it. No internet connection or R is needed to open it.'],
 ['<code>final_database.xlsx</code>', 'The same database as an Excel workbook: control and NTC checks, genotype calls, discordant positions, repeat notes, reagent lot numbers and expiry dates, the Excel-versus-PDF consistency check, the overall QC status and clickable links to the source files.'],
 ['<code>final_database_QC_passed.xlsx</code>', 'QC-passed samples, with the repeat handling you chose.'],
 ['<code>run_summary.txt</code>', 'Counts, QC results, top genotypes, date range and a check of whether repeated discordant genotypes changed.'],
 ['<code>followup_analysis.html</code>', 'The follow-up report with tables and nine figures (only if the follow-up analysis was run).'],
 ['<code>followup_database.xlsx</code>', 'One row per woman, with her category, the flags P, C and A, and the genotypes that persisted, cleared or were acquired.'],
 ['<code>followup_figures/</code>', 'The nine follow-up figures as PNG files.'],
 ['<code>Duplicate_Excel_files_report.csv</code>, <code>Duplicate_PDF_files_report.csv</code>', 'Files found more than once for the same Sample ID (one file per ID is used).'],
 ['<code>Sample_IDs_without_year.csv</code>, <code>Sample_IDs_year_from_2digit.csv</code>', 'Sample IDs whose visit year was missing or read from a two-digit year, for checking.'],
 ['<code>HybriFlow_Output.zip</code>', 'The output folder zipped (next to it), ready to share. Everything works once the recipient extracts it.']]) + '''
<h2 id="figures">The nine follow-up figures</h2>
''' + table(['Figure file', 'Shows'], [
 ['<code>fig1_participant_flow.png</code>', 'Participant flow: women set aside, seen at baseline only, and seen at 2 or more visits'],
 ['<code>fig2_followup_groups.png</code>', 'The six categories, as percentages of women with all visits'],
 ['<code>fig3_genotypes_at_baseline.png</code>', 'Genotypes present at enrolment'],
 ['<code>fig4_outcome_by_baseline_risk.png</code>', 'The categories by baseline infection (high-risk only or both)'],
 ['<code>fig5_persistence_vs_clearance.png</code>', 'Persist only compared with clear only'],
 ['<code>fig6_genotype_persistence_clearance.png</code>', 'Persistence and clearance of each baseline genotype'],
 ['<code>fig7_genotype_acquisition.png</code>', 'Newly detected genotypes'],
 ['<code>fig8_visit_transitions.png</code>', 'Genotype events between consecutive visits'],
 ['<code>fig9_high_vs_low_risk.png</code>', 'High-risk compared with low-risk genotypes']]) + '''
<h2 id="guide">How to read the database</h2>
<p>The <a href="user-guide.html">Database user guide</a> explains the page step by step: the buttons, finding a sample, the HPV result columns and colours, the quality-control columns, and repeats, lot numbers and file links.</p>
<div class="pager"><a href="parameters.html">&larr; Parameters</a><a href="user-guide.html">Database guide &rarr;</a></div>
'''))

# ------------------------------------------------------------------ build
NAV = [('index.html', 'Installation'), ('usage.html', 'Usage'), ('parameters.html', 'Parameters'),
       ('output.html', 'Output'), ('user-guide.html', 'Database guide')]

def add_ids_and_toc(body):
    toc = []
    def sub(m):
        lvl, attrs, inner = m.group(1), m.group(2), m.group(3)
        idm = re.search(r'id="([^"]+)"', attrs)
        text = re.sub(r'<[^>]+>', '', inner)
        hid = idm.group(1) if idm else re.sub(r'[^a-z0-9]+', '-', text.lower()).strip('-')
        toc.append((lvl, hid, text))
        return f'<h{lvl} id="{hid}">{inner}</h{lvl}>'
    body = re.sub(r'<h([23])([^>]*)>(.*?)</h\1>', sub, body, flags=re.S)
    return body, toc

index = []
rendered = {}
for fn, title, body in PAGES:
    body, toc = add_ids_and_toc(body)
    rendered[fn] = (title, body, toc)
    text = re.sub(r'<[^>]+>', ' ', body); text = re.sub(r'\s+', ' ', html.unescape(text))
    index.append({'title': title, 'section': '', 'url': fn, 'text': text})
    for lvl, hid, t in toc:
        index.append({'title': title, 'section': t, 'url': f'{fn}#{hid}', 'text': t})
index.append({'title': 'Database guide', 'section': '', 'url': 'user-guide.html',
              'text': 'database user guide buttons finding a sample HPV results colours quality control repeats lot numbers file links troubleshooting'})

for fn, (title, body, toc) in rendered.items():
    nav = ''.join(f'<a href="{u}"' + (' class="active"' if u == fn else '') + f'>{t}</a>' for u, t in NAV)
    tocs = ''.join(f'<a class="l{l}" href="#{h}">{html.escape(t)}</a>' for l, h, t in toc)
    page = f'''<!DOCTYPE html>
<html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width, initial-scale=1">
<title>{title} | HybriFlow</title><link rel="stylesheet" href="assets/site.css"></head>
<body><div class="layout">
<aside class="sidebar"><a class="brand" href="index.html">HybriFlow</a>
<input id="q" class="search" type="search" placeholder="Search" aria-label="Search the documentation"><ul id="results" class="results"></ul>
<nav class="menu">{nav}</nav></aside>
<main>{body}<div class="foot">HybriFlow is an independent tool, not developed, endorsed or supported by the manufacturer of the HybriSpot platform. <a href="{GH}">Project on GitHub</a></div></main>
<aside class="toc"><div class="h">On this page</div>{tocs}</aside>
</div>
<script>window.SEARCH_INDEX={json.dumps(index)};</script><script src="assets/site.js"></script></body></html>'''
    open(os.path.join(DOCS, fn), 'w', encoding='utf-8').write(page)
    print('wrote', fn)
