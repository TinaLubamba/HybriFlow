"""
Make synthetic (mock) HybriSpot output files for testing HybriFlow.

Everything here is invented: sample IDs, probe values, lot numbers and dates.
No participant data are used.

Run from the package root:
    python data-raw/make_mock_data.py

Needs the Python packages openpyxl and reportlab (pip install openpyxl reportlab).
Writes:
    inst/extdata/mock_data/run_01/ and run_02/   Excel files and PDF reports (3 samples have no PDF on purpose)
    inst/extdata/mock_expected_results.csv       what each sample was built to show
"""
import csv
import os
import random
from openpyxl import Workbook
from openpyxl.styles import Font
from reportlab.lib.pagesizes import A4
from reportlab.pdfgen import canvas

OUT = os.path.join("inst", "extdata", "mock_data")
EXPECTED = os.path.join("inst", "extdata", "mock_expected_results.csv")
THRESHOLD = 6
random.seed(2025)

# ---- chip layout: 9 x 9 grid, read left to right, row by row (x fastest) ----
GRID = [
    ["Control", "Control", "Amplification Control", "HPV universal probe", "16", "18", "26", "31", "Empty"],
    ["33", "35", "39", "45", "51", "52", "53", "56", "Control"],
    ["58", "59", "66", "68", "73", "82", "6", "11", "40"],
    ["42", "43", "44/55", "54", "61", "62/81", "67", "69", "70"],
    ["71", "72", "Empty", "84", "Control", "Amplification Control", "HPV universal probe", "42", "43"],
    ["16", "18", "26", "31", "33", "35", "39", "45", "51"],
    ["52", "53", "56", "58", "59", "66", "68", "73", "82"],
    ["Control", "6", "11", "40", "44/55", "54", "61", "62/81", "67"],
    ["Empty", "69", "70", "71", "72", "Empty", "84", "Empty", "Empty"],
]
SPOTS = [(x, y, GRID[y][x]) for y in range(9) for x in range(9)]
assert len(SPOTS) == 81

# ---- mock samples ----
V = {1: ("1-001-0", "20240115"), 2: ("1-010-0", "20240415"), 3: ("1-019-0", "20240715")}

def sid(pid, visit, suffix=""):
    code, date = V[visit]
    return f"100-00-{pid:04d}-{pid:04d}_{code}_{date}{suffix}"

# pos: genotypes positive on both duplicate spots; disc: positive on one spot only
SAMPLES = [
    dict(pid=1, v=1, pos=["16"], note="HPV16 positive at visit 1"),
    dict(pid=1, v=2, pos=["16"], note="HPV16 still positive"),
    dict(pid=1, v=3, pos=[], note="HPV negative; HPV16 cleared"),
    dict(pid=2, v=1, pos=["16", "52"], note="Two high-risk genotypes (multiple infection)"),
    dict(pid=2, v=2, pos=["16", "52"], note="Both persist"),
    dict(pid=2, v=3, pos=["16", "52"], nopdf=True, note="Both persist; deliberately NO PDF (missing-report case)"),
    dict(pid=3, v=1, pos=["31", "6"], note="High-risk 31 plus low-risk 6"),
    dict(pid=3, v=2, pos=["31", "51"], note="6 cleared; 51 newly detected"),
    dict(pid=3, v=3, pos=["31", "51"], nopdf=True, note="31 persists; 51 persists; deliberately NO PDF (missing-report case)"),
    dict(pid=4, v=1, pos=["18"], note="HPV18 positive at visit 1"),
    dict(pid=4, v=2, pos=[], disc=["18"],
         note="HPV18 DISCORDANT (one duplicate spot positive); software reports negative"),
    dict(pid=4, v=2, suffix="-1", pos=["18"], note="REPEAT of the discordant sample; 18 now positive on both spots"),
    dict(pid=4, v=3, pos=["18"], note="HPV18 positive"),
    dict(pid=5, v=1, pos=[], blank=True, note="Amplification controls negative: insufficient material (BLANK)"),
    dict(pid=6, v=1, pos=["16"], hybfail=True, nopdf=True, note="Hybridisation controls negative: QC failure; deliberately NO PDF"),
    dict(pid=7, v=1, pos=[], note="HPV negative, QC pass"),
]

def spot_value(target, s, occurrence):
    """Return the Value (0-99) for one spot of one sample."""
    if target == "Empty":
        return 0
    if target == "Control":                       # hybridisation control
        return 0 if s.get("hybfail") else random.choice([95, 99, 99, 99])
    if target == "Amplification Control":         # internal DNA control
        return 0 if s.get("blank") else random.choice([15, 18, 18, 16])
    if target == "HPV universal probe":
        return random.randint(40, 70) if s["pos"] and not s.get("blank") else 0
    if target in s["pos"]:
        return random.randint(35, 99)
    if target in s.get("disc", []):
        return random.randint(30, 60) if occurrence == 0 else 0   # first spot only
    return random.choice([0, 0, 0, 0, 0, 0, 2, 3])               # below threshold

def make_rows(sample_id, s):
    seen, rows = {}, []
    for x, y, target in SPOTS:
        occ = seen.get(target, 0)
        seen[target] = occ + 1
        v = spot_value(target, s, occ)
        result = "POSITIVE" if v >= THRESHOLD else "NEGATIVE"
        rows.append([sample_id, target, f"({x},{y})", str(THRESHOLD), str(THRESHOLD), str(v), result])
    return rows

def write_xlsx(path, rows):
    wb = Workbook(); ws = wb.active; ws.title = "Sheet1"
    ws.append(["Sample ID", "Target", "Position", "TH", "LTH", "Value", "Result"])
    for c in ws[1]:
        c.font = Font(bold=True)
    for r in rows:
        ws.append(r)                      # all cells are text, as in the real files
    for col in "ABCDEFG":
        ws.column_dimensions[col].width = 13 if col != "A" else 38
    wb.save(path)

def report_text(s):
    """Heading and sentence for the REPORT section. Only the HPV NEGATIVE wording comes from a real
    report; the POSITIVE and BLANK wording is ASSUMED. Edit here if your reports read differently."""
    if s.get("blank"):
        return "BLANK", "The sample has insufficient genetic material (internal DNA control negative)."
    if s["pos"]:
        return "HPV POSITIVE", "The sample is positive for the following genotypes: " + ", ".join(s["pos"]) + "."
    return "HPV NEGATIVE", "The sample is negative for all the genotypes included in the HPV direct flow chip test."

def write_pdf(path, sample_id, processed, heading, sentence):
    """Two-page report in the same text layout as the HybriSpot report."""
    c = canvas.Canvas(path, pagesize=A4)
    W, H = A4
    def header(page):
        c.setFont("Helvetica-Bold", 7); c.setFillGray(0.4)
        c.drawString(40, H - 22, "SYNTHETIC TEST REPORT - NOT REAL DATA")
        c.setFillGray(0); c.setFont("Helvetica-Bold", 13)
        c.drawString(330, H - 45, "HPV Direct Flow Chip Kit")
        c.setFont("Helvetica-Bold", 9); c.drawString(330, H - 72, "LOTS")
        for i, (lab, lot, exp) in enumerate([("PCR:", "MOCKP001", "4/1/2030"),
                                             ("Chips:", "MOCKE001.1", "9/1/2030"),
                                             ("Reagent:", "MOCKH001", "1/1/2030")]):
            y = H - 92 - i * 20
            c.setFont("Helvetica-Bold", 9); c.drawString(330, y, lab)
            c.setFont("Helvetica", 9); c.drawString(385, y, lot); c.drawString(470, y, exp)
        c.setFont("Helvetica-Bold", 11); c.drawString(40, H - 165, "SAMPLE DETAILS")
        c.setFont("Helvetica-Bold", 9); c.drawString(40, H - 190, "ID SAMPLE:")
        c.setFont("Helvetica", 9); c.drawString(110, H - 190, sample_id)
        c.setFont("Helvetica-Bold", 9); c.drawString(380, H - 190, "SAMPLE TYPE:")
        c.drawString(40, H - 210, "ID PATIENT:"); c.drawString(180, H - 210, "PATIENT:")
        c.drawString(40, H - 230, "SEX:"); c.setFont("Helvetica", 9); c.drawString(110, H - 230, "-")
        c.setFont("Helvetica-Bold", 9); c.drawString(180, H - 230, "BIRTHDATE:"); c.drawString(380, H - 230, "AGE:")
        c.setFont("Helvetica-Bold", 11); c.drawString(40, H - 262, "REPORT")
    def footer():
        c.setFont("Helvetica-Bold", 10); c.drawString(40, 150, "NOTES")
        c.setFont("Helvetica-Bold", 9); c.drawString(40, 100, "FACULTATIVE:")
        c.setFont("Helvetica", 9); c.drawString(120, 100, "Default Doctor, doctor")
        c.setFont("Helvetica-Bold", 9); c.drawString(380, 100, "Validated:")
        c.setFont("Helvetica", 9); c.drawString(450, 100, processed)
        c.drawString(40, 80, "Performed by:"); c.drawString(120, 80, "Default Tech, tech")
        c.setFont("Helvetica-Bold", 9); c.drawString(380, 80, "Processed:")
        c.setFont("Helvetica", 9); c.drawString(450, 80, processed)
        c.setFont("Helvetica-Bold", 9); c.drawString(40, 55, "Instr. :")
        c.setFont("Helvetica", 9); c.drawString(85, 55, "HS12")
        c.setFont("Helvetica-Bold", 9); c.drawString(150, 55, "Serial Nº:")
        c.setFont("Helvetica", 9); c.drawString(205, 55, "100000")
        c.setFont("Helvetica-Bold", 9); c.drawString(300, 55, "hybriSoft:")
        c.setFont("Helvetica", 9); c.drawString(350, 55, "MOCK 1.0.0 (HS12a)")
    # page 1: interpreted result
    header(1)
    c.setFont("Helvetica-Bold", 9); c.drawString(40, H - 282, heading)
    c.setFont("Helvetica", 9)
    c.drawString(40, H - 296, sentence)
    c.setFont("Helvetica-Bold", 10); c.drawString(40, H - 400, "PROTOCOL")
    c.setFont("Helvetica", 7)
    for i, t in enumerate(["Detection and genotyping of HPV viral DNA by PCR and reverse dot blot hybridization:",
                           " - High risk genotypes: 16, 18, 26, 31, 33, 35, 39, 45, 51, 52, 53, 56, 58, 59, 66, 68, 73, 82.",
                           " - Low risk genotypes: 6, 11, 40, 42, 43, 44/55, 54, 61, 62/81, 67, 69, 70, 71, 72, 84.",
                           "Automatic analysis of results"]):
        c.drawString(40, H - 415 - i * 11, t)
    footer(); c.showPage()
    # page 2: chip legend and threshold
    header(2)
    c.setFont("Helvetica", 8)
    for i, t in enumerate(["- Spot B: Hybridization control (5 signals to orientate the CHIP)",
                           "- Spot C: Internal DNA control (Genomic human DNA probe)",
                           "- Spot U: HPV Universal probe",
                           "- Spot #: Genotype specific probes",
                           "All the spots are printed in duplicate."]):
        c.drawString(40, H - 600 - i * 12, t)
    c.setFont("Helvetica-Bold", 10); c.drawString(40, 240, "ANALYSIS INFORMATION")
    c.setFont("Helvetica", 9); c.drawString(40, 222, f"Threshold: {THRESHOLD}")
    footer(); c.showPage(); c.save()

def main():
    runs = {"run_01": ("20250210", "AM", SAMPLES[:8]), "run_02": ("20250211", "PM", SAMPLES[8:])}
    summary = []
    for run, (date, ampm, group) in runs.items():
        folder = os.path.join(OUT, run); os.makedirs(folder, exist_ok=True)
        for n, s in enumerate(group, start=1):
            s_id = sid(s["pid"], s["v"], s.get("suffix", ""))
            fname = f"{date}_{n:02d}_{ampm}.xlsx"
            rows = make_rows(s_id, s)
            write_xlsx(os.path.join(folder, fname), rows)
            heading, sentence = report_text(s)
            if s.get("nopdf"):
                pdf = "no (deliberately missing)"
            else:
                write_pdf(os.path.join(folder, f"report_{s_id}.pdf"), s_id,
                          f"{int(date[4:6])}/{int(date[6:])}/{date[:4]}", heading, sentence)
                pdf = "yes (wording from a real report)" if heading == "HPV NEGATIVE" else "yes (wording ASSUMED)"
            qc = "FAIL" if s.get("blank") or s.get("hybfail") else "PASS"
            status = "BLANK" if s.get("blank") else ("POSITIVE" if s["pos"] and not s.get("hybfail") else "NEGATIVE")
            if s.get("hybfail"): status = "QC fail (hybridisation controls)"
            summary.append([f"{run}/{fname}", s_id, f"{s['pid']:04d}", s["v"], qc, status,
                            ";".join(s["pos"]), ";".join(s.get("disc", [])), pdf, s["note"]])
    with open(EXPECTED, "w", newline="", encoding="utf-8") as f:
        w = csv.writer(f)
        w.writerow(["file", "sample_id", "participant", "visit", "intended_qc", "intended_hpv_status",
                    "genotypes_positive", "genotypes_discordant", "pdf_report", "scenario"])
        w.writerows(summary)
    print(f"Wrote {len(summary)} Excel files to {OUT} and {EXPECTED}")

if __name__ == "__main__":
    main()
