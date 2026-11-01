"""
Figure 2 ("Lifecycle statistics") in
"Rent Control? Bad in Theory, Not Always in Practice" (Rotberg and Zhang),
European Economic Review, manuscript EEREV-D-26-00102R2.

Panel (a): homeownership rate by age (26-85), data vs. model.
Panel (b): average loan-to-value (LTV) ratio of homeowners by age group, data vs. model.

Inputs (paths relative to the replication package root):
  - data/2016_Census_vetted.xlsx, sheet "homeownership_housholds_szie":
    homeownership rate by age, 2016 Census, Toronto CMA.
  - data/2019_SFS.xlsx, sheet "LTV by Age Group":
    average LTV of homeowners by age group, 2019 Survey of Financial Security, Toronto.
  - outputs/outputs_control_sorting_benchmark_marginal_profits.txt:
    benchmark simulation; lines "homeownership_age1" ... "homeownership_age60"
    (model ages 1-60 = ages 26-85) and "LTV_age_group1" ... "LTV_age_group12"
    (age groups 26-29, 30-34, ..., 75-79, 80+).

Usage (from any folder):
    python figure_2_homeownership_and_LTV.py [output_directory]

Writes Figure_2_homeownership_and_LTV.pdf and Figure_2_homeownership_and_LTV.eps
(vector, fonts embedded) to output_directory (default: the folder containing this
script). Intended for full page width (190 mm).
Requires Python 3 with matplotlib and openpyxl
(tested with Python 3.13, matplotlib 3.10, openpyxl 3.1).
"""

import os
import sys

import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt
import openpyxl

# Elsevier artwork guidelines: Arial/Helvetica-type fonts, embedded; lettering at least 7 pt
# when printed. Liberation Sans is metrically identical to Arial; if it is not installed,
# matplotlib falls back to its default font.
matplotlib.rcParams.update({
    "pdf.fonttype": 42,
    "ps.fonttype": 42,
    "font.family": "Liberation Sans",
    "font.size": 12,
    "legend.framealpha": 1.0,  # EPS has no transparency
})

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
CENSUS_XLSX = os.path.join(ROOT, "data", "2016_Census_vetted.xlsx")
SFS_XLSX = os.path.join(ROOT, "data", "2019_SFS.xlsx")
BENCHMARK = os.path.join(ROOT, "outputs", "outputs_control_sorting_benchmark_marginal_profits.txt")

AGES = list(range(26, 86))
AGE_GROUPS = ["26-29", "30-34", "35-39", "40-44", "45-49", "50-54",
              "55-59", "60-64", "65-69", "70-74", "75-79", "80+"]


def read_census_homeownership():
    sheet = openpyxl.load_workbook(CENSUS_XLSX, data_only=True)["homeownership_housholds_szie"]
    by_age = {int(row[0]): row[1] for row in sheet.iter_rows(values_only=True)
              if isinstance(row[0], (int, float)) and 26 <= row[0] <= 85}
    return [100 * by_age[a] for a in AGES]


def read_sfs_ltv():
    sheet = openpyxl.load_workbook(SFS_XLSX, data_only=True)["LTV by Age Group"]
    # Rows are labelled "25-29", "30-34", ..., "80 years and older".
    values = [row[1] for row in sheet.iter_rows(values_only=True)
              if isinstance(row[0], str) and row[0][:2].isdigit()]
    assert len(values) == len(AGE_GROUPS), values
    return [100 * v for v in values]


def read_model_series(prefix, n):
    values = {}
    with open(BENCHMARK, errors="replace") as handle:
        for line in handle:
            parts = line.split()
            if parts and parts[0].startswith(prefix) and parts[0][len(prefix):].isdigit():
                values[int(parts[0][len(prefix):])] = float(parts[-1])
    return [100 * values[i] for i in range(1, n + 1)]


def make_figure(out_dir):
    homeownership_data = read_census_homeownership()
    homeownership_model = read_model_series("homeownership_age", len(AGES))
    ltv_data = read_sfs_ltv()
    ltv_model = read_model_series("LTV_age_group", len(AGE_GROUPS))

    # 10 in wide, printed at 190 mm: every label is at least 10 pt * 0.75 = 7.5 pt.
    fig, (ax_a, ax_b) = plt.subplots(1, 2, figsize=(10, 4.2))

    ax_a.plot(AGES, homeownership_data, label="Data", linewidth=1.8)
    ax_a.plot(AGES, homeownership_model, label="Model", linewidth=1.8)
    ax_a.set_title("Panel a: Homeownership by age", fontsize=14)
    ax_a.set_xlabel("Age", fontsize=13)
    ax_a.set_ylabel("Percentage", fontsize=13)
    ax_a.legend()
    ax_a.grid(True, color="0.85", linewidth=0.8)  # solid light grey: EPS has no transparency

    ax_b.plot(AGE_GROUPS, ltv_data, label="Data", linewidth=1.8)
    ax_b.plot(AGE_GROUPS, ltv_model, label="Model", linewidth=1.8)
    ax_b.set_title("Panel b: LTV by age group", fontsize=14)
    ax_b.set_xlabel("Age group", fontsize=13)
    ax_b.set_ylabel("Percentage", fontsize=13)
    ax_b.legend()
    ax_b.grid(True, color="0.85", linewidth=0.8)
    ax_b.tick_params(axis="x", labelsize=10, rotation=45)

    fig.tight_layout()
    base = os.path.join(out_dir, "Figure_2_homeownership_and_LTV")
    fig.savefig(base + ".pdf")
    fig.savefig(base + ".eps")
    plt.close(fig)
    print("Wrote", base + ".pdf", "and", base + ".eps")


if __name__ == "__main__":
    target = sys.argv[1] if len(sys.argv) > 1 else os.path.dirname(os.path.abspath(__file__))
    os.makedirs(target, exist_ok=True)
    make_figure(target)
