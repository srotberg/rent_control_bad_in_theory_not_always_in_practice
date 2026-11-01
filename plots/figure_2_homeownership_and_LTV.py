"""
Figure 2 ("Lifecycle statistics") in
"Rent Control? Bad in Theory, Not Always in Practice" (Rotberg and Zhang),
European Economic Review, manuscript EEREV-D-26-00102R2.

Panel (a): homeownership rate by age (26-85), data vs. model.
Panel (b): average loan-to-value (LTV) ratio of homeowners by age group, data vs. model.

Data sources (see ReadMe.pdf):
  - Homeownership by age: 2016 Census, Toronto CMA
    (data/2016_Census_vetted.xlsx, sheet "homeownership_housholds_szie").
  - LTV by age group: 2019 Survey of Financial Security, Toronto
    (data/2019_SFS.xlsx, sheet "LTV by Age Group").
  - Model: benchmark equilibrium
    (outputs/outputs_control_sorting_benchmark_marginal_profits.txt).

Usage:
    python figure_2_homeownership_and_LTV.py [output_directory]

Writes Figure_2_homeownership_and_LTV.png (300 dpi) and
Figure_2_homeownership_and_LTV.tiff (600 dpi) to output_directory
(default: the folder containing this script).
Requires Python 3 and matplotlib (tested with Python 3.13, matplotlib 3.10).
"""

import os
import sys

import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt

# ---------------------------------------------------------------------------
# Panel (a): homeownership rate (%) by age, ages 26-85
# ---------------------------------------------------------------------------
age_single_years = list(range(26, 86))

homeownership_data = [
    26.49, 31.40, 34.79, 38.85, 42.90, 45.81, 48.41, 53.05, 55.01, 56.48,
    58.71, 61.14, 62.92, 63.66, 65.44, 67.51, 67.35, 68.90, 69.00, 69.82,
    70.15, 71.07, 71.12, 72.45, 72.42, 72.89, 73.22, 74.18, 74.25, 73.95,
    74.69, 74.39, 74.46, 75.19, 74.85, 75.81, 75.29, 76.02, 75.84, 75.21,
    76.34, 75.71, 77.02, 74.70, 75.24, 75.90, 74.46, 76.00, 74.74, 75.22,
    75.83, 76.18, 74.96, 75.06, 75.02, 75.18, 76.08, 73.32, 74.23, 74.85,
]

# Model shares (fractions), converted to percent below.
homeownership_model_share = [
    0, 0, 0.001719499931, 0.04000999409, 0.06735062516, 0.09866190412,
    0.1878953123, 0.2179826468, 0.2434505027, 0.2588414492, 0.2890855077,
    0.3204306507, 0.3574096598, 0.4076818384, 0.4522581269, 0.5055719164,
    0.5592675263, 0.5778780658, 0.5952204449, 0.6100768879, 0.6275510519,
    0.6469389896, 0.6641973115, 0.6790131602, 0.7007185463, 0.7191308191,
    0.7380631028, 0.757334038, 0.7781021454, 0.7994107798, 0.8177713918,
    0.8363318309, 0.8500400029, 0.8571356264, 0.86425021, 0.8711930587,
    0.8752332355, 0.8780249922, 0.8790485084, 0.8789969874, 0.8813235834,
    0.7561633376, 0.7566148002, 0.7569362547, 0.7571538576, 0.7572678746,
    0.7573299456, 0.757365693, 0.7573799271, 0.7573854695, 0.75738825,
    0.7573891394, 0.7573894723, 0.7573895806, 0.7573896139, 0.7573896155,
    0.7573277287, 0.7573246407, 0.7578476668, 0.6498435669,
]
homeownership_model = [100 * x for x in homeownership_model_share]

# ---------------------------------------------------------------------------
# Panel (b): average LTV ratio (%) of homeowners by age group
# ---------------------------------------------------------------------------
age_groups = ["26-29", "30-34", "35-39", "40-44", "45-49", "50-54",
              "55-59", "60-64", "65-69", "70-74", "75-79", "80+"]

ltv_data = [49.00, 49.70, 48.51, 37.20, 34.44, 23.84,
            17.96, 14.39, 8.45, 6.33, 4.96, 0.43]

ltv_model_share = [0.8171187744, 0.7398428952, 0.6690844584, 0.6018519927,
                   0.5059517984, 0.3824145582, 0.2342526165, 0.09789341169,
                   0.0102047727, 0, 0, 0]
ltv_model = [100 * x for x in ltv_model_share]

assert len(homeownership_data) == len(homeownership_model) == len(age_single_years)
assert len(ltv_data) == len(ltv_model) == len(age_groups)


def make_figure(out_dir):
    plt.rcParams.update({"font.size": 12})
    fig, (ax_a, ax_b) = plt.subplots(1, 2, figsize=(12, 4.5))

    ax_a.plot(age_single_years, homeownership_data, label="Data", linewidth=1.8)
    ax_a.plot(age_single_years, homeownership_model, label="Model", linewidth=1.8)
    ax_a.set_title("Panel a: Homeownership by age", fontsize=16)
    ax_a.set_xlabel("Age", fontsize=14)
    ax_a.set_ylabel("Percentage", fontsize=14)
    ax_a.legend()
    ax_a.grid(True, alpha=0.4)

    ax_b.plot(age_groups, ltv_data, label="Data", linewidth=1.8)
    ax_b.plot(age_groups, ltv_model, label="Model", linewidth=1.8)
    ax_b.set_title("Panel b: LTV by age group", fontsize=16)
    ax_b.set_xlabel("Age group", fontsize=14)
    ax_b.set_ylabel("Percentage", fontsize=14)
    ax_b.legend()
    ax_b.grid(True, alpha=0.4)
    ax_b.tick_params(axis="x", labelsize=9.5)

    fig.tight_layout()
    base = os.path.join(out_dir, "Figure_2_homeownership_and_LTV")
    fig.savefig(base + ".png", dpi=300)
    fig.savefig(base + ".tiff", dpi=600, pil_kwargs={"compression": "tiff_lzw"})
    plt.close(fig)
    print("Wrote", base + ".png", "and", base + ".tiff")


if __name__ == "__main__":
    target = sys.argv[1] if len(sys.argv) > 1 else os.path.dirname(os.path.abspath(__file__))
    os.makedirs(target, exist_ok=True)
    make_figure(target)
