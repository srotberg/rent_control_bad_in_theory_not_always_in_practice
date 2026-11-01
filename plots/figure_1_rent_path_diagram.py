"""
Figure 1 in "Rent Control? Bad in Theory, Not Always in Practice" (Rotberg and Zhang),
European Economic Review, manuscript EEREV-D-26-00102R2.

Conceptual diagram of rent paths under tenancy control: the free-market rent p_{r,1}
is constant, while the controlled rent starts at p~_{r,0} and declines with the number
of years in the same unit, averaging to the dotted level. The diagram is qualitative
and uses no data.

Usage:
    python figure_1_rent_path_diagram.py [output_directory]

Writes Figure_1_rent_path_diagram.pdf and Figure_1_rent_path_diagram.eps (vector, fonts
embedded) to output_directory (default: the folder containing this script).
Intended for full page width (190 mm). Requires Python 3 with matplotlib
(tested with Python 3.13, matplotlib 3.10).
"""

import math
import os
import sys

import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib.patches import FancyArrowPatch

# Elsevier artwork guidelines: Arial/Helvetica-type fonts, embedded; lettering at least 7 pt
# when printed. Liberation Sans is metrically identical to Arial; if it is not installed,
# matplotlib falls back to its default font.
matplotlib.rcParams.update({
    "pdf.fonttype": 42,          # embed TrueType fonts
    "ps.fonttype": 42,
    "font.family": "Liberation Sans",
    "mathtext.fontset": "custom",
    "mathtext.rm": "Liberation Sans",
    "mathtext.it": "Liberation Sans:italic",
    "mathtext.bf": "Liberation Sans:bold",
    "mathtext.cal": "Liberation Sans",  # unused; set so matplotlib does not look for a cursive font
    "mathtext.sf": "Liberation Sans",
    "font.size": 13,             # prints at about 9.7 pt at full page width (190 mm)
})

BLUE = "#1aa7e0"
RED = "#e5231b"

# Canvas coordinates: x to the right, y upward, origin at the axes' corner.
X_AXIS_END, Y_AXIS_END = 387, 255
FREE_RENT = 145            # p_{r,1}
CURVE_END = 374            # horizontal extent of the controlled-rent path


def controlled_rent(t):
    """Convex, declining controlled rent: starts above the free-market rent."""
    return -62.5 + 272.5 * math.exp(-0.00199 * t)


def arrow(ax, start, end, color, lw=1.6, style="-|>"):
    ax.add_patch(FancyArrowPatch(start, end, arrowstyle=style, mutation_scale=14,
                                 color=color, lw=lw, shrinkA=0, shrinkB=0))


def make_figure(out_dir):
    fig, ax = plt.subplots(figsize=(10, 5.625))
    ax.set_xlim(-113, 527)
    ax.set_ylim(-65, 295)
    ax.axis("off")

    # Axes
    arrow(ax, (0, 0), (0, Y_AXIS_END), "black", lw=1.4)
    arrow(ax, (0, 0), (X_AXIS_END, 0), "black", lw=1.4)
    ax.text(-73, Y_AXIS_END, r"Rent ($p_r$)", ha="left", va="center")
    ax.text(367, -24, "Years in same\n" + r"rental ($\iota$)", ha="left", va="center",
            linespacing=1.3)

    # Free-market rent
    arrow(ax, (0, FREE_RENT), (CURVE_END + 25, FREE_RENT), RED)
    ax.text(-6, FREE_RENT, r"$p_{r,1}$", ha="right", va="center")
    ax.text(247, FREE_RENT - 12, "Rent path in free market", ha="left", va="center")

    # Controlled rent: declining path, its starting level and its average level
    ts = [i * CURVE_END / 400 for i in range(401)]
    ax.plot(ts[:-8], [controlled_rent(t) for t in ts[:-8]], color=BLUE, lw=1.6,
            solid_capstyle="butt")
    arrow(ax, (ts[-9], controlled_rent(ts[-9])), (CURVE_END, controlled_rent(CURVE_END)), BLUE)
    ax.text(-6, controlled_rent(0), r"$\tilde{p}_{r,0}$", ha="right", va="center")

    average = 92
    t_average = -math.log((average + 62.5) / 272.5) / 0.00199
    ax.plot([0, t_average], [average, average], color="black", lw=1.1, ls=(0, (1.5, 2)))
    ax.text(-6, average, r"$\overline{\tilde{p}}_{r,0}$", ha="right", va="center")
    ax.text(277, controlled_rent(CURVE_END) - 13, "Rent path in controlled market",
            ha="left", va="center")

    fig.subplots_adjust(0, 0, 1, 1)
    base = os.path.join(out_dir, "Figure_1_rent_path_diagram")
    fig.savefig(base + ".pdf")
    fig.savefig(base + ".eps")
    plt.close(fig)
    print("Wrote", base + ".pdf", "and", base + ".eps")


if __name__ == "__main__":
    target = sys.argv[1] if len(sys.argv) > 1 else os.path.dirname(os.path.abspath(__file__))
    os.makedirs(target, exist_ok=True)
    make_figure(target)
