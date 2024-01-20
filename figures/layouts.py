import os
from pathlib import Path

import common
import matplotlib.pyplot as plt

ROOT = Path("output")

results = [file for file in os.listdir(ROOT) if file.endswith(".err")]


def parse_result(file):
    with open(ROOT / file) as f:
        lines = f.readlines()
        return float(lines[0].strip()), [row.strip() for row in lines[2:]]


fig, axs = plt.subplots(2, 2)

for idx, result in enumerate(results):
    dist, layout = parse_result(result)
    ax = axs[idx // 2, idx % 2]

    ax.set_title(f"Fitness: {dist}")
    ax.grid(False)
    ax.set_axis_off()
    ax.set_xlim(0, 10)
    ax.set_ylim(-1, 3)

    for y, row in enumerate(layout):
        for x, letter in enumerate(row):
            ax.plot(x, y, c="white")
            ax.text(
                x,
                y,
                letter,
                ha="center",
                va="center",
                fontsize=32,
                color="orange"
                if letter in "etoahnsirl"
                else "blue"
                if ord(letter) < ord("a") or ord(letter) > ord("z")
                else "black",
            )

plt.savefig(Path("figures") / "layouts.png", dpi=200)
# plt.show()
