from pathlib import Path

import common
import matplotlib.pyplot as plt

letters = []
counts = []
with open("hist.csv", "r") as f:
    lines = f.readlines()
    for line in lines:
        letter, count = line.strip().split()
        letters.append(letter)
        counts.append(int(count))

_, stemlines, baseline = plt.stem(letters, counts)

stemlines.set_linewidth(8)
baseline.set_color("none")

plt.grid(False)
plt.tight_layout()

plt.savefig(Path("figures") / "hist.png", dpi=200)
# plt.show()
