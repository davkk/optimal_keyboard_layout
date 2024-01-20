import os
from pathlib import Path

import common
import matplotlib.pyplot as plt
import numpy as np

ROOT = Path("output")

stats = [
    file
    for file in os.listdir(ROOT)
    if not file.endswith(".err") and not file.endswith(".seq")
]

fig, axs = plt.subplots(ncols=2)

for stat in stats:
    min_value, avg_value = np.loadtxt(ROOT / stat).T

    axs[0].set_title("Min")
    axs[0].set_xlabel("Generation")
    axs[0].set_ylabel("Fitness")
    axs[0].plot(min_value)

    axs[1].set_title("Avg")
    axs[1].set_xlabel("Generation")
    axs[1].plot(avg_value)

fig.tight_layout()
plt.savefig(Path("figures") / "stats.png", dpi=200)
# plt.show()
