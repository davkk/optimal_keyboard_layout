from pathlib import Path

import common
import matplotlib.pyplot as plt

ROOT = Path("output")

fig, ax = plt.subplots()

ax.grid(False)
ax.set_axis_off()
ax.set_xlim(0, 10)
ax.set_ylim(-1, 3)

layout = [
    "qwertyuiop",
    "asdfghjkl;",
    "zxcvbnm,./",
]

for y, row in enumerate(layout[::-1]):
    for x, letter in enumerate(row):
        if y == 1 and (x <= 3 or x >= 6):
            ax.annotate(
                "",
                xy=(x, 2-0.25),
                xytext=(x, y),
                arrowprops=dict(
                    arrowstyle="->",
                    color="#cecacd",
                    lw=3,
                ),
                zorder=-1,
            )
            ax.annotate(
                "",
                xy=(x, 0.25),
                xytext=(x, y),
                arrowprops=dict(
                    arrowstyle="->",
                    color="#cecacd",
                    lw=3,
                    zorder=-1,
                ),
            )
        if letter == "f":
            for i in range(3):
                ax.annotate(
                    "",
                    xy=(x + 1 - 0.25, i - (i - 1) * 0.25),
                    xytext=(x, y),
                    arrowprops=dict(
                        arrowstyle="->",
                        color="#cecacd",
                        lw=3,
                    ),
                    zorder=-1,
                )
        if letter == "j":
            for i in range(3):
                ax.annotate(
                    "",
                    xy=(x - 1 + 0.25, i - (i - 1) * 0.25),
                    xytext=(x, y),
                    arrowprops=dict(
                        arrowstyle="->",
                        color="#cecacd",
                        lw=3,
                    ),
                    zorder=-1,
                )

        ax.plot(x, y, c="white")

        ax.text(
            x,
            y,
            letter,
            ha="center",
            va="center",
            fontsize=42,
            color="blue"
            if (x <= 3 and y == 1) or (x >= 6 and y == 1)
            else "black",
        )


plt.savefig(Path("figures") / "fitness.png", dpi=200)
# plt.show()
