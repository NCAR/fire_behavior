#!/usr/bin/env python3
# Created on 2026-10-04 by the CFBM development team assisted by GPT-6-Astra.
# run python -B tests/regression/doc/plot_cases.py --output-dir=tests/regression/doc/figures
"""Plot prescribed case geometry and winds, without running the fire model.

Read the production case settings and field generator so documentation follows
changes to the tested inputs. These are configuration plots, not fire forecasts.
"""
from __future__ import annotations

#--------------------------------------------------------------------------------
# Python modules and paths
#--------------------------------------------------------------------------------
import argparse
import os
from pathlib import Path
import sys
from typing import Any

MODULE_ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(MODULE_ROOT))
# Keep the Matplotlib cache outside the checkout on shared HPC filesystems.
if "MPLCONFIGDIR" not in os.environ and Path("/glade").exists():
    os.environ["MPLCONFIGDIR"] = (
        f"/glade/derecho/scratch/{os.environ['USER']}/tmp/matplotlib")

import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib.axes import Axes
from matplotlib.figure import Figure
from matplotlib.patches import Circle, Rectangle
import numpy as np

from config import load_yaml, resolve_spec
from generate_inputs import _fields

#--------------------------------------------------------------------------------
# Plot configuration
#--------------------------------------------------------------------------------
# Half-screen typography, matching the presentation-artifacts style without
# requiring a contributor to install a personal skill package.
FIGSIZE = (6.5, 6.5)
PROFILE_FIGSIZE = (6.5, 5.0)
DPI = 180
BASE_SIZE = 16
SMALL_SIZE = 14
ITEM_STYLES = {
    "u": {
        "color": "#0072B2",
        "linestyle": "-",
        "marker": "o"
    },
    "v": {
        "color": "#D55E00",
        "linestyle": "--",
        "marker": "s"
    },
}
plt.rcParams.update({
    "figure.facecolor": "white",
    "axes.facecolor": "white",
    "savefig.facecolor": "white",
    "font.size": BASE_SIZE,
    "axes.titlesize": BASE_SIZE + 2,
    "axes.labelsize": BASE_SIZE,
    "xtick.labelsize": SMALL_SIZE,
    "ytick.labelsize": SMALL_SIZE,
    "legend.fontsize": SMALL_SIZE,
    "lines.linewidth": 3,
    "text.color": "#1a1a1a",
    "axes.labelcolor": "#1a1a1a",
})

#--------------------------------------------------------------------------------
# Shared domain and ignition annotations
#--------------------------------------------------------------------------------


def domain_axes(spec: dict[str, Any], title: str) -> tuple[Figure, Axes]:
    """Draw the horizontal fire domain with projected distances in kilometres."""
    grid = spec["grid"]
    width = grid["nx"] * grid["dx_m"] / 1000
    height = grid["ny"] * grid["dy_m"] / 1000
    fig, ax = plt.subplots(figsize=FIGSIZE, constrained_layout=True)
    ax.set(xlim=(0, width),
           ylim=(0, height),
           aspect="equal",
           title=title,
           xlabel="Grid x (km)",
           ylabel="Grid y (km)")
    return fig, ax


def ignition_overlay(ax: Axes, spec: dict[str, Any]) -> None:
    """Mark prescribed ignition geometry rather than a simulated perimeter."""
    grid, ignition = spec["grid"], spec["ignition"]
    width = grid["nx"] * grid["dx_m"] / 1000
    height = grid["ny"] * grid["dy_m"] / 1000
    if ignition["kind"] == "line":
        x = ignition["line_x_fraction"] * width
        y0 = ignition["line_y_start_fraction"] * height
        y1 = ignition["line_y_end_fraction"] * height
        ax.plot([x, x], [y0, y1], color="#D55E00", linewidth=4)
        ax.text(x + 0.3,
                0.5 * (y0 + y1),
                "Line\nignition",
                fontsize=SMALL_SIZE,
                va="center",
                bbox={
                    "facecolor": "white",
                    "edgecolor": "none",
                    "alpha": 0.9
                })
    else:
        x = ignition["center_x_fraction"] * width
        y = ignition["center_y_fraction"] * height
        radius = ignition["radius_m"] / 1000
        ax.add_patch(
            Circle((x, y),
                   radius,
                   fill=False,
                   color="black",
                   linewidth=3,
                   linestyle="--"))
        if ignition["kind"] == "point":
            ax.plot(x, y, marker="+", color="black", markersize=12)
        label = (f"{ignition['radius_m']:g} m radius\n"
                 f"active from {ignition['start_time_s']:g} s")
        ax.text(x,
                y - radius - 0.35,
                label,
                ha="center",
                va="top",
                fontsize=SMALL_SIZE,
                bbox={
                    "facecolor": "white",
                    "edgecolor": "none",
                    "alpha": 0.9
                })


def save_figure(fig: Figure, destination: Path) -> None:
    """Save a portable PNG with script provenance and print its location."""
    fig.savefig(destination,
                dpi=DPI,
                metadata={
                    "Software": Path(__file__).name,
                    "Description": "Prescribed configuration from cases.yaml"
                })
    plt.close(fig)
    print(destination)


#--------------------------------------------------------------------------------
# Case maps and vertical profiles
#--------------------------------------------------------------------------------


def plot_fuels(spec: dict[str, Any], destination: Path) -> None:
    """Label fuel categories directly so strip colours imply no ordering."""
    case = spec["identity"]["case"]
    fig, ax = domain_axes(spec, case)
    fields = _fields(spec)
    fuel = fields["NFUEL_CAT"][:, 0]
    grid = spec["grid"]
    width = grid["nx"] * grid["dx_m"] / 1000
    starts = np.r_[0, np.flatnonzero(np.diff(fuel)) + 1, len(fuel)]
    for index, (start, stop) in enumerate(zip(starts[:-1], starts[1:])):
        bottom = start * grid["dy_m"] / 1000
        depth = (stop - start) * grid["dy_m"] / 1000
        ax.add_patch(
            Rectangle((0, bottom),
                      width,
                      depth,
                      facecolor=("#eeeeee" if index % 2 else "#cccccc"),
                      edgecolor="white"))
        ax.text(width - 0.25,
                bottom + depth / 2,
                f"{int(fuel[start])}",
                ha="right",
                va="center",
                fontsize=SMALL_SIZE)
    ignition_overlay(ax, spec)
    ax.set_title(f"{case}\nAnderson fuel categories", fontsize=BASE_SIZE)
    save_figure(fig, destination)


def plot_terrain(spec: dict[str, Any], destination: Path) -> None:
    """Show the actual generated terrain and supplied observed perimeter."""
    fig, ax = domain_axes(spec, "Shared terrain\nand observed perimeter")
    grid = spec["grid"]
    x = (np.arange(grid["nx"]) + 0.5) * grid["dx_m"] / 1000
    y = (np.arange(grid["ny"]) + 0.5) * grid["dy_m"] / 1000
    field = _fields(spec)["ZSF"]
    mesh = ax.pcolormesh(x, y, field, shading="nearest", cmap="cividis")
    ax.contour(x, y, field, levels=5, colors="white", linewidths=0.7)
    ignition_overlay(ax, spec)
    fig.colorbar(mesh,
                 ax=ax,
                 orientation="horizontal",
                 shrink=0.9,
                 label="Terrain elevation (m)",
                 pad=0.12)
    save_figure(fig, destination)


def plot_profiles(spec: dict[str, Any], destination: Path) -> None:
    """Plot the unscaled vertical profile and its logarithmic sampling height."""
    forcing = spec["forcing"]
    interfaces = np.asarray(forcing["height_interfaces_m"])
    heights = 0.5 * (interfaces[:-1] + interfaces[1:])
    target = spec["interpolation"]["fire_wind_height_m"]
    fig, ax = plt.subplots(figsize=PROFILE_FIGSIZE, constrained_layout=True)
    # Log-height axes make the interpolation used by the reader explicit.
    for component in ("u", "v"):
        ax.plot(forcing[f"{component}_profile_m_s"],
                heights,
                label=component.upper(),
                **ITEM_STYLES[component])
    ax.axhline(target,
               color="black",
               linestyle=":",
               linewidth=2,
               label=f"Fire wind height: {target:g} m")
    ax.set(yscale="log",
           xlabel="Wind component (m/s)",
           ylabel="Height AGL (m)",
           title="terrain_u3d: unscaled profile",
           yticks=heights)
    ax.set_yticklabels([f"{height:g}" for height in heights])
    ax.minorticks_off()
    ax.text(forcing["u_profile_m_s"][-1] - 1.0,
            heights[-1],
            "U",
            ha="right",
            va="center",
            color=ITEM_STYLES["u"]["color"])
    ax.text(forcing["v_profile_m_s"][-1] + 1.0,
            heights[-1],
            "V",
            ha="left",
            va="center",
            color=ITEM_STYLES["v"]["color"])
    ax.text(0.98,
            target,
            f"Sampling height: {target:g} m",
            transform=ax.get_yaxis_transform(),
            ha="right",
            va="bottom",
            fontsize=SMALL_SIZE)
    save_figure(fig, destination)


def main() -> None:
    """Generate documentation figures directly from the current standard cases."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output-dir", type=Path, required=True)
    args = parser.parse_args()
    args.output_dir.mkdir(parents=True, exist_ok=True)
    document = load_yaml(MODULE_ROOT / "cases.yaml")
    for case in ("circle_nowind", "fuel_strip_wind", "terrain_u10m"):
        spec = resolve_spec(document, case)
        plot_fuels(spec, args.output_dir / f"{case}_fuels.png")
    spec = resolve_spec(document, "terrain_u3d")
    plot_terrain(spec, args.output_dir / "terrain.png")
    plot_profiles(spec, args.output_dir / "terrain_u3d_profile.png")


if __name__ == "__main__":
    main()
