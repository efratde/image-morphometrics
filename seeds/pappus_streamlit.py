"""
Pappus Morphology Analyzer — Streamlit web app
Upload a seed image; adjust processing parameters; download measurements as CSV.
"""

import streamlit as st
import cv2
import numpy as np
from PIL import Image
from skimage import measure
from scipy import ndimage
from scipy.spatial.distance import pdist, squareform
import pandas as pd
import io

st.set_page_config(page_title="Pappus Morphology Analyzer", layout="wide")

st.title("Pappus Morphology Analyzer")
st.markdown(
    "Upload a seed image, tune the segmentation parameters, and download morphometric "
    "measurements. Designed for wind-dispersed seeds (*Asteraceae* pappus structures)."
)

# ── Sidebar: all parameters ────────────────────────────────────────────────
with st.sidebar:
    st.header("Image processing")

    bg_correction = st.checkbox("Background correction (rolling-ball)", value=True,
        help="Removes uneven illumination via morphological opening.")
    adaptive = st.checkbox("Adaptive threshold", value=True,
        help="Adapts threshold locally; better for uneven lighting.")

    blur = st.slider("Gaussian blur σ", 0.0, 10.0, 1.0, 0.5)
    morph = st.slider("Morphological strength", 0, 10, 2,
        help="Closing + opening kernel radius for cleaning the mask.")
    edge = st.slider("Edge enhancement", 0.0, 2.0, 0.0, 0.1,
        help="Weight of Canny-edge overlay added before thresholding.")

    if adaptive:
        block_size = st.slider("Adaptive block size", 3, 99, 11, 2,
            help="Neighbourhood for adaptive threshold (must be odd; forced if even).")
        threshold = None
    else:
        threshold = st.slider("Global threshold", 0, 255, 128)
        block_size = None

    fill_holes = st.checkbox("Fill holes", value=True)
    remove_small = st.checkbox("Remove small objects", value=True)
    min_px = st.slider("Min object size (px)", 10, 5000, 100,
        help="Objects below this area are removed before measurement.")

    st.divider()
    st.header("Scale calibration")
    pixels_per_mm = st.number_input(
        "Pixels per mm  (leave 1.0 for pixel units)", value=1.0, min_value=0.001, step=0.1)
    scale = 1.0 / pixels_per_mm   # mm per pixel


# ── Image upload ───────────────────────────────────────────────────────────
uploaded = st.file_uploader(
    "Upload seed image", type=["jpg", "jpeg", "png", "tiff", "tif", "bmp"])

if uploaded is None:
    st.info("Upload an image to begin.")
    st.stop()

# Decode
raw = np.frombuffer(uploaded.read(), np.uint8)
original_bgr = cv2.imdecode(raw, cv2.IMREAD_COLOR)
if original_bgr is None:
    st.error("Could not decode image. Try a different file.")
    st.stop()
original_rgb = cv2.cvtColor(original_bgr, cv2.COLOR_BGR2RGB)
gray_src = cv2.cvtColor(original_bgr, cv2.COLOR_BGR2GRAY)


# ── Processing pipeline ────────────────────────────────────────────────────
gray = gray_src.copy().astype(np.float32)

if bg_correction:
    kernel = cv2.getStructuringElement(cv2.MORPH_ELLIPSE, (50, 50))
    bg = cv2.morphologyEx(gray.astype(np.uint8), cv2.MORPH_OPEN, kernel).astype(np.float32)
    gray = cv2.subtract(gray.astype(np.uint8), bg.astype(np.uint8)).astype(np.float32)
    gray = cv2.normalize(gray, None, 0, 255, cv2.NORM_MINMAX)

gray_u8 = np.clip(gray, 0, 255).astype(np.uint8)

if blur > 0:
    gray_u8 = cv2.GaussianBlur(gray_u8, (0, 0), blur)

if edge > 0:
    edges = cv2.Canny(gray_u8, 50, 150)
    gray_u8 = cv2.addWeighted(gray_u8, 1.0, edges, edge, 0)

if adaptive:
    bs = int(block_size) if block_size % 2 == 1 else int(block_size) + 1
    mask = cv2.adaptiveThreshold(
        gray_u8, 255, cv2.ADAPTIVE_THRESH_GAUSSIAN_C, cv2.THRESH_BINARY, bs, 2)
else:
    _, mask = cv2.threshold(gray_u8, int(threshold), 255, cv2.THRESH_BINARY)

if morph > 0:
    k = cv2.getStructuringElement(cv2.MORPH_ELLIPSE, (morph * 2 + 1, morph * 2 + 1))
    mask = cv2.morphologyEx(mask, cv2.MORPH_CLOSE, k)
    mask = cv2.morphologyEx(mask, cv2.MORPH_OPEN, k)

if fill_holes:
    mask = (ndimage.binary_fill_holes(mask > 0).astype(np.uint8)) * 255

if remove_small:
    labeled_tmp = measure.label(mask > 0)
    for region in measure.regionprops(labeled_tmp):
        if region.area < min_px:
            mask[labeled_tmp == region.label] = 0


# ── Measurement ────────────────────────────────────────────────────────────
labeled = measure.label(mask > 0)
regions = measure.regionprops(labeled)

results = None
annotated = cv2.cvtColor(mask, cv2.COLOR_GRAY2RGB)

if regions:
    largest = max(regions, key=lambda r: r.area)

    area_mm2 = largest.area * (scale ** 2)
    perim_mm  = largest.perimeter * scale
    circ = (4 * np.pi * area_mm2) / (perim_mm ** 2) if perim_mm > 0 else 0.0

    coords = largest.coords
    feret_mm = 0.0
    if len(coords) > 2:
        dists = squareform(pdist(coords))
        feret_mm = float(np.max(dists)) * scale

    convexity = largest.area / largest.convex_area if largest.convex_area > 0 else 0.0
    unit = "mm" if pixels_per_mm != 1.0 else "px"

    results = {
        f"Area ({unit}²)":            round(area_mm2, 4),
        f"Perimeter ({unit})":        round(perim_mm, 4),
        "Circularity":                round(circ, 4),
        f"Feret diameter ({unit})":   round(feret_mm, 4),
        "Convexity":                  round(convexity, 4),
        "Eccentricity":               round(float(largest.eccentricity), 4),
        "Solidity":                   round(float(largest.solidity), 4),
        "N objects detected":         len(regions),
    }

    # Annotate mask
    contours, _ = cv2.findContours(mask, cv2.RETR_EXTERNAL, cv2.CHAIN_APPROX_SIMPLE)
    cv2.drawContours(annotated, contours, -1, (0, 200, 0), 2)
    r, c1, maxr, maxc = largest.bbox
    cv2.rectangle(annotated, (c1, r), (maxc, maxr), (255, 80, 0), 2)


# ── Display ────────────────────────────────────────────────────────────────
tab_orig, tab_mask, tab_result = st.tabs(["Original", "Mask", "Measurements"])

with tab_orig:
    st.image(original_rgb, use_container_width=True)

with tab_mask:
    col_mask, col_ann = st.columns(2)
    with col_mask:
        st.caption("Binary mask")
        st.image(mask, use_container_width=True, clamp=True)
    with col_ann:
        st.caption("Annotated (largest object)")
        st.image(annotated, use_container_width=True)

with tab_result:
    if results is None:
        st.warning("No objects detected. Try adjusting the threshold or morphological settings.")
    else:
        df = pd.DataFrame.from_dict(results, orient="index", columns=["Value"])
        st.dataframe(df, use_container_width=True)

        csv_bytes = pd.DataFrame([results]).to_csv(index=False).encode()
        st.download_button(
            "⬇ Download CSV", csv_bytes,
            file_name="pappus_measurements.csv", mime="text/csv")
