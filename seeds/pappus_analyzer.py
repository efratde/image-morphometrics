"""
Pappus Morphology Analyzer
Advanced tool for measuring wind-dispersed seed traits with sophisticated image processing
"""

import tkinter as tk
from tkinter import ttk, filedialog, messagebox
import cv2
import numpy as np
from PIL import Image, ImageTk
import json
from datetime import datetime
import pandas as pd
from scipy import ndimage
from skimage import morphology, measure, filters
import os

class PappusAnalyzer:
    def __init__(self, root):
        self.root = root
        self.root.title("Pappus Morphology Analyzer")
        self.root.geometry("1400x900")
        
        # Variables
        self.original_image = None
        self.processed_image = None
        self.mask = None
        self.scale_factor = 1.0  # mm per pixel
        self.drawing = False
        self.current_tool = "threshold"
        self.last_x = None
        self.last_y = None
        
        # Setup UI
        self.setup_ui()
        
    def setup_ui(self):
        # Main container
        main_frame = ttk.Frame(self.root, padding="10")
        main_frame.grid(row=0, column=0, sticky=(tk.W, tk.E, tk.N, tk.S))
        
        # Left panel - Image display
        left_frame = ttk.LabelFrame(main_frame, text="Image Processing", padding="10")
        left_frame.grid(row=0, column=0, sticky=(tk.W, tk.E, tk.N, tk.S), padx=5)
        
        # Image canvas
        self.canvas = tk.Canvas(left_frame, width=600, height=600, bg='gray')
        self.canvas.pack(pady=10)
        self.canvas.bind("<Button-1>", self.on_mouse_down)
        self.canvas.bind("<B1-Motion>", self.on_mouse_drag)
        self.canvas.bind("<ButtonRelease-1>", self.on_mouse_up)
        
        # Tool buttons
        tool_frame = ttk.Frame(left_frame)
        tool_frame.pack(pady=5)
        
        self.tool_var = tk.StringVar(value="threshold")
        tools = [
            ("Threshold", "threshold"),
            ("Eraser", "eraser"),
            ("Pencil", "pencil"),
            ("Magic Wand", "magic_wand"),
            ("Watershed", "watershed")
        ]
        
        for text, value in tools:
            ttk.Radiobutton(tool_frame, text=text, variable=self.tool_var, 
                          value=value, command=self.change_tool).pack(side=tk.LEFT, padx=5)
        
        # Load image button
        ttk.Button(left_frame, text="Load Image", command=self.load_image).pack(pady=5)
        
        # Right panel - Controls
        right_frame = ttk.LabelFrame(main_frame, text="Controls & Measurements", padding="10")
        right_frame.grid(row=0, column=1, sticky=(tk.W, tk.E, tk.N, tk.S), padx=5)
        
        # Preprocessing controls
        preprocess_frame = ttk.LabelFrame(right_frame, text="Preprocessing", padding="10")
        preprocess_frame.pack(fill=tk.X, pady=5)
        
        # Background correction
        ttk.Label(preprocess_frame, text="Background Correction:").grid(row=0, column=0, sticky=tk.W)
        self.bg_correction_var = tk.BooleanVar(value=True)
        ttk.Checkbutton(preprocess_frame, text="Enable", 
                       variable=self.bg_correction_var).grid(row=0, column=1)
        
        # Adaptive threshold
        ttk.Label(preprocess_frame, text="Adaptive Threshold:").grid(row=1, column=0, sticky=tk.W)
        self.adaptive_var = tk.BooleanVar(value=True)
        ttk.Checkbutton(preprocess_frame, text="Enable", 
                       variable=self.adaptive_var).grid(row=1, column=1)
        
        # Threshold controls
        threshold_frame = ttk.LabelFrame(right_frame, text="Threshold Settings", padding="10")
        threshold_frame.pack(fill=tk.X, pady=5)
        
        # Global threshold
        ttk.Label(threshold_frame, text="Global Threshold:").grid(row=0, column=0, sticky=tk.W)
        self.threshold_var = tk.IntVar(value=128)
        self.threshold_slider = ttk.Scale(threshold_frame, from_=0, to=255, 
                                         variable=self.threshold_var, 
                                         command=self.update_threshold)
        self.threshold_slider.grid(row=0, column=1, sticky=(tk.W, tk.E))
        self.threshold_label = ttk.Label(threshold_frame, text="128")
        self.threshold_label.grid(row=0, column=2)
        
        # Block size for adaptive threshold
        ttk.Label(threshold_frame, text="Block Size:").grid(row=1, column=0, sticky=tk.W)
        self.block_size_var = tk.IntVar(value=11)
        ttk.Scale(threshold_frame, from_=3, to=99, variable=self.block_size_var,
                 command=self.update_threshold).grid(row=1, column=1, sticky=(tk.W, tk.E))
        
        # Morphological operations
        morph_frame = ttk.LabelFrame(right_frame, text="Morphological Operations", padding="10")
        morph_frame.pack(fill=tk.X, pady=5)
        
        # Blur
        ttk.Label(morph_frame, text="Gaussian Blur:").grid(row=0, column=0, sticky=tk.W)
        self.blur_var = tk.DoubleVar(value=1.0)
        ttk.Scale(morph_frame, from_=0, to=10, variable=self.blur_var,
                 command=self.update_processing).grid(row=0, column=1, sticky=(tk.W, tk.E))
        
        # Morphology strength
        ttk.Label(morph_frame, text="Morph. Strength:").grid(row=1, column=0, sticky=tk.W)
        self.morph_var = tk.IntVar(value=2)
        ttk.Scale(morph_frame, from_=0, to=10, variable=self.morph_var,
                 command=self.update_processing).grid(row=1, column=1, sticky=(tk.W, tk.E))
        
        # Edge enhancement
        ttk.Label(morph_frame, text="Edge Enhancement:").grid(row=2, column=0, sticky=tk.W)
        self.edge_var = tk.DoubleVar(value=0.0)
        ttk.Scale(morph_frame, from_=0, to=2, variable=self.edge_var,
                 command=self.update_processing).grid(row=2, column=1, sticky=(tk.W, tk.E))
        
        # Tool settings
        tool_settings_frame = ttk.LabelFrame(right_frame, text="Tool Settings", padding="10")
        tool_settings_frame.pack(fill=tk.X, pady=5)
        
        ttk.Label(tool_settings_frame, text="Brush Size:").grid(row=0, column=0, sticky=tk.W)
        self.brush_size_var = tk.IntVar(value=5)
        ttk.Scale(tool_settings_frame, from_=1, to=50, variable=self.brush_size_var).grid(row=0, column=1, sticky=(tk.W, tk.E))
        
        ttk.Label(tool_settings_frame, text="Tolerance (Magic Wand):").grid(row=1, column=0, sticky=tk.W)
        self.tolerance_var = tk.IntVar(value=10)
        ttk.Scale(tool_settings_frame, from_=1, to=50, variable=self.tolerance_var).grid(row=1, column=1, sticky=(tk.W, tk.E))
        
        # Scale calibration
        scale_frame = ttk.LabelFrame(right_frame, text="Scale Calibration", padding="10")
        scale_frame.pack(fill=tk.X, pady=5)
        
        ttk.Label(scale_frame, text="Pixels:").grid(row=0, column=0, sticky=tk.W)
        self.pixels_entry = ttk.Entry(scale_frame, width=10)
        self.pixels_entry.grid(row=0, column=1)
        self.pixels_entry.insert(0, "1")
        
        ttk.Label(scale_frame, text="= mm:").grid(row=0, column=2, sticky=tk.W)
        self.mm_entry = ttk.Entry(scale_frame, width=10)
        self.mm_entry.grid(row=0, column=3)
        self.mm_entry.insert(0, "1")
        
        # Action buttons
        button_frame = ttk.Frame(right_frame)
        button_frame.pack(pady=10)
        
        ttk.Button(button_frame, text="Process", command=self.process_image).grid(row=0, column=0, padx=2)
        ttk.Button(button_frame, text="Fill Holes", command=self.fill_holes).grid(row=0, column=1, padx=2)
        ttk.Button(button_frame, text="Remove Small", command=self.remove_small_objects).grid(row=0, column=2, padx=2)
        ttk.Button(button_frame, text="Measure", command=self.measure).grid(row=1, column=0, padx=2)
        ttk.Button(button_frame, text="Reset", command=self.reset_image).grid(row=1, column=1, padx=2)
        ttk.Button(button_frame, text="Export", command=self.export_results).grid(row=1, column=2, padx=2)
        
        # Measurements display
        self.measurements_frame = ttk.LabelFrame(right_frame, text="Measurements", padding="10")
        self.measurements_frame.pack(fill=tk.BOTH, expand=True, pady=5)
        
        self.measurements_text = tk.Text(self.measurements_frame, height=10, width=40)
        self.measurements_text.pack()
        
    def load_image(self):
        file_path = filedialog.askopenfilename(
            filetypes=[("Image files", "*.jpg *.jpeg *.png *.tiff *.tif *.bmp")]
        )
        if file_path:
            self.original_image = cv2.imread(file_path)
            self.original_image = cv2.cvtColor(self.original_image, cv2.COLOR_BGR2RGB)
            self.display_image(self.original_image)
            self.process_image()
    
    def display_image(self, img):
        if img is None:
            return
            
        # Resize image to fit canvas
        height, width = img.shape[:2]
        max_size = 600
        
        if height > max_size or width > max_size:
            scale = min(max_size/width, max_size/height)
            new_width = int(width * scale)
            new_height = int(height * scale)
            img = cv2.resize(img, (new_width, new_height))
        
        # Convert to PIL Image and display
        if len(img.shape) == 2:  # Grayscale
            img = cv2.cvtColor(img, cv2.COLOR_GRAY2RGB)
        
        image = Image.fromarray(img)
        self.photo = ImageTk.PhotoImage(image)
        self.canvas.delete("all")
        self.canvas.create_image(0, 0, anchor=tk.NW, image=self.photo)
        self.canvas.config(width=img.shape[1], height=img.shape[0])
    
    def process_image(self):
        if self.original_image is None:
            return
        
        # Convert to grayscale
        gray = cv2.cvtColor(self.original_image, cv2.COLOR_RGB2GRAY)
        
        # Background correction
        if self.bg_correction_var.get():
            # Rolling ball background subtraction
            kernel_size = 50
            kernel = cv2.getStructuringElement(cv2.MORPH_ELLIPSE, (kernel_size, kernel_size))
            background = cv2.morphologyEx(gray, cv2.MORPH_OPEN, kernel)
            gray = cv2.subtract(gray, background)
            gray = cv2.normalize(gray, None, 0, 255, cv2.NORM_MINMAX)
        
        # Apply Gaussian blur
        blur_amount = self.blur_var.get()
        if blur_amount > 0:
            gray = cv2.GaussianBlur(gray, (0, 0), blur_amount)
        
        # Edge enhancement
        edge_strength = self.edge_var.get()
        if edge_strength > 0:
            edges = cv2.Canny(gray, 50, 150)
            gray = cv2.addWeighted(gray, 1, edges, edge_strength, 0)
        
        # Thresholding
        if self.adaptive_var.get():
            # Adaptive threshold for varying illumination
            block_size = self.block_size_var.get()
            if block_size % 2 == 0:
                block_size += 1
            self.mask = cv2.adaptiveThreshold(gray, 255, cv2.ADAPTIVE_THRESH_GAUSSIAN_C,
                                             cv2.THRESH_BINARY, block_size, 2)
        else:
            # Global threshold
            threshold = self.threshold_var.get()
            _, self.mask = cv2.threshold(gray, threshold, 255, cv2.THRESH_BINARY)
        
        # Morphological operations
        morph_size = self.morph_var.get()
        if morph_size > 0:
            kernel = cv2.getStructuringElement(cv2.MORPH_ELLIPSE, (morph_size*2+1, morph_size*2+1))
            self.mask = cv2.morphologyEx(self.mask, cv2.MORPH_CLOSE, kernel)
            self.mask = cv2.morphologyEx(self.mask, cv2.MORPH_OPEN, kernel)
        
        self.processed_image = self.mask.copy()
        self.display_image(self.mask)
    
    def update_threshold(self, value=None):
        self.threshold_label.config(text=str(self.threshold_var.get()))
        self.process_image()
    
    def update_processing(self, value=None):
        self.process_image()
    
    def change_tool(self):
        self.current_tool = self.tool_var.get()
    
    def on_mouse_down(self, event):
        if self.processed_image is None:
            return
        
        self.drawing = True
        self.last_x = event.x
        self.last_y = event.y
        
        if self.current_tool == "magic_wand":
            self.magic_wand_select(event.x, event.y)
        elif self.current_tool == "watershed":
            self.apply_watershed()
    
    def on_mouse_drag(self, event):
        if not self.drawing or self.processed_image is None:
            return
        
        if self.current_tool in ["eraser", "pencil"]:
            brush_size = self.brush_size_var.get()
            color = 0 if self.current_tool == "eraser" else 255
            
            # Draw on the mask
            cv2.line(self.mask, (self.last_x, self.last_y), (event.x, event.y), 
                    color, thickness=brush_size*2)
            cv2.circle(self.mask, (event.x, event.y), brush_size, color, -1)
            
            self.last_x = event.x
            self.last_y = event.y
            self.processed_image = self.mask.copy()
            self.display_image(self.mask)
    
    def on_mouse_up(self, event):
        self.drawing = False
    
    def magic_wand_select(self, x, y):
        if self.original_image is None:
            return
        
        gray = cv2.cvtColor(self.original_image, cv2.COLOR_RGB2GRAY)
        tolerance = self.tolerance_var.get()
        
        # Get seed point value
        seed_value = gray[y, x]
        
        # Create mask based on tolerance
        lower = max(0, seed_value - tolerance)
        upper = min(255, seed_value + tolerance)
        
        # Flood fill from seed point
        flood_mask = np.zeros((gray.shape[0]+2, gray.shape[1]+2), np.uint8)
        cv2.floodFill(gray, flood_mask, (x, y), 255, 
                     loDiff=tolerance, upDiff=tolerance, 
                     flags=cv2.FLOODFILL_MASK_ONLY | cv2.FLOODFILL_FIXED_RANGE)
        
        # Update mask
        self.mask = flood_mask[1:-1, 1:-1] * 255
        self.processed_image = self.mask.copy()
        self.display_image(self.mask)
    
    def apply_watershed(self):
        if self.original_image is None or self.mask is None:
            return
        
        # Distance transform
        dist_transform = cv2.distanceTransform(self.mask, cv2.DIST_L2, 5)
        
        # Find sure foreground
        _, sure_fg = cv2.threshold(dist_transform, 0.4*dist_transform.max(), 255, 0)
        sure_fg = np.uint8(sure_fg)
        
        # Find unknown region
        sure_bg = cv2.dilate(self.mask, None, iterations=3)
        unknown = cv2.subtract(sure_bg, sure_fg)
        
        # Marker labeling
        _, markers = cv2.connectedComponents(sure_fg)
        markers = markers + 1
        markers[unknown == 255] = 0
        
        # Apply watershed
        img_for_watershed = self.original_image.copy()
        markers = cv2.watershed(img_for_watershed, markers)
        
        # Update mask
        self.mask[markers == -1] = 0
        self.mask[markers > 1] = 255
        self.processed_image = self.mask.copy()
        self.display_image(self.mask)
    
    def fill_holes(self):
        if self.mask is None:
            return
        
        # Fill holes using morphological reconstruction
        self.mask = ndimage.binary_fill_holes(self.mask).astype(np.uint8) * 255
        self.processed_image = self.mask.copy()
        self.display_image(self.mask)
    
    def remove_small_objects(self):
        if self.mask is None:
            return
        
        # Remove small objects
        min_size = 100  # Minimum area in pixels
        labeled = measure.label(self.mask > 0)
        regions = measure.regionprops(labeled)
        
        for region in regions:
            if region.area < min_size:
                self.mask[labeled == region.label] = 0
        
        self.processed_image = self.mask.copy()
        self.display_image(self.mask)
    
    def measure(self):
        if self.mask is None:
            messagebox.showwarning("Warning", "Please process an image first")
            return
        
        # Calculate scale
        try:
            pixels = float(self.pixels_entry.get())
            mm = float(self.mm_entry.get())
            self.scale_factor = mm / pixels
        except ValueError:
            self.scale_factor = 1.0
        
        # Label connected components
        labeled = measure.label(self.mask > 0)
        regions = measure.regionprops(labeled)
        
        if len(regions) == 0:
            messagebox.showwarning("Warning", "No objects detected")
            return
        
        # Find largest region (assumed to be the pappus)
        largest_region = max(regions, key=lambda x: x.area)
        
        # Calculate measurements
        area_pixels = largest_region.area
        area_mm2 = area_pixels * (self.scale_factor ** 2)
        
        perimeter_pixels = largest_region.perimeter
        perimeter_mm = perimeter_pixels * self.scale_factor
        
        # Circularity
        circularity = (4 * np.pi * area_mm2) / (perimeter_mm ** 2) if perimeter_mm > 0 else 0
        
        # Feret diameter (maximum caliper diameter)
        coords = largest_region.coords
        from scipy.spatial.distance import pdist, squareform
        if len(coords) > 2:
            distances = squareform(pdist(coords))
            feret_diameter_pixels = np.max(distances)
            feret_diameter_mm = feret_diameter_pixels * self.scale_factor
        else:
            feret_diameter_mm = 0
        
        # Convexity
        if largest_region.convex_area > 0:
            convexity = area_pixels / largest_region.convex_area
        else:
            convexity = 0
        
        # Eccentricity
        eccentricity = largest_region.eccentricity
        
        # Solidity
        solidity = largest_region.solidity
        
        # Display measurements
        measurements = f"""
Area: {area_mm2:.2f} mm²
Perimeter: {perimeter_mm:.2f} mm
Circularity: {circularity:.3f}
Feret Diameter: {feret_diameter_mm:.2f} mm
Convexity: {convexity:.3f}
Eccentricity: {eccentricity:.3f}
Solidity: {solidity:.3f}

Centroid: ({largest_region.centroid[0]:.1f}, {largest_region.centroid[1]:.1f})
Bounding Box: {largest_region.bbox}
        """
        
        self.measurements_text.delete(1.0, tk.END)
        self.measurements_text.insert(1.0, measurements)
        
        # Store for export
        self.last_measurements = {
            'area_mm2': area_mm2,
            'perimeter_mm': perimeter_mm,
            'circularity': circularity,
            'feret_diameter_mm': feret_diameter_mm,
            'convexity': convexity,
            'eccentricity': eccentricity,
            'solidity': solidity,
            'timestamp': datetime.now().isoformat()
        }
        
        # Highlight the measured region
        display_img = cv2.cvtColor(self.mask, cv2.COLOR_GRAY2RGB)
        contours, _ = cv2.findContours(self.mask, cv2.RETR_EXTERNAL, cv2.CHAIN_APPROX_SIMPLE)
        cv2.drawContours(display_img, contours, -1, (0, 255, 0), 2)
        
        # Draw bounding box
        minr, minc, maxr, maxc = largest_region.bbox
        cv2.rectangle(display_img, (minc, minr), (maxc, maxr), (255, 0, 0), 2)
        
        self.display_image(display_img)
    
    def reset_image(self):
        if self.original_image is not None:
            self.display_image(self.original_image)
            self.mask = None
            self.processed_image = None
    
    def export_results(self):
        if not hasattr(self, 'last_measurements'):
            messagebox.showwarning("Warning", "No measurements to export")
            return
        
        file_path = filedialog.asksaveasfilename(
            defaultextension=".csv",
            filetypes=[("CSV files", "*.csv"), ("JSON files", "*.json")]
        )
        
        if file_path:
            if file_path.endswith('.json'):
                with open(file_path, 'w') as f:
                    json.dump(self.last_measurements, f, indent=2)
            else:
                df = pd.DataFrame([self.last_measurements])
                df.to_csv(file_path, index=False)
            
            messagebox.showinfo("Success", f"Results exported to {file_path}")

def main():
    root = tk.Tk()
    app = PappusAnalyzer(root)
    root.mainloop()

if __name__ == "__main__":
    main()