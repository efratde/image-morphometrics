#!/usr/bin/env python3
"""
RSML (Root System Markup Language) Viewer and Analyzer
Displays root system data from RSML files with interactive visualization
"""

import xml.etree.ElementTree as ET
import matplotlib.pyplot as plt
import numpy as np
from matplotlib.patches import Circle
import argparse
import os

class RSMLViewer:
    def __init__(self, rsml_file):
        self.tree = ET.parse(rsml_file)
        self.root = self.tree.getroot()
        self.namespace = {'rsml': 'http://www.plantontology.org/xml-dtd/po.dtd'}
        
    def extract_metadata(self):
        """Extract metadata from RSML file"""
        metadata = {}
        meta_elem = self.root.find('.//metadata')
        if meta_elem:
            for child in meta_elem:
                if child.tag not in ['property-definitions', 'image']:
                    metadata[child.tag] = child.text
        return metadata
    
    def extract_plant_data(self):
        """Extract plant and root system data"""
        plants = []
        for plant in self.root.findall('.//plant'):
            plant_data = {
                'id': plant.get('id', 'unknown'),
                'label': plant.get('label', 'unknown'),
                'roots': []
            }
            
            # Extract root data
            for root_elem in plant.findall('.//root'):
                root_data = self.extract_root_data(root_elem)
                plant_data['roots'].append(root_data)
            
            plants.append(plant_data)
        return plants
    
    def extract_root_data(self, root_elem, parent_point=None):
        """Recursively extract root and its branches"""
        root_data = {
            'id': root_elem.get('id', 'unknown'),
            'label': root_elem.get('label', 'unknown'),
            'po_accession': root_elem.get('po:accession', ''),
            'points': [],
            'properties': {},
            'branches': []
        }
        
        # Extract polyline points
        polyline = root_elem.find('.//polyline')
        if polyline is not None:
            for point in polyline.findall('.//point'):
                x = float(point.get('x', 0))
                y = float(point.get('y', 0))
                root_data['points'].append((x, y))
        
        # Extract properties
        properties = root_elem.find('.//properties')
        if properties is not None:
            for prop in properties:
                prop_name = prop.get('label', prop.tag)
                prop_value = prop.get('value', prop.text)
                root_data['properties'][prop_name] = prop_value
        
        # Extract branches (child roots)
        for child_root in root_elem.findall('.//root'):
            branch_data = self.extract_root_data(child_root, 
                                               root_data['points'][-1] if root_data['points'] else None)
            root_data['branches'].append(branch_data)
        
        return root_data
    
    def plot_root_system(self, plant_data, fig_size=(12, 10)):
        """Plot the root system"""
        fig, ax = plt.subplots(figsize=fig_size)
        
        # Plot each root
        colors = plt.cm.tab10(np.linspace(0, 1, len(plant_data['roots'])))
        
        for i, root in enumerate(plant_data['roots']):
            self.plot_root(ax, root, color=colors[i], label=f"Root {root['id']}")
        
        # Set plot properties
        ax.set_xlabel('X (inches)')
        ax.set_ylabel('Y (inches)')
        ax.set_title(f"Root System - Plant: {plant_data['label']}")
        ax.invert_yaxis()  # Invert Y axis as roots grow downward
        ax.set_aspect('equal')
        ax.grid(True, alpha=0.3)
        ax.legend()
        
        plt.tight_layout()
        return fig, ax
    
    def plot_root(self, ax, root_data, color='blue', parent_point=None, label=None):
        """Recursively plot root and its branches"""
        if not root_data['points']:
            return
        
        # Convert points to numpy array
        points = np.array(root_data['points'])
        
        # If there's a parent point, connect to it
        if parent_point is not None:
            connection = np.vstack([parent_point, points[0]])
            ax.plot(connection[:, 0], connection[:, 1], color=color, alpha=0.5, linewidth=1)
        
        # Plot the root polyline
        ax.plot(points[:, 0], points[:, 1], color=color, linewidth=2, 
                label=label if label else None)
        
        # Mark root tip
        ax.scatter(points[-1, 0], points[-1, 1], color=color, s=50, zorder=5)
        
        # Plot branches
        for branch in root_data['branches']:
            self.plot_root(ax, branch, color=color, parent_point=points[-1])
    
    def calculate_root_metrics(self, plant_data):
        """Calculate various root system metrics"""
        metrics = {
            'total_roots': len(plant_data['roots']),
            'total_length': 0,
            'max_depth': 0,
            'lateral_spread': 0,
            'root_details': []
        }
        
        all_x = []
        all_y = []
        
        for root in plant_data['roots']:
            root_metrics = self.calculate_single_root_metrics(root)
            metrics['root_details'].append(root_metrics)
            metrics['total_length'] += root_metrics['total_length']
            
            # Collect all points for spread calculation
            all_points = self.collect_all_points(root)
            if all_points:
                all_x.extend([p[0] for p in all_points])
                all_y.extend([p[1] for p in all_points])
        
        if all_y:
            metrics['max_depth'] = max(all_y) - min(all_y)
        if all_x:
            metrics['lateral_spread'] = max(all_x) - min(all_x)
        
        return metrics
    
    def calculate_single_root_metrics(self, root_data):
        """Calculate metrics for a single root including branches"""
        metrics = {
            'id': root_data['id'],
            'label': root_data['label'],
            'length': 0,
            'branch_count': len(root_data['branches']),
            'total_length': 0,
            'properties': root_data['properties']
        }
        
        # Calculate main root length
        if len(root_data['points']) > 1:
            points = np.array(root_data['points'])
            segments = points[1:] - points[:-1]
            metrics['length'] = np.sum(np.sqrt(np.sum(segments**2, axis=1)))
        
        metrics['total_length'] = metrics['length']
        
        # Add branch lengths
        for branch in root_data['branches']:
            branch_metrics = self.calculate_single_root_metrics(branch)
            metrics['total_length'] += branch_metrics['total_length']
        
        return metrics
    
    def collect_all_points(self, root_data):
        """Collect all points from root and its branches"""
        all_points = root_data['points'].copy()
        for branch in root_data['branches']:
            all_points.extend(self.collect_all_points(branch))
        return all_points
    
    def display_analysis(self, plant_data, metrics):
        """Display analysis results"""
        print(f"\n{'='*50}")
        print(f"Root System Analysis - Plant: {plant_data['label']}")
        print(f"{'='*50}")
        print(f"Total number of primary roots: {metrics['total_roots']}")
        print(f"Total root length: {metrics['total_length']:.2f} inches")
        print(f"Maximum depth: {metrics['max_depth']:.2f} inches")
        print(f"Lateral spread: {metrics['lateral_spread']:.2f} inches")
        
        print(f"\n{'Root Details:':-^50}")
        for root_detail in metrics['root_details']:
            print(f"\nRoot {root_detail['id']} ({root_detail['label']}):")
            print(f"  - Main root length: {root_detail['length']:.2f} inches")
            print(f"  - Number of branches: {root_detail['branch_count']}")
            print(f"  - Total length (with branches): {root_detail['total_length']:.2f} inches")
            if root_detail['properties']:
                print(f"  - Properties:")
                for prop, value in root_detail['properties'].items():
                    print(f"    * {prop}: {value}")


def main():
    parser = argparse.ArgumentParser(description='View and analyze RSML root system files')
    parser.add_argument('rsml_file', help='Path to RSML file')
    parser.add_argument('--no-plot', action='store_true', help='Skip plotting')
    parser.add_argument('--save-plot', help='Save plot to file')
    
    args = parser.parse_args()
    
    if not os.path.exists(args.rsml_file):
        print(f"Error: File {args.rsml_file} not found")
        return
    
    # Create viewer instance
    viewer = RSMLViewer(args.rsml_file)
    
    # Extract metadata
    metadata = viewer.extract_metadata()
    print("\nMetadata:")
    for key, value in metadata.items():
        print(f"  {key}: {value}")
    
    # Extract plant data
    plants = viewer.extract_plant_data()
    
    if not plants:
        print("No plant data found in RSML file")
        return
    
    # Analyze and plot each plant
    for plant in plants:
        # Calculate metrics
        metrics = viewer.calculate_root_metrics(plant)
        
        # Display analysis
        viewer.display_analysis(plant, metrics)
        
        # Plot if requested
        if not args.no_plot:
            fig, ax = viewer.plot_root_system(plant)
            
            if args.save_plot:
                plt.savefig(args.save_plot, dpi=300, bbox_inches='tight')
                print(f"\nPlot saved to: {args.save_plot}")
            else:
                plt.show()


if __name__ == "__main__":
    main()