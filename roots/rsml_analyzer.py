#!/usr/bin/env python3
"""
RSML Root System Analyzer
Extracts detailed metrics and statistics from RSML files
"""

import xml.etree.ElementTree as ET
import pandas as pd
import numpy as np
import argparse
import os
from datetime import datetime
import json

class RSMLAnalyzer:
    def __init__(self, rsml_file):
        self.rsml_file = rsml_file
        self.tree = ET.parse(rsml_file)
        self.root = self.tree.getroot()
        self.namespace = {'rsml': 'http://www.plantontology.org/xml-dtd/po.dtd'}
        
    def analyze(self):
        """Perform complete analysis of RSML file"""
        metadata = self.extract_metadata()
        plants = self.extract_all_plants()
        
        analysis_results = {
            'file': os.path.basename(self.rsml_file),
            'metadata': metadata,
            'plants': []
        }
        
        for plant in plants:
            plant_analysis = self.analyze_plant(plant)
            analysis_results['plants'].append(plant_analysis)
        
        # Calculate summary statistics
        analysis_results['summary'] = self.calculate_summary_stats(analysis_results['plants'])
        
        return analysis_results
    
    def extract_metadata(self):
        """Extract metadata from RSML file"""
        metadata = {}
        meta_elem = self.root.find('.//metadata')
        if meta_elem:
            for child in meta_elem:
                if child.tag not in ['property-definitions', 'image']:
                    metadata[child.tag] = child.text
                elif child.tag == 'image':
                    image_data = {}
                    for img_child in child:
                        image_data[img_child.tag] = img_child.text
                    metadata['image'] = image_data
        return metadata
    
    def extract_all_plants(self):
        """Extract all plant data from RSML"""
        plants = []
        for plant_elem in self.root.findall('.//plant'):
            plant_data = {
                'id': plant_elem.get('id', 'unknown'),
                'label': plant_elem.get('label', 'unknown'),
                'roots': []
            }
            
            # Extract all roots
            for root_elem in plant_elem.findall('./root'):
                root_data = self.extract_root_recursive(root_elem)
                plant_data['roots'].append(root_data)
            
            plants.append(plant_data)
        return plants
    
    def extract_root_recursive(self, root_elem, parent_id=None, depth=0):
        """Recursively extract root data including all branches"""
        root_data = {
            'id': root_elem.get('id', 'unknown'),
            'label': root_elem.get('label', 'unknown'),
            'parent_id': parent_id,
            'depth': depth,
            'po_accession': root_elem.get('po:accession', ''),
            'points': [],
            'properties': {},
            'branches': []
        }
        
        # Extract polyline points
        polyline = root_elem.find('.//polyline')
        if polyline is not None:
            for point in polyline.findall('./point'):
                x = float(point.get('x', 0))
                y = float(point.get('y', 0))
                root_data['points'].append((x, y))
        
        # Extract properties
        properties = root_elem.find('./properties')
        if properties is not None:
            for prop in properties:
                prop_label = prop.get('label', prop.tag)
                prop_value = prop.get('value', prop.text)
                try:
                    # Try to convert to float if possible
                    prop_value = float(prop_value)
                except (ValueError, TypeError):
                    pass
                root_data['properties'][prop_label] = prop_value
        
        # Extract child roots (branches)
        for child_root in root_elem.findall('./root'):
            branch_data = self.extract_root_recursive(child_root, root_data['id'], depth + 1)
            root_data['branches'].append(branch_data)
        
        return root_data
    
    def analyze_plant(self, plant_data):
        """Analyze a single plant's root system"""
        analysis = {
            'plant_id': plant_data['id'],
            'plant_label': plant_data['label'],
            'primary_roots': len(plant_data['roots']),
            'total_roots': 0,
            'total_length': 0,
            'average_root_length': 0,
            'max_depth': 0,
            'lateral_spread': 0,
            'branching_frequency': 0,
            'root_system_width_depth_ratio': 0,
            'root_details': [],
            'depth_distribution': {},
            'angle_distribution': []
        }
        
        all_points = []
        all_roots_flat = []
        
        # Analyze each primary root
        for root in plant_data['roots']:
            root_analysis = self.analyze_root_recursive(root, all_roots_flat, all_points)
            analysis['root_details'].append(root_analysis)
        
        # Calculate aggregate metrics
        analysis['total_roots'] = len(all_roots_flat)
        analysis['total_length'] = sum(r['length'] for r in all_roots_flat)
        
        if analysis['total_roots'] > 0:
            analysis['average_root_length'] = analysis['total_length'] / analysis['total_roots']
        
        if all_points:
            xs = [p[0] for p in all_points]
            ys = [p[1] for p in all_points]
            analysis['max_depth'] = max(ys) - min(ys)
            analysis['lateral_spread'] = max(xs) - min(xs)
            
            if analysis['max_depth'] > 0:
                analysis['root_system_width_depth_ratio'] = analysis['lateral_spread'] / analysis['max_depth']
        
        # Calculate branching frequency
        total_branches = sum(1 for r in all_roots_flat if r['parent_id'] is not None)
        if analysis['primary_roots'] > 0:
            analysis['branching_frequency'] = total_branches / analysis['primary_roots']
        
        # Depth distribution
        depth_counts = {}
        for root in all_roots_flat:
            depth = root['depth']
            depth_counts[depth] = depth_counts.get(depth, 0) + 1
        analysis['depth_distribution'] = depth_counts
        
        # Angle distribution (for primary roots)
        for root in plant_data['roots']:
            if len(root['points']) >= 2:
                angle = self.calculate_root_angle(root['points'][:2])
                analysis['angle_distribution'].append({
                    'root_id': root['id'],
                    'angle': angle
                })
        
        return analysis
    
    def analyze_root_recursive(self, root_data, all_roots_flat, all_points):
        """Recursively analyze root and its branches"""
        root_analysis = {
            'id': root_data['id'],
            'label': root_data['label'],
            'parent_id': root_data['parent_id'],
            'depth': root_data['depth'],
            'length': 0,
            'branch_count': len(root_data['branches']),
            'total_branch_length': 0,
            'properties': root_data['properties']
        }
        
        # Calculate root length
        if len(root_data['points']) > 1:
            points = np.array(root_data['points'])
            segments = points[1:] - points[:-1]
            root_analysis['length'] = np.sum(np.sqrt(np.sum(segments**2, axis=1)))
        
        # Add to flat list and collect points
        all_roots_flat.append(root_analysis)
        all_points.extend(root_data['points'])
        
        # Analyze branches
        branch_analyses = []
        for branch in root_data['branches']:
            branch_analysis = self.analyze_root_recursive(branch, all_roots_flat, all_points)
            branch_analyses.append(branch_analysis)
            root_analysis['total_branch_length'] += branch_analysis['length'] + branch_analysis['total_branch_length']
        
        root_analysis['branches'] = branch_analyses
        
        return root_analysis
    
    def calculate_root_angle(self, first_two_points):
        """Calculate the angle of root emergence"""
        if len(first_two_points) < 2:
            return None
        
        p1, p2 = first_two_points
        dx = p2[0] - p1[0]
        dy = p2[1] - p1[1]
        
        # Calculate angle from vertical (0 degrees is straight down)
        angle_rad = np.arctan2(dx, dy)
        angle_deg = np.degrees(angle_rad)
        
        return angle_deg
    
    def calculate_summary_stats(self, plant_analyses):
        """Calculate summary statistics across all plants"""
        if not plant_analyses:
            return {}
        
        summary = {
            'total_plants': len(plant_analyses),
            'avg_primary_roots_per_plant': np.mean([p['primary_roots'] for p in plant_analyses]),
            'avg_total_roots_per_plant': np.mean([p['total_roots'] for p in plant_analyses]),
            'avg_total_length': np.mean([p['total_length'] for p in plant_analyses]),
            'avg_max_depth': np.mean([p['max_depth'] for p in plant_analyses]),
            'avg_lateral_spread': np.mean([p['lateral_spread'] for p in plant_analyses]),
            'avg_branching_frequency': np.mean([p['branching_frequency'] for p in plant_analyses]),
            'avg_width_depth_ratio': np.mean([p['root_system_width_depth_ratio'] for p in plant_analyses])
        }
        
        return summary
    
    def export_to_csv(self, analysis_results, output_file):
        """Export analysis results to CSV"""
        rows = []
        
        for plant in analysis_results['plants']:
            row = {
                'file': analysis_results['file'],
                'plant_id': plant['plant_id'],
                'plant_label': plant['plant_label'],
                'primary_roots': plant['primary_roots'],
                'total_roots': plant['total_roots'],
                'total_length': plant['total_length'],
                'average_root_length': plant['average_root_length'],
                'max_depth': plant['max_depth'],
                'lateral_spread': plant['lateral_spread'],
                'branching_frequency': plant['branching_frequency'],
                'width_depth_ratio': plant['root_system_width_depth_ratio']
            }
            
            # Add metadata
            for key, value in analysis_results['metadata'].items():
                if isinstance(value, dict):
                    for sub_key, sub_value in value.items():
                        row[f'metadata_{key}_{sub_key}'] = sub_value
                else:
                    row[f'metadata_{key}'] = value
            
            rows.append(row)
        
        df = pd.DataFrame(rows)
        df.to_csv(output_file, index=False)
        
        return df
    
    def export_detailed_json(self, analysis_results, output_file):
        """Export detailed analysis results to JSON"""
        with open(output_file, 'w') as f:
            json.dump(analysis_results, f, indent=2)


def main():
    parser = argparse.ArgumentParser(description='Analyze RSML root system files')
    parser.add_argument('rsml_file', help='Path to RSML file')
    parser.add_argument('--csv', help='Export results to CSV file')
    parser.add_argument('--json', help='Export detailed results to JSON file')
    parser.add_argument('--quiet', action='store_true', help='Suppress console output')
    
    args = parser.parse_args()
    
    if not os.path.exists(args.rsml_file):
        print(f"Error: File {args.rsml_file} not found")
        return
    
    # Create analyzer and run analysis
    analyzer = RSMLAnalyzer(args.rsml_file)
    results = analyzer.analyze()
    
    # Display results
    if not args.quiet:
        print(f"\n{'='*60}")
        print(f"RSML Analysis Report: {results['file']}")
        print(f"{'='*60}")
        
        print("\nMetadata:")
        for key, value in results['metadata'].items():
            if isinstance(value, dict):
                print(f"  {key}:")
                for sub_key, sub_value in value.items():
                    print(f"    - {sub_key}: {sub_value}")
            else:
                print(f"  {key}: {value}")
        
        print(f"\nPlants analyzed: {len(results['plants'])}")
        
        for plant in results['plants']:
            print(f"\n{'-'*50}")
            print(f"Plant: {plant['plant_label']} (ID: {plant['plant_id']})")
            print(f"  Primary roots: {plant['primary_roots']}")
            print(f"  Total roots (including branches): {plant['total_roots']}")
            print(f"  Total length: {plant['total_length']:.2f} inches")
            print(f"  Average root length: {plant['average_root_length']:.2f} inches")
            print(f"  Maximum depth: {plant['max_depth']:.2f} inches")
            print(f"  Lateral spread: {plant['lateral_spread']:.2f} inches")
            print(f"  Width/Depth ratio: {plant['root_system_width_depth_ratio']:.2f}")
            print(f"  Branching frequency: {plant['branching_frequency']:.2f} branches/primary root")
            
            if plant['depth_distribution']:
                print(f"  Depth distribution:")
                for depth, count in sorted(plant['depth_distribution'].items()):
                    print(f"    Level {depth}: {count} roots")
        
        if 'summary' in results and results['summary']:
            print(f"\n{'Summary Statistics':=^50}")
            for key, value in results['summary'].items():
                formatted_key = key.replace('_', ' ').title()
                print(f"  {formatted_key}: {value:.2f}")
    
    # Export results if requested
    if args.csv:
        df = analyzer.export_to_csv(results, args.csv)
        print(f"\nResults exported to CSV: {args.csv}")
        print(f"Rows exported: {len(df)}")
    
    if args.json:
        analyzer.export_detailed_json(results, args.json)
        print(f"\nDetailed results exported to JSON: {args.json}")


if __name__ == "__main__":
    main()