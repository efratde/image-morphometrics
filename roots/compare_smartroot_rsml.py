#!/usr/bin/env python3
"""
Compare SmartRoot CSV exports with RSML analysis results
"""

import pandas as pd
import numpy as np
from rsml_analyzer import RSMLAnalyzer
import argparse
import os

def load_smartroot_data(csv_file):
    """Load and summarize SmartRoot CSV data"""
    df = pd.read_csv(csv_file)
    
    # Clean column names
    df.columns = df.columns.str.strip()
    
    # Calculate metrics per plant
    summary = {
        'file': os.path.basename(csv_file),
        'total_roots': len(df),
        'primary_roots': len(df[df['root_order'] == 0]),
        'lateral_roots': len(df[df['root_order'] > 0]),
        'total_length': df['length'].sum(),
        'avg_length': df['length'].mean(),
        'avg_diameter': df['diameter'].mean(),
        'total_surface': df['surface'].sum(),
        'total_volume': df['volume'].sum(),
        'root_details': []
    }
    
    # Get primary root details
    primary_roots = df[df['root_order'] == 0]
    for _, root in primary_roots.iterrows():
        root_info = {
            'name': root['root_name'],
            'length': root['length'],
            'diameter': root['diameter'],
            'n_children': root['n_child'],
            'child_density': root['child_density']
        }
        summary['root_details'].append(root_info)
    
    # Calculate branching metrics
    roots_with_children = df[df['n_child'] > 0]
    if len(roots_with_children) > 0:
        summary['avg_branching'] = roots_with_children['n_child'].mean()
        summary['avg_child_density'] = roots_with_children['child_density'].mean()
    else:
        summary['avg_branching'] = 0
        summary['avg_child_density'] = 0
    
    return summary, df

def compare_results(smartroot_summary, rsml_analysis):
    """Compare SmartRoot and RSML analysis results"""
    comparison = {
        'metrics': {},
        'differences': {}
    }
    
    # Extract RSML summary (assuming single plant)
    if rsml_analysis['plants']:
        rsml_plant = rsml_analysis['plants'][0]
        
        # Compare common metrics
        metrics_map = [
            ('total_roots', 'total_roots', 'Total Roots'),
            ('primary_roots', 'primary_roots', 'Primary Roots'),
            ('total_length', 'total_length', 'Total Length (inches)')
        ]
        
        for sr_key, rsml_key, label in metrics_map:
            sr_value = smartroot_summary.get(sr_key, 0)
            rsml_value = rsml_plant.get(rsml_key, 0)
            
            comparison['metrics'][label] = {
                'smartroot': sr_value,
                'rsml': rsml_value,
                'difference': rsml_value - sr_value,
                'percent_diff': ((rsml_value - sr_value) / sr_value * 100) if sr_value > 0 else 0
            }
    
    return comparison

def generate_comparison_report(csv_file, rsml_file, output_file=None):
    """Generate a detailed comparison report"""
    # Load SmartRoot data
    sr_summary, sr_df = load_smartroot_data(csv_file)
    
    # Analyze RSML file
    analyzer = RSMLAnalyzer(rsml_file)
    rsml_analysis = analyzer.analyze()
    
    # Compare results
    comparison = compare_results(sr_summary, rsml_analysis)
    
    # Generate report
    report_lines = []
    report_lines.append("="*60)
    report_lines.append("SmartRoot vs RSML Analysis Comparison")
    report_lines.append("="*60)
    report_lines.append(f"SmartRoot file: {csv_file}")
    report_lines.append(f"RSML file: {rsml_file}")
    report_lines.append("")
    
    # SmartRoot Summary
    report_lines.append("SmartRoot Summary:")
    report_lines.append("-"*30)
    report_lines.append(f"Total roots: {sr_summary['total_roots']}")
    report_lines.append(f"Primary roots: {sr_summary['primary_roots']}")
    report_lines.append(f"Lateral roots: {sr_summary['lateral_roots']}")
    report_lines.append(f"Total length: {sr_summary['total_length']:.2f} inches")
    report_lines.append(f"Average length: {sr_summary['avg_length']:.2f} inches")
    report_lines.append(f"Average diameter: {sr_summary['avg_diameter']:.4f} inches")
    report_lines.append(f"Total surface area: {sr_summary['total_surface']:.2f}")
    report_lines.append(f"Total volume: {sr_summary['total_volume']:.4f}")
    report_lines.append("")
    
    # RSML Summary
    if rsml_analysis['plants']:
        plant = rsml_analysis['plants'][0]
        report_lines.append("RSML Analysis Summary:")
        report_lines.append("-"*30)
        report_lines.append(f"Total roots: {plant['total_roots']}")
        report_lines.append(f"Primary roots: {plant['primary_roots']}")
        report_lines.append(f"Total length: {plant['total_length']:.2f} inches")
        report_lines.append(f"Average length: {plant['average_root_length']:.2f} inches")
        report_lines.append(f"Max depth: {plant['max_depth']:.2f} inches")
        report_lines.append(f"Lateral spread: {plant['lateral_spread']:.2f} inches")
        report_lines.append(f"Branching frequency: {plant['branching_frequency']:.2f}")
        report_lines.append("")
    
    # Comparison
    report_lines.append("Metric Comparison:")
    report_lines.append("-"*30)
    report_lines.append(f"{'Metric':<20} {'SmartRoot':>12} {'RSML':>12} {'Diff':>10} {'%Diff':>8}")
    report_lines.append("-"*62)
    
    for metric, values in comparison['metrics'].items():
        report_lines.append(
            f"{metric:<20} {values['smartroot']:>12.2f} {values['rsml']:>12.2f} "
            f"{values['difference']:>10.2f} {values['percent_diff']:>7.1f}%"
        )
    
    # Root-by-root comparison
    report_lines.append("")
    report_lines.append("Primary Root Details:")
    report_lines.append("-"*30)
    
    # SmartRoot primary roots
    report_lines.append("SmartRoot primary roots:")
    for root in sr_summary['root_details']:
        report_lines.append(f"  {root['name']}: length={root['length']:.2f}, "
                          f"children={root['n_children']}, "
                          f"child_density={root['child_density']:.2f}")
    
    # RSML primary roots
    if rsml_analysis['plants'] and rsml_analysis['plants'][0]['root_details']:
        report_lines.append("\nRSML primary roots:")
        for root in rsml_analysis['plants'][0]['root_details']:
            if root['depth'] == 0:  # Primary roots
                report_lines.append(f"  {root['label']}: length={root['length']:.2f}, "
                                  f"branches={root['branch_count']}")
    
    # Additional SmartRoot metrics
    report_lines.append("")
    report_lines.append("Additional SmartRoot Metrics:")
    report_lines.append("-"*30)
    report_lines.append(f"Average branching: {sr_summary.get('avg_branching', 0):.2f}")
    report_lines.append(f"Average child density: {sr_summary.get('avg_child_density', 0):.2f}")
    
    # Root order distribution
    root_orders = sr_df['root_order'].value_counts().sort_index()
    report_lines.append("\nRoot order distribution:")
    for order, count in root_orders.items():
        report_lines.append(f"  Order {order}: {count} roots")
    
    # Output report
    report_text = "\n".join(report_lines)
    
    if output_file:
        with open(output_file, 'w') as f:
            f.write(report_text)
        print(f"Report saved to: {output_file}")
    else:
        print(report_text)
    
    return sr_summary, rsml_analysis, comparison

def batch_compare(pattern="*.rsml"):
    """Compare all matching RSML files with their corresponding CSV files"""
    import glob
    
    rsml_files = glob.glob(pattern)
    results = []
    
    for rsml_file in rsml_files:
        # Find corresponding CSV file
        base_name = os.path.basename(rsml_file).replace('w', 'r').replace('.rsml', '.csv')
        csv_file = os.path.join(os.path.dirname(rsml_file), base_name)
        
        if os.path.exists(csv_file):
            print(f"\nComparing {rsml_file} with {csv_file}")
            try:
                sr_summary, rsml_analysis, comparison = generate_comparison_report(
                    csv_file, rsml_file, 
                    output_file=rsml_file.replace('.rsml', '_comparison.txt')
                )
                results.append({
                    'rsml': rsml_file,
                    'csv': csv_file,
                    'comparison': comparison
                })
            except Exception as e:
                print(f"Error comparing files: {e}")
        else:
            print(f"No matching CSV file found for {rsml_file}")
    
    return results

def main():
    parser = argparse.ArgumentParser(description='Compare SmartRoot CSV with RSML analysis')
    parser.add_argument('--csv', help='SmartRoot CSV file')
    parser.add_argument('--rsml', help='RSML file')
    parser.add_argument('--batch', action='store_true', help='Compare all RSML files with matching CSV files')
    parser.add_argument('--output', help='Output file for comparison report')
    
    args = parser.parse_args()
    
    if args.batch:
        results = batch_compare()
        print(f"\nCompleted {len(results)} comparisons")
    elif args.csv and args.rsml:
        generate_comparison_report(args.csv, args.rsml, args.output)
    else:
        # Auto-match files in current directory
        import glob
        rsml_files = glob.glob("*.rsml")
        
        if rsml_files:
            print("Found RSML files. Running automatic comparison...")
            results = batch_compare()
        else:
            print("No RSML files found. Please specify --csv and --rsml files.")

if __name__ == "__main__":
    main()