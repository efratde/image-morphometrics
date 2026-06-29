#!/usr/bin/env python3
"""
RSML Batch Processor
Process multiple RSML files and generate comparative analysis
"""

import os
import glob
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
from rsml_analyzer import RSMLAnalyzer
from rsml_viewer import RSMLViewer
import argparse
import json
from datetime import datetime
import numpy as np

class RSMLBatchProcessor:
    def __init__(self, input_pattern):
        self.input_pattern = input_pattern
        self.results = []
        
    def process_files(self):
        """Process all RSML files matching the pattern"""
        # Find all RSML files
        if os.path.isdir(self.input_pattern):
            rsml_files = glob.glob(os.path.join(self.input_pattern, "*.rsml"))
        else:
            rsml_files = glob.glob(self.input_pattern)
        
        if not rsml_files:
            print(f"No RSML files found matching pattern: {self.input_pattern}")
            return
        
        print(f"Found {len(rsml_files)} RSML files to process")
        
        # Process each file
        for i, rsml_file in enumerate(rsml_files, 1):
            print(f"\nProcessing [{i}/{len(rsml_files)}]: {os.path.basename(rsml_file)}")
            try:
                analyzer = RSMLAnalyzer(rsml_file)
                analysis = analyzer.analyze()
                self.results.append(analysis)
            except Exception as e:
                print(f"  Error processing {rsml_file}: {str(e)}")
        
        print(f"\nSuccessfully processed {len(self.results)} files")
        
    def create_comparison_dataframe(self):
        """Create a DataFrame for comparative analysis"""
        rows = []
        
        for result in self.results:
            for plant in result['plants']:
                row = {
                    'file': result['file'],
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
                
                # Extract sample/treatment info from filename if possible
                filename_parts = result['file'].replace('.rsml', '').split('_')
                if len(filename_parts) >= 2:
                    row['sample_id'] = filename_parts[0]
                    row['replicate'] = filename_parts[1]
                
                rows.append(row)
        
        return pd.DataFrame(rows)
    
    def generate_summary_statistics(self, df):
        """Generate summary statistics across all samples"""
        summary_stats = {
            'total_files': len(self.results),
            'total_plants': len(df),
            'metrics': {}
        }
        
        # Calculate statistics for each metric
        metrics = ['primary_roots', 'total_roots', 'total_length', 'average_root_length',
                  'max_depth', 'lateral_spread', 'branching_frequency', 'width_depth_ratio']
        
        for metric in metrics:
            summary_stats['metrics'][metric] = {
                'mean': df[metric].mean(),
                'std': df[metric].std(),
                'min': df[metric].min(),
                'max': df[metric].max(),
                'median': df[metric].median()
            }
        
        return summary_stats
    
    def create_visualizations(self, df, output_dir):
        """Create comparative visualizations"""
        os.makedirs(output_dir, exist_ok=True)
        
        # Set style
        sns.set_style("whitegrid")
        
        # 1. Distribution plots for key metrics
        fig, axes = plt.subplots(2, 4, figsize=(16, 8))
        axes = axes.flatten()
        
        metrics = ['primary_roots', 'total_roots', 'total_length', 'average_root_length',
                  'max_depth', 'lateral_spread', 'branching_frequency', 'width_depth_ratio']
        
        for i, metric in enumerate(metrics):
            sns.histplot(data=df, x=metric, kde=True, ax=axes[i])
            axes[i].set_title(metric.replace('_', ' ').title())
        
        plt.tight_layout()
        plt.savefig(os.path.join(output_dir, 'metric_distributions.png'), dpi=300)
        plt.close()
        
        # 2. Correlation heatmap
        plt.figure(figsize=(10, 8))
        correlation_matrix = df[metrics].corr()
        sns.heatmap(correlation_matrix, annot=True, cmap='coolwarm', center=0,
                   square=True, linewidths=1, cbar_kws={"shrink": 0.8})
        plt.title('Correlation Matrix of Root System Metrics')
        plt.tight_layout()
        plt.savefig(os.path.join(output_dir, 'correlation_matrix.png'), dpi=300)
        plt.close()
        
        # 3. Box plots by sample (if sample_id exists)
        if 'sample_id' in df.columns:
            fig, axes = plt.subplots(2, 2, figsize=(12, 10))
            axes = axes.flatten()
            
            key_metrics = ['total_length', 'max_depth', 'lateral_spread', 'branching_frequency']
            
            for i, metric in enumerate(key_metrics):
                sns.boxplot(data=df, x='sample_id', y=metric, ax=axes[i])
                axes[i].set_title(metric.replace('_', ' ').title())
                axes[i].tick_params(axis='x', rotation=45)
            
            plt.tight_layout()
            plt.savefig(os.path.join(output_dir, 'metrics_by_sample.png'), dpi=300)
            plt.close()
        
        # 4. Scatter plot matrix for key relationships
        key_metrics = ['total_length', 'max_depth', 'lateral_spread', 'branching_frequency']
        scatter_df = df[key_metrics]
        
        g = sns.pairplot(scatter_df, diag_kind='kde', plot_kws={'alpha': 0.6})
        g.fig.suptitle('Root System Metrics Relationships', y=1.02)
        plt.tight_layout()
        plt.savefig(os.path.join(output_dir, 'scatter_matrix.png'), dpi=300)
        plt.close()
        
        print(f"Visualizations saved to: {output_dir}")
    
    def export_results(self, output_prefix):
        """Export all results to various formats"""
        # Create comparison dataframe
        df = self.create_comparison_dataframe()
        
        # Export to CSV
        csv_file = f"{output_prefix}_comparison.csv"
        df.to_csv(csv_file, index=False)
        print(f"Comparison data exported to: {csv_file}")
        
        # Export detailed JSON
        json_file = f"{output_prefix}_detailed.json"
        with open(json_file, 'w') as f:
            json.dump(self.results, f, indent=2)
        print(f"Detailed results exported to: {json_file}")
        
        # Generate and export summary statistics
        summary_stats = self.generate_summary_statistics(df)
        summary_file = f"{output_prefix}_summary.json"
        with open(summary_file, 'w') as f:
            json.dump(summary_stats, f, indent=2)
        print(f"Summary statistics exported to: {summary_file}")
        
        # Create visualizations
        viz_dir = f"{output_prefix}_visualizations"
        self.create_visualizations(df, viz_dir)
        
        return df, summary_stats
    
    def generate_report(self, output_file="rsml_batch_report.txt"):
        """Generate a comprehensive text report"""
        df = self.create_comparison_dataframe()
        summary_stats = self.generate_summary_statistics(df)
        
        with open(output_file, 'w') as f:
            f.write("="*80 + "\n")
            f.write("RSML Batch Processing Report\n")
            f.write(f"Generated: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}\n")
            f.write("="*80 + "\n\n")
            
            f.write(f"Total files processed: {summary_stats['total_files']}\n")
            f.write(f"Total plants analyzed: {summary_stats['total_plants']}\n\n")
            
            f.write("Summary Statistics Across All Samples:\n")
            f.write("-"*50 + "\n")
            
            for metric, stats in summary_stats['metrics'].items():
                f.write(f"\n{metric.replace('_', ' ').title()}:\n")
                f.write(f"  Mean ± SD: {stats['mean']:.2f} ± {stats['std']:.2f}\n")
                f.write(f"  Median: {stats['median']:.2f}\n")
                f.write(f"  Range: {stats['min']:.2f} - {stats['max']:.2f}\n")
            
            # Per-file summary
            f.write("\n\nPer-File Summary:\n")
            f.write("-"*50 + "\n")
            
            for result in self.results:
                f.write(f"\nFile: {result['file']}\n")
                if result['plants']:
                    total_length = sum(p['total_length'] for p in result['plants'])
                    avg_depth = np.mean([p['max_depth'] for p in result['plants']])
                    f.write(f"  Plants: {len(result['plants'])}\n")
                    f.write(f"  Total root length: {total_length:.2f} inches\n")
                    f.write(f"  Average max depth: {avg_depth:.2f} inches\n")
        
        print(f"\nReport generated: {output_file}")


def main():
    parser = argparse.ArgumentParser(description='Batch process multiple RSML files')
    parser.add_argument('input', help='Input directory or file pattern (e.g., "*.rsml" or "./data/")')
    parser.add_argument('--output-prefix', default='rsml_batch', help='Prefix for output files')
    parser.add_argument('--no-viz', action='store_true', help='Skip visualization generation')
    
    args = parser.parse_args()
    
    # Create processor and run
    processor = RSMLBatchProcessor(args.input)
    processor.process_files()
    
    if not processor.results:
        print("No files were successfully processed")
        return
    
    # Export results
    df, summary_stats = processor.export_results(args.output_prefix)
    
    # Generate report
    processor.generate_report(f"{args.output_prefix}_report.txt")
    
    # Display summary
    print("\n" + "="*50)
    print("Batch Processing Complete")
    print("="*50)
    print(f"Files processed: {len(processor.results)}")
    print(f"Total plants analyzed: {len(df)}")
    print(f"\nAverage metrics across all plants:")
    for metric in ['total_length', 'max_depth', 'lateral_spread', 'branching_frequency']:
        print(f"  {metric.replace('_', ' ').title()}: {df[metric].mean():.2f} ± {df[metric].std():.2f}")


if __name__ == "__main__":
    main()