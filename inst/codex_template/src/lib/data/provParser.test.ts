import { describe, it, expect } from 'vitest';
import { parseProvData } from './provParser';

describe('parseProvData', () => {
  const sampleProvData = {
    entity: {
      'rdt:environment': {
        'rdt:script': '/Users/researcher/analysis/04_botany_leaf_identification.R',
        'rdt:langVersion': 'R 4.2.2',
        'rdt:operatingSystem': 'Darwin 20.6.0',
        'rdt:architecture': 'arm64',
        'rdt:provTimestamp': '2026.08.26.12.00.00',
        'rdt:totalElapsedTime': '1.45'
      },
      'd1': {
        'rdt:name': 'specimen_data',
        'rdt:valType': 'data.frame',
        'rdt:type': 'Data',
        'rdt:value': '120 rows, 6 cols'
      },
      'd2': {
        'rdt:name': 'botany_leaf_morphology.png',
        'rdt:valType': 'image/png',
        'rdt:type': 'OutputFile',
        'rdt:hash': 'a1b2c3d4e5'
      },
      'd3': {
        'rdt:name': 'input_specimens.csv',
        'rdt:valType': 'text/csv',
        'rdt:type': 'InputFile'
      },
      'd4': {
        'rdt:name': 'species_morphometrics.rds',
        'rdt:valType': 'rds',
        'rdt:type': 'Data'
      }
    },
    activity: {
      'p1': {
        'rdt:name': 'set.seed(108)',
        'rdt:elapsedTime': '0.01',
        'rdt:startLine': '7',
        'rdt:type': 'Operation'
      },
      'p2': {
        'rdt:name': 'specimen_data <- data.frame(...)',
        'rdt:elapsedTime': '0.12',
        'rdt:startLine': '11',
        'rdt:type': 'Operation'
      },
      'p3': {
        'rdt:name': 'png("botany_leaf_morphology.png")',
        'rdt:elapsedTime': '0.25',
        'rdt:startLine': '35',
        'rdt:type': 'Operation'
      },
      'p4': {
        'rdt:name': 'saveRDS(species_morphometrics, "species_morphometrics.rds")',
        'rdt:elapsedTime': '0.05',
        'rdt:startLine': '52',
        'rdt:type': 'Operation'
      }
    }
  };

  it('correctly extracts metadata from rdt:environment', () => {
    const summary = parseProvData(sampleProvData);

    expect(summary.scriptName).toBe('04_botany_leaf_identification.R');
    expect(summary.langVersion).toBe('R 4.2.2');
    expect(summary.operatingSystem).toBe('Darwin 20.6.0');
    expect(summary.architecture).toBe('arm64');
    expect(summary.timestamp).toBe('2026:08:26:12:00:00');
    expect(summary.totalElapsedTime).toBe('1.45');
  });

  it('categorizes entities into variables, outputArtifacts, and inputFiles', () => {
    const summary = parseProvData(sampleProvData);

    // Variables
    expect(summary.variables.length).toBe(1);
    expect(summary.variables[0].name).toBe('specimen_data');
    expect(summary.variables[0].type).toBe('data.frame');

    // Output artifacts (both OutputFile type and file extension matching)
    expect(summary.outputArtifacts.length).toBe(2);
    const outputNames = summary.outputArtifacts.map(a => a.name);
    expect(outputNames).toContain('botany_leaf_morphology.png');
    expect(outputNames).toContain('species_morphometrics.rds');
    expect(summary.outputArtifacts.find(a => a.name === 'botany_leaf_morphology.png')?.hash).toBe('a1b2c3d4e5');

    // Input files
    expect(summary.inputFiles.length).toBe(1);
    expect(summary.inputFiles[0].name).toBe('input_specimens.csv');
  });

  it('correctly parses activities with line numbers and elapsed time', () => {
    const summary = parseProvData(sampleProvData);

    expect(summary.activities.length).toBe(4);
    expect(summary.operationsCount).toBe(4);
    expect(summary.activities[0]).toEqual({
      id: 'p1',
      name: 'set.seed(108)',
      elapsedTime: '0.01',
      startLine: 7,
      type: 'Operation'
    });
  });

  it('generates logical execution flow steps from operations', () => {
    const summary = parseProvData(sampleProvData);

    expect(summary.flowSteps.length).toBeGreaterThan(0);
    expect(summary.flowSteps[0].step).toBe(1);
    expect(summary.flowSteps[0].title).toBe('1. Ingestion & Setup');
    expect(summary.flowSteps[summary.flowSteps.length - 1].title).toContain('Finalization');
  });

  it('prioritizes script source name if environment script is absent', () => {
    const noEnvScript = {
      entity: {
        'rdt:environment': {}
      }
    };
    const summary = parseProvData(noEnvScript, { name: 'custom_experiment.R', code: 'x <- 1' });
    expect(summary.scriptName).toBe('custom_experiment.R');
    expect(summary.sourceCode).toBe('x <- 1');
  });

  it('gracefully handles empty, null, or undefined provenance objects without throwing', () => {
    expect(() => parseProvData(null)).not.toThrow();
    expect(() => parseProvData(undefined)).not.toThrow();
    expect(() => parseProvData({})).not.toThrow();

    const emptySummary = parseProvData({});
    expect(emptySummary.scriptName).toBe('Analysis Script');
    expect(emptySummary.operationsCount).toBe(0);
    expect(emptySummary.dataObjectsCount).toBe(0);
    expect(emptySummary.variables).toEqual([]);
    expect(emptySummary.outputArtifacts).toEqual([]);
    expect(emptySummary.inputFiles).toEqual([]);
    expect(emptySummary.flowSteps).toEqual([]);
  });

  it('handles NA startLines in activities cleanly', () => {
    const provWithNA = {
      activity: {
        'p1': {
          'rdt:name': 'library(tools)',
          'rdt:startLine': 'NA',
          'rdt:elapsedTime': '0.00',
          'rdt:type': 'Operation'
        }
      }
    };
    const summary = parseProvData(provWithNA);
    expect(summary.activities[0].startLine).toBeUndefined();
  });
});
