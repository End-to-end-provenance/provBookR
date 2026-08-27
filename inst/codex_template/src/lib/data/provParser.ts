export interface ProvSummary {
  scriptName: string;
  langVersion: string;
  operatingSystem: string;
  architecture: string;
  timestamp: string;
  totalElapsedTime: string;
  operationsCount: number;
  dataObjectsCount: number;
  variables: Array<{ name: string; type: string; value?: string }>;
  outputArtifacts: Array<{ name: string; type: string; hash?: string }>;
  inputFiles: Array<{ name: string; type: string }>;
  activities: Array<{ id: string; name: string; elapsedTime: string; startLine?: number; type: string }>;
  flowSteps: Array<{ step: number; title: string; category: string; description: string; input?: string; logic: string; output?: string }>;
  sourceCode: string;
}

export function parseProvData(provData: any, scriptSource?: { name?: string; code?: string }): ProvSummary {
  const env = provData?.entity?.['rdt:environment'] || {};
  const envScript = env?.['rdt:script'] ? env['rdt:script'].split(/[/\\]/).pop() : null;
  const scriptName = envScript || (scriptSource?.name && scriptSource.name !== 'everyday_workflow.R' ? scriptSource.name : null) || 'Analysis Script';
  
  const activitiesObj = provData?.activity || {};
  const entitiesObj = provData?.entity || {};
  
  const activities = Object.entries(activitiesObj).map(([id, act]: [string, any]) => ({
    id,
    name: act?.['rdt:name'] || '',
    elapsedTime: act?.['rdt:elapsedTime'] || '0.00',
    startLine: act?.['rdt:startLine'] !== 'NA' ? Number(act?.['rdt:startLine']) : undefined,
    type: act?.['rdt:type'] || 'Operation'
  }));

  const variables: Array<{ name: string; type: string; value?: string }> = [];
  const outputArtifacts: Array<{ name: string; type: string; hash?: string }> = [];
  const inputFiles: Array<{ name: string; type: string }> = [];

  for (const [key, ent] of Object.entries(entitiesObj) as [string, any][]) {
    if (key.includes('environment')) continue;
    const name = ent?.['rdt:name'] || key;
    const valType = ent?.['rdt:valType'] || ent?.['rdt:type'] || 'Variable';
    const type = ent?.['rdt:type'] || 'Data';
    const value = ent?.['rdt:value'] || '';

    if (type === 'InputFile') {
      inputFiles.push({ name, type: valType });
    } else if (type === 'OutputFile' || name.endsWith('.pdf') || name.endsWith('.csv') || name.endsWith('.png') || name.endsWith('.rds') || name.endsWith('.md')) {
      outputArtifacts.push({ name, type: valType, hash: ent?.['rdt:hash'] });
    } else {
      if (!variables.some(v => v.name === name)) {
        variables.push({ name, type: valType, value });
      }
    }
  }

  // Derive logical execution milestones / flow steps from activities
  const flowSteps: Array<{ step: number; title: string; category: string; description: string; input?: string; logic: string; output?: string }> = [];
  
  const operations = activities.filter(a => a.type === 'Operation' || a.name.length > 0);
  if (operations.length > 0) {
    const chunkCount = Math.min(operations.length, 4);
    const chunkSize = Math.max(1, Math.ceil(operations.length / chunkCount));
    
    for (let i = 0; i < operations.length; i += chunkSize) {
      const chunk = operations.slice(i, i + chunkSize);
      const stepNum = flowSteps.length + 1;
      const firstOp = chunk[0];
      const chunkCode = chunk.map(c => c.name).join('; ');
      
      let title = `Stage ${stepNum}: Computation`;
      if (stepNum === 1) title = `1. Ingestion & Setup`;
      else if (i + chunkSize >= operations.length) title = `${stepNum}. Output & Finalization`;
      else title = `${stepNum}. Processing & Transformations`;

      flowSteps.push({
        step: stepNum,
        title,
        category: 'Execution',
        description: `Executed ${chunk.length} statement(s) starting at line ${firstOp.startLine || 'N/A'}.`,
        input: stepNum === 1 ? (inputFiles[0]?.name || variables[0]?.name || 'Initial State') : variables[Math.min(i, variables.length - 1)]?.name,
        logic: chunk[0]?.name?.slice(0, 45) || 'Operation',
        output: stepNum === flowSteps.length && outputArtifacts[0] ? outputArtifacts[0].name : variables[Math.min(i + chunkSize, variables.length - 1)]?.name || 'Artifact State'
      });
    }
  }

  return {
    scriptName,
    langVersion: env?.['rdt:langVersion'] || 'R 4.x',
    operatingSystem: env?.['rdt:operatingSystem'] || 'POSIX/Darwin',
    architecture: env?.['rdt:architecture'] || 'arm64',
    timestamp: env?.['rdt:provTimestamp']?.replace(/\./g, ':') || new Date().toISOString().split('T')[0],
    totalElapsedTime: env?.['rdt:totalElapsedTime'] || '0.00',
    operationsCount: activities.length,
    dataObjectsCount: variables.length + outputArtifacts.length,
    variables,
    outputArtifacts,
    inputFiles,
    activities,
    flowSteps,
    sourceCode: scriptSource?.code || (operations.map(a => a.name).join('\n') || '# Source code execution trace')
  };
}
