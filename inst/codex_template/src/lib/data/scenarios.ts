import everydayProv from './prov.json';
import scriptSource from './script_source.json';
import { scriptMetadata } from '../scriptData';

export type ScenarioId = 'everyday' | 'blank';

const activeScriptName = (scriptSource as any)?.name || 'Provenance Notebook';

export interface GuideCaption {
  title: string;
  body: string;
  lookAt?: string;   // optional hint like "Check the left page…"
}

export interface Scenario {
  id: ScenarioId;
  name: string;
  description: string;
  provData: any;
  scriptMetadata: any;
  guide: GuideCaption[];  // indexed by spread number (0 = cover)
}

export const scenarios: Record<ScenarioId, Scenario> = {
  blank: {
    id: 'blank',
    name: 'Blank Notebook',
    description: 'A completely blank canvas for thoughts, animations, and focus.',
    provData: { environment: {}, execution: {}, entities: [], operations: [] },
    scriptMetadata: {
      blank_script: {
        id: 'blank_script',
        name: 'Blank',
        icon: 'book',
        category: 'All',
        sourceCode: '# This page is intentionally left blank.\n# Engage the animation panel to draw here.',
        outputPreview: ''
      }
    },
    guide: [
      { title: 'Blank Notebook', body: 'This is a blank environment.' },
      { title: 'Empty Space', body: 'No scripts found here.' },
      { title: 'Empty Space', body: 'No code to review.' },
      { title: 'Empty Space', body: 'No lineage.' },
      { title: 'Empty Space', body: 'No data.' },
      { title: 'Empty Space', body: 'No execution timeline.' }
    ]
  },
  everyday: {
    id: 'everyday',
    name: activeScriptName,
    description: `Computational provenance and execution blueprint for ${activeScriptName}.`,
    provData: everydayProv,
    scriptMetadata: scriptMetadata,
    guide: [
      {
        title: 'Welcome to provBookR',
        body: 'This cover page shows the script that was executed, the system it ran on, and high-level stats. Think of it as the title page of a research notebook.',
        lookAt: 'Notice the operation & data object counts at the center.'
      },
      {
        title: 'How Provenance Works',
        body: 'Every script follows a flow: raw inputs → operations → output artifacts. The left page explains this pattern; the right page shows the specific scripts available for this scenario.',
        lookAt: 'The numbered flow pills on the left break down the three stages.'
      },
      {
        title: 'The Source Code',
        body: 'Here you can read the actual R script that was recorded. Each highlighted line corresponds to a provenance operation — hover to see what data it produced.',
        lookAt: 'The left page shows the full script; the right page previews the output artifact.'
      },
      {
        title: 'The Lineage Graph',
        body: 'This is the heart of provenance: a directed graph showing how each data entity was created. Circles are data nodes; diamonds are operations.',
        lookAt: 'Follow the arrows from inputs at the top to outputs at the bottom.'
      },
      {
        title: 'Data Lineage & Artifacts',
        body: 'Every entity the script produced is listed here with its type, value, and scope. File outputs include hash checksums for reproducibility.',
        lookAt: 'The left page lists all data entities; the right page shows file artifacts.'
      },
      {
        title: 'Execution Timeline',
        body: 'The final chapter shows the temporal order of operations — which step ran first, how long each took, and the overall elapsed time.',
        lookAt: 'The timeline bar on the left visualizes execution order and duration.'
      }
    ]
  }
};
