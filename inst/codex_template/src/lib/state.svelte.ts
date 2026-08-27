import type { ScenarioId } from './data/scenarios';
import { scenarios } from './data/scenarios';

export const activeScenarioState = $state({
  currentId: 'everyday' as ScenarioId,
  isOpen: false
});

export const activeScriptState = $state({
  currentId: 'proposal_workflow.R'
});

export const annotationsPanelState = $state({
  isOpen: false
});

export const viewModeState = $state({
  mode: 'book' as 'book' | 'graph'
});

export const bookNavigationState = $state({
  currentSpread: 0
});

export const guideState = $state({
  enabled: false
});

export interface Annotation {
  text: string;
  timestamp: number;
}

export const annotationsState = $state<Record<string, Annotation[]>>({});

export function currentScenarioMetadata() {
  return scenarios[activeScenarioState.currentId].scriptMetadata;
}

export function currentScriptMeta() {
  return currentScenarioMetadata()[activeScriptState.currentId];
}

export function currentScenario() {
  return scenarios[activeScenarioState.currentId];
}
