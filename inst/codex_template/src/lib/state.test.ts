import { describe, it, expect } from 'vitest';
import { 
  activeScenarioState, 
  activeScriptState, 
  viewModeState, 
  bookNavigationState, 
  annotationsState,
  currentScenario,
  currentScenarioMetadata,
  currentScriptMeta
} from './state.svelte';
import { scenarios } from './data/scenarios';

describe('Application State & Scenario Management', () => {
  it('initializes with the default everyday scenario', () => {
    expect(activeScenarioState.currentId).toBe('everyday');
    expect(viewModeState.mode).toBe('book');
    expect(bookNavigationState.currentSpread).toBe(0);
  });

  it('retrieves the active scenario configuration correctly', () => {
    const scenario = currentScenario();
    expect(scenario.id).toBe('everyday');
    expect(scenario.guide.length).toBeGreaterThan(0);
  });

  it('allows adding and retrieving annotations across pages', () => {
    const pageKey = 'spread-2-left';
    annotationsState[pageKey] = [
      { text: 'Verify this vector multiplication step', timestamp: Date.now() }
    ];

    expect(annotationsState[pageKey].length).toBe(1);
    expect(annotationsState[pageKey][0].text).toBe('Verify this vector multiplication step');
  });

  it('handles switching to the blank scenario cleanly', () => {
    activeScenarioState.currentId = 'blank';
    expect(currentScenario().id).toBe('blank');
    expect(currentScenario().description).toContain('blank canvas');

    // Reset back
    activeScenarioState.currentId = 'everyday';
  });

  it('maintains scenario metadata consistency', () => {
    const metadata = currentScenarioMetadata();
    expect(metadata).toBeDefined();
  });
});
