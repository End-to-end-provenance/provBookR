<script lang="ts">
  import { activeScenarioState, activeScriptState, currentScenarioMetadata } from '../state.svelte';
  import type { ScenarioId } from '../data/scenarios';
  import scriptSource from '../data/script_source.json';
  import Icon from './Icon.svelte';

  const activeLabel = (scriptSource as any)?.name ? (scriptSource as any).name : 'Notebook';

  const scenariosList = [
    { id: 'everyday', label: activeLabel, icon: 'file-text' },
    { id: 'blank', label: 'Blank', icon: 'book' }
  ];

  function switchScenario(id: ScenarioId) {
    activeScenarioState.currentId = id;
    setTimeout(() => {
      const keys = Object.keys(currentScenarioMetadata());
      if (keys.length > 0) {
        activeScriptState.currentId = keys[0];
      }
    }, 0);
  }
</script>

{#if activeScenarioState.isOpen}
  <!-- svelte-ignore a11y_click_events_have_key_events -->
  <!-- svelte-ignore a11y_no_static_element_interactions -->
  <div class="settings-backdrop" onclick={() => activeScenarioState.isOpen = false}></div>
  <div class="settings-popover">
    
    <div class="settings-section">
      <div class="settings-row">
        <span class="row-label">Choose Notebook</span>
      </div>
      <div class="preset-grid">
        {#each scenariosList as p}
          <button 
            class="preset-card" 
            class:on={activeScenarioState.currentId === p.id} 
            onclick={() => switchScenario(p.id as any)}
          >
            <Icon name={p.icon} size={20} strokeWidth={activeScenarioState.currentId === p.id ? 2.5 : 1.5} />
            <span class="preset-label" title={p.label}>{p.label}</span>
          </button>
        {/each}
      </div>
    </div>

  </div>
{/if}

<style>
  /* ── Popover Container ── */
  .settings-popover {
    position: fixed;
    bottom: 5.5rem;
    left: 2rem;
    width: 320px;
    background: var(--popover-bg, #faf9f5);
    background-color: var(--popover-bg, #faf9f5);
    border: 1px solid var(--popover-border, rgba(0, 0, 0, 0.14));
    border-radius: 20px;
    padding: 1.25rem;
    display: flex;
    flex-direction: column;
    gap: 1.25rem;
    box-shadow: var(--popover-shadow, 0 24px 60px -8px rgba(0, 0, 0, 0.25));
    z-index: 1000;
    font-family: var(--font-sans);
    animation: popIn 0.3s cubic-bezier(0.16, 1, 0.3, 1) forwards;
    transform-origin: bottom left;
  }

  @keyframes popIn {
    0% { opacity: 0; transform: scale(0.95) translateY(10px); }
    100% { opacity: 1; transform: scale(1) translateY(0); }
  }

  .settings-backdrop {
    position: fixed;
    inset: 0;
    z-index: 999;
  }

  /* ── Section & Rows ── */
  .settings-section {
    display: flex;
    flex-direction: column;
    gap: 0.8rem;
  }

  .settings-row {
    display: flex;
    align-items: center;
    justify-content: space-between;
    gap: 1rem;
  }

  .row-label {
    font-family: var(--font-sans);
    font-size: 0.75rem;
    font-weight: 600;
    text-transform: uppercase;
    letter-spacing: 0.05em;
    color: var(--text-muted, #94a3b8);
    white-space: nowrap;
    flex-shrink: 0;
  }

  /* ── Preset Grid ── */
  .preset-grid {
    display: grid;
    grid-template-columns: repeat(2, 1fr);
    gap: 0.5rem;
  }

  .preset-card {
    all: unset;
    cursor: pointer;
    display: flex;
    flex-direction: column;
    align-items: center;
    justify-content: center;
    gap: 0.5rem;
    padding: 1rem 0.5rem;
    background: var(--pill-bg, rgba(0, 0, 0, 0.04));
    border-radius: 12px;
    color: var(--text-secondary, #4a4a5a);
    transition: all 0.2s cubic-bezier(0.16, 1, 0.3, 1);
    border: 1px solid transparent;
    max-width: 100%;
    box-sizing: border-box;
  }

  .preset-card:hover {
    background: var(--glass-bg-hover, rgba(255, 255, 255, 0.5));
    color: var(--text, #1a1a24);
  }

  .preset-card.on {
    background: var(--pill-badge-bg, #1a1a24);
    color: var(--pill-badge-text, #ffffff);
    border: 1px solid var(--card-border-active, transparent);
    box-shadow: 0 4px 12px rgba(0, 0, 0, 0.1);
    transform: translateY(-2px);
  }

  .preset-label {
    font-family: var(--font-sans);
    font-size: 0.7rem;
    font-weight: 500;
    max-width: 100%;
    overflow: hidden;
    text-overflow: ellipsis;
    white-space: nowrap;
    text-align: center;
    padding: 0 0.25rem;
  }
  .preset-card.on .preset-label {
    font-weight: 600;
  }
</style>
