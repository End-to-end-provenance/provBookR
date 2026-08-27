<script lang="ts">
  import { themeState } from '../themeState.svelte';
  import { annotationsPanelState, viewModeState, annotationsState, currentScriptMeta, guideState } from '../state.svelte';
  import Icon from './Icon.svelte';

  let { leftControls = undefined, rightControls = undefined } = $props<{
    leftControls?: import('svelte').Snippet;
    rightControls?: import('svelte').Snippet;
  }>();

  const activeMeta = $derived(currentScriptMeta());
  const currentComments = $derived.by(() => {
    if (!activeMeta) return [];
    return annotationsState[activeMeta.id] || [];
  });
</script>

<div class="dock-container">
  <div class="dock-pill">
    {#if leftControls}
      {@render leftControls()}
      <div class="divider"></div>
    {/if}

    <!-- View Mode Toggle -->
    <button 
      class="dock-btn"
      class:active={viewModeState.mode === 'graph'}
      onclick={() => viewModeState.mode = viewModeState.mode === 'book' ? 'graph' : 'book'}
      aria-label="Toggle Graph View"
      title="Graph View"
    >
      <Icon name="grid" size={18} />
    </button>

    <!-- Guide Toggle -->
    <button 
      class="dock-btn"
      class:active={guideState.enabled}
      onclick={() => guideState.enabled = !guideState.enabled}
      aria-label="Toggle Guide"
      title={guideState.enabled ? 'Hide Guide' : 'Show Guide'}
    >
      <Icon name="info" size={18} />
    </button>

    <!-- Annotations Toggle -->
    <button 
      class="dock-btn"
      class:active={annotationsPanelState.isOpen}
      onclick={() => annotationsPanelState.isOpen = !annotationsPanelState.isOpen}
      aria-label="Annotations"
      title="Annotations"
    >
      <Icon name="edit-3" size={18} />
      {#if currentComments.length > 0}
        <span class="badge">{currentComments.length}</span>
      {/if}
    </button>

    <!-- Theme/Typography Toggle -->
    <button 
      class="dock-btn"
      class:active={themeState.isOpen}
      onclick={() => themeState.isOpen = !themeState.isOpen}
      aria-label="Typography Settings"
      title="Typography & Theme"
    >
      <Icon name="type" size={18} />
    </button>

    {#if rightControls}
      <div class="divider"></div>
      {@render rightControls()}
    {/if}
  </div>
</div>

<style>
  .dock-container {
    position: fixed;
    bottom: calc((5rem - 38px) / 2);
    left: 50%;
    transform: translateX(-50%);
    z-index: 1000;
  }

  .dock-pill {
    display: flex;
    align-items: center;
    gap: 0.35rem;
    padding: 0.25rem 0.45rem;
    background: var(--glass-bg, rgba(255, 255, 255, 0.75));
    backdrop-filter: blur(16px);
    -webkit-backdrop-filter: blur(16px);
    border: 1px solid var(--border, rgba(0, 0, 0, 0.1));
    border-radius: 9999px;
    box-shadow: var(--glass-shadow);
  }

  :global(.dock-btn) {
    all: unset;
    cursor: pointer;
    position: relative;
    width: 30px;
    height: 30px;
    border-radius: 50%;
    display: flex;
    align-items: center;
    justify-content: center;
    color: var(--text-secondary, #4a4a5a);
    transition: all 0.2s cubic-bezier(0.16, 1, 0.3, 1);
  }
  :global(.dock-btn:hover) {
    background: var(--pill-bg, rgba(0, 0, 0, 0.06));
    color: var(--text, #1a1a24);
  }
  :global(.dock-btn.active) {
    background: var(--pill-badge-bg, #1a1a24);
    color: var(--pill-badge-text, #ffffff);
    box-shadow: 0 3px 10px rgba(0, 0, 0, 0.3);
  }
  :global(.dock-btn:disabled) {
    opacity: 0.3;
    cursor: not-allowed;
  }
  :global(.dock-btn:disabled:hover) {
    background: transparent;
    color: var(--text-secondary, #4a4a5a);
  }

  :global(.divider) {
    width: 1px;
    height: 16px;
    background: var(--border, rgba(0, 0, 0, 0.15));
    margin: 0 0.15rem;
  }

  :global(.badge) {
    position: absolute;
    top: -2px;
    right: -2px;
    background: var(--text, #111);
    color: var(--bg, #fff);
    font-size: 0.55rem;
    font-weight: 700;
    font-family: var(--font-sans);
    width: 14px;
    height: 14px;
    display: flex;
    align-items: center;
    justify-content: center;
    border-radius: 50%;
    box-shadow: 0 2px 4px rgba(0,0,0,0.2);
  }
  :global(.dock-btn.active) :global(.badge) {
    border: 2px solid var(--pill-badge-bg, #1a1a24);
  }

  :global(.page-indicator) {
    all: unset;
    cursor: pointer;
    box-sizing: border-box;
    font-family: var(--font-sans);
    font-size: 0.68rem;
    font-weight: 500;
    line-height: 1;
    color: var(--text-secondary, #4a4a5a);
    background: var(--pill-bg, rgba(0, 0, 0, 0.05));
    padding: 0 0.65rem;
    height: 30px;
    border-radius: 9999px;
    display: inline-flex;
    align-items: center;
    justify-content: center;
    user-select: none;
    margin: 0 0.1rem 0 0.45rem;
    transform: translateY(1.5px);
    border: none;
    outline: none;
    transition: all 0.18s ease;
  }
  :global(.page-indicator:hover) {
    background: var(--pill-bg, rgba(0, 0, 0, 0.12));
    color: var(--text, #1a1a24);
  }
</style>
