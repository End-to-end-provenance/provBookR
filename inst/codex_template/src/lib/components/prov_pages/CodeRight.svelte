<script lang="ts">
  import { activeScriptState, currentScriptMeta, currentScenarioMetadata } from '../../state.svelte';
  import { parseProvData } from '../../data/provParser';
  import scriptSource from '../../data/script_source.json';
  import Icon from '../Icon.svelte';
  import Prism from 'prismjs';
  import 'prismjs/components/prism-r';

  import type { ScriptMeta } from '../../scriptData';

  let { provData } = $props<{ provData?: any }>();

  const parsed = $derived(provData ? parseProvData(provData, scriptSource) : null);
  const activeMeta = $derived(currentScriptMeta());

  const scripts = $derived(
    parsed
      ? [{ id: parsed.scriptName, name: parsed.scriptName, icon: 'document' }]
      : (Object.values(currentScenarioMetadata()) as ScriptMeta[])
  );

  const displayMeta = $derived(
    parsed
      ? {
          id: parsed.scriptName,
          name: parsed.scriptName,
          category: 'Analysis Script',
          icon: 'document',
          variables: parsed.variables.map(v => v.name),
          outputFile: parsed.outputArtifacts.map(a => a.name).join(', ') || 'In-memory workspace',
          code: parsed.sourceCode
        }
      : activeMeta
  );

  const highlightedCode = $derived(
    displayMeta ? Prism.highlight(displayMeta.code, Prism.languages.r, 'r') : ''
  );
</script>

<div class="code-right-container">
  <!-- Tab Bar -->
  <div class="tab-bar">
    {#each scripts as sc}
      <button 
        class="tab-pill" 
        class:active={activeScriptState.currentId === sc.id}
        onclick={() => activeScriptState.currentId = sc.id}
      >
        <Icon name={sc.icon} size={14} />
        <span class="tab-label">{sc.name}</span>
      </button>
    {/each}
  </div>

  <!-- E-Ink Paper View -->
  <div class="eink-paper">
    {#if displayMeta}
      <div class="paper-header">
        <div class="header-left">
          <span class="file-tag">R Source</span>
          <span class="file-title">{displayMeta.name}</span>
        </div>
        <span class="file-cat">{displayMeta.category}</span>
      </div>

      <!-- Quick Flow Bar -->
      <div class="flow-bar">
        <span class="flow-item">Variables: <code>{displayMeta.variables.slice(0, 4).join(', ')}{displayMeta.variables.length > 4 ? '...' : ''}</code></span>
        <span class="flow-sep">→</span>
        <span class="flow-item">Output: <code>{displayMeta.outputFile}</code></span>
      </div>

      <!-- Source Code Display -->
      <div class="code-scroll">
        <pre class="eink-code"><code class="language-r"><!-- eslint-disable-next-line svelte/no-at-html-tags -->
{@html highlightedCode}</code></pre>
      </div>
    {:else}
      <p class="empty-msg">No script selected.</p>
    {/if}
  </div>
</div>

<style>
  .code-right-container {
    display: flex;
    flex-direction: column;
    height: 100%;
    gap: 0.7rem;
  }

  .tab-bar {
    display: flex;
    gap: 0.45rem;
    overflow-x: auto;
    padding: 0.2rem 0.1rem;
  }

  .tab-pill {
    all: unset;
    cursor: pointer;
    display: flex;
    align-items: center;
    gap: 0.4rem;
    padding: 0.4rem 0.85rem;
    background: var(--pill-bg);
    backdrop-filter: blur(12px);
    -webkit-backdrop-filter: blur(12px);
    border: 1px solid var(--border);
    border-radius: 9999px;
    font-family: var(--font-mono, monospace);
    font-size: 0.72rem;
    color: var(--text-muted);
    transition: all 0.2s cubic-bezier(0.16, 1, 0.3, 1);
    box-shadow: 0 2px 8px rgba(0, 0, 0, 0.02);
  }

  .tab-pill:hover {
    background: var(--glass-bg-hover);
    color: var(--text);
    transform: translateY(-1px);
  }

  .tab-pill.active {
    background: var(--card-bg-active);
    border-color: var(--card-border-active);
    color: var(--text);
    font-weight: 600;
    box-shadow: 0 4px 12px -2px rgba(0, 0, 0, 0.06), 0 2px 4px -1px rgba(0, 0, 0, 0.03);
  }

  .eink-paper {
    flex: 1;
    background: var(--code-paper-bg);
    border: 1px solid var(--code-paper-border);
    border-radius: 16px;
    padding: 0.85rem 1.1rem;
    display: flex;
    flex-direction: column;
    box-shadow: var(--glass-shadow);
    overflow: hidden;
  }

  .paper-header {
    display: flex;
    justify-content: space-between;
    align-items: center;
    border-bottom: 1px solid var(--border);
    padding-bottom: 0.4rem;
    margin-bottom: 0.4rem;
  }

  .header-left {
    display: flex;
    align-items: center;
    gap: 0.5rem;
  }

  .file-tag {
    font-family: var(--font-mono, monospace);
    font-size: 0.64rem;
    text-transform: uppercase;
    background: var(--pill-bg);
    padding: 0.1rem 0.35rem;
    border-radius: 4px;
    color: var(--text-muted);
  }

  .file-title {
    font-family: var(--font-mono, monospace);
    font-size: 0.82rem;
    font-weight: 600;
    color: var(--text);
  }

  .file-cat {
    font-family: var(--font-sans);
    font-size: 0.7rem;
    color: var(--text-muted);
  }

  .flow-bar {
    display: flex;
    align-items: center;
    gap: 0.5rem;
    font-family: var(--font-mono, monospace);
    font-size: 0.68rem;
    color: var(--text-secondary);
    padding: 0.35rem 0.6rem;
    background: var(--glass-bg);
    border-radius: 8px;
    margin-bottom: 0.5rem;
  }

  .flow-bar code {
    color: var(--accent);
    font-weight: 600;
  }

  .flow-sep {
    color: var(--text-muted);
  }

  .code-scroll {
    flex: 1;
    overflow-y: auto;
  }

  .eink-code {
    margin: 0;
    font-family: var(--font-mono, monospace);
    font-size: 0.72rem;
    line-height: 1.45;
  }

  .empty-msg {
    text-align: center;
    color: var(--text-muted);
    font-size: 0.8rem;
    padding: 2rem 0;
  }
</style>
