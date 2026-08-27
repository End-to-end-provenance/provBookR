<script lang="ts">
  import { themeState } from '../../themeState.svelte';
  import { currentScriptMeta } from '../../state.svelte';
  import { parseProvData } from '../../data/provParser';
  import scriptSource from '../../data/script_source.json';
  import Icon from '../Icon.svelte';

  let { provData, active = true } = $props<{ provData?: any; active?: boolean }>();

  const activeMeta = $derived(currentScriptMeta());
  const parsed = $derived(provData ? parseProvData(provData, scriptSource) : null);
  const isLight = $derived(themeState.readingMode === 'light');

  const scriptName = $derived(parsed ? parsed.scriptName : (activeMeta?.name || 'Analysis Script'));
  const variables = $derived(parsed ? parsed.variables.map(v => v.name) : (activeMeta?.variables || []));
  const operations = $derived(parsed ? parsed.activities.map(a => a.name) : (activeMeta?.operations || []));
  const outputFile = $derived(parsed ? (parsed.outputArtifacts.map(a => a.name).join(', ') || 'In-memory workspace') : (activeMeta?.outputFile || 'In-memory workspace'));

  const displayedVars = $derived(
    isLight ? variables.slice(0, Math.min(4, variables.length)) : variables
  );
  const displayedOps = $derived(
    isLight ? operations.slice(0, Math.min(3, operations.length)) : operations.slice(0, 6)
  );
</script>

<div class="code-left-page" class:mode-light={isLight}>
  <p class="chapter-label">Chapter 2: Source Code</p>
  <h2>Source Code Breakdown</h2>
  <p class="section-desc">
    Execution profile and structural analysis for <code>{scriptName}</code>.
  </p>

  <!-- Active Script Breakdown Card (Liquid Glass Bubble) -->
  <div class="glass-breakdown-card">
    <div class="breakdown-header">
      <Icon name={activeMeta?.icon || "document"} size={18} />
      <div>
        <h3>{scriptName}</h3>
        <span class="breakdown-cat">{activeMeta?.category || 'R 4.x'} • {operations.length} Operations</span>
      </div>
    </div>

    <div class="breakdown-body">
      <div class="breakdown-row">
        <span class="row-label">Variables ({variables.length}):</span>
        <div class="tag-wrap">
          {#each displayedVars as v}
            <span class="var-tag"><code>{v}</code></span>
          {/each}
          {#if isLight && variables.length > 4}
            <span class="more-pill">+{variables.length - displayedVars.length} more in @detailedmode</span>
          {/if}
        </div>
      </div>

      <div class="breakdown-row">
        <span class="row-label">Output Artifact:</span>
        <div class="tag-wrap">
          <span class="out-tag"><code>{outputFile}</code></span>
        </div>
      </div>

      <div class="breakdown-row">
        <span class="row-label">Key Operations:</span>
        <div class="op-list">
          {#each displayedOps as op}
            <div class="op-item">
              <code class="op-code">{op}</code>
            </div>
          {/each}
          {#if isLight && operations.length > 3}
            <span class="more-pill">+{operations.length - displayedOps.length} more in @detailedmode</span>
          {/if}
        </div>
      </div>
    </div>
  </div>
</div>

<style>
  .code-left-page {
    display: flex;
    flex-direction: column;
  }
  .code-left-page h2 {
    margin-top: 0.2rem;
    margin-bottom: 0.2rem;
    color: var(--text);
  }
  .section-desc {
    font-size: 0.78rem;
    color: var(--text-secondary);
    margin-bottom: 0.8rem;
  }
  .section-desc code {
    font-family: var(--font-mono, monospace);
    font-size: 0.74rem;
    padding: 0.1rem 0.35rem;
    background: var(--pill-bg, rgba(0, 0, 0, 0.05));
    border-radius: 4px;
  }

  .glass-breakdown-card {
    background: var(--glass-bg);
    backdrop-filter: blur(14px);
    -webkit-backdrop-filter: blur(14px);
    border: 1px solid var(--glass-border);
    border-radius: 16px;
    padding: 0.9rem 1.1rem;
    box-shadow: var(--glass-shadow);
  }

  .breakdown-header {
    display: flex;
    align-items: center;
    gap: 0.7rem;
    border-bottom: 1px solid var(--border);
    padding-bottom: 0.6rem;
    margin-bottom: 0.6rem;
  }

  .breakdown-header h3 {
    margin: 0;
    font-size: 0.88rem;
    color: var(--text);
  }

  .breakdown-cat {
    font-family: var(--font-mono, monospace);
    font-size: 0.65rem;
    color: var(--text-muted);
  }

  .breakdown-body {
    display: flex;
    flex-direction: column;
    gap: 0.55rem;
  }

  .breakdown-row {
    display: flex;
    flex-direction: column;
    gap: 0.25rem;
  }

  .row-label {
    font-family: var(--font-sans);
    font-size: 0.7rem;
    font-weight: 600;
    color: var(--text-muted);
    text-transform: uppercase;
    letter-spacing: 0.05em;
  }

  .tag-wrap {
    display: flex;
    flex-wrap: wrap;
    gap: 0.3rem;
  }

  .var-tag {
    font-family: var(--font-mono, monospace);
    font-size: 0.64rem;
    padding: 0.1rem 0.45rem;
    background: var(--pill-bg);
    border: 1px solid var(--border);
    border-radius: 9999px;
    color: var(--text);
  }

  .out-tag {
    display: inline-flex;
    align-items: center;
    justify-content: center;
    line-height: 1;
    font-family: var(--font-mono, monospace);
    font-size: 0.64rem;
    font-weight: 600;
    padding: 0.16rem 0.5rem;
    background: rgba(16, 185, 129, 0.1);
    border: 1px solid #059669;
    border-radius: 9999px;
    color: var(--text);
    box-shadow: 0 1px 3px rgba(0, 0, 0, 0.04);
  }

  .dim-text {
    font-size: 0.68rem;
    color: var(--text-muted);
  }

  .op-list {
    display: flex;
    flex-direction: column;
    gap: 0.2rem;
  }

  .op-item {
    display: flex;
    align-items: center;
    gap: 0.4rem;
    font-family: var(--font-mono, monospace);
    font-size: 0.64rem;
    background: rgba(0, 0, 0, 0.03);
    padding: 0.15rem 0.4rem;
    border-radius: 4px;
  }

  .op-time {
    font-weight: 600;
    color: var(--accent);
    min-width: 38px;
  }

  .more-pill {
    font-family: var(--font-sans);
    font-size: 0.62rem;
    padding: 0.15rem 0.5rem;
    background: var(--pill-bg, rgba(0, 0, 0, 0.04));
    border-radius: 9999px;
    color: var(--text-muted);
    font-style: italic;
  }

  .op-code {
    color: var(--text);
    overflow: hidden;
    text-overflow: ellipsis;
    white-space: nowrap;
  }
</style>
