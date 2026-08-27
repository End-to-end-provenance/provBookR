<script lang="ts">
  import { themeState } from '../../themeState.svelte';

  let { provData } = $props<{ provData: any }>();

  const activities = $derived(provData?.activity || {});
  const genRels = $derived(provData?.wasGeneratedBy || {});
  const usedRels = $derived(provData?.used || {});

  const operations = $derived(
    Object.entries(activities)
      .filter(([_, a]: [string, any]) => a['rdt:type'] === 'Operation')
      .map(([id, a]: [string, any]) => ({
        id,
        name: a['rdt:name'] || id,
        time: parseFloat(a['rdt:elapsedTime'] || '0'),
        line: a['rdt:startLine'] || '?',
        script: a['rdt:scriptNum'] || '?'
      }))
  );

  const isLight = $derived(themeState.readingMode === 'light');
  const displayedOps = $derived(
    isLight ? operations.slice(0, Math.min(3, operations.length)) : operations
  );

  const totalOps = $derived(operations.length);
  const totalTime = $derived(operations.reduce((sum, op) => sum + op.time, 0));
  const totalEdges = $derived(Object.keys(genRels).length + Object.keys(usedRels).length);
</script>

<div class="output-intro" class:mode-light={isLight}>
  <p class="chapter-label">Chapter 5</p>
  <h2>Execution Trace</h2>
  <p class="exec-desc">
    {#if isLight}
      A high-level summary of the computational stages executed by the pipeline.
    {:else}
      Each operation below represents a single step in the computation.
      Together they form the full granular execution trace.
    {/if}
  </p>

  <div class="glass-summary">
    <div class="summary-stat">
      <span class="ss-val">{totalOps}</span>
      <span class="ss-key">steps</span>
    </div>
    <div class="summary-stat">
      <span class="ss-val">{totalTime.toFixed(3)}s</span>
      <span class="ss-key">total</span>
    </div>
    <div class="summary-stat">
      <span class="ss-val">{totalEdges}</span>
      <span class="ss-key">edges</span>
    </div>
  </div>

  <div class="timeline">
    {#each displayedOps as op, idx}
      <div class="glass-step">
        <div class="step-badge">{idx + 1}</div>
        <div class="step-body">
          <code class="step-code">{op.name}</code>
          <span class="step-meta">Line {op.line} · {op.time.toFixed(3)}s</span>
        </div>
      </div>
    {/each}
  </div>

  {#if isLight && operations.length > 3}
    <div class="sparse-footer-pill">
      <span>Showing {displayedOps.length} milestone steps ({operations.length - displayedOps.length} more in @detailedmode)</span>
    </div>
  {/if}
</div>

<style>
  .output-intro {
    min-height: 100%;
  }
  .output-intro h2 {
    margin-top: 0.25rem;
    color: var(--text);
  }

  .glass-summary {
    display: flex;
    justify-content: space-around;
    padding: 0.55rem 1rem;
    background: var(--glass-bg);
    backdrop-filter: blur(14px);
    -webkit-backdrop-filter: blur(14px);
    border: 1px solid var(--glass-border);
    border-radius: 9999px;
    margin: 0.75rem 0;
    box-shadow: var(--glass-shadow);
  }

  .summary-stat {
    display: flex;
    flex-direction: column;
    align-items: center;
  }
  .ss-val {
    font-family: var(--font-mono, monospace);
    font-size: 0.95rem;
    font-weight: 700;
    color: var(--text);
  }
  .ss-key {
    font-family: var(--font-sans);
    font-size: 0.6rem;
    text-transform: uppercase;
    letter-spacing: 0.12em;
    color: var(--text-muted);
  }

  .timeline {
    display: flex;
    flex-direction: column;
    gap: 0.35rem;
  }

  .glass-step {
    display: flex;
    gap: 0.6rem;
    align-items: center;
    padding: 0.4rem 0.8rem;
    background: var(--glass-bg);
    backdrop-filter: blur(12px);
    -webkit-backdrop-filter: blur(12px);
    border: 1px solid var(--glass-border);
    border-radius: 9999px;
    box-shadow: var(--glass-shadow);
    transition: transform 0.2s cubic-bezier(0.16, 1, 0.3, 1), background 0.2s;
  }

  .glass-step:hover {
    transform: translateY(-1px);
    background: var(--glass-bg-hover);
  }

  .step-badge {
    flex-shrink: 0;
    width: 22px;
    height: 22px;
    border-radius: 50%;
    background: var(--pill-badge-bg, var(--text));
    color: var(--pill-badge-text, #ffffff);
    font-family: var(--font-mono, monospace);
    font-size: 0.68rem;
    font-weight: 700;
    display: flex;
    align-items: center;
    justify-content: center;
  }

  .step-body {
    display: flex;
    flex-direction: column;
    gap: 0.1rem;
    min-width: 0;
  }
  .step-code {
    font-family: var(--font-mono, monospace);
    font-size: 0.76rem;
    color: var(--text);
    white-space: nowrap;
    overflow: hidden;
    text-overflow: ellipsis;
    font-weight: 500;
  }
  .step-meta {
    font-family: var(--font-sans);
    font-size: 0.64rem;
    color: var(--text-muted);
  }

  .sparse-footer-pill {
    margin-top: 0.75rem;
    padding: 0.5rem 0.8rem;
    background: var(--pill-bg, rgba(0, 0, 0, 0.04));
    border-radius: 12px;
    font-family: var(--font-sans);
    font-size: 0.68rem;
    color: var(--text-muted);
    text-align: center;
  }

  .mode-light .timeline {
    gap: 0.45rem;
  }
</style>
