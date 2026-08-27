<script lang="ts">
  import { parseProvData } from '../../data/provParser';
  import scriptSource from '../../data/script_source.json';
  
  let { provData } = $props<{ provData: any }>();
  
  const summary = $derived(parseProvData(provData, scriptSource));
</script>

<div class="intro-page">
  <p class="chapter-label">Chapter 1: Overview</p>
  <h2>{summary.scriptName}</h2>
  <p class="subtitle">Computational Provenance & Execution Blueprint</p>

  <p class="intro-text">
    This notebook documents the execution of <code>{summary.scriptName}</code>, tracking all data inputs,
    computational operations, intermediate state mutations, and generated output artifacts.
  </p>

  <!-- Flow Breakdown Cards (Liquid Glass Bubble Pills) -->
  <div class="flow-container">
    <div class="flow-pill">
      <div class="pill-badge">1</div>
      <div class="pill-content">
        <h4>Inputs & Variables ({summary.variables.length})</h4>
        <p>
          {#if summary.variables.length > 0}
            Tracking: <code>{summary.variables.slice(0, 3).map(v => v.name).join(', ')}{summary.variables.length > 3 ? '...' : ''}</code>
          {:else}
            No external inputs declared.
          {/if}
        </p>
      </div>
    </div>

    <div class="flow-pill">
      <div class="pill-badge">2</div>
      <div class="pill-content">
        <h4>Operations & Activities ({summary.operationsCount})</h4>
        <p>Executed in <strong>{summary.totalElapsedTime}s</strong> across <strong>{summary.langVersion}</strong> on <strong>{summary.operatingSystem}</strong>.</p>
      </div>
    </div>

    <div class="flow-pill">
      <div class="pill-badge">3</div>
      <div class="pill-content">
        <h4>Artifacts & Outputs ({summary.outputArtifacts.length})</h4>
        <p>
          {#if summary.outputArtifacts.length > 0}
            Generated: <code>{summary.outputArtifacts.map(a => a.name).join(', ')}</code>
          {:else}
            Interactive memory state snapshot.
          {/if}
        </p>
      </div>
    </div>
  </div>

  <div class="note">
    <strong>Lineage Guarantee:</strong> Every intermediate variable and final artifact is connected via deterministic W3C PROV-JSON dependency edges.
  </div>
</div>

<style>
  .intro-page {
    display: flex;
    flex-direction: column;
  }
  .intro-page h2 {
    margin-top: 0.2rem;
    margin-bottom: 0.3rem;
    color: var(--text);
  }
  .subtitle {
    margin-bottom: 0.6rem;
    color: var(--text-secondary);
    font-size: 0.88rem;
  }
  .intro-text {
    font-size: 0.82rem;
    color: var(--text-secondary);
    line-height: 1.45;
    margin-bottom: 0.8rem;
  }
  .intro-text code {
    font-family: var(--font-mono, monospace);
    font-size: 0.76rem;
    padding: 0.1rem 0.35rem;
    background: var(--pill-bg, rgba(0, 0, 0, 0.05));
    border-radius: 4px;
    color: var(--text);
  }

  .flow-container {
    display: flex;
    flex-direction: column;
    gap: 0.45rem;
    margin: 0.4rem 0 0.8rem;
  }

  .flow-pill {
    display: flex;
    align-items: flex-start;
    gap: 0.7rem;
    padding: 0.55rem 0.9rem;
    background: var(--glass-bg);
    backdrop-filter: blur(14px);
    -webkit-backdrop-filter: blur(14px);
    border: 1px solid var(--glass-border);
    border-radius: 16px;
    box-shadow: var(--glass-shadow);
    transition: transform 0.2s cubic-bezier(0.16, 1, 0.3, 1), background 0.2s, box-shadow 0.2s;
  }

  .flow-pill:hover {
    transform: translateY(-1px);
    background: var(--glass-bg-hover);
  }

  .pill-badge {
    flex-shrink: 0;
    width: 20px;
    height: 20px;
    border-radius: 50%;
    background: var(--pill-badge-bg, var(--text));
    color: var(--pill-badge-text, #ffffff);
    font-family: var(--font-mono, monospace);
    font-size: 0.68rem;
    font-weight: 700;
    display: flex;
    align-items: center;
    justify-content: center;
    margin-top: 0.1rem;
    box-shadow: 0 2px 6px rgba(0, 0, 0, 0.15);
  }

  .pill-content h4 {
    font-family: var(--font-sans);
    font-size: 0.8rem;
    font-weight: 600;
    color: var(--text);
    margin: 0 0 0.1rem;
  }

  .pill-content p {
    font-family: var(--font-sans);
    font-size: 0.72rem;
    color: var(--text-secondary);
    margin: 0;
    line-height: 1.38;
  }

  .pill-content code {
    font-family: var(--font-mono, monospace);
    font-size: 0.68rem;
    padding: 0.05rem 0.25rem;
    background: var(--pill-bg, rgba(0, 0, 0, 0.05));
    border-radius: 4px;
  }

  .note {
    font-size: 0.72rem;
    color: var(--text-muted);
    margin-top: 0.2rem;
    line-height: 1.4;
  }
</style>
