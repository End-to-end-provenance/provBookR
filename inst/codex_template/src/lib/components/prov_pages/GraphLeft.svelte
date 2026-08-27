<script lang="ts">
  import { themeState } from '../../themeState.svelte';
  import { parseProvData } from '../../data/provParser';
  import scriptSource from '../../data/script_source.json';
  import Icon from '../Icon.svelte';

  let { provData } = $props<{ provData: any }>();

  const parsed = $derived(parseProvData(provData, scriptSource));
  const isLight = $derived(themeState.readingMode === 'light');

  const filteredEntities = $derived.by(() => {
    const ents = provData?.entity || {};
    return Object.entries(ents).filter(([k, e]: [string, any]) => 
      e['rdt:type'] !== 'Environment' && k.startsWith('rdt:d')
    ) as [string, any][];
  });

  const snapshots = $derived(filteredEntities.filter(([_, e]) => e['rdt:type'] === 'Snapshot' || e['rdt:type'] === 'Data'));
  const files = $derived(filteredEntities.filter(([_, e]) => e['rdt:type'] === 'File' || e['rdt:type'] === 'OutputFile'));

  const displayedSnapshots = $derived(
    isLight ? snapshots.slice(0, Math.min(4, snapshots.length)) : snapshots
  );
  const displayedFiles = $derived(
    isLight ? files.slice(0, Math.min(2, files.length)) : files
  );
</script>

<div class="graph-intro" class:mode-light={isLight}>
  <p class="chapter-label">Chapter 3: Lineage Graph</p>
  <h2>Lineage &amp; Data Flow</h2>
  <p class="section-desc">
    Computational data lineage for <code>{parsed.scriptName}</code>.
    Inspect variables, intermediate state mutations, and generated files below.
  </p>

  <div class="script-badge-bar">
    <div class="script-badge active">
      <Icon name="document" size={14} />
      <span class="sc-name">{parsed.scriptName}</span>
    </div>
  </div>

  <div class="entity-section">
    <h3>Variables &amp; Data <span class="count">({snapshots.length})</span></h3>
    <div class="entity-grid">
      {#each displayedSnapshots as [id, ent]}
        <div class="glass-pill">
          <span class="ent-name">{ent['rdt:name']}</span>
          <span class="ent-value">{ent['rdt:value'] || ent['rdt:valType'] || 'Value'}</span>
        </div>
      {/each}
      {#if snapshots.length === 0}
        <div class="empty-state">No intermediate variables recorded.</div>
      {/if}
    </div>
    {#if isLight && snapshots.length > 4}
      <div class="sparse-footer-pill">
        <span>Showing {displayedSnapshots.length} key variables ({snapshots.length - displayedSnapshots.length} more in @detailedmode)</span>
      </div>
    {/if}
  </div>

  <div class="entity-section">
    <h3>Artifacts &amp; Files <span class="count">({files.length})</span></h3>
    <div class="entity-grid">
      {#each displayedFiles as [id, ent]}
        <div class="glass-pill file-pill">
          <span class="ent-name">{ent['rdt:name']}</span>
          <span class="ent-hash">{ent['rdt:hash'] ? ent['rdt:hash'].slice(0, 16) + '…' : 'artifact'}</span>
        </div>
      {/each}
      {#if files.length === 0}
        <div class="empty-state">No files generated.</div>
      {/if}
    </div>
    {#if isLight && files.length > 2}
      <div class="sparse-footer-pill">
        <span>Showing {displayedFiles.length} primary artifacts ({files.length - displayedFiles.length} more in @detailedmode)</span>
      </div>
    {/if}
  </div>
</div>

<style>
  .graph-intro {
    min-height: 100%;
  }
  .graph-intro h2 {
    margin-top: 0.25rem;
    color: var(--text);
  }
  .section-desc {
    font-size: 0.78rem;
    line-height: 1.45;
    color: var(--text-secondary);
    margin-bottom: 0.6rem;
  }
  .section-desc code {
    font-family: var(--font-mono, monospace);
    font-size: 0.74rem;
    padding: 0.1rem 0.35rem;
    background: var(--pill-bg, rgba(0, 0, 0, 0.05));
    border-radius: 4px;
  }
  .script-badge-bar {
    display: flex;
    margin-bottom: 0.8rem;
  }
  .script-badge {
    display: inline-flex;
    align-items: center;
    gap: 0.35rem;
    padding: 0.3rem 0.75rem;
    font-size: 0.72rem;
    font-family: var(--font-mono, monospace);
    color: var(--text);
    background: var(--glass-bg);
    border: 1px solid var(--border);
    border-radius: 9999px;
  }
  .sc-name {
    font-weight: 600;
  }
  .entity-section {
    margin-bottom: 0.8rem;
  }
  .entity-section h3 {
    font-family: var(--font-sans);
    font-size: 0.85rem;
    font-weight: 600;
    margin: 0 0 0.4rem;
    color: var(--text);
  }
  .count {
    font-weight: 400;
    color: var(--text-muted);
  }
  .entity-grid {
    display: flex;
    flex-direction: column;
    gap: 0.45rem;
    max-height: 180px;
    overflow-y: auto;
  }
  
  .glass-pill {
    display: flex;
    justify-content: space-between;
    align-items: center;
    padding: 0.5rem 0.9rem;
    background: var(--glass-bg);
    backdrop-filter: blur(14px);
    -webkit-backdrop-filter: blur(14px);
    border: 1px solid var(--glass-border);
    border-radius: 9999px;
    font-size: 0.75rem;
    gap: 0.75rem;
    box-shadow: var(--glass-shadow);
    transition: transform 0.2s cubic-bezier(0.16, 1, 0.3, 1), background 0.2s;
  }

  .glass-pill:hover {
    transform: translateY(-1px);
    background: var(--glass-bg-hover);
  }

  .ent-name {
    font-family: var(--font-mono, monospace);
    color: var(--text);
    font-weight: 600;
    white-space: nowrap;
  }
  .ent-value {
    font-family: var(--font-mono, monospace);
    color: var(--text-secondary);
    font-size: 0.72rem;
    text-align: right;
    word-break: break-word;
  }
  .file-pill {
    background: var(--card-bg);
  }
  .ent-hash {
    font-family: var(--font-mono, monospace);
    font-size: 0.68rem;
    color: var(--text-muted);
  }
  .empty-state {
    font-size: 0.8rem;
    color: var(--text-muted);
    font-style: italic;
    padding: 0.4rem;
  }

  .sparse-footer-pill {
    margin-top: 0.5rem;
    padding: 0.45rem 0.75rem;
    background: var(--pill-bg, rgba(0, 0, 0, 0.04));
    border-radius: 12px;
    font-family: var(--font-sans);
    font-size: 0.68rem;
    color: var(--text-muted);
    text-align: center;
  }
</style>
