<script lang="ts">
  import { themeState } from '../../themeState.svelte';

  let { provData } = $props<{ provData: any }>();

  const entities = $derived(provData?.entity || {});

  const snapshots = $derived(
    Object.entries(entities)
      .filter(([k, e]: [string, any]) => {
        const t = e['rdt:type'];
        const name = e['rdt:name'] || k;
        // Ignore files, libraries, standard output, and environment collections
        if (t === 'File' || t === 'OutputFile' || t === 'InputFile' || t === 'StandardOutput' || name === 'output' || k.startsWith('rdt:l')) {
          return false;
        }
        return t === 'Data' || t === 'Snapshot' || !t;
      })
      .map(([id, e]: [string, any]) => {
        let val = e['rdt:value'] || 'Recorded';
        if (typeof val === 'string') {
          val = val.trim();
        }
        let typeStr = 'Variable';
        if (e['rdt:valType']) {
          try {
            const parsed = typeof e['rdt:valType'] === 'string' ? JSON.parse(e['rdt:valType']) : e['rdt:valType'];
            if (parsed.type) {
              const innerType = Array.isArray(parsed.type) ? parsed.type.join(', ') : parsed.type;
              if (parsed.container && parsed.container !== 'vector') {
                typeStr = `${parsed.container} (${innerType})`;
              } else {
                typeStr = innerType;
              }
            }
          } catch {
            typeStr = String(e['rdt:valType']);
          }
        }
        return {
          id,
          name: e['rdt:name'] || id,
          value: val,
          type: typeStr
        };
      })
  );

  const isLight = $derived(themeState.readingMode === 'light');
  const displayedSnapshots = $derived(
    isLight ? snapshots.slice(0, Math.min(4, snapshots.length)) : snapshots
  );
</script>

<div class="lineage-page" class:mode-light={isLight}>
  <p class="chapter-label">Chapter 4</p>
  <h2>Variable Snapshots</h2>
  <p class="subtitle">
    {#if isLight}
      Curated state summary across recorded pipeline operations.
    {:else}
      Inspecting computational state across all recorded operations.
    {/if}
  </p>

  <div class="snapshot-list">
    {#each displayedSnapshots as snap}
      <div class="glass-snap-card">
        <div class="snap-head">
          <span class="snap-name">{snap.name}</span>
          <span class="snap-type">{snap.type}</span>
        </div>
        <div class="snap-body">
          <code class="snap-val">{snap.value}</code>
        </div>
      </div>
    {/each}
  </div>

  {#if isLight && snapshots.length > 4}
    <div class="sparse-footer-pill">
      <span>Showing {displayedSnapshots.length} key variables ({snapshots.length - displayedSnapshots.length} more in @detailedmode)</span>
    </div>
  {/if}
</div>

<style>
  .lineage-page {
    min-height: 100%;
    display: flex;
    flex-direction: column;
  }
  .lineage-page h2 {
    margin-top: 0.2rem;
    margin-bottom: 0.2rem;
    color: var(--text);
  }
  .subtitle {
    margin-bottom: 0.5rem;
    font-size: 0.8rem;
    color: var(--text-secondary);
  }

  .snapshot-list {
    display: flex;
    flex-direction: column;
    gap: 0.35rem;
    padding-right: 0.2rem;
  }

  /* Liquid Glass Snap Card */
  .glass-snap-card {
    display: flex;
    flex-direction: column;
    gap: 0.3rem;
    padding: 0.65rem 1rem;
    background: var(--glass-bg);
    backdrop-filter: blur(14px);
    -webkit-backdrop-filter: blur(14px);
    border: 1px solid var(--glass-border);
    border-radius: 16px;
    box-shadow: var(--glass-shadow);
    transition: transform 0.2s cubic-bezier(0.16, 1, 0.3, 1), background 0.2s;
  }

  .glass-snap-card:hover {
    transform: translateY(-1px);
    background: var(--glass-bg-hover);
  }

  .snap-head {
    display: flex;
    justify-content: space-between;
    align-items: center;
  }

  .snap-name {
    font-family: var(--font-mono, monospace);
    font-size: 0.8rem;
    font-weight: 600;
    color: var(--text);
  }

  .snap-type {
    font-family: var(--font-sans);
    font-size: 0.65rem;
    color: var(--text-muted);
  }

  .snap-body {
    font-family: var(--font-mono, monospace);
    font-size: 0.73rem;
  }

  .snap-val {
    color: var(--text-secondary);
    word-break: break-word;
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

  .mode-light .snapshot-list {
    gap: 0.6rem;
  }
</style>
