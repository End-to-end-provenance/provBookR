<script lang="ts">
  import { themeState } from '../../themeState.svelte';

  let { provData } = $props<{ provData: any }>();

  const usedRels = $derived(provData?.used || {});
  const genRels = $derived(provData?.wasGeneratedBy || {});
  const entities = $derived(provData?.entity || {});
  const activities = $derived(provData?.activity || {});

  type EdgeDesc = { source: string; target: string; verb: string };

  const edgeDescs = $derived.by(() => {
    const list: EdgeDesc[] = [];
    Object.values(genRels).forEach((g: any) => {
      const actName = activities[g['prov:activity']]?.['rdt:name'] || g['prov:activity'];
      const entName = entities[g['prov:entity']]?.['rdt:name'] || g['prov:entity'];
      const shortAct = actName.length > 24 ? actName.slice(0, 22) + '…' : actName;
      list.push({ source: shortAct, target: entName, verb: 'generated' });
    });

    Object.values(usedRels).forEach((u: any) => {
      const entName = entities[u['prov:entity']]?.['rdt:name'] || u['prov:entity'];
      const actName = activities[u['prov:activity']]?.['rdt:name'] || u['prov:activity'];
      const shortAct = actName.length > 24 ? actName.slice(0, 22) + '…' : actName;
      list.push({ source: shortAct, target: entName, verb: 'used' });
    });
    return list;
  });

  const isLight = $derived(themeState.readingMode === 'light');
  const displayedEdges = $derived(
    isLight ? edgeDescs.slice(0, Math.min(4, edgeDescs.length)) : edgeDescs
  );
</script>

<div class="derivation-page" class:mode-light={isLight}>
  <h3>Derivation Flow</h3>
  <p class="deriv-intro">
    {#if isLight}
      Key dependency relationships between computational operations and data artifacts:
    {:else}
      All <strong>generated</strong> (creates value) and <strong>used</strong> (reads value) derivation edges.
    {/if}
  </p>

  <div class="edge-list">
    {#each displayedEdges as edge}
      <div class="glass-edge-pill">
        <span class="edge-source">{edge.source}</span>
        <span class="edge-arrow">
          {#if edge.verb === 'generated'}→{:else}←{/if}
        </span>
        <span class="edge-target">{edge.target}</span>
        <span class="edge-verb">{edge.verb}</span>
      </div>
    {/each}
  </div>

  {#if isLight && edgeDescs.length > 4}
    <div class="sparse-footer-pill">
      <span>Showing {displayedEdges.length} primary relationships ({edgeDescs.length - displayedEdges.length} more in @detailedmode)</span>
    </div>
  {/if}

  <div class="colophon">
    <p>
      Assembled by <strong>provBookR</strong>
    </p>
  </div>
</div>

<style>
  .derivation-page {
    min-height: 100%;
    display: flex;
    flex-direction: column;
  }
  .derivation-page h3 {
    margin-top: 0;
    color: var(--text);
  }
  .deriv-intro {
    font-size: 0.85rem;
    color: var(--text-secondary);
    margin-bottom: 1rem;
  }

  .edge-list {
    flex: 1;
    display: flex;
    flex-direction: column;
    gap: 0.45rem;
    overflow-y: auto;
    padding-right: 0.3rem;
  }

  /* Liquid Glass Edge Pill */
  .glass-edge-pill {
    display: flex;
    align-items: center;
    gap: 0.5rem;
    padding: 0.5rem 0.9rem;
    background: var(--glass-bg);
    backdrop-filter: blur(12px);
    -webkit-backdrop-filter: blur(12px);
    border: 1px solid var(--glass-border);
    border-radius: 9999px;
    font-family: var(--font-mono, monospace);
    font-size: 0.73rem;
    box-shadow: var(--glass-shadow);
    transition: transform 0.2s cubic-bezier(0.16, 1, 0.3, 1), background 0.2s;
  }

  .glass-edge-pill:hover {
    transform: translateY(-1px);
    background: var(--glass-bg-hover);
  }

  .edge-source {
    color: var(--text);
    flex: 1;
    min-width: 0;
    overflow: hidden;
    text-overflow: ellipsis;
    white-space: nowrap;
    font-weight: 500;
  }

  .edge-arrow {
    color: var(--text);
    font-weight: 700;
    flex-shrink: 0;
  }

  .edge-target {
    color: var(--text);
    font-weight: 600;
    flex-shrink: 0;
  }

  .edge-verb {
    font-family: var(--font-sans);
    font-size: 0.6rem;
    text-transform: uppercase;
    letter-spacing: 0.08em;
    color: var(--text-muted);
    flex-shrink: 0;
    width: 58px;
    text-align: right;
  }

  .colophon {
    margin-top: 1.5rem;
    padding-top: 0.8rem;
    border-top: 1px solid var(--border);
    font-family: var(--font-sans);
    font-size: 0.72rem;
    color: var(--text-muted);
    text-align: center;
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

  .mode-light .edge-list {
    gap: 0.55rem;
  }
</style>
