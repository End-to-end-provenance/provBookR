<script lang="ts">
  let { provData } = $props<{ provData: any }>();

  let selectedNode = $state<{ id: string; name: string; type: string; detail: string } | null>(null);
  let hoveredNodeId = $state<string | null>(null);
  let viewMode = $state<'backbone' | 'full'>('backbone');
  let backboneExpanded = $state(false);

  const activities = $derived(provData?.activity || {});
  const entities = $derived(provData?.entity || {});
  const usedRels = $derived(provData?.used || {});
  const genRels = $derived(provData?.wasGeneratedBy || {});

  const graphData = $derived.by(() => {
    // 1. Filter operations: remove script-level container activity
    const actKeys = Object.keys(activities).filter(k => {
      const a = activities[k];
      const name = a['rdt:name'] || '';
      return (a['rdt:type'] === 'Operation' || name) && 
             a['rdt:type'] !== 'Environment' &&
             !name.endsWith('.R') &&
             !name.endsWith('.ipynb');
    });
    const scriptActSet = new Set(actKeys);

    // 2. Classify entities
    const usedEntityIds = new Set<string>();
    Object.values(usedRels).forEach((u: any) => {
      if (scriptActSet.has(u['prov:activity'])) usedEntityIds.add(u['prov:entity']);
    });

    const genEntityIds = new Set<string>();
    Object.values(genRels).forEach((g: any) => {
      if (scriptActSet.has(g['prov:activity'])) genEntityIds.add(g['prov:entity']);
    });

    const allEntityIds = Array.from(new Set([...usedEntityIds, ...genEntityIds])).filter(k => {
      const e = entities[k];
      if (!e || !k.startsWith('rdt:d') || e['rdt:type'] === 'Environment') return false;
      const name = e['rdt:name'] || '';
      if (name.startsWith('dev.') && !name.includes('.pdf') && !name.includes('.png') && !e['rdt:value']) {
        return false;
      }
      return true;
    });

    const inputEntKeys = allEntityIds.filter(id => usedEntityIds.has(id) && !genEntityIds.has(id));
    const allOutputEntKeys = allEntityIds.filter(id => genEntityIds.has(id));

    // Terminal outputs: Files or entities generated and not used by any subsequent activity
    const terminalOutputEntKeys = allEntityIds.filter(id => {
      const e = entities[id];
      const isFile = e?.['rdt:type'] === 'File' || e?.['rdt:type'] === 'OutputFile' || /\.(pdf|png|csv|rds|html|json)$/i.test(e?.['rdt:name'] || '');
      const isTerminal = genEntityIds.has(id) && !usedEntityIds.has(id);
      return isFile || isTerminal;
    });

    const isBackboneCollapsed = viewMode === 'backbone' && !backboneExpanded && actKeys.length > 1;
    
    // In Backbone mode, show only terminal outputs so it cleanly fits within the frame!
    const outputEntKeys = isBackboneCollapsed
      ? (terminalOutputEntKeys.length > 0 ? terminalOutputEntKeys : allOutputEntKeys.slice(-4))
      : allOutputEntKeys;
    const hasInputs = inputEntKeys.length > 0;

    // 3. Proportional Layout Calculation (Frame-Fitted)
    const CANVAS_WIDTH = 580;
    const effectiveActCount = isBackboneCollapsed ? 1 : actKeys.length;
    const maxCount = Math.max(
      hasInputs ? inputEntKeys.length : 0,
      effectiveActCount,
      outputEntKeys.length,
      1
    );
    const ROW_HEIGHT = Math.max(28, Math.min(46, 360 / Math.max(maxCount, 1)));
    const PAD_Y = 24;
    const canvasHeight = Math.max(maxCount * ROW_HEIGHT + PAD_Y * 2, 280);

    type GNode = {
      id: string;
      label: string;
      type: 'act' | 'ent' | 'backbone';
      x: number;
      y: number;
      halfW: number;
      halfH: number;
      detail: string;
      isFile: boolean;
      collapsedCount?: number;
    };

    const nodes: GNode[] = [];
    const nodeMap = new Map<string, GNode>();

    function getY(index: number, totalInCol: number): number {
      const colSpan = totalInCol * ROW_HEIGHT;
      const startY = (canvasHeight - colSpan) / 2 + ROW_HEIGHT / 2;
      return startY + index * ROW_HEIGHT;
    }

    if (hasInputs) {
      // 3-Column Layout: Inputs (Left), Operations (Middle), Outputs (Right)
      const COL_X = [100, 290, 480];
      const ACT_HALFW = 68;
      const ENT_HALFW = 58;

      inputEntKeys.forEach((k, idx) => {
        const e = entities[k] || {};
        const rawName = e['rdt:name'] || k;
        const label = rawName.length > 16 ? rawName.slice(0, 14) + '…' : rawName;
        const isFile = e['rdt:type'] === 'File' || e['rdt:type'] === 'OutputFile';
        const n: GNode = {
          id: k,
          label,
          type: 'ent',
          x: COL_X[0],
          y: getY(idx, inputEntKeys.length),
          halfW: ENT_HALFW,
          halfH: 14,
          detail: `${rawName} = ${e['rdt:value'] || 'input'}`,
          isFile
        };
        nodes.push(n);
        nodeMap.set(k, n);
      });

      if (isBackboneCollapsed) {
        // Consolidated Backbone Capsule
        const n: GNode = {
          id: 'backbone-capsule',
          label: `▶ ${actKeys.length} Operations`,
          type: 'backbone',
          x: COL_X[1],
          y: getY(0, 1),
          halfW: 76,
          halfH: 16,
          detail: `${actKeys.length} computational operations collapsed on backbone. Click to unfold on canvas.`,
          isFile: false,
          collapsedCount: actKeys.length
        };
        nodes.push(n);
        nodeMap.set('backbone-capsule', n);
      } else {
        actKeys.forEach((k, idx) => {
          const a = activities[k] || {};
          const rawName = a['rdt:name'] || k;
          const label = rawName.length > 20 ? rawName.slice(0, 18) + '…' : rawName;
          const n: GNode = {
            id: k,
            label,
            type: 'act',
            x: COL_X[1],
            y: getY(idx, actKeys.length),
            halfW: ACT_HALFW,
            halfH: 15,
            detail: rawName,
            isFile: false
          };
          nodes.push(n);
          nodeMap.set(k, n);
        });
      }

      outputEntKeys.forEach((k, idx) => {
        const e = entities[k] || {};
        const rawName = e['rdt:name'] || k;
        const label = rawName.length > 16 ? rawName.slice(0, 14) + '…' : rawName;
        const isFile = e['rdt:type'] === 'File' || e['rdt:type'] === 'OutputFile';
        const n: GNode = {
          id: k,
          label,
          type: 'ent',
          x: COL_X[2],
          y: getY(idx, outputEntKeys.length),
          halfW: ENT_HALFW,
          halfH: 14,
          detail: `${rawName} = ${e['rdt:value'] || 'output'}`,
          isFile
        };
        nodes.push(n);
        nodeMap.set(k, n);
      });
    } else {
      // 2-Column Balanced Layout: Operations (Left: 30%) -> Outputs & State (Right: 70%)
      const COL_X = [160, 420];
      const ACT_HALFW = 80;
      const ENT_HALFW = 70;

      if (isBackboneCollapsed) {
        const n: GNode = {
          id: 'backbone-capsule',
          label: `▶ ${actKeys.length} Operations`,
          type: 'backbone',
          x: COL_X[0],
          y: getY(0, 1),
          halfW: 84,
          halfH: 16,
          detail: `${actKeys.length} computational operations collapsed on backbone. Click to unfold on canvas.`,
          isFile: false,
          collapsedCount: actKeys.length
        };
        nodes.push(n);
        nodeMap.set('backbone-capsule', n);
      } else {
        actKeys.forEach((k, idx) => {
          const a = activities[k] || {};
          const rawName = a['rdt:name'] || k;
          const label = rawName.length > 22 ? rawName.slice(0, 20) + '…' : rawName;
          const n: GNode = {
            id: k,
            label,
            type: 'act',
            x: COL_X[0],
            y: getY(idx, actKeys.length),
            halfW: ACT_HALFW,
            halfH: 15,
            detail: rawName,
            isFile: false
          };
          nodes.push(n);
          nodeMap.set(k, n);
        });
      }

      outputEntKeys.forEach((k, idx) => {
        const e = entities[k] || {};
        const rawName = e['rdt:name'] || k;
        const label = rawName.length > 18 ? rawName.slice(0, 16) + '…' : rawName;
        const isFile = e['rdt:type'] === 'File' || e['rdt:type'] === 'OutputFile';
        const n: GNode = {
          id: k,
          label,
          type: 'ent',
          x: COL_X[1],
          y: getY(idx, outputEntKeys.length),
          halfW: ENT_HALFW,
          halfH: 14,
          detail: `${rawName} = ${e['rdt:value'] || 'output'}`,
          isFile
        };
        nodes.push(n);
        nodeMap.set(k, n);
      });
    }

    // 4. Direction-Aware Flush Wires
    type GEdge = {
      id: string;
      sourceId: string;
      targetId: string;
      d: string;
      x1: number;
      y1: number;
      x2: number;
      y2: number;
    };
    const edges: GEdge[] = [];

    function addEdge(sourceNode: GNode, targetNode: GNode, relId: string) {
      let x1 = 0;
      let y1 = sourceNode.y;
      let x2 = 0;
      let y2 = targetNode.y;

      if (sourceNode.x < targetNode.x) {
        // Forward flow: start at source right, end at target left
        x1 = sourceNode.x + sourceNode.halfW;
        x2 = targetNode.x - targetNode.halfW;
        const dx = Math.abs(x2 - x1) * 0.45;
        const d = `M ${x1} ${y1} C ${x1 + dx} ${y1}, ${x2 - dx} ${y2}, ${x2} ${y2}`;
        edges.push({ id: relId, sourceId: sourceNode.id, targetId: targetNode.id, d, x1, y1, x2, y2 });
      } else if (sourceNode.x > targetNode.x) {
        // Backward / feedback flow: start at source left, end at target right
        x1 = sourceNode.x - sourceNode.halfW;
        x2 = targetNode.x + targetNode.halfW;
        const dx = Math.abs(x1 - x2) * 0.45;
        const d = `M ${x1} ${y1} C ${x1 - dx} ${y1}, ${x2 + dx} ${y2}, ${x2} ${y2}`;
        edges.push({ id: relId, sourceId: sourceNode.id, targetId: targetNode.id, d, x1, y1, x2, y2 });
      } else {
        // Same column loop
        x1 = sourceNode.x + sourceNode.halfW;
        x2 = targetNode.x + targetNode.halfW;
        const d = `M ${x1} ${y1} C ${x1 + 35} ${y1}, ${x2 + 35} ${y2}, ${x2} ${y2}`;
        edges.push({ id: relId, sourceId: sourceNode.id, targetId: targetNode.id, d, x1, y1, x2, y2 });
      }
    }

    if (isBackboneCollapsed) {
      const backboneNode = nodeMap.get('backbone-capsule');
      if (backboneNode) {
        inputEntKeys.forEach(k => {
          const inNode = nodeMap.get(k);
          if (inNode) addEdge(inNode, backboneNode, `${inNode.id}->backbone`);
        });
        outputEntKeys.forEach(k => {
          const outNode = nodeMap.get(k);
          if (outNode) addEdge(backboneNode, outNode, `backbone->${outNode.id}`);
        });
      }
    } else {
      // used: entity -> activity
      Object.values(usedRels).forEach((u: any) => {
        const eNode = nodeMap.get(u['prov:entity']);
        const aNode = nodeMap.get(u['prov:activity']);
        if (eNode && aNode) {
          addEdge(eNode, aNode, `${eNode.id}->${aNode.id}`);
        }
      });

      // wasGeneratedBy: activity -> entity
      Object.values(genRels).forEach((g: any) => {
        const aNode = nodeMap.get(g['prov:activity']);
        const eNode = nodeMap.get(g['prov:entity']);
        if (aNode && eNode) {
          addEdge(aNode, eNode, `${aNode.id}->${eNode.id}`);
        }
      });
    }

    return { nodes, edges, width: CANVAS_WIDTH, height: canvasHeight };
  });

  function isEdgeActive(edge: { sourceId: string; targetId: string }) {
    const activeId = selectedNode?.id || hoveredNodeId;
    if (!activeId) return false;
    return edge.sourceId === activeId || edge.targetId === activeId;
  }
</script>

<div class="graph-right-page">
  <div class="graph-right-card">
    <div class="graph-header">
      <div class="graph-title-group">
        <span class="graph-title">Lineage DAG</span>
        <div class="dag-mode-toggle">
          <button 
            class="mode-toggle-btn" 
            class:active={viewMode === 'backbone'} 
            onclick={() => { viewMode = 'backbone'; backboneExpanded = false; }}
          >
            Backbone
          </button>
          <button 
            class="mode-toggle-btn" 
            class:active={viewMode === 'full'} 
            onclick={() => { viewMode = 'full'; }}
          >
            Full DAG
          </button>
        </div>
      </div>
      <div class="graph-legend">
        <div class="legend-item">
          <span class="legend-symbol act-symbol"></span>
          <span>Operation</span>
        </div>
        <div class="legend-item">
          <span class="legend-symbol ent-symbol"></span>
          <span>Variable</span>
        </div>
        <div class="legend-item">
          <span class="legend-symbol file-symbol"></span>
          <span>Output File</span>
        </div>
      </div>
    </div>

    <!-- 2D Spatial Canvas -->
    <!-- svelte-ignore a11y_click_events_have_key_events -->
    <!-- svelte-ignore a11y_no_static_element_interactions -->
    <div 
      class="graph-canvas-wrap"
      onclick={() => selectedNode = null}
    >
      <svg 
        class="graph-svg" 
        viewBox="0 0 {graphData.width} {graphData.height}" 
        preserveAspectRatio="xMidYMid meet"
      >
        <defs>
          <!-- Arrowhead Marker -->
          <marker 
            id="dag-arrowhead" 
            markerWidth="5" 
            markerHeight="5" 
            refX="4.5" 
            refY="2.5" 
            orient="auto"
          >
            <polygon points="0 0, 5 2.5, 0 5" fill="var(--text-muted)" opacity="0.65" />
          </marker>

          <!-- Active Arrowhead Marker -->
          <marker 
            id="dag-arrowhead-active" 
            markerWidth="5" 
            markerHeight="5" 
            refX="4.5" 
            refY="2.5" 
            orient="auto"
          >
            <polygon points="0 0, 5 2.5, 0 5" fill="var(--accent)" />
          </marker>
        </defs>

        <!-- Connection Wires / Flow Paths Layer (Under Nodes) -->
        <g class="edges-layer">
          {#each graphData.edges as edge (edge.id)}
            <!-- Base Wire -->
            <path
              d={edge.d}
              class="edge-path"
              class:active={isEdgeActive(edge)}
              marker-end={isEdgeActive(edge) ? "url(#dag-arrowhead-active)" : "url(#dag-arrowhead)"}
            />
            <!-- Animated Signal Flow -->
            <path
              d={edge.d}
              class="edge-flow-pulse"
              class:active={isEdgeActive(edge)}
            />
            <!-- Flush Hardware Terminals -->
            <circle cx={edge.x1} cy={edge.y1} r="2" class="terminal-pin" class:active={isEdgeActive(edge)} />
            <circle cx={edge.x2} cy={edge.y2} r="2" class="terminal-pin" class:active={isEdgeActive(edge)} />
          {/each}
        </g>

        <!-- Opaque Centered Nodes Layer (Over Wires) -->
        <g class="nodes-layer">
          {#each graphData.nodes as node (node.id)}
            <!-- svelte-ignore a11y_click_events_have_key_events -->
            <!-- svelte-ignore a11y_no_static_element_interactions -->
            <g 
              class="node-group"
              class:selected={selectedNode?.id === node.id}
              class:hovered={hoveredNodeId === node.id}
              class:is-backbone={node.type === 'backbone'}
              transform="translate({node.x}, {node.y})"
              onmouseenter={() => hoveredNodeId = node.id}
              onmouseleave={() => hoveredNodeId = null}
              onclick={(e) => {
                e.stopPropagation();
                if (node.type === 'backbone') {
                  viewMode = 'full';
                  backboneExpanded = false;
                }
                selectedNode = { id: node.id, name: node.label, type: node.type, detail: node.detail };
              }}
            >
              {#if node.type === 'backbone'}
                <rect
                  x={-node.halfW}
                  y={-node.halfH}
                  width={node.halfW * 2}
                  height={node.halfH * 2}
                  rx="8"
                  class="node-rect backbone-rect"
                />
              {:else if node.type === 'act'}
                <rect
                  x={-node.halfW}
                  y={-node.halfH}
                  width={node.halfW * 2}
                  height={node.halfH * 2}
                  rx="6"
                  class="node-rect act-rect"
                />
              {:else}
                <rect
                  x={-node.halfW}
                  y={-node.halfH}
                  width={node.halfW * 2}
                  height={node.halfH * 2}
                  rx={node.halfH}
                  class="node-rect ent-rect"
                  class:is-file={node.isFile}
                />
              {/if}

              <!-- Node Label with Centered Monospace Typography -->
              <text class="node-label" class:backbone-label={node.type === 'backbone'} y="3.5" text-anchor="middle">{node.label}</text>
            </g>
          {/each}
        </g>
      </svg>
    </div>
  </div>

  <!-- Detail Bottom Sheet (Positioned off graph surface, on page) -->
  {#if selectedNode}
    <div class="node-detail-sheet">
      <div class="detail-head">
        <span class="detail-type">{selectedNode.type === 'act' ? 'Operation Statement' : 'Data Object / Artifact'}</span>
        <button class="close-btn" onclick={() => selectedNode = null}>✕</button>
      </div>
      <code class="detail-body">{selectedNode.detail}</code>
    </div>
  {/if}
</div>

<style>
  .graph-right-page {
    display: flex;
    flex-direction: column;
    height: 100%;
    gap: 0.5rem;
  }

  .graph-right-card {
    flex: 1;
    min-height: 0;
    position: relative;
    display: flex;
    flex-direction: column;
    background: var(--code-paper-bg);
    border: 1px solid var(--border);
    border-radius: 16px;
    overflow: hidden;
    isolation: isolate;
  }

  .graph-header {
    display: flex;
    justify-content: space-between;
    align-items: center;
    padding: 0.55rem 0.9rem;
    border-bottom: 1px solid var(--border);
    background: var(--glass-bg);
    backdrop-filter: blur(12px);
    -webkit-backdrop-filter: blur(12px);
    border-top-left-radius: 15px;
    border-top-right-radius: 15px;
  }

  .graph-title-group {
    display: flex;
    align-items: center;
    gap: 0.75rem;
  }

  .graph-title {
    font-family: var(--font-sans);
    font-size: 0.76rem;
    font-weight: 600;
    color: var(--text);
  }

  .dag-mode-toggle {
    display: flex;
    background: rgba(0, 0, 0, 0.05);
    padding: 2px;
    border-radius: 999px;
    border: 1px solid var(--border);
    gap: 2px;
  }

  .mode-toggle-btn {
    all: unset;
    cursor: pointer;
    font-family: var(--font-sans);
    font-size: 0.58rem;
    font-weight: 600;
    color: var(--text-secondary);
    padding: 0.15rem 0.5rem;
    border-radius: 999px;
    transition: all 0.15s ease;
  }

  .mode-toggle-btn.active {
    background: var(--pill-badge-bg, var(--text));
    color: var(--pill-badge-text, #ffffff);
    box-shadow: 0 1px 3px rgba(0, 0, 0, 0.12);
  }

  .mode-toggle-btn:hover:not(.active) {
    color: var(--text);
  }

  .graph-legend {
    display: flex;
    align-items: center;
    gap: 0.75rem;
    font-family: var(--font-sans);
    font-size: 0.64rem;
    color: var(--text-secondary);
  }

  .legend-item {
    display: flex;
    align-items: center;
    gap: 0.3rem;
  }

  .legend-symbol {
    width: 9px;
    height: 9px;
    display: inline-block;
  }

  .act-symbol {
    background: var(--code-paper-bg, var(--bg));
    border: 1.5px solid var(--accent);
    border-radius: 2px;
  }

  .ent-symbol {
    background: var(--code-paper-bg, var(--bg));
    border: 1.5px solid var(--border);
    border-radius: 9999px;
  }

  .file-symbol {
    background: rgba(16, 185, 129, 0.15);
    border: 1.5px solid #059669;
    border-radius: 9999px;
  }

  .graph-canvas-wrap {
    flex: 1;
    overflow: hidden;
    display: flex;
    align-items: center;
    justify-content: center;
    padding: 0.6rem 0.8rem;
    min-height: 0;
  }

  .graph-svg {
    width: 100%;
    height: 100%;
    max-height: 100%;
    display: block;
  }

  /* ── Wires & Signal Flow ── */
  .edge-path {
    stroke: var(--text-muted);
    stroke-width: 1.25;
    stroke-opacity: 0.38;
    fill: none;
    transition: stroke 0.2s, stroke-opacity 0.2s, stroke-width 0.2s;
  }

  .edge-path.active {
    stroke: var(--accent);
    stroke-width: 2;
    stroke-opacity: 1;
  }

  .edge-flow-pulse {
    stroke: var(--text-muted);
    stroke-width: 1.25;
    stroke-dasharray: 4 6;
    stroke-opacity: 0.45;
    fill: none;
    animation: flowPulse 20s linear infinite;
    pointer-events: none;
  }

  .edge-flow-pulse.active {
    stroke: var(--accent);
    stroke-width: 2;
    stroke-opacity: 0.85;
    animation: flowPulse 8s linear infinite;
  }

  @keyframes flowPulse {
    from { stroke-dashoffset: 200; }
    to { stroke-dashoffset: 0; }
  }

  .terminal-pin {
    fill: var(--text-muted);
    opacity: 0.5;
    transition: fill 0.2s, opacity 0.2s, r 0.2s;
  }

  .terminal-pin.active {
    fill: var(--accent);
    opacity: 1;
    r: 2.75;
  }

  /* ── Nodes & Typography (Opaque, never obscured) ── */
  .node-group {
    cursor: pointer;
  }

  .node-rect {
    transition: fill 0.15s, stroke 0.15s, stroke-width 0.15s;
  }

  .act-rect {
    fill: var(--code-paper-bg, var(--bg));
    stroke: var(--accent);
    stroke-width: 1.25;
  }

  .backbone-rect {
    fill: var(--glass-bg, var(--bg));
    stroke: var(--accent, #2563eb);
    stroke-width: 1.5;
    stroke-dasharray: 4 2;
    filter: drop-shadow(0 2px 6px rgba(37, 99, 235, 0.15));
  }

  .node-group:hover .backbone-rect,
  .node-group.hovered .backbone-rect {
    fill: var(--accent, #2563eb);
    stroke-width: 2;
    stroke-dasharray: none;
  }

  .node-group:hover .backbone-label,
  .node-group.hovered .backbone-label {
    fill: #ffffff;
    font-weight: 700;
  }

  .node-group:hover .act-rect,
  .node-group.hovered .act-rect {
    fill: var(--card-bg-hover, var(--glass-bg-hover));
    stroke-width: 2;
  }

  .ent-rect {
    fill: var(--code-paper-bg, var(--bg));
    stroke: var(--border);
    stroke-width: 1.25;
  }

  .node-group:hover .ent-rect,
  .node-group.hovered .ent-rect {
    fill: var(--card-bg-hover, var(--glass-bg-hover));
    stroke-width: 2;
  }

  .ent-rect.is-file {
    stroke: #059669;
    fill: rgba(16, 185, 129, 0.1);
    filter: drop-shadow(0 2px 6px rgba(16, 185, 129, 0.15));
  }

  .node-group.selected .node-rect {
    stroke: var(--text);
    stroke-width: 2.25;
  }

  .node-label {
    font-family: var(--font-mono, monospace);
    font-size: 0.62rem;
    fill: var(--text);
    pointer-events: none;
    user-select: none;
  }

  /* ── Detail Bottom Sheet (Off Graph Card, on Book Page Surface) ── */
  .node-detail-sheet {
    flex-shrink: 0;
    background: var(--glass-bg);
    backdrop-filter: blur(16px);
    -webkit-backdrop-filter: blur(16px);
    border: 1px solid var(--border);
    border-radius: 12px;
    padding: 0.55rem 0.8rem;
    box-shadow: var(--glass-shadow);
  }

  .detail-head {
    display: flex;
    justify-content: space-between;
    align-items: center;
    margin-bottom: 0.25rem;
  }

  .detail-type {
    font-family: var(--font-sans);
    font-size: 0.66rem;
    font-weight: 600;
    color: var(--text-muted);
    text-transform: uppercase;
  }

  .close-btn {
    all: unset;
    cursor: pointer;
    font-size: 0.75rem;
    color: var(--text-muted);
  }

  .detail-body {
    font-family: var(--font-mono, monospace);
    font-size: 0.7rem;
    color: var(--text);
    word-break: break-all;
  }
</style>
