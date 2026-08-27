<script lang="ts">
  import { parseProvData } from '../../data/provParser';
  import scriptSource from '../../data/script_source.json';
  
  let { provData } = $props<{ provData: any }>();
  
  const summary = $derived(parseProvData(provData, scriptSource));
  let selectedStep = $state(0);
</script>

<div class="intro-right">
  <div class="header-row">
    <h3>Pipeline Milestones</h3>
    <span class="step-count">{summary.flowSteps.length} Stages</span>
  </div>
  <p class="ex-desc">Click an execution stage to review data flow from input variables to final outputs:</p>

  <div class="ex-list">
    {#each summary.flowSteps as step, idx}
      <!-- Liquid Glass Pill Card -->
      <button 
        class="glass-ex-card" 
        class:active={selectedStep === idx}
        onclick={() => selectedStep = idx}
      >
        <div class="card-head">
          <span class="ex-title">{step.title}</span>
          <span class="ex-cat">{step.category}</span>
        </div>
        <p class="step-desc">{step.description}</p>
        <div class="card-flow">
          {#if step.input}
            <span class="flow-tag input-tag">in: {step.input}</span>
            <span class="flow-arrow">→</span>
          {/if}
          <span class="flow-tag logic-tag">{step.logic}</span>
          {#if step.output}
            <span class="flow-arrow">→</span>
            <span class="flow-tag out-tag">out: {step.output}</span>
          {/if}
        </div>
      </button>
    {/each}

    {#if summary.flowSteps.length === 0}
      <div class="empty-box">
        <p>Execution captured. {summary.operationsCount} total operations recorded.</p>
      </div>
    {/if}
  </div>
</div>

<style>
  .intro-right {
    display: flex;
    flex-direction: column;
  }
  .header-row {
    display: flex;
    justify-content: space-between;
    align-items: center;
    margin-bottom: 0.1rem;
  }
  .intro-right h3 {
    margin: 0;
    color: var(--text);
  }
  .step-count {
    font-family: var(--font-mono, monospace);
    font-size: 0.72rem;
    font-weight: 600;
    color: var(--accent);
    padding: 0.1rem 0.4rem;
    background: var(--pill-bg, rgba(0, 0, 0, 0.05));
    border-radius: 9999px;
  }
  .ex-desc {
    font-size: 0.76rem;
    color: var(--text-secondary);
    margin-bottom: 0.5rem;
    margin-top: 0.2rem;
  }

  .ex-list {
    display: flex;
    flex-direction: column;
    gap: 0.35rem;
  }

  /* Liquid Glass Pill Card */
  .glass-ex-card {
    all: unset;
    cursor: pointer;
    display: flex;
    flex-direction: column;
    gap: 0.25rem;
    padding: 0.45rem 0.75rem;
    background: var(--glass-bg);
    backdrop-filter: blur(14px);
    -webkit-backdrop-filter: blur(14px);
    border: 1px solid var(--glass-border);
    border-radius: 14px;
    box-shadow: var(--glass-shadow);
    transition: all 0.2s cubic-bezier(0.16, 1, 0.3, 1);
  }

  .glass-ex-card:hover {
    transform: translateY(-1px);
    background: var(--glass-bg-hover);
  }

  .glass-ex-card.active {
    background: var(--card-bg-active);
    border-color: var(--card-border-active);
    box-shadow: 0 4px 12px -2px rgba(0, 0, 0, 0.06), inset 0 1px 1px rgba(255, 255, 255, 0.4);
  }

  .card-head {
    display: flex;
    justify-content: space-between;
    align-items: center;
  }

  .ex-title {
    font-family: var(--font-sans);
    font-size: 0.78rem;
    font-weight: 600;
    color: var(--text);
  }

  .ex-cat {
    font-family: var(--font-mono, monospace);
    font-size: 0.62rem;
    color: var(--text-muted);
    text-transform: uppercase;
  }

  .step-desc {
    font-family: var(--font-sans);
    font-size: 0.7rem;
    color: var(--text-secondary);
    margin: 0;
    line-height: 1.35;
  }

  .card-flow {
    display: flex;
    align-items: center;
    gap: 0.25rem;
    flex-wrap: wrap;
    font-family: var(--font-mono, monospace);
    font-size: 0.62rem;
    margin-top: 0.15rem;
  }

  .flow-tag {
    padding: 0.08rem 0.4rem;
    border-radius: 9999px;
    background: var(--pill-bg);
    color: var(--pill-text);
    border: 1px solid var(--border);
    max-width: 140px;
    overflow: hidden;
    text-overflow: ellipsis;
    white-space: nowrap;
  }

  .input-tag {
    background: rgba(74, 111, 165, 0.1);
    color: var(--accent);
    border-color: rgba(74, 111, 165, 0.25);
  }

  .logic-tag {
    color: var(--text);
    font-weight: 500;
  }

  .out-tag {
    display: inline-flex;
    align-items: center;
    justify-content: center;
    line-height: 1;
    font-weight: 600;
    padding: 0.16rem 0.5rem;
    background: rgba(16, 185, 129, 0.1);
    color: var(--text);
    border: 1px solid #059669;
    border-radius: 9999px;
    box-shadow: 0 1px 3px rgba(0, 0, 0, 0.04);
  }

  .flow-arrow {
    color: var(--text-muted);
    font-size: 0.65rem;
  }

  .empty-box {
    padding: 1rem;
    text-align: center;
    font-size: 0.75rem;
    color: var(--text-muted);
    background: var(--glass-bg);
    border-radius: 12px;
  }
</style>
