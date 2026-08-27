<script lang="ts">
  import { activeScriptState, annotationsPanelState, annotationsState, currentScriptMeta } from '../state.svelte';
  
  let newComment = $state('');

  const activeMeta = $derived(currentScriptMeta());
  
  // Get comments for current script, fallback to empty array
  const currentComments = $derived.by(() => {
    if (!activeMeta) return [];
    return annotationsState[activeMeta.id] || [];
  });

  function addComment(e: Event) {
    e.preventDefault();
    if (!newComment.trim() || !activeMeta) return;

    if (!annotationsState[activeMeta.id]) {
      annotationsState[activeMeta.id] = [];
    }

    annotationsState[activeMeta.id].push({
      text: newComment.trim(),
      timestamp: Date.now()
    });

    newComment = '';
  }

  function formatTime(ts: number) {
    const d = new Date(ts);
    return d.toLocaleTimeString([], { hour: '2-digit', minute: '2-digit' });
  }
</script>



<!-- Popover Modal -->
{#if annotationsPanelState.isOpen}
  <!-- svelte-ignore a11y_click_events_have_key_events -->
  <!-- svelte-ignore a11y_no_static_element_interactions -->
  <div class="anno-backdrop" onclick={() => annotationsPanelState.isOpen = false}></div>
  <div class="anno-popover annotations-panel">
    <div class="popover-header">
      <h3>Annotations</h3>
      <button class="close-btn" onclick={() => annotationsPanelState.isOpen = false}>✕</button>
    </div>

    {#if activeMeta}
      <p class="context-info">Commenting on: <strong>{activeMeta.name}</strong></p>
    {/if}
    
    <div class="comments-list">
      {#each currentComments as comment}
        <div class="comment-bubble">
          <p>{comment.text}</p>
          <span class="time">{formatTime(comment.timestamp)}</span>
        </div>
      {/each}
      {#if currentComments.length === 0}
        <p class="empty-msg">No annotations yet for this script.</p>
      {/if}
    </div>

    <form class="comment-input-area" onsubmit={addComment}>
      <input 
        type="text" 
        bind:value={newComment} 
        placeholder="Leave a note..."
      />
      <button type="submit" disabled={!newComment.trim()}>Add</button>
    </form>
  </div>
{/if}

<style>


  .badge {
    position: absolute;
    top: -4px;
    right: -4px;
    background: #1a1a24;
    color: white;
    font-size: 0.65rem;
    font-weight: 700;
    font-family: var(--font-mono, monospace);
    width: 18px;
    height: 18px;
    display: flex;
    align-items: center;
    justify-content: center;
    border-radius: 50%;
    box-shadow: 0 2px 4px rgba(0,0,0,0.2);
  }

  .anno-backdrop {
    position: fixed;
    inset: 0;
    z-index: 998;
  }

  .anno-popover {
    position: fixed;
    bottom: 6rem;
    left: 50%;
    transform: translateX(-50%);
    z-index: 999;
    width: 340px;
    max-height: 70vh;
    display: flex;
    flex-direction: column;
    background: var(--popover-bg, #faf9f5);
    background-color: var(--popover-bg, #faf9f5);
    border: 1px solid var(--popover-border, rgba(0, 0, 0, 0.14));
    border-radius: 20px;
    box-shadow: var(--popover-shadow, 0 24px 60px -8px rgba(0, 0, 0, 0.25));
    animation: slideUpCenter 0.3s cubic-bezier(0.16, 1, 0.3, 1) forwards;
  }

  @keyframes slideUpCenter {
    from { opacity: 0; transform: translate(-50%, 16px) scale(0.96); }
    to { opacity: 1; transform: translate(-50%, 0) scale(1); }
  }

  .popover-header {
    display: flex;
    justify-content: space-between;
    align-items: center;
    padding: 1.25rem 1.25rem 0.75rem;
    border-bottom: 1px solid var(--border, rgba(0, 0, 0, 0.05));
  }
  .popover-header h3 {
    margin: 0;
    font-size: 1.05rem;
    font-weight: 600;
    font-family: var(--font-sans);
    color: var(--text);
  }
  .close-btn {
    all: unset;
    cursor: pointer;
    color: var(--text-muted, #8e8e9e);
    font-size: 1rem;
    padding: 0.25rem;
  }
  .close-btn:hover { color: var(--text, #1a1a24); }

  .context-info {
    margin: 0;
    padding: 0.5rem 1.25rem;
    font-size: 0.75rem;
    background: var(--pill-bg, rgba(0,0,0,0.03));
    font-family: var(--font-sans);
    color: var(--text-secondary, #4a4a5a);
  }
  .context-info strong {
    font-family: var(--font-mono, monospace);
  }

  .comments-list {
    flex: 1;
    overflow-y: auto;
    padding: 1rem 1.25rem;
    display: flex;
    flex-direction: column;
    gap: 0.75rem;
  }

  .comment-bubble {
    background: var(--card-bg-active, #ffffff);
    border: 1px solid var(--card-border, rgba(0,0,0,0.06));
    padding: 0.75rem 1rem;
    border-radius: 16px;
    border-bottom-left-radius: 4px;
    box-shadow: 0 2px 8px rgba(0,0,0,0.02);
  }
  .comment-bubble p {
    margin: 0 0 0.4rem;
    font-size: 0.85rem;
    font-family: var(--font-sans);
    line-height: 1.4;
    color: var(--text, #1a1a24);
  }
  .comment-bubble .time {
    font-size: 0.65rem;
    color: var(--text-muted, #8e8e9e);
    font-family: var(--font-mono, monospace);
  }

  .empty-msg {
    color: var(--text-muted, #8e8e9e);
    font-style: italic;
    font-size: 0.85rem;
    text-align: center;
    margin-top: 1rem;
  }

  .comment-input-area {
    display: flex;
    padding: 1rem 1.25rem;
    border-top: 1px solid var(--border, rgba(0,0,0,0.05));
    background: var(--pill-bg, rgba(255,255,255,0.4));
    border-bottom-left-radius: 20px;
    border-bottom-right-radius: 20px;
    gap: 0.5rem;
  }
  
  .comment-input-area input {
    flex: 1;
    border: 1px solid var(--border, rgba(0,0,0,0.1));
    border-radius: 9999px;
    padding: 0.5rem 1rem;
    font-size: 0.85rem;
    font-family: var(--font-sans);
    background: var(--card-bg, #ffffff);
    color: var(--text, #1a1a24);
    outline: none;
    transition: border-color 0.2s;
  }
  .comment-input-area input:focus {
    border-color: var(--accent);
  }
  
  .comment-input-area button {
    all: unset;
    cursor: pointer;
    background: var(--pill-badge-bg, #1a1a24);
    color: var(--pill-badge-text, white);
    font-size: 0.8rem;
    font-weight: 600;
    font-family: var(--font-sans);
    padding: 0 1.25rem;
    border-radius: 9999px;
    transition: opacity 0.2s;
  }
  .comment-input-area button:disabled {
    opacity: 0.5;
    cursor: not-allowed;
  }
</style>
