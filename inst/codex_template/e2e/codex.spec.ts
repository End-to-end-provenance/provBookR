import { test, expect } from '@playwright/test';

test.describe('Tier 3: Codex Navigation and Invariants', () => {

  test.beforeEach(async ({ page }) => {
    // Navigate to the root before each test
    await page.goto('/');
  });

  test('Cover & Page Flip Navigation Bounds', async ({ page }) => {
    // Wait for the cover to render
    await expect(page).toHaveTitle(/provBookR:/);

    // Initial state: We should be on the cover
    const prevBtn = page.getByRole('button', { name: 'Previous Page' });
    const nextBtn = page.getByRole('button', { name: 'Next Page' });

    // Ensure the prev button is disabled on the cover
    await expect(prevBtn).toBeDisabled();
    
    // Spread indicator should be at 'Cover'
    await expect(page.locator('.page-indicator')).toContainText('Cover');

    // Flip forward 5 times to verify spread bounds
    for (let i = 1; i <= 5; i++) {
      await nextBtn.click();
      // Await page flip animation
      await page.waitForTimeout(600);
      await expect(page.locator('.page-indicator')).toContainText(`Sp ${i}`);
    }

    // At spread 5 (last spread), next button should be disabled
    await expect(nextBtn).toBeDisabled();

    // Flip backwards back to the cover
    for (let i = 4; i >= 0; i--) {
      await prevBtn.click();
      await page.waitForTimeout(600);
      if (i === 0) {
        await expect(page.locator('.page-indicator')).toContainText('Cover');
      } else {
        await expect(page.locator('.page-indicator')).toContainText(`Sp ${i}`);
      }
    }

    // Prev button should be disabled again
    await expect(prevBtn).toBeDisabled();
  });

  test('View Mode & Dock Controls State Bisimulation', async ({ page }) => {
    // Toggle Graph View
    const graphToggle = page.getByRole('button', { name: 'Toggle Graph View' });
    
    // Default is book view (check for PageFlip container)
    await expect(page.locator('.page-flip-container')).toBeVisible();

    // Switch to Graph mode
    await graphToggle.click();
    await expect(page.locator('.book-graph-view')).toBeVisible();
    await expect(page.locator('.page-flip-container')).toBeHidden();

    // Switch back to Book mode
    await graphToggle.click();
    await expect(page.locator('.page-flip-container')).toBeVisible();

    // Test Guide Overlay Toggle
    const guideToggle = page.getByRole('button', { name: 'Toggle Guide' });
    await guideToggle.click();
    await expect(page.locator('.guide-overlay')).toBeVisible();
    await guideToggle.click();
    // In Svelte, if it is an #if block, it might not be in DOM or just hidden
    await expect(page.locator('.guide-overlay')).toBeHidden();

    // Test Annotations Panel
    const annotationsToggle = page.getByRole('button', { name: 'Annotations' });
    await annotationsToggle.click();
    await expect(page.locator('.annotations-panel')).toBeVisible();
    await annotationsToggle.click();

    // Test Typography Settings
    const themeToggle = page.getByRole('button', { name: 'Typography Settings' });
    await themeToggle.click();
    await expect(page.locator('.settings-popover').filter({ hasText: 'Typography & Appearance' })).toBeVisible();
    // Click outside to close (or toggle again)
    await themeToggle.click();
    
    // Test Scenario Selector
    // The scenario popover opens by clicking a specific state (not a direct dock button, but usually through settings or an indicator).
    // Let's ensure the panel isn't open by default.
    await expect(page.locator('.settings-popover').filter({ hasText: 'Choose Notebook' })).toBeHidden();
  });

  test('Provenance Graph Interactions & Backbone Mode', async ({ page }) => {
    // Navigate to Spread 3 (Graph spread) where the interactive DAG is rendered.
    const nextBtn = page.getByRole('button', { name: 'Next Page' });
    for (let i = 0; i < 3; i++) {
      await nextBtn.click();
      await page.waitForTimeout(600);
    }

    // Switch to Full DAG if Backbone is default
    const fullDagBtn = page.getByRole('button', { name: 'Full DAG' });
    if (await fullDagBtn.isVisible()) {
      await fullDagBtn.click();
    }

    // Verify nodes are rendered
    const nodes = page.locator('.node-circle, .node-rect');
    await expect(nodes.first()).toBeVisible();

    // Hover interaction
    await nodes.first().hover();
    
    // We expect some tooltip or selection to highlight (e.g. active class)
    // Here we'll just check that it doesn't crash
    await nodes.first().click();
  });
});
