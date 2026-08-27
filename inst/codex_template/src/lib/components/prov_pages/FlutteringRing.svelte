<script lang="ts">
  import { onMount } from 'svelte';

  let containerRef: HTMLDivElement;
  let canvasRef: HTMLCanvasElement;

  function createNoise2D() {
    const p = new Uint8Array(256);
    const seed = [
      151,160,137,91,90,15,131,13,201,95,96,53,194,233,7,225,140,36,103,30,69,142,
      8,99,37,240,21,10,23,190,6,148,247,120,234,75,0,26,197,62,94,252,219,203,117,
      35,11,32,57,177,33,88,237,149,56,87,174,20,125,136,171,168,68,175,74,165,71,
      134,139,48,27,166,77,146,158,231,83,111,229,122,60,211,133,230,220,105,92,41,
      55,46,245,40,244,102,143,54,65,25,63,161,1,216,80,73,209,76,132,187,208,89,
      18,169,200,196,135,130,116,188,159,86,164,100,109,198,173,186,3,64,52,217,226,
      250,124,123,5,202,38,147,118,126,255,82,85,212,207,206,59,227,47,16,58,17,182,
      189,28,42,223,183,170,213,119,248,152,2,44,154,163,70,221,153,101,155,167,43,
      172,9,129,22,39,253,19,98,108,110,79,113,224,232,178,185,115,165,193,34,106,
      138,184,204,181,31,180,195,112,84,179,162,18,150,238,249,104,242,127,215,218,
      176,155,67,192,241,51,214,121,97,49,61,66,128,160,236,114,243,24,199,246,39,
      157,175,191,228,141,121
    ];
    for (let i = 0; i < 256; i++) p[i] = seed[i % seed.length];

    const perm = new Uint8Array(512);
    const gradP = new Float64Array(512 * 2);
    for (let i = 0; i < 512; i++) {
      perm[i] = p[i & 255];
      const a = (perm[i] / 512) * Math.PI * 2;
      gradP[i * 2] = Math.cos(a);
      gradP[i * 2 + 1] = Math.sin(a);
    }
    const fade = (t: number) => t * t * t * (t * (t * 6 - 15) + 10);
    const lerp = (a: number, b: number, t: number) => a + t * (b - a);

    return function noise(x: number, y: number): number {
      const X = Math.floor(x) & 255;
      const Y = Math.floor(y) & 255;
      x -= Math.floor(x);
      y -= Math.floor(y);
      const u = fade(x);
      const v = fade(y);
      const n00 = gradP[(X + perm[Y]) * 2] * x + gradP[(X + perm[Y]) * 2 + 1] * y;
      const n01 = gradP[(X + perm[Y + 1]) * 2] * x + gradP[(X + perm[Y + 1]) * 2 + 1] * (y - 1);
      const n10 = gradP[(X + 1 + perm[Y]) * 2] * (x - 1) + gradP[(X + 1 + perm[Y]) * 2 + 1] * y;
      const n11 = gradP[(X + 1 + perm[Y + 1]) * 2] * (x - 1) + gradP[(X + 1 + perm[Y + 1]) * 2 + 1] * (y - 1);
      return (lerp(lerp(n00, n10, u), lerp(n01, n11, u), v) + 1) * 0.5;
    };
  }

  function drawCatmullRom(ctx: CanvasRenderingContext2D, pts: { x: number; y: number }[]) {
    if (pts.length < 3) return;
    ctx.beginPath();
    ctx.moveTo(pts[0].x, pts[0].y);

    const len = pts.length;
    for (let i = 0; i < len; i++) {
      const p0 = pts[(i - 1 + len) % len];
      const p1 = pts[i];
      const p2 = pts[(i + 1) % len];
      const p3 = pts[(i + 2) % len];

      const cp1x = p1.x + (p2.x - p0.x) / 6;
      const cp1y = p1.y + (p2.y - p0.y) / 6;
      const cp2x = p2.x - (p3.x - p1.x) / 6;
      const cp2y = p2.y - (p3.y - p1.y) / 6;

      ctx.bezierCurveTo(cp1x, cp1y, cp2x, cp2y, p2.x, p2.y);
    }
    ctx.closePath();
  }

  onMount(() => {
    if (!canvasRef || !containerRef) return;
    const ctx = canvasRef.getContext('2d');
    if (!ctx) return;

    const noise = createNoise2D();
    let animId: number;

    const points: { x: number; y: number }[] = [];
    const maxPoints = 22; // Reduced layer count to expose distinct paper sheets clearly
    let noiseT = 0.0;

    function resize() {
      if (!containerRef || !canvasRef) return;
      const rect = containerRef.getBoundingClientRect();
      const dpr = Math.min(window.devicePixelRatio || 1, 2.5);
      
      canvasRef.width = Math.floor(rect.width * dpr);
      canvasRef.height = Math.floor(rect.height * dpr);
      canvasRef.style.width = `${rect.width}px`;
      canvasRef.style.height = `${rect.height}px`;
    }

    resize();
    const ro = new ResizeObserver(() => resize());
    ro.observe(containerRef);

    function render() {
      if (!canvasRef || !ctx) return;

      const dpr = Math.min(window.devicePixelRatio || 1, 2.5);
      const w = canvasRef.width / dpr;
      const h = canvasRef.height / dpr;

      ctx.save();
      ctx.scale(dpr, dpr);
      ctx.clearRect(0, 0, w, h);

      // Read theme colors
      const comp = getComputedStyle(canvasRef);
      const strokeColor = comp.getPropertyValue('--text').trim() || '#355070';
      const pageBg = comp.getPropertyValue('--page-bg').trim() || '#f7f6f1';

      // Larger radius so it comfortably engulfs the script name title in the center
      const baseRadius = Math.min(w, h) * 0.44;

      ctx.save();
      ctx.translate(w / 2, h / 2 - 10);

      for (let i = 0; i < points.length; i++) {
        const pos = points[i];

        ctx.save();
        ctx.translate(pos.x, pos.y);
        ctx.rotate((i * 1.8 * Math.PI) / 180);

        // Smooth curve vertices matching @takawo flutter math
        const shapePts: { x: number; y: number }[] = [];
        for (let deg = 0; deg < 360; deg += 10) {
          const rad = (deg * Math.PI) / 180;
          const nn = noise(
            noiseT + baseRadius * (deg / 100 * 0.005),
            noiseT + baseRadius * (deg / 100 * 0.005)
          );
          const ex = baseRadius * Math.sin(rad) * nn;
          const ey = baseRadius * Math.cos(rad) * nn;
          shapePts.push({ x: ex, y: ey });
        }

        // Lighter stroke & opaque page fill so each layer stands out cleanly
        ctx.fillStyle = pageBg;
        ctx.strokeStyle = strokeColor;
        ctx.globalAlpha = Math.min(0.5, 0.18 + (i / points.length) * 0.35);
        ctx.lineWidth = 1.0;

        drawCatmullRom(ctx, shapePts);
        ctx.fill();
        ctx.stroke();

        ctx.restore();
        noiseT += 0.0001;
      }

      points.push({ x: 0, y: 0 });
      ctx.restore();

      // Increased vertical drift spacing to fan out individual layers clearly
      for (let i = 0; i < points.length; i++) {
        points[i].y += 2.2;
      }

      if (points.length > maxPoints) {
        points.shift();
      }

      ctx.restore();
      animId = requestAnimationFrame(render);
    }

    render();

    return () => {
      ro.disconnect();
      if (animId) cancelAnimationFrame(animId);
    };
  });
</script>

<div class="ring-container" bind:this={containerRef}>
  <canvas bind:this={canvasRef}></canvas>
</div>

<style>
  .ring-container {
    width: 100%;
    height: 100%;
    position: absolute;
    inset: 0;
    display: flex;
    align-items: center;
    justify-content: center;
    pointer-events: none;
  }

  canvas {
    display: block;
    pointer-events: none;
  }
</style>
