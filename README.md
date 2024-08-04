# Ray tracing in a weekend

Implementing [https://raytracing.github.io/books/RayTracingInOneWeekend.html](https://raytracing.github.io/books/RayTracingInOneWeekend.html) in Haskell to see how slow it'll be.

Had to create my own ppm viewer, which is the index.html.

# Generate image

Using a justfile that's set to powershell (since I'm on Windows) to run the cabal script correctly. Takes forever to render the image. I should've measured the time, but took forever. And I'm using all the cores. The plus is that it was easy to parallelize. The minus is that it still takes forever, but at least not a lifetime.

```bash
just run
```

![Final render](./render.png)
