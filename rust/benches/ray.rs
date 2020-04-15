use criterion::{black_box, criterion_group, criterion_main, Criterion};
use ray;

pub fn irreg_benchmark(c: &mut Criterion) {
    let mut group = c.benchmark_group("irreg");

    group.sample_size(10);

    let irreg_scene = (*ray::sample_scenes::IRREG).clone();

    group.bench_function("from_scene", |b| {
        b.iter(|| ray::from_scene(1000, 1000, black_box(&irreg_scene)))
    });

    let (objs, cam) = ray::from_scene(1000, 1000, &irreg_scene);

    group.bench_function("render", |b| {
        b.iter(|| ray::render(black_box(&objs), 1000, 1000, black_box(&cam)))
    });
}

pub fn rgbbox_benchmark(c: &mut Criterion) {
    let mut group = c.benchmark_group("rgbbox");

    group.sample_size(10);

    let irreg_scene = (*ray::sample_scenes::RGBBOX).clone();

    group.bench_function("from_scene", |b| {
        b.iter(|| ray::from_scene(1000, 1000, black_box(&irreg_scene)))
    });

    let (objs, cam) = ray::from_scene(1000, 1000, &irreg_scene);

    group.bench_function("render", |b| {
        b.iter(|| ray::render(black_box(&objs), 1000, 1000, black_box(&cam)))
    });
}

criterion_group!(benches, irreg_benchmark);
criterion_main!(benches);
