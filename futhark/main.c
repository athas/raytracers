#include <getopt.h>
#include <stdio.h>
#include <assert.h>
#include <string.h>
#include <sys/time.h>

#include "ray.h"

void ppm_to_file(const char *filename, int *pixels, int height, int width) {
  FILE *file = fopen(filename, "w");
  assert(file != NULL);

  fprintf(file, "P3\n%d %d\n255\n", width, height);

  for (int i = 0; i < height*width; i++) {
    fprintf(file, "%d %d %d\n",
            (pixels[i]>>16)&0xFF,
            (pixels[i]>>8)&0xFF,
            (pixels[i])&0xFF);
  }

  fclose(file);
}

int main(int argc, char** argv) {
  int height = 200;
  int width = 200;
  char *imgfile = NULL;
  char *scene_name = "rgbbox";
  int runs = 10;

  int opt;
  while ((opt = getopt(argc, argv, "m:n:f:s:r:")) != -1) {
    switch (opt) {
    case 'n':
      height = atoi(optarg);
      break;
    case 'm':
      width = atoi(optarg);
      break;
    case 'f':
      imgfile = optarg;
      break;
    case 's':
      scene_name = optarg;
      break;
    case 'r':
      runs = 10;
      break;
    default: /* '?' */
      fprintf(stderr,
              "Usage: %s [-m height] [-n width] [-s scene] [-f FILE.ppm]\n",
              argv[0]);
      exit(EXIT_FAILURE);
    }
  }

  struct futhark_context_config *cfg = futhark_context_config_new();
  assert(cfg != NULL);
  struct futhark_context *ctx = futhark_context_new(cfg);
  assert(ctx != NULL);

  int ret;
  struct futhark_opaque_scene *scene;
  struct futhark_opaque_prepared_scene *prepared_scene = NULL;
  struct futhark_i32_2d *img = NULL;
  struct timeval t_start, t_end;

  if (strcmp(scene_name, "rgbbox") == 0) {
    ret = futhark_entry_rgbbox(ctx, &scene);
    assert(ret == 0);
  } else if (strcmp(scene_name, "irreg") == 0) {
    futhark_entry_irreg(ctx, &scene);
    assert(ret == 0);
  } else {
    fprintf(stderr, "Unknown scene: %s\n", scene_name);
    fprintf(stderr, "Known scenes: rgbbox, irreg\n");
  }
  assert(scene != NULL);

  printf("Using scene %s (-s to switch).\n", scene_name);
  printf("Timing over average of %d runs (-r to change).\n", runs);

  gettimeofday(&t_start, NULL);
  for (int i = 0; i < runs; i++) {
    if (prepared_scene != NULL) {
      futhark_free_opaque_prepared_scene(ctx, prepared_scene);
    }

    ret = futhark_entry_prepare_scene(ctx,
                                      &prepared_scene,
                                      height, width, scene);
    assert(ret == 0);
    ret = futhark_context_sync(ctx);
    assert(ret == 0);
  }
  gettimeofday(&t_end, NULL);

  printf("Scene BVH construction in %fs.\n",
         ((t_end.tv_sec+t_end.tv_usec/1000000.0) -
          (t_start.tv_sec+t_start.tv_usec/1000000.0))/runs);

  gettimeofday(&t_start, NULL);
  for (int i = 0; i < runs; i++) {
    if (img != NULL) {
      futhark_free_i32_2d(ctx, img);
    }

    ret = futhark_entry_render(ctx,
                               &img,
                               height, width, prepared_scene);
    assert(ret == 0);
    ret = futhark_context_sync(ctx);
    assert(ret == 0);
  }
  gettimeofday(&t_end, NULL);

  printf("Rendering in %fs.\n",
         ((t_end.tv_sec+t_end.tv_usec/1000000.0) -
          (t_start.tv_sec+t_start.tv_usec/1000000.0))/runs);

  if (imgfile == NULL) {
    printf("-f not passed, so not writing image to file\n");
  } else {
    int *img_host = malloc(sizeof(int) * height * width);
    ret = futhark_values_i32_2d(ctx, img, img_host);
    assert(ret == 0);
    printf("Writing image to %s.\n", imgfile);
    ppm_to_file(imgfile, img_host, height, width);
    free(img_host);
  }

  futhark_free_i32_2d(ctx, img);
  futhark_free_opaque_prepared_scene(ctx, prepared_scene);
  futhark_free_opaque_scene(ctx, scene);
  futhark_context_free(ctx);
  futhark_context_config_free(cfg);
}
