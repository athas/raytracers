alias Raytracer

{rgbbox_objs, rgbbox_cam} = Raytracer.from_scene(1000, 1000, Raytracer.rgbbox())
{irreg_objs, irreg_cam} = Raytracer.from_scene(1000, 1000, Raytracer.irreg())

Benchee.run(
  %{
    "rgbbox" => fn -> Raytracer.render(rgbbox_objs, 1000, 1000, rgbbox_cam) end,
    "irreg" => fn -> Raytracer.render(irreg_objs, 1000, 1000, irreg_cam) end,
  },
  time: 10,
  memory_time: 2
)
