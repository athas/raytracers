defmodule Raytracer do
  def x({x, _y, _z}), do: x
  def y({_x, y, _z}), do: y
  def z({_x, _y, z}), do: z

  def scale({x, y, z}, s) do
    {x * s, y * s, z * s}
  end

  def norm(v), do: v |> dot(v) |> :math.sqrt()

  def normalise(v) do
    scale(v, 1.0 / norm(v))
  end

  def add({x1, y1, z1}, {x2, y2, z2}) do
    {x1 + x2, y1 + y2, z1 + z2}
  end

  def sub({x1, y1, z1}, {x2, y2, z2}) do
    {x1 - x2, y1 - y2, z1 - z2}
  end

  def mult({x1, y1, z1}, {x2, y2, z2}) do
    {x1 * x2, y1 * y2, z1 * z2}
  end

  def dot(v1, v2) do
    {x, y, z} = mult(v1, v2)
    x + y + z
  end

  def cross({x1, y1, z1}, {x2, y2, z2}) do
    {y1 * z2 - z1 * y2, z1 * x2 - x1 * z2, x1 * y2 - y1 * x2}
  end

  def reflect(v1, v2) do
    sub(v1, scale(v2, 2.0 * dot(v1, v2)))
  end

  def centre({{x1, y1, z1}, {x2, y2, z2}}) do
    {x1 + 0.5 * (x2 - x1), y1 + 0.5 * (y2 - y1), z1 + 0.5 * (z2 - z1)}
  end

  def hit(
        {{x1, y1, z1}, {x2, y2, z2}},
        {{x_origin, y_origin, z_origin}, {x_dir, y_dir, z_dir}},
        tmin0,
        tmax0
      ) do
    iter = fn min, max, origin, dir, tmin, tmax ->
      if dir == 0.0 do
        {tmin, tmax}
      else
        inv_d = 1.0 / dir
        t0 = (min - origin) * inv_d
        t1 = (max - origin) * inv_d
        {t0, t1} = if inv_d < 0.0, do: {t1, t0}, else: {t0, t1}
        tmin = max(t0, tmin)
        tmax = min(t1, tmax)
        {tmin, tmax}
      end
    end

    {tmin1, tmax1} = iter.(x1, x2, x_origin, x_dir, tmin0, tmax0)

    if tmax1 <= tmin1 do
      false
    else
      {tmin2, tmax2} = iter.(y1, y2, y_origin, y_dir, tmin1, tmax1)

      if tmax2 <= tmin2 do
        false
      else
        {tmin3, tmax3} = iter.(z1, z2, z_origin, z_dir, tmin2, tmax2)
        tmax3 > tmin3
      end
    end
  end

  def hit({:leaf, _, s}, r, tmin, tmax) do
    hit(s, r, tmin, tmax)
  end

  def hit({:split, b, left, right}, r, tmin, tmax) do
    if not hit(b, r, tmin, tmax) do
      nil
    else
      case hit(left, r, tmin, tmax) do
        {t, _, _, _} = h ->
          if h_ = hit(right, r, tmin, t) do
            h_
          else
            h
          end

        nil ->
          hit(right, r, tmin, tmax)
      end
    end
  end

  def hit({pos, colour, radius}, {origin, dir}, tmin, tmax) do
    oc = sub(origin, pos)
    a = dot(dir, dir)
    b = dot(oc, dir)
    c = dot(oc, oc) - radius * radius
    discriminant = b * b - a * c

    helper = fn temp ->
      if temp < tmax && temp > tmin do
        {temp, point_at_param({origin, dir}, temp),
         scale(sub(point_at_param({origin, dir}, temp), pos), 1.0 / radius), colour}
      else
        nil
      end
    end

    if discriminant <= 0.0 do
      nil
    else
      case helper.((-b - :math.sqrt(b * b - a * c)) / a) do
        nil -> helper.((-b + :math.sqrt(b * b - a * c)) / a)
        x -> x
      end
    end
  end

  def enclosing(
        {{x1, y1, z1}, {x2, y2, z2}},
        {{x3, y3, z3}, {x4, y4, z4}}
      ) do
    {{min(x1, x3), min(y1, y3), min(z1, z3)}, {max(x2, x4), max(y2, y4), max(z2, z4)}}
  end

  def aabb({:leaf, aabb, _}), do: aabb
  def aabb({:split, aabb, _, _}), do: aabb

  @black {0.0, 0.0, 0.0}
  @white {1.0, 1.0, 1.0}

  def point_at_param({origin, dir}, t) do
    add(origin, scale(dir, t))
  end

  def scatter({_origin, dir}, {_t, p, normal, colour}) do
    reflected =
      dir
      |> normalise
      |> reflect(normal)

    scattered = {p, reflected}

    if dot(reflected, normal) > 0.0 do
      {scattered, colour}
    else
      nil
    end
  end

  def colour({origin, dir}, objs, depth) do
    case hit(objs, {origin, dir}, 0.001, 1_000_000_000.0) do
      {_, _, _, _} = hit ->
        case scatter({origin, dir}, hit) do
          {scattered, attenuation} ->
            if depth < 50 do
              mult(attenuation, colour(scattered, objs, depth + 1))
            else
              @black
            end

          nil ->
            @black
        end

      nil ->
        unit_dir = normalise(dir)
        t = 0.5 * (y(unit_dir) + 1.0)
        bg = {0.5, 0.7, 1.0}
        add(scale(@white, 1.0 - t), scale(bg, t))
    end
  end

  def to_aabb({pos, _colour, radius}) do
    {sub(pos, {radius, radius, radius}), add(pos, {radius, radius, radius})}
  end

  def new(all_objs) do
    helper = fn
      _helper, _d, _n, [] ->
        raise "no nodes"

      _helper, _d, _n, [x] ->
        {:leaf, to_aabb(x), x}

      helper, d, n, xs ->
        xs =
          MergeSort.sort(xs, n, fn a, b ->
            {a, b} =
              case Integer.mod(d, 3) do
                0 -> {a |> to_aabb() |> centre() |> x(), b |> to_aabb() |> centre() |> x()}
                1 -> {a |> to_aabb() |> centre() |> y(), b |> to_aabb() |> centre() |> y()}
                _ -> {a |> to_aabb() |> centre() |> z(), b |> to_aabb() |> centre() |> z()}
              end

            a <= b
          end)

        {xs_left, xs_right} = Enum.split(xs, round(n / 2))

        [left, right] =
          if n < 100 do
            [helper.(helper, d + 1, n / 2, xs_left), helper.(helper, d + 1, n - n / 2, xs_right)]
          else
            [
              Task.async(fn -> helper.(helper, d + 1, n / 2, xs_left) end),
              Task.async(fn -> helper.(helper, d + 1, n - n / 2, xs_right) end)
            ]
            |> Task.await_many()
          end

        b = left |> aabb() |> enclosing(aabb(right))
        {:split, b, left, right}
    end

    helper.(helper, 0, length(all_objs), all_objs)
  end

  def new_camera(lookfrom, lookat, vup, vfov, aspect) do
    theta = vfov * :math.pi() / 180.0
    half_height = :math.tan(theta / 2.0)
    half_width = aspect * half_height
    w = sub(lookfrom, lookat) |> normalise()
    u = vup |> cross(w) |> normalise()
    v = cross(w, u)

    {lookfrom, lookfrom |> sub(scale(u, half_width)) |> sub(scale(v, half_height)) |> sub(w),
     scale(u, 2.0 * half_width), scale(v, 2.0 * half_height)}
  end

  def ray({origin, llc, horizontal, vertical}, s, t) do
    {origin,
     llc
     |> add(scale(horizontal, s))
     |> add(scale(vertical, t))
     |> sub(origin)}
  end

  def trace_ray(objs, width, height, cam, j, i) do
    u = i / width
    v = j / height

    cam
    |> ray(u, v)
    |> colour(objs, 0)
  end

  def colour_to_pixel({x, y, z}) do
    {floor(x * 255.99), floor(y * 255.99), floor(z * 255.99)}
  end

  def image_to_ppm(dev, {:image, pixels, height, width}) do
    IO.write(dev, ~s"""
    P3
    #{width} #{height}
    255
    """)

    for {r, g, b} <- pixels do
      IO.write(dev, "#{r} #{g} #{b}\n")
    end
  end

  def render(objs, width, height, cam) do
    pixel = fn l ->
      i = Integer.mod(l, width)
      j = height - l / width

      objs
      |> trace_ray(width, height, cam, j, i)
      |> colour_to_pixel()
    end

    pixels =
      0..(height * width)
      |> ParallelStream.map(pixel)
      |> Enum.to_list()

    {:image, pixels, height, width}
  end

  def from_scene(width, height, {cam_look_from, cam_look_at, cam_fov, spheres}) do
    {new(spheres),
     new_camera(cam_look_from, cam_look_at, {0.0, 1.0, 0.0}, cam_fov, width / height)}
  end

  def rgbbox() do
    n = 10
    k = 60

    leftwall =
      Enum.flat_map(0..n, fn y ->
        Enum.map(0..n, fn z ->
          {{-k / 2.0, -k / 2.0 + k / n * y, -k / 2.0 + k / n * z}, {1.0, 0.0, 0.0}, k / (n * 2.0)}
        end)
      end)

    midwall =
      Enum.flat_map(0..n, fn x ->
        Enum.map(0..n, fn y ->
          {{-k / 2.0 + k / n * x, -k / 2.0 + k / n * y, -k / 2.0}, {1.0, 1.0, 0.0}, k / (n * 2.0)}
        end)
      end)

    rightwall =
      Enum.flat_map(0..n, fn y ->
        Enum.map(0..n, fn z ->
          {{k / 2.0, -k / 2.0 + k / n * y, -k / 2.0 + k / n * z}, {0.0, 0.0, 1.0}, k / (n * 2.0)}
        end)
      end)

    bottom =
      Enum.flat_map(0..n, fn x ->
        Enum.map(0..n, fn z ->
          {{-k / 2.0 + k / n * x, -k / 2.0, -k / 2.0 + k / n * z}, {1.0, 1.0, 1.0}, k / (n * 2.0)}
        end)
      end)

    {{0.0, 30.0, 30.0}, {0.0, -1.0, -1.0}, 75.0,
     Enum.concat([leftwall, midwall, rightwall, bottom])}
  end

  def irreg() do
    n = 100
    k = 600

    spheres =
      Enum.flat_map(0..n, fn x ->
        Enum.map(0..n, fn z ->
          {{-k / 2.0 + k / n * x, 0.0, -k / 2.0 + k / n * z}, @white, k / (n * 2.0)}
        end)
      end)

    {{0.0, 12.0, 30.0}, {0.0, 10.0, -1.0}, 75.0, spheres}
  end

  def main(args \\ []) do
    {opts, scene, _} =
      OptionParser.parse(args,
        strict: [file_out: :string, height: :integer, width: :integer]
      )

    scene =
      case scene do
        ["irreg"] -> irreg()
        ["rgbbox"] -> rgbbox()
        [] -> rgbbox()
        _ -> raise "invalid scene"
      end

    width = Keyword.get(opts, :width, 200)
    height = Keyword.get(opts, :height, 200)

    {objs, cam} = from_scene(width, height, scene)

    result = render(objs, width, height, cam)

    dev =
      case Keyword.fetch(opts, :file_out) do
        {:ok, filename} ->
          File.open!(filename, [:write])

        :error ->
          :stdio
      end

    image_to_ppm(dev, result)
  end
end
