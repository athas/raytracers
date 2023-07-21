defmodule MergeSort do
  def sort([], _, _), do: []
  def sort([x], _, _), do: [x]

  def sort(xs, n, lt?) do
    if n < 100 do
      Enum.sort(xs, lt?)
    else
      {left, right} = Enum.split(xs, round(n / 2))

      [left, right] =
        [
          Task.async(fn -> sort(left, ceil(n / 2), lt?) end),
          Task.async(fn -> sort(right, floor(n / 2), lt?) end)
        ]
        |> Task.await_many()

      merge(left, right, lt?)
    end
  end

  def merge([], yx, _lt?), do: yx
  def merge(xs, [], _lt?), do: xs

  def merge([x | xs] = xs0, [y | ys] = ys0, lt?) do
    if lt?.(x, y) do
      [x | merge(xs, ys0, lt?)]
    else
      [y | merge(xs0, ys, lt?)]
    end
  end
end
