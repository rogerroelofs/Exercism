defmodule JigsawPuzzle do
  @doc """
  Fill in missing jigsaw puzzle details from partial data
  """

  @type format() :: :landscape | :portrait | :square
  @type t() :: %__MODULE__{
          pieces: pos_integer() | nil,
          rows: pos_integer() | nil,
          columns: pos_integer() | nil,
          format: format() | nil,
          aspect_ratio: float() | nil,
          border: pos_integer() | nil,
          inside: pos_integer() | nil
        }

  defstruct [:pieces, :rows, :columns, :format, :aspect_ratio, :border, :inside]

  @spec data(jigsaw_puzzle :: JigsawPuzzle.t()) ::
          {:ok, JigsawPuzzle.t()} | {:error, String.t()}
  def data(jigsaw_puzzle) do
    filled = fill_in(jigsaw_puzzle)

    if not consistent?(filled) do
      {:error, "Contradictory data"}
    else
      if valid?(filled) and all_critical_fields_filled?(filled) do
        {:ok, filled}
      else
        {:error, "Insufficient data"}
      end
    end
  end

  defp all_critical_fields_filled?(jp),
    do:
      Enum.all?(
        [:pieces, :rows, :columns, :format, :aspect_ratio, :border, :inside],
        &(!is_nil(Map.get(jp, &1)))
      )

  defp valid?(jp),
    do:
      Enum.count(
        [jp.pieces, jp.rows, jp.columns, jp.format, jp.aspect_ratio, jp.border, jp.inside],
        &(!is_nil(&1))
      ) >= 2 and consistent?(jp)

  defp consistent?(jp) do
    if Enum.all?(
         [:pieces, :rows, :columns, :format, :aspect_ratio, :border, :inside],
         &(!is_nil(Map.get(jp, &1)))
       ) do
      with false <- jp.pieces != jp.rows * jp.columns,
           false <- jp.format == :landscape and jp.aspect_ratio <= 1.0,
           false <- jp.format == :portrait and jp.aspect_ratio >= 1.0,
           false <- jp.format == :square and (jp.aspect_ratio != 1.0 or jp.rows != jp.columns) do
        true
      else
        _ -> false
      end
    else
      true
    end
  end

  defp fill_in(%JigsawPuzzle{} = jp) do
    new_jp =
      jp
      |> fill_in_pieces_rows_columns()
      |> fill_in_rows_and_columns_from_pieces_aspect()
      |> fill_in_rows_and_columns_from_pieces_border_format()
      |> fill_in_columns_from_rows_aspect()
      |> fill_in_aspect_ratio_and_format()
      |> fill_in_format()
      |> fill_in_border_and_inside()

    if new_jp == jp, do: jp, else: fill_in(new_jp)
  end

  # Deduce rows and columns for portrait/landscape puzzles with pieces, border, and format
  defp fill_in_rows_and_columns_from_pieces_border_format(
         %JigsawPuzzle{rows: nil, columns: nil, pieces: p, border: b, format: f} = jp
       )
       when not is_nil(p) and not is_nil(b) and not is_nil(f) and p > b do
    # Try all possible pairs - border = 2r + 2c - 4, pieces = r * c
    candidates =
      for r <- 1..p, c <- 1..p, r * c == p, 2 * r + 2 * c - 4 == b, do: {r, c}

    {rows, columns} =
      case f do
        :portrait ->
          candidates
          |> Enum.filter(fn {r, c} -> r > c end)
          |> Enum.min_by(fn {_r, c} -> c end, fn -> {nil, nil} end)

        :landscape ->
          candidates
          |> Enum.filter(fn {r, c} -> c > r end)
          |> Enum.max_by(fn {_r, c} -> c end, fn -> {nil, nil} end)

        _ ->
          {nil, nil}
      end

    jp = %{jp | rows: rows, columns: columns}

    if not is_nil(rows) and not is_nil(columns) and columns != 0 do
      aspect_ratio =
        case f do
          :portrait -> columns / rows
          _ -> rows / columns
        end

      %{jp | aspect_ratio: aspect_ratio}
    else
      jp
    end
  end

  defp fill_in_rows_and_columns_from_pieces_border_format(jp), do: jp

  # Deduce columns, pieces, border, inside for landscape puzzle with rows and aspect_ratio
  defp fill_in_columns_from_rows_aspect(
         %JigsawPuzzle{
           columns: nil,
           pieces: nil,
           border: nil,
           inside: nil,
           rows: r,
           aspect_ratio: ar,
           format: :landscape
         } = jp
       )
       when not is_nil(r) and not is_nil(ar) and ar > 1.0 do
    columns = round(r * ar)

    if columns > 0 do
      pieces = r * columns
      border = 2 * r + 2 * columns - 4
      inside = pieces - border
      %{jp | columns: columns, pieces: pieces, border: border, inside: inside}
    else
      jp
    end
  end

  defp fill_in_columns_from_rows_aspect(jp), do: jp

  # Fill pieces, rows, columns if any two are known
  defp fill_in_pieces_rows_columns(%JigsawPuzzle{pieces: nil, rows: r, columns: c} = jp)
       when not is_nil(r) and not is_nil(c) do
    %{jp | pieces: r * c}
  end

  defp fill_in_pieces_rows_columns(%JigsawPuzzle{rows: nil, pieces: p, columns: c} = jp)
       when not is_nil(p) and not is_nil(c) and c != 0 and rem(p, c) == 0 do
    %{jp | rows: div(p, c)}
  end

  defp fill_in_pieces_rows_columns(%JigsawPuzzle{columns: nil, pieces: p, rows: r} = jp)
       when not is_nil(p) and not is_nil(r) and r != 0 and rem(p, r) == 0 do
    %{jp | columns: div(p, r)}
  end

  defp fill_in_pieces_rows_columns(
         %JigsawPuzzle{pieces: nil, rows: r, columns: nil, format: :square} = jp
       )
       when not is_nil(r) do
    %{jp | columns: r, pieces: r * r}
  end

  defp fill_in_pieces_rows_columns(
         %JigsawPuzzle{pieces: nil, rows: nil, columns: c, format: :square} = jp
       )
       when not is_nil(c) do
    %{jp | rows: c, pieces: c * c}
  end

  defp fill_in_pieces_rows_columns(jp), do: jp

  # Fill rows and columns from pieces and aspect ratio, including square puzzle inside/aspect special case
  defp fill_in_rows_and_columns_from_pieces_aspect(
         %JigsawPuzzle{rows: nil, columns: nil, pieces: nil, aspect_ratio: 1.0, inside: i} = jp
       )
       when not is_nil(i) do
    # For a square puzzle, inside = (n-2)^2, so n = trunc(:math.sqrt(i)) + 2
    n = trunc(:math.sqrt(i)) + 2
    pieces = n * n
    border = pieces - i
    %{jp | rows: n, columns: n, pieces: pieces, border: border, format: :square}
  end

  defp fill_in_rows_and_columns_from_pieces_aspect(
         %JigsawPuzzle{rows: nil, columns: nil, pieces: p, aspect_ratio: ar} = jp
       )
       when not is_nil(p) and not is_nil(ar) and ar > 0 do
    rows = round(:math.sqrt(p / ar))
    columns = div(p, rows)
    pieces = rows * columns
    %{jp | rows: rows, columns: columns, pieces: pieces}
  end

  defp fill_in_rows_and_columns_from_pieces_aspect(jp), do: jp

  # Fill aspect ratio if rows and columns are known
  # Fill aspect ratio if rows and columns are known
  defp fill_in_aspect_ratio_and_format(
         %JigsawPuzzle{aspect_ratio: nil, rows: r, columns: c, format: f} = jp
       )
       when not is_nil(r) and not is_nil(c) and c != 0 do
    aspect_ratio =
      case f do
        :portrait -> c / r
        :landscape -> r / c
        :square -> 1.0
        nil -> r / c
      end

    %{jp | aspect_ratio: aspect_ratio}
  end

  defp fill_in_aspect_ratio_and_format(jp), do: jp

  # Fill format if aspect ratio is known
  defp fill_in_format(%JigsawPuzzle{format: nil, aspect_ratio: ar} = jp)
       when not is_nil(ar) do
    format =
      cond do
        ar > 1.0 -> :landscape
        ar < 1.0 -> :portrait
        ar == 1.0 -> :square
      end

    %{jp | format: format}
  end

  defp fill_in_format(jp), do: jp

  # Fill border and inside if possible
  defp fill_in_border_and_inside(
         %JigsawPuzzle{border: nil, inside: nil, pieces: p, rows: r, columns: c} = jp
       )
       when not is_nil(p) and not is_nil(r) and not is_nil(c) and p >= 4 do
    border = 2 * r + 2 * c - 4
    inside = p - border
    %{jp | border: border, inside: inside}
  end

  defp fill_in_border_and_inside(%JigsawPuzzle{border: nil, pieces: p, inside: i} = jp)
       when not is_nil(p) and not is_nil(i) and p >= i do
    %{jp | border: p - i}
  end

  defp fill_in_border_and_inside(%JigsawPuzzle{inside: nil, pieces: p, border: b} = jp)
       when not is_nil(p) and not is_nil(b) and p >= b do
    %{jp | inside: p - b}
  end

  defp fill_in_border_and_inside(jp), do: jp
end
