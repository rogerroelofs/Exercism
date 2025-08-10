defmodule Diamond do
  @doc """
  Given a letter, it prints a diamond starting with 'A',
  with the supplied letter at the widest point.
  """
  @spec build_shape(char) :: String.t()
  def build_shape(letter) do
    width = (letter - ?A) * 2 + 1

    # (Enum.to_list(?A..letter) ++ Enum.to_list((letter - 1)..?A))
    # |> Enum.map(&build_line(&1, width))
    front = ?A..letter |> Enum.map(&build_line(&1, width))

    (front ++ (front |> Enum.reverse() |> tl))
    |> Enum.join("\n")
    |> Kernel.<>("\n")
  end

  defp build_line(letter, width) do
    inner_width = (letter - ?A) * 2 - 1
    outer_width = div(width - ((letter - ?A) * 2 + 1), 2)
    letter_str = List.to_string([letter])
    outer_pad = String.duplicate(" ", outer_width)

    content =
      if inner_width < 1 do
        letter_str
      else
        inner_space = String.duplicate(" ", inner_width)
        letter_str <> inner_space <> letter_str
      end

    outer_pad <> content <> outer_pad
  end
end
