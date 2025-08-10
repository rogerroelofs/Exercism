defmodule Garden do
  @doc """
    Accepts a string representing the arrangement of cups on a windowsill and a
    list with names of students in the class. The student names list does not
    have to be in alphabetical order.

    It decodes that string into the various gardens for each student and returns
    that information in a map.
  """

  @default_students [
    :alice,
    :bob,
    :charlie,
    :david,
    :eve,
    :fred,
    :ginny,
    :harriet,
    :ileana,
    :joseph,
    :kincaid,
    :larry
  ]

  @plants %{"G" => :grass, "C" => :clover, "R" => :radishes, "V" => :violets}

  @spec info(String.t(), list) :: map
  def info(info_string, student_names \\ @default_students) do
    plants = String.split(info_string, "\n")

    student_names
    |> Enum.sort()
    |> Enum.with_index()
    |> Enum.into(%{}, &process_plants(&1, plants))
  end

  defp process_plants({name, i}, plants) do
    {name,
     plants
     |> slice_plants(i)
     |> String.graphemes()
     |> Enum.map(&@plants[&1])
     |> List.to_tuple()}
  end

  defp slice_plants(plants, i) do
    plants
    |> Enum.map(fn line ->
      start = i * 2
      String.slice(line, start..(start + 1))
    end)
    |> Enum.join()
  end
end
