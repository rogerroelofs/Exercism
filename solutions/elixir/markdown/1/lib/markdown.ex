defmodule Markdown do
  @doc """
    Parses a given string with Markdown syntax and returns the associated HTML for that string.

    ## Examples

      iex> Markdown.parse("This is a paragraph")
      "<p>This is a paragraph</p>"

      iex> Markdown.parse("# Header!\\n* __Bold Item__\\n* _Italic Item_")
      "<h1>Header!</h1><ul><li><strong>Bold Item</strong></li><li><em>Italic Item</em></li></ul>"
  """
  @spec parse(String.t()) :: String.t()
  def parse(markdown) do
    markdown
    |> String.split("\n")
    |> Enum.map(&process/1) # decode each line
    |> Enum.join()
    |> patch # fix up the bits
  end

  defp process(line) do
    cond do
      String.starts_with?(line, "*") ->
        line
        |> parse_list_md_level
      String.starts_with?(line, "#") && !String.starts_with?(line, "#######") ->
        line
        |> parse_header_md_level
        |> enclose_with_header_tag
      true ->
        line
        |> String.split
        |> enclose_with_paragraph_tag
    end
  end

  defp parse_header_md_level(line) do
    [level | content] = String.split(line)
    {to_string(String.length(level)), Enum.join(content, " ")}
  end

  defp parse_list_md_level(line) do
    tag = line
    |> String.trim_leading("* ")
    |> String.split
    "<li>" <> join_words_with_tags(tag) <> "</li>"
  end

  defp enclose_with_header_tag({level, content}) do
    "<h" <> level <> ">" <> content <> "</h" <> level <> ">"
  end

  defp enclose_with_paragraph_tag(content) do
    "<p>#{join_words_with_tags(content)}</p>"
  end

  defp join_words_with_tags(content) do
    content
    |> Enum.map(&replace_md_with_tag/1)
    |> Enum.join(" ")
  end

  defp replace_md_with_tag(content) do
    content
    |> replace_prefix_md
    |> replace_suffix_md
  end

  defp replace_prefix_md(w) do
    cond do
      w =~ ~r/^#{"__"}{1}/ -> String.replace(w, ~r/^#{"__"}{1}/, "<strong>", global: false)
      w =~ ~r/^[#{"_"}{1}][^#{"_"}+]/ -> String.replace(w, ~r/_/, "<em>", global: false)
      true -> w
    end
  end

  defp replace_suffix_md(w) do
    cond do
      w =~ ~r/#{"__"}{1}$/ -> String.replace(w, ~r/#{"__"}{1}$/, "</strong>")
      w =~ ~r/[^#{"_"}{1}]/ -> String.replace(w, ~r/_/, "</em>")
      true -> w
    end
  end

  defp patch(md) do
    md
    |> String.replace("<li>", "<ul>" <> "<li>", global: false)
    |> String.reverse()
    |> String.replace(String.reverse("</li>"), String.reverse("</li></ul>"), global: false)
    |> String.reverse()
  end
end
