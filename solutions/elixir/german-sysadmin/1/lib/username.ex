defmodule Username do
  @spec sanitize(any) :: list
  def sanitize(username) do
    Enum.reduce(username, '', fn c, accum ->
      accum ++ case c do
        ?ä -> 'ae'
        ?ö -> 'oe'
        ?ß -> 'ss'
        ?ü -> 'ue'
        c when c in ?a..?z -> [c]
        ?_ -> '_'
        _ -> ''
      end
    end)
  end
end
