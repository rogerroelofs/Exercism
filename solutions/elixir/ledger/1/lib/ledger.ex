defmodule Ledger do
  @doc """
  Format the given entries given a currency and locale
  """
  @type currency :: :usd | :eur
  @type locale :: :en_US | :nl_NL
  @type entry :: %{amount_in_cents: integer(), date: Date.t(), description: String.t()}

  @spec format_entries(currency(), locale(), list(entry())) :: String.t()
  def format_entries(currency, locale, entries) do
    header = format_header(locale)
    if entries == [] do
      header
    else
      formatted_entries =
        entries
        |> Enum.sort(&entry_sorter/2)
        |> Enum.map(&format_entry(currency, locale, &1))
        |> Enum.join("\n")
      header <> formatted_entries <> "\n"
    end
  end

  defp format_entry(currency, locale, entry) do
    formatted_date = format_date(entry.date, locale)
    formatted_amount = format_amount(entry.amount_in_cents, currency, locale)
    formatted_description = format_description(entry.description)
    formatted_date <> "|" <> formatted_description <> " |" <> formatted_amount
  end

  defp format_header(:en_US), do: "Date       | Description               | Change       \n"
  defp format_header(:nl_NL), do: "Datum      | Omschrijving              | Verandering  \n"

  defp entry_sorter(a, b) do
    case {a.date.day < b.date.day, a.date.day > b.date.day} do
      {true, false} -> true
      {false, true} -> false
      _ ->
        case {a.description < b.description, a.description > b.description} do
          {true, false} -> true
          {false, true} -> false
          _ -> a.amount_in_cents <= b.amount_in_cents
        end
    end
  end

  defp format_date(%Date{year: year, month: month, day: day}, :en_US) do
    month_str = String.pad_leading(to_string(month), 2, "0")
    day_str = String.pad_leading(to_string(day), 2, "0")
    year_str = to_string(year)
    month_str <> "/" <> day_str <> "/" <> year_str <> " "
  end
  defp format_date(%Date{year: year, month: month, day: day}, :nl_NL) do
    month_str = String.pad_leading(to_string(month), 2, "0")
    day_str = String.pad_leading(to_string(day), 2, "0")
    year_str = to_string(year)
    day_str <> "-" <> month_str <> "-" <> year_str <> " "
  end

  defp format_amount(amount_in_cents, currency, locale) do
    number = format_number(amount_in_cents, locale)
    symbol = if currency == :eur, do: "€", else: "$"
    cond do
      amount_in_cents >= 0 and locale == :en_US -> "  #{symbol}#{number} "
      amount_in_cents >= 0 and locale == :nl_NL -> " #{symbol} #{number} "
      amount_in_cents < 0 and locale == :en_US -> " (#{symbol}#{number})"
      amount_in_cents < 0 and locale == :nl_NL -> " #{symbol} -#{number} "
    end
    |> String.pad_leading(14, " ")
  end

  defp format_number(amount_in_cents, :en_US) do
    abs_amount = abs(amount_in_cents)
    decimal = rem(abs_amount, 100) |> to_string() |> String.pad_leading(2, "0")
    whole = div(abs_amount, 100)
    whole_str =
      if whole < 1000 do
        to_string(whole)
      else
        thousands = div(whole, 1000)
        remainder = rem(whole, 1000)
        to_string(thousands) <> "," <> String.pad_leading(to_string(remainder), 3, "0")
      end
    whole_str <> "." <> decimal
  end
  defp format_number(amount_in_cents, :nl_NL) do
    abs_amount = abs(amount_in_cents)
    decimal = rem(abs_amount, 100) |> to_string() |> String.pad_leading(2, "0")
    whole = div(abs_amount, 100)
    whole_str =
      if whole < 1000 do
        to_string(whole)
      else
        thousands = div(whole, 1000)
        remainder = rem(whole, 1000)
        to_string(thousands) <> "." <> String.pad_leading(to_string(remainder), 3, "0")
      end
    whole_str <> "," <> decimal
  end

  defp format_description(description) do
    if String.length(description) > 26 do
      " " <> String.slice(description, 0, 22) <> "..."
    else
      " " <> String.pad_trailing(description, 25, " ")
    end
  end
end
