defmodule PhoneNumber do
  @doc """
  Remove formatting from a phone number if the given number is valid. Return an error otherwise.
  """
  @spec clean(String.t()) :: {:ok, String.t()} | {:error, String.t()}
  def clean(raw) do
    with {:ok, cleaned} <- do_clean(raw),
         {:ok, phone} <- check_country_code(cleaned),
         {:ok, phone} <- check_length(phone),
         {:ok, phone} <- check_area_code(phone),
         {:ok, phone} <- check_exchange(phone)
    do
      {:ok, phone}
    else
      err -> err
    end
  end

  defp do_clean(raw) do
    if String.match?(raw, ~r/[^0-9\.+\- \(\)]/) do
      {:error, "must contain digits only"}
    else
      {:ok, raw |> String.replace(~r/\D/, "")}
    end
  end

  defp check_country_code(phone) do
    cond do
      String.match?(phone, ~r/^1\d{10}/) ->
        {:ok, String.slice(phone, 1..-1)}
      String.length(phone) == 11 ->
        {:error, "11 digits must start with 1"}
      true ->
        {:ok, phone}
    end
  end

  defp check_length(phone) do
    if String.length(phone) != 10 do
      {:error, "incorrect number of digits"}
    else
      {:ok, phone}
    end
  end

  defp check_area_code(phone) do
    cond do
      String.match?(phone, ~r/0\d{9}/) ->
        {:error, "area code cannot start with zero"}
      String.match?(phone, ~r/1\d{9}/) ->
        {:error, "area code cannot start with one"}
      true ->
        {:ok, phone}
    end
  end

  defp check_exchange(phone) do
    cond do
    String.match?(phone, ~r/[2-9]\d\d0\d{6}/) ->
      {:error, "exchange code cannot start with zero"}
    String.match?(phone, ~r/[2-9]\d\d1\d{6}/) ->
      {:error, "exchange code cannot start with one"}
    true ->
      {:ok, phone}
    end
  end
end
