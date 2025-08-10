defmodule BankAccount do
  @moduledoc """
  A bank account that supports access from multiple processes.
  """

  @typedoc """
  An account handle.
  """
  @opaque account :: pid

  @error_closed {:error, :account_closed}

  @doc """
  Open the bank. Makes the account available.
  """
  @spec open_bank() :: account
  def open_bank() do
    {:ok, account} = Agent.start_link(fn -> %{balance: 0, open: true} end)
    account
  end

  @doc """
  Close the bank. Makes the account unavailable.
  """
  @spec close_bank(account) :: none
  def close_bank(account) do
    Agent.update(account, fn %{balance: balance} ->
      %{open: false, balance: balance}
    end)

    nil
  end

  @doc """
  Get the account's balance.
  """
  @spec balance(account) :: integer
  def balance(account) do
    %{balance: balance, open: open} = Agent.get(account, fn x -> x end)

    if open do
      balance
    else
      @error_closed
    end
  end

  @doc """
  Update the account's balance by adding the given amount which may be negative.
  """
  @spec update(account, integer) :: any
  def update(account, amount) do
    Agent.get_and_update(account, fn %{balance: balance, open: open} ->
      if open do
        {nil, %{open: open, balance: balance + amount}}
      else
        {@error_closed, %{open: open, balance: balance}}
      end
    end)
  end
end
