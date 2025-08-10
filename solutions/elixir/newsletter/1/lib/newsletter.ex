defmodule Newsletter do
  @spec read_emails(binary) :: [binary]
  def read_emails(path) do
    {:ok, contents} = File.read(path)
    contents |> String.split("\n", trim: true)
  end

  @spec open_log(binary) :: pid
  def open_log(path) do
    File.open!(path, [:write])
  end

  @spec log_sent_email(pid, binary) :: :ok
  def log_sent_email(pid, email) do
    IO.puts(pid, email)
  end

  @spec close_log(pid) :: :ok
  def close_log(pid) do
    File.close(pid)
  end

  @spec send_newsletter(binary, binary, function) :: :ok
  def send_newsletter(emails_path, log_path, send_fun) do
    emails = read_emails(emails_path)
    log = open_log(log_path)
    Enum.each(emails, fn email ->
      case send_fun.(email) do
        :ok -> log_sent_email(log, email)
        :error -> :error
      end
    end)
  end
end
