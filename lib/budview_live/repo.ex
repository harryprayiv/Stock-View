defmodule BudviewLive.Repo do
  use Ecto.Repo,
    otp_app: :budview_live,
    adapter: Ecto.Adapters.Postgres
end
