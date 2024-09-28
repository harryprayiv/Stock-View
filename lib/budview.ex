defmodule MyApp.Inventory.MenuItem do
  defstruct name: "", category: "", subcategory: "", species: "", sku: "", price: 0.0, quantity: 0
end

defmodule MyApp.Inventory.Inventory do
  defstruct items: []
end


defmodule MyApp.InventoryFetcher do
  alias MyApp.Inventory.{MenuItem, Inventory}

  @json_file "priv/static/inventory.json"

  def fetch_inventory_from_json do
    @json_file
    |> File.read()
    |> case do
      {:ok, body} ->
        {:ok, Jason.decode!(body, as: %Inventory{items: [%MenuItem{}]})}
      {:error, reason} ->
        {:error, reason}
    end
  end

  def fetch_inventory_from_http do
    case HTTPoison.post("https://httpbin.org/post", Jason.encode!(%{hello: "world"}), [{"Content-Type", "application/json"}]) do
      {:ok, %HTTPoison.Response{body: body}} ->
        {:ok, Jason.decode!(body)}
      {:error, reason} ->
        {:error, reason}
    end
  end
end


defmodule MyAppWeb.InventoryLive do
  use Phoenix.LiveView
  alias MyApp.InventoryFetcher
  alias MyApp.Inventory.{Inventory, MenuItem}

  def mount(_params, _session, socket) do
    send(self(), :load_inventory)
    {:ok, assign(socket, inventory: %Inventory{items: []}, config: default_config())}
  end

  def handle_info(:load_inventory, socket) do
    case InventoryFetcher.fetch_inventory_from_json() do
      {:ok, inventory} -> {:noreply, assign(socket, :inventory, inventory)}
      {:error, reason} -> {:noreply, put_flash(socket, :error, "Error loading inventory: #{reason}")}
    end
  end

  def handle_event("sort", %{"field" => field, "order" => order}, socket) do
    sorted_inventory = sort_inventory(socket.assigns.inventory, String.to_existing_atom(field), String.to_existing_atom(order))
    {:noreply, assign(socket, :inventory, sorted_inventory)}
  end

  defp default_config do
    %{sort_field: :category, sort_order: :ascending, hide_out_of_stock: true}
  end

  defp sort_inventory(inventory, field, order) do
    items = Enum.sort_by(inventory.items, &Map.get(&1, field))
    sorted_items = if order == :descending, do: Enum.reverse(items), else: items
    %Inventory{inventory | items: sorted_items}
  end
end


def mount(_params, _session, socket) do
  schedule_poll()
  {:ok, assign(socket, inventory: %Inventory{items: []}, config: default_config())}
end

def handle_info(:poll_inventory, socket) do
  case InventoryFetcher.fetch_inventory_from_json() do
    {:ok, inventory} -> {:noreply, assign(socket, :inventory, inventory)}
    {:error, reason} -> {:noreply, put_flash(socket, :error, "Error loading inventory: #{reason}")}
  end
end

defp schedule_poll do
  Process.send_after(self(), :poll_inventory, 5000)  # Poll every 5 seconds
end
