defmodule BudviewTest do
  use ExUnit.Case
  doctest Budview

  test "greets the world" do
    assert Budview.hello() == :world
  end
end
