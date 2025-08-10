defmodule BinarySearchTree do
  defstruct data: nil, left: nil, right: nil

  @type bst_node :: %BinarySearchTree{data: any, left: bst_node | nil, right: bst_node | nil}

  @doc """
  Create a new Binary Search Tree with root's value as the given 'data'
  """
  @spec new(any) :: bst_node
  def new(data) do
    %BinarySearchTree{data: data}
  end

  @doc """
  Creates and inserts a node with its value as 'data' into the tree.
  """
  @spec insert(bst_node, any) :: bst_node
  def insert(nil, data), do: new(data)

  def insert(tree, data) when tree.data >= data do
    Map.update!(tree, :left, &insert(&1, data))
    # alternative methods
    # Map.update(tree, :left, nil, &insert(&1, data))
    # %BinarySearchTree{tree | left: insert(tree.left, data)}
    # Map.put(tree, :left, insert(tree.left, data))
  end

  def insert(tree, data) when tree.data < data do
    Map.update!(tree, :right, &insert(&1, data))
  end

  @doc """
  Traverses the Binary Search Tree in order and returns a list of each node's data.
  """
  @spec in_order(bst_node) :: [any]
  def in_order(nil), do: []

  def in_order(tree) do
    in_order(tree.left) ++ [tree.data] ++ in_order(tree.right)
  end
end
