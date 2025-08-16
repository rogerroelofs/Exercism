interface BinarySearchTreeNode {
  data: number;
  left?: BinarySearchTreeNode;
  right?: BinarySearchTreeNode;
}

export class BinarySearchTree {
  #data?: BinarySearchTreeNode;

  constructor(data: unknown) {
    if (typeof data === 'number') {
      this.#data = { data: data as number };
    } else if (data instanceof BinarySearchTree) {
      this.#data = data.#data;
    } else if (data && typeof data === 'object' && 'data' in (data as object)) {
      this.#data = data as BinarySearchTreeNode;
    }
  }

  get data(): number | undefined {
    return this.#data?.data;
  }

  get left(): BinarySearchTree | undefined {
    return this.#data?.left ? new BinarySearchTree(this.#data.left) : undefined;
  }

  get right(): BinarySearchTree | undefined {
    return this.#data?.right ? new BinarySearchTree(this.#data.right) : undefined;
  }

  insert(item: number): BinarySearchTree {
    if (!this.#data) {
      this.#data = { data: item };
      return this;
    }
    let node = this.#data;
    while (true) {
      if (item <= node.data) {
        if (node.left) {
          node = node.left;
        } else {
          node.left = { data: item };
          break;
        }
      } else {
        if (node.right) {
          node = node.right;
        } else {
          node.right = { data: item };
          break;
        }
      }
    }
    return this;
  }

  each(callback: (data: unknown) => void): void {
    const traverse = (node?: BinarySearchTreeNode): void => {
      if (!node) return;
      traverse(node.left);
      callback(node.data);
      traverse(node.right);
    };
    traverse(this.#data);
  }
}