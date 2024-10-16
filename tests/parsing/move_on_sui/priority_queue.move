// Copyright (c) Mysten Labs, Inc.
// SPDX-License-Identifier: Apache-2.0

/// Priority queue implemented using a max heap.
module sui::priority_queue {
    use std::vector;

    /// For when heap is empty and there's no data to pop.
    const EPopFromEmptyHeap: u64 = 0;

    /// Struct representing a priority queue. The `entries` vector represents a max
    /// heap structure, where entries[0] is the root, entries[1] and entries[2] are the
    /// left child and right child of the root, etc. More generally, the children of
    /// entries[i] are at at i * 2 + 1 and i * 2 + 2. The max heap should have the invariant
    /// that the parent node's priority is always higher than its child nodes' priorities.
    struct PriorityQueue<T: drop> has store, drop {
        entries: vector<Entry<T>>,
    }

    struct Entry<T: drop> has store, drop {
        priority: u64, // higher value means higher priority and will be popped first
        value: T,
    }

    /// Create a new priority queue from the input entry vectors.
    public fun new<T: drop>(entries: vector<Entry<T>>) : PriorityQueue<T> {
        let len = vector::length(&entries);
        let i = len / 2;
        // Max heapify from the first node that is a parent (node at len / 2).
        while (i > 0) {
            i = i - 1;
            max_heapify_recursive(&mut entries, len, i);
        };
        PriorityQueue { entries }
    }

    /// Pop the entry with the highest priority value.
    public fun pop_max<T: drop>(pq: &mut PriorityQueue<T>) : (u64, T) {
        let len = vector::length(&pq.entries);
        assert!(len > 0, EPopFromEmptyHeap);
        // Swap the max element with the last element in the entries and remove the max element.
        let Entry { priority, value } = vector::swap_remove(&mut pq.entries, 0);
        // Now the max heap property has been violated at the root node, but nowhere else
        // so we call max heapify on the root node.
        max_heapify_recursive(&mut pq.entries, len - 1, 0);
        (priority, value)
    }

    /// Insert a new entry into the queue.
    public fun insert<T: drop>(pq: &mut PriorityQueue<T>, priority: u64, value: T) {
        vector::push_back(&mut pq.entries, Entry { priority, value});
        let index = vector::length(&pq.entries) - 1;
        restore_heap_recursive(&mut pq.entries, index);
    }

    public fun new_entry<T: drop>(priority: u64, value: T): Entry<T> {
        Entry { priority, value }
    }

    public fun create_entries<T: drop>(p: vector<u64>, v: vector<T>): vector<Entry<T>> {
        let len = vector::length(&p);
        assert!(vector::length(&v) == len, 0);
        let res = vector::empty();
        let i = 0;
        while (i < len) {
            let priority = vector::remove(&mut p, 0);
            let value = vector::remove(&mut v, 0);
            vector::push_back(&mut res, Entry { priority, value });
            i = i + 1;
        };
        res
    }

    // TODO: implement iterative version too and see performance difference.
    fun restore_heap_recursive<T: drop>(v: &mut vector<Entry<T>>, i: u64) {
        if (i == 0) {
            return
        };
        let parent = (i - 1) / 2;

        // If new elem is greater than its parent, swap them and recursively
        // do the restoration upwards.
        if (vector::borrow(v, i).priority > vector::borrow(v, parent).priority) {
            vector::swap(v, i, parent);
            restore_heap_recursive(v, parent);
        }
    }

    spec restore_heap_recursive {
        pragma opaque;
    }

    /// Max heapify the subtree whose root is at index `i`. That means after this function
    /// finishes, the subtree should have the property that the parent node has higher priority
    /// than both child nodes.
    /// This function assumes that all the other nodes in the subtree (nodes other than the root)
    /// do satisfy the max heap property.
    fun max_heapify_recursive<T: drop>(v: &mut vector<Entry<T>>, len: u64, i: u64) {
        if (len == 0) {
            return
        };
        assert!(i < len, 1);
        let left = i * 2 + 1;
        let right = left + 1;
        let max = i;
        // Find the node with highest priority among node `i` and its two children.
        if (left < len && vector::borrow(v, left).priority> vector::borrow(v, max).priority) {
            max = left;
        };
        if (right < len && vector::borrow(v, right).priority > vector::borrow(v, max).priority) {
            max = right;
        };
        // If the parent node (node `i`) doesn't have the highest priority, we swap the parent with the
        // max priority node.
        if (max != i) {
            vector::swap(v, max, i);
            // After the swap, we have restored the property at node `i` but now the max heap property
            // may be violated at node `max` since this node now has a new value. So we need to now
            // max heapify the subtree rooted at node `max`.
            max_heapify_recursive(v, len, max);
        }
    }

    spec max_heapify_recursive {
        pragma opaque;
    }

    public fun priorities<T: drop>(pq: &PriorityQueue<T>): vector<u64> {
        let res = vector[];
        let i = 0;
        while (i < vector::length(&pq.entries)) {
            vector::push_back(&mut res, vector::borrow(&pq.entries, i).priority);
            i = i +1;
        };
        res
    }

    #[test]
    fun test_pq() {
        let h = new(create_entries(vector[3,1,4,2,5,2], vector[10, 20, 30, 40, 50, 60]));
        check_pop_max(&mut h, 5, 50);
        check_pop_max(&mut h, 4, 30);
        check_pop_max(&mut h, 3, 10);
        insert(&mut h, 7, 70);
        check_pop_max(&mut h, 7, 70);
        check_pop_max(&mut h, 2, 40);
        insert(&mut h, 0, 80);
        check_pop_max(&mut h, 2, 60);
        check_pop_max(&mut h, 1, 20);
        check_pop_max(&mut h, 0, 80);


        let h = new(create_entries(vector[5,3,1,2,4], vector[10, 20, 30, 40, 50]));
        check_pop_max(&mut h, 5, 10);
        check_pop_max(&mut h, 4, 50);
        check_pop_max(&mut h, 3, 20);
        check_pop_max(&mut h, 2, 40);
        check_pop_max(&mut h, 1, 30);
    }

    #[test]
    fun test_swap_remove_edge_case() {
        // This test would fail if `remove` is used incorrectly instead of `swap_remove` in `pop_max`.
        // It's hard to characterize exactly under what condition this bug is triggered but roughly
        // it happens when the entire tree vector is shifted left by one because of the incorrect usage
        // of `remove`, and the resulting new root and its two children appear to satisfy the heap invariant
        // so we stop max-heapifying there, while the rest of the tree is all messed up because of the shift.
        let priorities = vector[8, 7, 3, 6, 2, 1, 0, 5, 4];
        let values = vector[0, 0, 0, 0, 0, 0, 0, 0, 0];
        let h = new(create_entries(priorities, values));
        check_pop_max(&mut h, 8, 0);
        check_pop_max(&mut h, 7, 0);
        check_pop_max(&mut h, 6, 0);
        check_pop_max(&mut h, 5, 0);
        check_pop_max(&mut h, 4, 0);
        check_pop_max(&mut h, 3, 0);
        check_pop_max(&mut h, 2, 0);
        check_pop_max(&mut h, 1, 0);
        check_pop_max(&mut h, 0, 0);
    }

    #[test_only]
    fun check_pop_max(h: &mut PriorityQueue<u64>, expected_priority: u64, expected_value: u64) {
        let (priority, value) = pop_max(h);
        assert!(priority == expected_priority, 0);
        assert!(value == expected_value, 0);
    }
}
