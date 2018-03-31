package com.jasongj.algorithm;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.Setter;
import org.testng.annotations.Test;

public class Algorithm4 {


    @AllArgsConstructor
    private static class ListNode {

        @Getter
        @Setter
        int val;

        @Getter
        @Setter
        ListNode next;

        ListNode(int val) {
            this.val = val;
            next = null;
        }

        public String toString() {
            return String.valueOf(val);
        }
    }

    public static class Solution {
        public boolean hasCycle(ListNode head) {
            if(head == null || head.next == null) return false;

            ListNode quick = head;
            ListNode slow = head;
            while(quick != null && quick.next != null && quick.next.next != null) {
                quick = quick.next.next;
                slow = slow.next;
                if(quick.val == slow.val) {
                    return true;
                }
            }
            return false;
        }

        public ListNode detectCycle(ListNode head) {
            if(head == null || head.next == null) return null;

            ListNode quick = head;
            ListNode slow = head;
            ListNode meet = null;
            while(quick != null && quick.next != null && quick.next.next != null) {
                quick = quick.next.next;
                slow = slow.next;
                if(quick == slow) {
                    meet = slow;
                    break;
                }
            }
            if(meet == null) return null;
            quick = head;
            slow = meet;
            while(quick != slow) {
                quick = quick.next;
                slow = slow.next;
            }
            return quick;
        }
    }

    public static void main(String[] args) {
        Solution solution = new Solution();
        ListNode meet = new ListNode(9);
//        ListNode tail = new ListNode(8, meet);
//        meet.next = new ListNode(10, new ListNode(7, new ListNode(6, new ListNode(5, new ListNode(4, new ListNode(3, tail))))));
        meet.next = meet;
        ListNode head = new ListNode(11, new ListNode(12, new ListNode(13, new ListNode(14, new ListNode(15, meet)))));
        solution.detectCycle(head);
    }

    @Test
    public void heapSort () {
        int[] data = new int[]{1,4,3,5,2,8,7,6,0};

        class MaxHeap {
            void createMaxHeap (int[] array, int root, int end) {
                if(root == end) return;
                if(2*root+1 <= end){
                    createMaxHeap(array, 2*root+1, end);
                    if(array[2*root+1] > array[root]) {
                        swap(array, 2*root+1, root);
                    }
                }
                if(2*root+2 <= end) {
                    createMaxHeap(array, 2*root+2, end);
                    if(array[2*root+2] > array[root]) {
                        swap(array, 2*root+2, root);
                    }
                }
            }

            void swap (int[] array, int one, int two) {
                int old = array[one];
                array[one] = array[two];
                array[two] = old;
            }
        }

        MaxHeap heap = new MaxHeap();
        for (int i = data.length - 1; i >= 0; i--) {
            heap.createMaxHeap(data, 0, i);
            heap.swap(data, 0, i);
        }
        for ( int i : data ) {
            System.out.printf("%3s", i);
        }
        System.out.println();


    }


}
