package com.jasongj.algorithm;

import org.testng.annotations.Test;

public class TestCBM {


    @Test
    public void testCBM() {
        System.out.println("Free memory before " + (Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()));
        byte[] bytes = new byte[Integer.MAX_VALUE >>> 3];
        setNum(bytes);
        System.gc();
        System.gc();
        System.out.println("Free memory after " + (Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()));


    }

    public static void setNum (byte[] bytes) {
        for (int i = 0; i < bytes.length; i++){
            bytes[i] = Byte.MAX_VALUE;
        }
    }

    @Test
    public void test(){
        int[] array = new int[20];
        int total = 0;
        for (int i = 0; i < array.length; i++) {
            array[i] = i;
            total += array[i];
        }
        System.out.println(total);

    }

}
