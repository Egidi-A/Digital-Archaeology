package com.generated;

public class hello_world {

    private String ws_message;

    public String getWs_message() {
        return ws_message;
    }

    public void setWs_message(String value) {
        this.ws_message = value;
    }

    public static void main(String[] args) {
        hello_world hw = new hello_world();
        hw.setWs_message("Hello, World!");
        System.out.println(hw.getWs_message());
    }
}