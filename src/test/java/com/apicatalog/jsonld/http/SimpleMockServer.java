package com.apicatalog.jsonld.http;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.ServerSocket;
import java.net.Socket;
import java.nio.charset.StandardCharsets;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.stream.Collectors;

public class SimpleMockServer implements AutoCloseable {

    private ServerSocket server;

    private final ExecutorService pool = Executors.newSingleThreadExecutor();

    private final Map<String, Stub> stubs = new ConcurrentHashMap<>();

    private volatile boolean running = true;

    private final int port;

    private record Stub(int status, Map<String, String> headers, byte[] body) {
    }

    public SimpleMockServer(int port) {
        this.port = port;
    }

    public void when(String path, int status, Map<String, String> headers, byte[] body) {
        stubs.put(path, new Stub(status, headers, body));
    }

    public void when(String path, int status, Map<String, String> headers) {
        stubs.put(path, new Stub(status, headers, null));
    }

    private void serve() {
        while (running) {
            try (Socket socket = server.accept()) {
                handle(socket);
            } catch (IOException ignored) {
            }
        }
    }

    private void handle(Socket socket) throws IOException {

        var in = new BufferedReader(new InputStreamReader(socket.getInputStream()));
        var out = socket.getOutputStream();

        var line = in.readLine();

        if (line == null || !line.startsWith("GET")) {
            return;
        }

        var path = line.split(" ")[1];
        System.out.println("2 " + path);
        System.out.println("3 " + stubs.keySet());
        var stub = stubs.get(path);

        if (stub == null) {
            var notFoundResponse = """
                    HTTP/1.1 404 Not Found\r
                    \r
                    no stub for %s
                    """.formatted(path);
            out.write(notFoundResponse.getBytes(StandardCharsets.UTF_8));
            out.flush();
            return;
        }
        System.out.println("3 " + new String(stub.body));

        var headersString = stub.headers().entrySet().stream()
                .map(e -> String.format("%s: %s", e.getKey(), e.getValue()))
                .collect(Collectors.joining("\r\n"));

        out.write("""
                HTTP/1.1 %d OK\r
                %s\r
                \r
                """.formatted(stub.status(), headersString)
                .getBytes(StandardCharsets.UTF_8));

        out.write(stub.body);
        out.flush();
    }

    @Override
    public void close() throws IOException {
        running = false;
        server.close();
        pool.shutdownNow();
    }

    public static void main(String[] args) throws Exception {
//        try (SimpleMockServer mock = new SimpleMockServer(8080)) {
//            mock.when("/old", 302, Map.of("Location", "/new"), "");
//            mock.when("/new", 200, "redirected!");
//
//            System.out.println("Mock server running on http://localhost:8080");
//            Thread.sleep(15000);
//        }
    }

    public void start() throws IOException {
        server = new ServerSocket(port);
        pool.submit(this::serve);

    }

    public void stop() {
        // TODO Auto-generated method stub

    }

    public String baseUrl() {
        return "http://localhost:8080";
    }
}
