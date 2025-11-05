/*
 * Copyright 2020 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package com.apicatalog.jsonld.test;

import static org.junit.jupiter.api.Assertions.assertNotNull;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.ServerSocket;
import java.net.Socket;
import java.net.URI;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.stream.Collectors;

import com.apicatalog.jsonld.JsonLdException;
import com.apicatalog.jsonld.JsonLdException.ErrorCode;
import com.apicatalog.jsonld.loader.HttpLoader;
import com.apicatalog.jsonld.loader.ZipResourceLoader;
import com.apicatalog.web.link.Link;
import com.apicatalog.web.media.MediaType;
import com.apicatalog.web.uri.UriResolver;

public class MockServer implements AutoCloseable {

    final String testBase;
    final String resourceBase;
    final int port;

    final ExecutorService pool = Executors.newSingleThreadExecutor();

    final Map<String, Stub> stubs = new ConcurrentHashMap<>();

    ServerSocket server;

    volatile boolean running = true;

    private record Stub(
            String acceptHeader,
            int statusCode,
            List<Entry<String, String>> responseHeaders,
            byte[] responseBody) {
    }

    public MockServer(String testBase, String resourceBase) {
        this(0, testBase, resourceBase);
    }

    public MockServer(int port, String testBase, String resourceBase) {
        this.port = port;
        this.testBase = testBase;
        this.resourceBase = resourceBase;
    }

    public void when(String path, String accept, int status, List<Entry<String, String>> headers, byte[] body) {
        stubs.put(path, new Stub(accept, status, headers, body));
    }

    public void when(String path, String accept, int status, List<Entry<String, String>> headers) {
        stubs.put(path, new Stub(accept, status, headers, null));
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

        final var in = new BufferedReader(new InputStreamReader(socket.getInputStream()));
        final var out = socket.getOutputStream();

        final var line = in.readLine();

        if (line == null || !line.startsWith("GET")) {
            return;
        }

        final var path = line.split(" ")[1];

        final var stub = stubs.get(path);

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

        // read headers
        var headers = new LinkedHashMap<String, String>();
        boolean nextHeader = false;

        do {
            var headerLine = in.readLine();

            nextHeader = headerLine != null && !headerLine.isBlank() && !headerLine.equals("\n") && !headerLine.equals("\r");
            if (nextHeader) {
                var entry = headerLine.split(": ");
                headers.put(entry[0].toLowerCase(), entry[1].strip());
            }

        } while (nextHeader);

        var accept = headers.get("accept");

        if (stub.acceptHeader != null && !stub.acceptHeader.equals(accept)) {
            var notAcceptable = """
                    HTTP/1.1 406 Not Acceptable\r
                    \r
                    not acceptable media type %s for %s
                    """.formatted(accept, path);
            out.write(notAcceptable.getBytes(StandardCharsets.UTF_8));
            out.flush();
            return;
        }

        var headersString = stub.responseHeaders().stream()
                .map(e -> String.format("%s: %s", e.getKey(), e.getValue()))
                .collect(Collectors.joining("\r\n"));

        out.write("""
                HTTP/1.1 %d OK\r
                %s\r
                \r
                """.formatted(stub.statusCode(), headersString)
                .getBytes(StandardCharsets.UTF_8));

        if (stub.responseBody != null) {
            out.write(stub.responseBody);
        }
        out.flush();
    }

    public void start() throws JsonLdException {
        try {
            server = new ServerSocket(port);
            pool.submit(this::serve);
        } catch (IOException e) {
            throw new JsonLdException(ErrorCode.UNSPECIFIED, e);
        }
    }

    @Override
    public void close() throws JsonLdException {
        try {
            running = false;
            server.close();
            pool.shutdownNow();
        } catch (IOException e) {
            throw new JsonLdException(ErrorCode.UNSPECIFIED, e);
        }
    }

    public String baseUrl() {
        return "http://%s:%d".formatted(
                server.getInetAddress().getHostAddress(),
                server.getLocalPort());
    }

    public void setup(TestCase testCase) throws JsonLdException {

        stubs.clear();

        String inputPath;

        if (testCase.options.redirectTo != null) {
            inputPath = testCase.options.redirectTo.toString();

        } else {
            inputPath = testCase.input.toString();
        }

        if (testCase.options.redirectTo != null) {
            when(
                    testCase.input.toString().substring(testBase.length()),
                    HttpLoader.acceptHeader(),
                    testCase.options.httpStatus,
                    List.of(
                            Map.entry(
                                    "Location",
                                    testCase.options.redirectTo.toASCIIString().substring(testBase.length()))));
        }

        if (testCase.options.httpLink != null && testCase.options.httpLink.size() == 1) {

            String linkValue = testCase.options.httpLink.iterator().next();

            Link link = Link.of(linkValue, URI.create(".")).iterator().next();

            final MediaType contentType;

            if (link.type() != null) {

                contentType = link.type();

            } else {

                if (link.target().toString().endsWith(".html")) {
                    contentType = MediaType.HTML;

                } else if (link.target().toString().endsWith(".jsonld")) {
                    contentType = MediaType.JSON_LD;

                } else if (link.target().toString().endsWith(".json")) {
                    contentType = MediaType.JSON;

                } else {
                    contentType = null;
                }
            }

            assertNotNull(contentType);

            String linkUri = UriResolver.resolve(testCase.input, link.target().toString());

            byte[] content = ZipResourceLoader.fetchBytes(URI.create(resourceBase + linkUri.substring(testCase.baseUri.length())));

            if (content != null) {
                when(
                        linkUri.substring(testBase.length()),
                        null,
                        200,
                        List.of(Map.entry("Content-Type", contentType.toString())),
                        content);
            }
        }

        byte[] content = ZipResourceLoader.fetchBytes(URI.create(resourceBase + inputPath.substring(testCase.baseUri.length())));

        if (content != null) {

            var headers = new ArrayList<Entry<String, String>>();

            if (testCase.options.httpLink != null) {
                testCase.options.httpLink.forEach(link -> headers.add(Map.entry("Link", link)));
            }

            if (testCase.options.contentType != null) {
                headers.add(Map.entry("Content-Type", testCase.options.contentType.toString()));
            }

            when(
                    inputPath.substring(testBase.length()),
                    HttpLoader.acceptHeader(),
                    200,
                    headers,
                    content);
        }
    }
}
