package com.apicatalog.jsonld.loader;

import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpClient.Redirect;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.net.http.HttpResponse.BodyHandlers;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

import com.apicatalog.jsonld.api.JsonLdError;
import com.apicatalog.jsonld.api.JsonLdErrorCode;
import com.apicatalog.jsonld.document.Document;
import com.apicatalog.jsonld.document.JsonDocument;
import com.apicatalog.jsonld.document.RemoteDocument;
import com.apicatalog.jsonld.http.Link;
import com.apicatalog.jsonld.http.LinkHeaderParser;
import com.apicatalog.jsonld.http.MediaType;
import com.apicatalog.jsonld.http.JsonLdProfile;
import com.apicatalog.jsonld.uri.UriResolver;

public class HttpLoader implements LoadDocumentCallback {

    public static final String ACCEPT_HEADER = 
                                    MediaType.JSON_LD.toString()
                                        .concat(",")
                                        .concat(MediaType.JSON.toString())
                                        .concat(");q=0.9,*/*;q=0.8");
    
    public static final int MAX_REDIRECTIONS = 10;
    
    private static final String PLUS_JSON = "+json";
    
    private int maxRedirections;

    private final HttpClient httpClient;
    
    public HttpLoader() {
        this(HttpClient.newBuilder().followRedirects(Redirect.NEVER).build(), MAX_REDIRECTIONS);
    }
    
    public HttpLoader(HttpClient httpClient, int maxRedirections) {
        this.httpClient = httpClient;
        this.maxRedirections = maxRedirections;
    }
    
    @Override
    public RemoteDocument loadDocument(final URI uri, final LoadDocumentOptions options) throws JsonLdError {

        try {

            final RemoteDocument remoteDocument = new RemoteDocument();
            
            int redirection = 0;
            boolean done = false;
            
            URI targetUri = uri;
            
            MediaType contentType = null;
            
            HttpResponse<InputStream> response = null;
            
            while (!done) {
                
                // 2.
                HttpRequest request = 
                                HttpRequest.newBuilder()
                                    .GET()
                                    .uri(targetUri)
                                    .header("Accept", ACCEPT_HEADER) //TODO add profile
                                    .build();
                
                response = httpClient.send(request, BodyHandlers.ofInputStream());
                                    
                // 3.
                if (response.statusCode() == 301
                    || response.statusCode() == 302
                    || response.statusCode() == 303
                    || response.statusCode() == 307
                    ) {

                    final Optional<String> location = response.headers().firstValue("Location");
                    
                    if (location.isPresent()) {
                        targetUri = URI.create(UriResolver.resolve(targetUri, location.get()));

                    } else {
                        throw new JsonLdError(JsonLdErrorCode.LOADING_DOCUMENT_FAILED, "Header location is required for code [" + response.statusCode() + "].");
                    }
                    

                    redirection++;
                    
                    if (maxRedirections > 0 && redirection >= maxRedirections) {
                        throw new JsonLdError(JsonLdErrorCode.LOADING_DOCUMENT_FAILED, "Too many redirections");
                    }

                    continue;
                }
                
                if (response.statusCode() != 200) {
                    throw new JsonLdError(JsonLdErrorCode.LOADING_DOCUMENT_FAILED, "Unexpected response code [" + response.statusCode() + "]");
                }

                final Optional<String> contentTypeValue = response.headers().firstValue("Content-Type");
                
                if (contentTypeValue.isPresent()) {                    
                    contentType = MediaType.valueOf(contentTypeValue.get());
                }
                
                final List<String> linkValues = response.headers().map().get("link");

                if (linkValues != null && !linkValues.isEmpty()) {

                    // 4.
                    if (contentType == null 
                            || (!MediaType.JSON.match(contentType)
                                    && !contentType.subtype().toLowerCase().endsWith(PLUS_JSON))
                            ) {

                        Optional<Link> alternate = 
                                            linkValues.stream()
                                                .flatMap(l -> new LinkHeaderParser(l).parse().stream())
                                                .filter(
                                                        f -> "alternate".equalsIgnoreCase(f.rel())
                                                                && MediaType.JSON_LD.toString().equals(f.type())
                                                        )
                                                .findFirst();

                        System.out.println("alternate:" + alternate);
                        
                        if (alternate.isPresent()) {
                        
                            targetUri = alternate.get().uri();
                            
                            redirection++;
                            
                            if (maxRedirections > 0 && redirection >= maxRedirections) {
                                throw new JsonLdError(JsonLdErrorCode.LOADING_DOCUMENT_FAILED, "Too many redirections");
                            }

                            continue;
                        }
                    }
                    
                    // 5.
                    if (contentType != null 
                            && !MediaType.JSON_LD.match(contentType) 
                            && (MediaType.JSON.match(contentType)
                                    || contentType.subtype().toLowerCase().endsWith(PLUS_JSON))
                            ) {
                        

                        List<Link> contextUri = 
                                        linkValues.stream()
                                            .flatMap(l -> new LinkHeaderParser(l).parse().stream())
                                            .filter(f -> JsonLdProfile.CONTEXT.equals(f.rel()))
                                            .collect(Collectors.toList());

                        System.out.println("contextUri:" + contextUri);
                        
                        if (contextUri.size() > 1) {
                            
                            throw new JsonLdError(JsonLdErrorCode.MULTIPLE_CONTEXT_LINK_HEADERS);
                            
                        } else if(contextUri.size() == 1) {

                            //TODO setContextUrl
                            
                        }
                    }
                }
                    
                done = true;
            }
            
            // 6.
            if (contentType == null ||
                    (!MediaType.JSON_LD.match(contentType)
                    && !MediaType.JSON.match(contentType)
                    && !contentType.subtype().toLowerCase().endsWith(PLUS_JSON))  
                    ) {
                
                throw new JsonLdError(JsonLdErrorCode.LOADING_DOCUMENT_FAILED, 
                                            "Unsupported media type '" + contentType 
                                            + "'. Supported content types are [" 
                                            + MediaType.JSON_LD + ", " 
                                            + MediaType.JSON  + ", "
                                            + PLUS_JSON
                                            + "]"
                                            );
            }

            // 7.   
            Document document = JsonDocument.parse(new InputStreamReader(response.body()));

            if (remoteDocument.getDocumentUrl() == null) {
                remoteDocument.setDocumentUrl(targetUri);
            }

            remoteDocument.setContentType(contentType);
            remoteDocument.setDocument(document);
             
            //TODO set profile

            return remoteDocument;
            
        } catch (InterruptedException e) {
            
            Thread.currentThread().interrupt();
            throw new JsonLdError(JsonLdErrorCode.LOADING_DOCUMENT_FAILED, e);
            
        } catch (IOException e) {
            throw new JsonLdError(JsonLdErrorCode.LOADING_DOCUMENT_FAILED, e);            
        }        
    }

    public void setMaxRedirections(int maxRedirections) {
        this.maxRedirections = maxRedirections;
    }
}
