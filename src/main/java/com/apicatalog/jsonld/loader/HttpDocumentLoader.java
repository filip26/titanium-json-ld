package com.apicatalog.jsonld.loader;

import java.io.IOException;
import java.io.InputStreamReader;
import java.net.HttpURLConnection;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import com.apicatalog.jsonld.api.JsonLdError;
import com.apicatalog.jsonld.api.JsonLdErrorCode;
import com.apicatalog.jsonld.document.Document;
import com.apicatalog.jsonld.document.JsonDocument;
import com.apicatalog.jsonld.document.RemoteDocument;
import com.apicatalog.jsonld.uri.UriResolver;

public class HttpDocumentLoader implements LoadDocumentCallback {

    public static final String ACCEPT_HEADER = 
                                    MediaType.JSON_LD.toString()
                                        .concat(",")
                                        .concat(MediaType.JSON.toString())
                                        .concat(");q=0.9,*/*;q=0.8");
    
    public static final int MAX_REDIRECTIONS = 10;
    
    private static final String PLUS_JSON = "+json";
    
    private int connectTimeout;
    private int readTimeout;
    private int maxRedirections;

    public HttpDocumentLoader() {
        this.connectTimeout = -1;
        this.readTimeout = -1;
        this.maxRedirections = MAX_REDIRECTIONS;
    }
    
    @Override
    public RemoteDocument loadDocument(final URI uri, final LoadDocumentOptions options) throws JsonLdError {

        try {

            final RemoteDocument remoteDocument = new RemoteDocument();
            
            int redirection = 0;
            boolean done = false;
            
            HttpURLConnection connection = null;
            
            URL url = uri.toURL();
            
            MediaType contentType = null;
            
            while (!done) {
                
                // 2.
                connection = (HttpURLConnection)url.openConnection();
    
                connection.setInstanceFollowRedirects(false);
                
                if (connectTimeout >= 0) {
                    connection.setConnectTimeout(connectTimeout);
                }
                if (readTimeout >= 0) {
                    connection.setReadTimeout(readTimeout);
                }
                
                connection.setRequestMethod("GET");
                connection.setRequestProperty("Accept", ACCEPT_HEADER); //TODO add profile
    
                connection.setUseCaches(false);
                connection.setDoInput(false);
                connection.setDoInput(true);
                
                connection.connect();
    
                // 3.
                if (connection.getResponseCode() == 301
                    || connection.getResponseCode() == 302
                    || connection.getResponseCode() == 303
                    || connection.getResponseCode() == 307
                    ) {

                    if (connection.getResponseCode() == 303) {
                    //    remoteDocument.setDocumentUrl(url.toURI());
                    }
                    
                    url = new URL(UriResolver.resolve(url.toURI(), connection.getHeaderField("Location")));
                    connection.disconnect();
                    redirection++;
                    
                    if (maxRedirections > 0 && redirection >= maxRedirections) {
                        throw new JsonLdError(JsonLdErrorCode.LOADING_DOCUMENT_FAILED, "Too many redirections");
                    }

                    continue;
                }
                
                if (connection.getResponseCode() != 200) {
                    connection.disconnect();
                    throw new JsonLdError(JsonLdErrorCode.LOADING_DOCUMENT_FAILED, "Unexpected response code [" + connection.getResponseCode() + "]");
                }

                final String contentTypeValue = connection.getHeaderField("Content-Type");
                
                if (contentTypeValue != null) {                    
                    contentType = MediaType.valueOf(contentTypeValue);
                }
                
                if (contentType != null
                     && !MediaType.JSON_LD.match(contentType)
                     && !MediaType.HTML.match(contentType)
                     && !MediaType.XHTML.match(contentType)
                        ) {

                    final List<String> linkValues = connection.getHeaderFields().get("Link");
                    
                    System.out.println("link: " + linkValues + ", content-type: " + contentType);
                    
                    Set<Link> links = null;
                    
                    if (linkValues != null) {
                        links = linkValues.stream().flatMap(l -> new LinkHeaderParser(l).parse().stream()).collect(Collectors.toSet());
                    }
                    
                    System.out.println("weblinks: " + links);


                    if (links != null) {
                        
                        // 4.
                        if (!MediaType.JSON.match(contentType)
                                && !contentType.subtype().toLowerCase().endsWith(PLUS_JSON)
//                                && links.stream().fin
                                ) {
                            
//                            url = new URL(UriResolver.resolve(url.toURI(), .connection.));
                            
                            connection.disconnect();
                            redirection++;
                            
                            if (maxRedirections > 0 && redirection >= maxRedirections) {
                                throw new JsonLdError(JsonLdErrorCode.LOADING_DOCUMENT_FAILED, "Too many redirections");
                            }

                            continue;
                        }
                        
                        // 5.
                        if (MediaType.JSON.match(contentType)
                                || contentType.subtype().toLowerCase().endsWith(PLUS_JSON)
//                                && links.stream().fin
                                ) {
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
                
                connection.disconnect();
                
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
                        
            Document document = JsonDocument.parse(new InputStreamReader(connection.getInputStream()));

            connection.disconnect();

            if (remoteDocument.getDocumentUrl() == null) {
                remoteDocument.setDocumentUrl(url.toURI());
            }

            remoteDocument.setContentType(contentType);
            remoteDocument.setDocument(document);
             
            //TODO set profile
            
            return remoteDocument;
            
        } catch (IOException | URISyntaxException e) {
            throw new JsonLdError(JsonLdErrorCode.LOADING_DOCUMENT_FAILED, e);            
        }        
    }
    
    /**
     *  Connect timeout value in milliseconds
     *  
     * @param timeout
     */
    public void setConnectTimeout(int timeout) {
        this.connectTimeout = timeout;
    }

    /**
     *  Read timeout value in milliseconds
     *  
     * @param timeout
     */
    public void setReadTimeout(int timeout) {
        this.readTimeout = timeout;
    }
    
    public void setMaxRedirections(int maxRedirections) {
        this.maxRedirections = maxRedirections;
    }
}
