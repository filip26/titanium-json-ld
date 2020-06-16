package com.apicatalog.jsonld.loader;

import java.io.IOException;
import java.io.InputStreamReader;
import java.net.HttpURLConnection;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.List;

import com.apicatalog.jsonld.api.JsonLdError;
import com.apicatalog.jsonld.api.JsonLdErrorCode;
import com.apicatalog.jsonld.document.Document;
import com.apicatalog.jsonld.document.JsonDocument;
import com.apicatalog.jsonld.document.RemoteDocument;
import com.apicatalog.jsonld.uri.UriResolver;

public class HttpDocumentLoader implements LoadDocumentCallback {

    public static final String ACCEPT_HEADER = 
                                    MediaTypes.JSON_LD_MEDIA_TYPE
                                        .concat(",")
                                        .concat(MediaTypes.JSON_MEDIA_TYPE)
                                        .concat(");q=0.9,*/*;q=0.8");
    
    public static final int MAX_REDIRECTIONS = 10;
    
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
            
            String contentType = null;
            
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

                contentType = connection.getHeaderField("Content-Type");
                List<String> link = connection.getHeaderFields().get("Link");
                
                System.out.println("link: " + link + ", content-type: " + contentType);
                
                // 4.
                //TODO

                done = true;
            }
            
            if (remoteDocument.getDocumentUrl() == null) {
                remoteDocument.setDocumentUrl(url.toURI());
            }
            
            // 5.
            //TODO
            
            // 6.
            if (!MediaTypes.JSON_LD_MEDIA_TYPE.equalsIgnoreCase(contentType)
                    && !MediaTypes.JSON_MEDIA_TYPE.equalsIgnoreCase(contentType)
                    && (contentType == null || !contentType.toLowerCase().endsWith("+json"))  
                    ) {
                connection.disconnect();
                throw new JsonLdError(JsonLdErrorCode.LOADING_DOCUMENT_FAILED, 
                                            "Unsupported media type '" + contentType 
                                            + "'. Supported content types are [" 
                                            + MediaTypes.JSON_LD_MEDIA_TYPE + ", " 
                                            + MediaTypes.JSON_MEDIA_TYPE + ", *+json]"
                                            );
            }
            
            // 7.
            Document document = JsonDocument.parse(new InputStreamReader(connection.getInputStream()));
            
            connection.disconnect();
            
            remoteDocument.setDocument(document);
             

            //TODO set content type, profile & contextUrl
            
            return remoteDocument;
            
        } catch (IOException | URISyntaxException e) {
            e.printStackTrace();
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
