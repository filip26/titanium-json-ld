package com.apicatalog.jsonld.loader;

import java.util.Map;

import com.apicatalog.jsonld.http.DefaultHttpClient;
import com.apicatalog.tree.io.NodeReader;
import com.apicatalog.tree.io.jakarta.JakartaReader;

import jakarta.json.Json;

public class HttpLoader extends DefaulLoader  {

//    private static final HttpLoader INSTANCE = new HttpLoader(DefaultHttpClient.defaultInstance());

//    ;
    
    public HttpLoader(LoaderClient httpClient, NodeReader reader) {
        super(httpClient, reader);
    }

    public HttpLoader(LoaderClient httpClient, NodeReader reader, int maxRedirections) {
        super(httpClient, reader, maxRedirections);
    }
    
    public static final HttpLoader newHttpLoader() {
        return new HttpLoader(
                DefaultHttpClient.defaultInstance(),
                new JakartaReader(Json.createReaderFactory(Map.of()))          
                );
    }

//    @Deprecated
//    public static final DefaulLoader defaultInstance() {
//        return INSTANCE;
//    }
}
