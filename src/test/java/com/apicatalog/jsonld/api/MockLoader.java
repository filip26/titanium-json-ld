package com.apicatalog.jsonld.api;

import java.net.URI;

import javax.json.JsonStructure;

import com.apicatalog.jsonld.document.Document;
import com.apicatalog.jsonld.document.JsonDocument;
import com.apicatalog.jsonld.document.RdfDocument;
import com.apicatalog.jsonld.loader.DocumentLoader;
import com.apicatalog.jsonld.loader.DocumentLoaderOptions;
import com.apicatalog.rdf.RdfDataset;

public class MockLoader implements DocumentLoader {

    private final JsonStructure structure;
    private final RdfDataset dataset;
    
    public MockLoader(final JsonStructure response) {
        this.structure = response;
        this.dataset = null;
    }

    public MockLoader(final RdfDataset dataset) {
        this.structure = null;
        this.dataset = dataset;
    }
    
    @Override
    public Document loadDocument(URI url, DocumentLoaderOptions options) throws JsonLdError {

        if (structure != null) {
            final Document remoteDocument = JsonDocument.of(structure);
            remoteDocument.setDocumentUrl(url);
            
            return remoteDocument;
        }

        if (dataset != null) {
            final Document remoteDocument = RdfDocument.of(dataset);
            remoteDocument.setDocumentUrl(url);
            
            return remoteDocument;            
        }
        
        return null;
    }
    
    
    
}
