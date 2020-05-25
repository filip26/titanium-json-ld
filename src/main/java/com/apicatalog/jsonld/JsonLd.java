package com.apicatalog.jsonld;

import com.apicatalog.jsonld.api.JsonLdProcessor;
import com.apicatalog.jsonld.processor.JsonLd11Processor;

public interface JsonLd {

    public static JsonLdProcessor createProcessor() {
        return new JsonLd11Processor();
    }
    
}
