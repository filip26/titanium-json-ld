package com.apicatalog.jsonld.serialization;

import java.util.Collections;
import java.util.Map;

import javax.json.JsonValue;

import com.apicatalog.jsonld.api.JsonLdError;
import com.apicatalog.jsonld.api.JsonLdOptions.RdfDirection;
import com.apicatalog.rdf.RdfObject;

public final class RdfToObject {

    // required
    private RdfObject object;
    private RdfDirection rdfDirection;
    private boolean useNativeTypes;
    
    private RdfToObject(final RdfObject object, final RdfDirection rdfDirection, final boolean useNativeTypes) {
        this.object = object;
        this.rdfDirection = rdfDirection;
        this.useNativeTypes = useNativeTypes;
    }
    
    public static final RdfToObject with(final RdfObject object, final RdfDirection rdfDirection, final boolean useNativeTypes) {
        return new RdfToObject(object, rdfDirection, useNativeTypes);
    }
    
    
    public Map<String, JsonValue> build() throws JsonLdError {
        return Collections.emptyMap();
    }
    
}
