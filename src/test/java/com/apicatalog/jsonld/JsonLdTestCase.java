package com.apicatalog.jsonld;

import javax.json.JsonObject;

public class JsonLdTestCase {

    public String id;
    public String name;
    public String input;
    public String expect;
    public String expectErrorCode;
    public JsonLdTestCaseOptions options;
    
    public JsonLdTestCase expectErrorCode(String errorCode) {
        
        if (errorCode == null || errorCode.isBlank()) {
            return this;
        }
        
        expectErrorCode = errorCode.strip().toUpperCase().replace(" ", "_").replace("-", "_").replaceAll("\\_\\@", "_KEYWORD_" ); 
        
        return this;
    }
    
    public static final JsonLdTestCase of(JsonObject o, String baseUri) {
        final JsonLdTestCase testDefinition = new JsonLdTestCase();
        testDefinition.id = o.getString("@id");
        testDefinition.name = o.getString("name");
        testDefinition.input = o.getString("input");
        testDefinition.expect = o.getString("expect", null);
        testDefinition.expectErrorCode(o.getString("expectErrorCode", null));
        
        testDefinition.options =
                            o.containsKey("option")
                                ? JsonLdTestCaseOptions.of(o.getJsonObject("option"), baseUri)
                                : new JsonLdTestCaseOptions();
        
        return testDefinition;
    }    
}
