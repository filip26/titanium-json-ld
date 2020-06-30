package com.apicatalog.rdf.io.nquad.writer;

import javax.json.JsonObject;

import com.apicatalog.jsonld.lang.Keywords;

public final class NQuadsWriterTestCase {

    private String id;
    private String name;
    private String input;
    private String expected;

    public static final NQuadsWriterTestCase of(final JsonObject json) {
        
        final NQuadsWriterTestCase testCase = new NQuadsWriterTestCase();
        
        testCase.id = json.getString(Keywords.ID);
        testCase.name = json.getString("name");
        testCase.input = json.getString("input");
        testCase.expected = json.getString("expected");
        
        return testCase;
    }
    
    public String getId() {
        return id;
    }
    
    public String getName() {
        return name;
    }
    
    public String getInput() {
        return input;
    }
    
    public String getExpected() {
        return expected;
    }
}
