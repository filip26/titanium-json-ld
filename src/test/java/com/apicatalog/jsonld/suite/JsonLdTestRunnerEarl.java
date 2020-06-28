package com.apicatalog.jsonld.suite;

import java.util.Objects;

import com.apicatalog.jsonld.api.JsonLdError;
import com.apicatalog.jsonld.api.JsonLdOptions;
import com.apicatalog.jsonld.document.Document;
import com.apicatalog.jsonld.json.JsonLdComparison;
import com.apicatalog.jsonld.loader.LoadDocumentOptions;
import com.apicatalog.rdf.RdfComparison;

public class JsonLdTestRunnerEarl {

    private final JsonLdTestCase testCase;
    
    public JsonLdTestRunnerEarl(JsonLdTestCase testCase) {
        this.testCase = testCase;
    }
    
    public boolean execute(JsonLdTestCaseMethod method) {

        JsonLdOptions options = testCase.getOptions();
        
        Document result = null;
        
        try {
  
            result = method.invoke(options);
            
            if (result == null || testCase.expectErrorCode != null) {
                return false;
            }
            
            if (result.getRdfContent().isPresent() && testCase.expect == null && testCase.type.contains("jld:PositiveSyntaxTest")) {
                return true;
                
            } else if (testCase.expect == null) {
                return false;
            }
            
            Document expectedDocument = options.getDocumentLoader().loadDocument(testCase.expect, new LoadDocumentOptions());
            
            if (expectedDocument == null) {
                return false;
            }
            
                                    
            // compare expected with the result
            if (expectedDocument.getJsonContent().isPresent()) {
                
                return result.getJsonContent().isPresent() 
                       && JsonLdComparison.equals(
                                   expectedDocument.getJsonContent().get(), 
                                   result.getJsonContent().get()
                                   );
                
            } else if (expectedDocument.getRdfContent().isPresent()) {

                return result.getRdfContent().isPresent() 
                        && RdfComparison.equals(
                                    expectedDocument.getRdfContent().get(), 
                                    result.getRdfContent().get()
                                    );

                
            }
            
            return false;
            
        } catch (JsonLdError e) {
            
            return Objects.equals(e.getCode(), testCase.expectErrorCode);
            
        } catch (Throwable e ) {
         
        }
        return false;
    }
}
