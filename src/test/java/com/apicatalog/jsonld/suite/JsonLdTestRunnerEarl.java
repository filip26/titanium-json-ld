package com.apicatalog.jsonld.suite;

import java.util.Objects;

import com.apicatalog.jsonld.api.JsonLdError;
import com.apicatalog.jsonld.api.JsonLdOptions;
import com.apicatalog.jsonld.document.Document;
import com.apicatalog.jsonld.loader.LoadDocumentOptions;

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
            
            Document expectedDocument = options.getDocumentLoader().loadDocument(testCase.expect, new LoadDocumentOptions());
                                    
            // compare expected with the result
            if (expectedDocument.getJsonContent().isPresent()) {
//FIXME                return JsonLdComparison.equals(expectedDocument.getJsonContent().get(), result);                
            }
            
            return false;
            
        } catch (JsonLdError e) {
            
            return Objects.equals(e.getCode(), testCase.expectErrorCode);
            
        } catch (Throwable e ) {
         
        }
        return false;
    }    
}
