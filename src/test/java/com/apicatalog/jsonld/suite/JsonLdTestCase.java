package com.apicatalog.jsonld.suite;

import java.net.URI;

import javax.json.JsonObject;
import javax.json.JsonValue;

import org.junit.Assert;

import com.apicatalog.jsonld.api.JsonLdError;
import com.apicatalog.jsonld.api.JsonLdErrorCode;
import com.apicatalog.jsonld.api.JsonLdOptions;
import com.apicatalog.jsonld.document.RemoteDocument;
import com.apicatalog.jsonld.loader.ClassPathLoader;
import com.apicatalog.jsonld.loader.LoadDocumentCallback;
import com.apicatalog.jsonld.loader.LoadDocumentOptions;
import com.apicatalog.jsonld.loader.UrlRewrite;

public class JsonLdTestCase {

    public String id;
    
    public String name;
    
    public URI input;
    
    public URI context;
    
    public URI expect;
    
    public JsonLdErrorCode expectErrorCode;
    
    public String baseUri;
    
    public JsonLdTestCaseOptions options;
    
    public static final JsonLdTestCase of(JsonObject o, String baseUri) {
        
        final JsonLdTestCase testCase = new JsonLdTestCase();
        
        testCase.id = o.getString("@id");
        
        testCase.name = o.getString("name");
        
        testCase.input = o.containsKey("input")
                            ? URI.create(baseUri + o.getString("input"))
                            : null;
        
        testCase.context = o.containsKey("context")
                                ? URI.create(baseUri + o.getString("context"))
                                : null;
                                
        testCase.expect = o.containsKey("expect")
                                ? URI.create(baseUri + o.getString("expect"))
                                : null;
        
        testCase.expectErrorCode = o.containsKey("expectErrorCode")
                                            ? errorCode((o.getString("expectErrorCode")))
                                            : null;
        
        testCase.options = o.containsKey("option")
                                ? JsonLdTestCaseOptions.of(o.getJsonObject("option"), baseUri)
                                : new JsonLdTestCaseOptions();
                                
        testCase.baseUri = baseUri;
                                
        return testCase;
    }
    
    public void execute(JsonLdTestCaseMethod method) throws JsonLdError {

        Assert.assertNotNull(baseUri);
        Assert.assertNotNull(input);

        JsonLdOptions options = getOptions();
        
        Assert.assertNotNull(options);
        Assert.assertNotNull(options.getDocumentLoader());
        
        JsonValue result = null;
        
        try {
  
            result = method.invoke(options);
            
            Assert.assertNotNull(result);
            
        } catch (JsonLdError e) {
            Assert.assertEquals(expectErrorCode, e.getCode());
            return;
        }
        
        Assert.assertNull(expectErrorCode);
        Assert.assertNotNull(expect);
        
        RemoteDocument expectedDocument = options.getDocumentLoader().loadDocument(expect, new LoadDocumentOptions());
                    
        Assert.assertNotNull(expectedDocument);
        Assert.assertNotNull(expectedDocument.getDocument());
        
        // compare expected with the result        
        Assert.assertEquals(expectedDocument.getDocument().asJsonStructure(), result);
    }
    
    JsonLdOptions getOptions() {
        
        final LoadDocumentCallback loader = 
                new UrlRewrite(
                            baseUri, 
                            "classpath:" + JsonLdManifestLoader.RESOURCES_BASE,
                            new ClassPathLoader()
                        );
        
        JsonLdOptions jsonLdOptions = new JsonLdOptions();
        jsonLdOptions.setOrdered(true);
        jsonLdOptions.setDocumentLoader(loader);
        
        options.setup(jsonLdOptions);
        
        return jsonLdOptions;
    }
    
    static final JsonLdErrorCode errorCode(String errorCode) {
        
        if (errorCode == null || errorCode.isBlank()) {
            return null;
        }
        
        /*
         * Because scoped contexts can lead to contexts being reloaded, 
         * replace the recursive context inclusion error with a context overflow error.
         * 
         * @see <a href="https://www.w3.org/TR/json-ld11-api/#changes-from-cg">Changes since JSON-LD Community Group Final Report</a>
         */
        if ("recursive context inclusion".equalsIgnoreCase(errorCode)) {
            return JsonLdErrorCode.CONTEXT_OVERFLOW;
        }
        if ("list of lists".equalsIgnoreCase(errorCode)) {
            return JsonLdErrorCode.UNSPECIFIED;
        }
        if ("compaction to list of lists".equalsIgnoreCase(errorCode)) {
            return JsonLdErrorCode.UNSPECIFIED;
        }

        return JsonLdErrorCode.valueOf(errorCode.strip().toUpperCase().replace(" ", "_").replace("-", "_").replaceAll("\\_\\@", "_KEYWORD_" )); 
    }
    
}
