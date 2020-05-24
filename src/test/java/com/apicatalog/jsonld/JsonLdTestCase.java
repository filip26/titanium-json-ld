package com.apicatalog.jsonld;

import javax.json.JsonObject;

import com.apicatalog.jsonld.api.JsonLdErrorCode;
import com.apicatalog.jsonld.api.JsonLdOptions;
import com.apicatalog.jsonld.loader.JavaResourceLoader;
import com.apicatalog.jsonld.loader.LoadDocumentCallback;
import com.apicatalog.jsonld.loader.UrlRewriteLoader;

public class JsonLdTestCase {

    public String id;
    public String name;
    public String input;
    public String expect;
    public JsonLdErrorCode expectErrorCode;
    public String baseUri;
    public JsonLdTestCaseOptions options;
    
    public static final JsonLdTestCase of(JsonObject o, String baseUri) {
        
        final JsonLdTestCase testCase = new JsonLdTestCase();
        
        testCase.id = o.getString("@id");
        testCase.name = o.getString("name");
        testCase.input = o.getString("input");
        testCase.expect = o.getString("expect", null);
        
        testCase.expectErrorCode = o.containsKey("expectErrorCode")
                                            ? errorCode((o.getString("expectErrorCode")))
                                            : null;
        
        testCase.options =
                            o.containsKey("option")
                                ? JsonLdTestCaseOptions.of(o.getJsonObject("option"), baseUri)
                                : new JsonLdTestCaseOptions();
                                
        testCase.baseUri = baseUri;
                                
        return testCase;
    }
    
    public JsonLdOptions getOptions() {
        final LoadDocumentCallback loader = 
                new UrlRewriteLoader(
                            baseUri, 
                            "classpath:" + JsonLdManifestLoader.RESOURCES_BASE,
                            new JavaResourceLoader()
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
