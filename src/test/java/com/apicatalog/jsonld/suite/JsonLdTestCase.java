package com.apicatalog.jsonld.suite;

import java.net.URI;
import java.util.Set;
import java.util.stream.Collectors;

import javax.json.JsonObject;
import javax.json.JsonString;

import com.apicatalog.jsonld.api.JsonLdErrorCode;
import com.apicatalog.jsonld.api.JsonLdOptions;
import com.apicatalog.jsonld.lang.Keywords;
import com.apicatalog.jsonld.loader.ClassPathLoader;
import com.apicatalog.jsonld.loader.LoadDocumentCallback;
import com.apicatalog.jsonld.loader.UrlRewrite;

public final class JsonLdTestCase {

    public String id;
    
    public String name;
    
    public URI input;
    
    public URI context;
    
    public URI expect;
    
    public JsonLdErrorCode expectErrorCode;
    
    public String baseUri;
    
    public String uri;
    
    public Set<String> type;
    
    public JsonLdTestCaseOptions options;
    
    public static final JsonLdTestCase of(JsonObject o, String manifestUri, String baseUri) {
        
        final JsonLdTestCase testCase = new JsonLdTestCase();
        
        testCase.id = o.getString(Keywords.ID);
        
        testCase.uri = baseUri + manifestUri.substring(0, manifestUri.length() - ".jsonld".length()) + testCase.id;
        
        testCase.type = o.get(Keywords.TYPE).asJsonArray().stream()
                            .map(JsonString.class::cast)
                            .map(JsonString::getString)
                            .collect(Collectors.toSet());
        
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
        
    public JsonLdOptions getOptions() {
        
        final LoadDocumentCallback loader = 
                new UrlRewrite(
                            baseUri, 
                            "classpath:" + JsonLdManifestLoader.JSON_LD_API_BASE,
                            new ClassPathLoader()
                        );
        
        JsonLdOptions jsonLdOptions = new JsonLdOptions();
        jsonLdOptions.setOrdered(true);
        jsonLdOptions.setDocumentLoader(loader);
        
        options.setup(jsonLdOptions);
        
        return jsonLdOptions;
    }
    
    public static final JsonLdErrorCode errorCode(String errorCode) {
        
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
