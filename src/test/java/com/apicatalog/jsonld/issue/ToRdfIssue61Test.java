package com.apicatalog.jsonld.issue;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.io.IOException;
import java.io.InputStream;

import org.junit.Test;

import com.apicatalog.jsonld.JsonLd;
import com.apicatalog.jsonld.api.JsonLdError;
import com.apicatalog.jsonld.document.Document;
import com.apicatalog.jsonld.document.JsonDocument;
import com.apicatalog.jsonld.document.RdfDocument;
import com.apicatalog.rdf.RdfComparison;
import com.apicatalog.rdf.RdfDataset;
import com.apicatalog.rdf.RdfNQuad;
import com.apicatalog.jsonld.api.JsonLdOptions;

public class ToRdfIssue61Test {

    
    @Test
    public void testToRdfMissingTriples() throws JsonLdError, IOException {

        final Document document = readDocument("issue61-in.json");
        final Document context = readDocument("issue61-context.json");
        
        final RdfDataset result = JsonLd.toRdf(document).context(context).get();
        
        assertNotNull(result);

        try (final InputStream is = getClass().getResourceAsStream("issue61-out.nq")) {

            assertNotNull(is);

        System.out.println("testToRdfMissingTriples");
        printResult(result);

            boolean match = RdfComparison.equals(result, RdfDocument.of(is).getRdfContent().orElse(null));

            assertTrue(match);
        }
    }

    @Test
    public void testToRdfMissingTriples2() throws JsonLdError, IOException {

        final Document document = readDocument("issue61-in.json");
        final Document context = readDocument("issue61-context.json");
        JsonLdOptions options = new JsonLdOptions();
        options.setExpandContext(context);

        final RdfDataset result = JsonLd.toRdf(document).options(options).get();

        assertNotNull(result);

        try (final InputStream is = getClass().getResourceAsStream("issue61-out.nq")) {
            
            assertNotNull(is);

        System.out.println("testToRdfMissingTriples2");
        printResult(result);

            boolean match = RdfComparison.equals(result, RdfDocument.of(is).getRdfContent().orElse(null));
            
            assertTrue(match);            
        }
    }

    @Test
    public void testToRdfMissingTriples3() throws JsonLdError, IOException {

        final String document = getTestURL("issue61-in.json");
        final String context = getTestURL("issue61-context.json");
        JsonLdOptions options = new JsonLdOptions();
        options.setExpandContext(context);

        final RdfDataset result = JsonLd.toRdf(document).options(options).get();

        assertNotNull(result);

        try (final InputStream is = getClass().getResourceAsStream("issue61-out.nq")) {

            assertNotNull(is);

        System.out.println("testToRdfMissingTriples3");
        printResult(result);

            boolean match = RdfComparison.equals(result, RdfDocument.of(is).getRdfContent().orElse(null));

            assertTrue(match);
        }
    }

    private final String getTestURL(String p) {
      return "file://" + System.getProperty("user.dir") + "/src/test/resources/com/apicatalog/jsonld/issue/" + p;
    }

    private final void printResult(RdfDataset result) {
      String res = "Size " + result.toList().size();
      StringBuilder sb = new StringBuilder();
      for ( RdfNQuad e : result.toList() )
       sb.append(  "<" + e.getSubject() + "> <" + e.getPredicate() + "> " + e.getObject() ) . append("\n");
      System.out.println(res + "\n" + sb);
    }

    private final Document readDocument(final String name) throws JsonLdError, IOException {
        try (final InputStream is = getClass().getResourceAsStream(name)) {
            return JsonDocument.of(is);
        }
    }

}
