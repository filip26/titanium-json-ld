package com.apicatalog.rdf.io.nquad;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Enumeration;
import java.util.zip.ZipEntry;
import java.util.zip.ZipException;
import java.util.zip.ZipFile;

import org.junit.Assert;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;

import com.apicatalog.jsonld.api.JsonLdError;
import com.apicatalog.rdf.RdfDataset;

@RunWith(Parameterized.class)
public class NQuadsReaderTest {

    private final static String TEST_SUITE_NAME = "/n-quads-test-suite-20200629.zip";
    
    @Parameterized.Parameter(0)
    public String testPath;

    @Parameterized.Parameter(1)
    public String testType;
    
    @Parameterized.Parameter(2)
    public String testName;

    
    @Test
    public void testRead() throws IOException, URISyntaxException {

        final URL zipFileUrl =  (new NQuadsReaderTest()).getClass().getResource(TEST_SUITE_NAME);

        Assert.assertNotNull(zipFileUrl);

        try (final ZipFile zip = new ZipFile(new File(zipFileUrl.toURI()))) {
            
            Assert.assertNotNull(zip);
            
            try (final InputStream is = zip.getInputStream(zip.getEntry(testPath))) {

                RdfDataset dataset = (new NQuadsReader(new InputStreamReader(is))).readDataset();
                System.out.println(">>> " + testPath + ", " + dataset);
                
                Assert.assertEquals("positive", testType);
                
            } catch (NQuadsReaderException e) {
                e.printStackTrace();
                Assert.assertEquals("negative", testType);
            }   
        }        
    }

    @Parameterized.Parameters(name = "{1}: {2}")
    public static Collection<Object[]> data() throws JsonLdError, URISyntaxException, ZipException, IOException {
        
        final URL zipFileUrl =  (new NQuadsReaderTest()).getClass().getResource(TEST_SUITE_NAME);

        Assert.assertNotNull(zipFileUrl);

        final Collection<Object[]> suite = new ArrayList<>();
        
        try (final ZipFile zip = new ZipFile(new File(zipFileUrl.toURI()))) {

            final Enumeration<? extends ZipEntry> entries = zip.entries();
            
            while (entries.hasMoreElements()) {

                final ZipEntry entry = entries.nextElement();
                
                if (entry.getName().endsWith(".nq")) {
                    
                    final String name = entry.getName().substring("n-quads-test-suite".length(), entry.getName().length() - ".nq".length());
                    
                    final String type = name.startsWith("nt-") ? "negative" : "positive";
                    
                    suite.add(new Object[] {entry.getName(), type, name});
                }
            }
        }                                    
        return suite;
    }
}
