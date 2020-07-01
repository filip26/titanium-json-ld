package com.apicatalog.rdf.io.nquad;

import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.Collection;
import java.util.stream.Collectors;
import java.util.zip.ZipException;
import java.util.zip.ZipFile;

import org.junit.Assert;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;

import com.apicatalog.rdf.RdfDataset;
import com.apicatalog.rdf.io.nquad.reader.NQuadsReaderTestCase;
import com.apicatalog.rdf.io.nquad.reader.NQuadsReaderTestSuite;
import com.apicatalog.rdf.io.nquad.reader.NQuadsReaderTestCase.Type;

@RunWith(Parameterized.class)
public class NQuadsReaderTest {

    private final static String TEST_SUITE_NAME = "/n-quads-test-suite-20200629.zip";
    private final static String TEST_CASE_BASE_PATH = "nquads-test-suite/";
    
    @Parameterized.Parameter(0)
    public NQuadsReaderTestCase testCase;

    @Parameterized.Parameter(1)
    public String testType;
    
    @Parameterized.Parameter(2)
    public String testName;

    @Test
    public void testRead() throws IOException, URISyntaxException {

        Assert.assertNotNull(testCase);
        Assert.assertNotNull(testCase.getName());
        Assert.assertNotNull(testCase.getType());
        
        final URL zipFileUrl =  (new NQuadsReaderTest()).getClass().getResource(TEST_SUITE_NAME);

        Assert.assertNotNull(zipFileUrl);

        try (final ZipFile zip = new ZipFile(new File(zipFileUrl.toURI()))) {
            
            Assert.assertNotNull(zip);
            
            try (final Reader reader = new InputStreamReader(zip.getInputStream(zip.getEntry(TEST_CASE_BASE_PATH + testCase.getName() + ".nq")))) {

                RdfDataset dataset = (new NQuadsReader(reader)).readDataset();
                
                Assert.assertNotNull(dataset);
                
                Assert.assertEquals(Type.POSITIVE, testCase.getType());
                
            } catch (NQuadsReaderException | IllegalArgumentException e) {
                Assert.assertEquals(Type.NEGATIVE, testCase.getType());
            }
        }
    }

    @Parameterized.Parameters(name = "{1}: {2}")
    public static Collection<Object[]> data() throws ZipException, IOException, URISyntaxException {
        return (new NQuadsReaderTestSuite(TEST_SUITE_NAME, TEST_CASE_BASE_PATH + "manifest.json"))
                .load()
                .stream()            
                .map(o -> new Object[] {o, o.getType().name().toLowerCase(), o.getComment() + " : " + o.getName()})
                .collect(Collectors.toList());
    }
}
