package com.apicatalog.jsonld;

import org.junit.platform.suite.api.SelectClasses;
import org.junit.platform.suite.api.Suite;
import org.junit.platform.suite.api.SuiteDisplayName;

@Suite(failIfNoTests = true)
@SuiteDisplayName("Transform")
@SelectClasses({ ExpandTest.class })
public class TransformSuite {

}
