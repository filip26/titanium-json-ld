package com.apicatalog.jsonld.engine;

import java.io.IOException;

import com.apicatalog.jsonld.JsonLdError;

@FunctionalInterface
public interface Action {

    void execute(Runtime runtime) throws JsonLdError, IOException;
    
}
