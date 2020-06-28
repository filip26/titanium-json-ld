package com.apicatalog.jsonld.api.impl;

import com.apicatalog.jsonld.loader.DocumentLoader;

public interface LoaderApi<R> {

    /**
     * Set the loader to be used to retrieve remote documents and
     * contexts, implementing the {@link DocumentLoader}. If specified, it is
     * used to retrieve remote documents and contexts; otherwise, if not specified,
     * the processor's built-in loader is used.
     * 
     * @param loader
     * @return builder instance
     */
    R loader(DocumentLoader loader);
    
}
