package com.apicatalog.jsonld.runtime;

import java.util.logging.Level;
import java.util.logging.Logger;

import com.apicatalog.jsonld.JsonLdException;
import com.apicatalog.jsonld.JsonLdException.ErrorCode;
import com.apicatalog.jsonld.Options.ProcessingPolicy;
import com.apicatalog.jsonld.runtime.Execution.EventProcessor;
import com.apicatalog.jsonld.runtime.Execution.EventType;

public final class PolicyEnforcer implements EventProcessor {

    private static final Logger LOGGER = Logger.getLogger(PolicyEnforcer.class.getName());
    
    private final ProcessingPolicy undefinedTerms;
    private final ProcessingPolicy droppedNodes;

    public PolicyEnforcer(
            ProcessingPolicy undefinedTerms,
            ProcessingPolicy droppedNodes) {

        this.undefinedTerms = undefinedTerms;
        this.droppedNodes = droppedNodes;
    }

    @Override
    public void onEvent(EventType type, String key, String value) throws JsonLdException {
        switch (type) {
        case onDroppedNode:
            enforceNodes(droppedNodes, key);
            return;
            
        case onUndefinedTerm:
            enforceTerms(undefinedTerms, key);
            return;
            
        default:
            return;
        }

    }

    private static void enforceTerms(ProcessingPolicy policy, String key) throws JsonLdException {
        switch (policy) {
        case Fail:
            throw new JsonLdException(ErrorCode.UNDEFINED_TERM,
                    "An undefined term has been found [" + key + "]. Change policy to Ignore or Warn or define the term in a context");
        case Warn:
            LOGGER.log(Level.WARNING, "An undefined term has been detected, term={0}", key);

        case Ignore:
            return;
        }
    }

    private static void enforceNodes(ProcessingPolicy policy, String key) throws JsonLdException {
        switch (policy) {
        case Fail:
            throw new JsonLdException(ErrorCode.DROPPED_NODE,
                    "A dropped node has been detected [" + key + "]. Change policy to Ignore or Warn");
        case Warn:
            LOGGER.log(Level.WARNING, "A dropped node has been detected, key={0}", key);

        case Ignore:
            return;
        }
    }
}