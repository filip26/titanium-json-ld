package com.apicatalog.rdf.api;

import com.apicatalog.jsonld.lang.BlankNode;
import com.apicatalog.rdf.RdfLiteral;
import com.apicatalog.rdf.RdfObject;
import com.apicatalog.rdf.lang.IRI;

public final class StatementObjectBuilder {

    
    protected StatementObjectBuilder() {
        
    }

    public StatementContextBuilder iri(IRI iri) {
        return null;
    }

    public StatementContextBuilder iri(String iri) {
        return null;
    }
    
    public StatementContextBuilder blank(String label) {
        return null;        
    }
    
    public StatementContextBuilder blank(BlankNode blankNode) {
        return null;        
    }
    
    public StatementContextBuilder litteral(String lexicalForm) {
        return null;
    }

    public StatementContextBuilder litteral(String lexicalForm, IRI dataType) {
        return null;
    }
    
    public StatementContextBuilder litteral(String lexicalForm, String langTag) {
        return null;
    }

    public StatementContextBuilder litteral(RdfLiteral literal) {
        return null;
    }
    
    public StatementContextBuilder object(RdfObject object) {
        return null;
    }
        
}
