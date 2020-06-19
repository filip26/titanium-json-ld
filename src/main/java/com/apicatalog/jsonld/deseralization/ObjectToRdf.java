package com.apicatalog.jsonld.deseralization;

import java.math.BigDecimal;
import java.text.DecimalFormat;
import java.util.List;

import javax.json.JsonNumber;
import javax.json.JsonObject;
import javax.json.JsonString;
import javax.json.JsonValue;

import com.apicatalog.jsonld.api.JsonLdError;
import com.apicatalog.jsonld.api.JsonLdOptions.RdfDirection;
import com.apicatalog.jsonld.flattening.NodeMap;
import com.apicatalog.jsonld.json.JsonCanonicalizer;
import com.apicatalog.jsonld.json.JsonUtils;
import com.apicatalog.jsonld.lang.BlankNode;
import com.apicatalog.jsonld.lang.Keywords;
import com.apicatalog.jsonld.lang.LanguageTag;
import com.apicatalog.jsonld.lang.ListObject;
import com.apicatalog.jsonld.lang.NodeObject;
import com.apicatalog.jsonld.lang.ValueObject;
import com.apicatalog.jsonld.uri.UriUtils;
import com.apicatalog.rdf.Rdf;
import com.apicatalog.rdf.RdfLiteral;
import com.apicatalog.rdf.RdfObject;
import com.apicatalog.rdf.RdfPredicate;
import com.apicatalog.rdf.RdfSubject;
import com.apicatalog.rdf.RdfTriple;
import com.apicatalog.rdf.lang.RdfContants;
import com.apicatalog.rdf.lang.XsdContants;

/**
 * 
 * @see <a href="https://w3c.github.io/json-ld-api/#deserialize-json-ld-to-rdf-algorithm">Object to RDF Conversion</a>
 *
 */
final class ObjectToRdf {

    // required
    private JsonObject item;
    private List<RdfTriple> triples;
    private NodeMap nodeMap;
    
    // optional
    private RdfDirection rdfDirection;
    
    private ObjectToRdf(JsonObject item, List<RdfTriple> triples, NodeMap nodeMap) {
        this.item = item;
        this.triples = triples;
        this.nodeMap = nodeMap;
        
        // default values
        this.rdfDirection = null;
    }
    
    public static final ObjectToRdf with(JsonObject item, List<RdfTriple> triples, NodeMap nodeMap) {
        return  new ObjectToRdf(item, triples, nodeMap);
    }
    
    public ObjectToRdf rdfDirection(RdfDirection rdfDirection) {
        this.rdfDirection = rdfDirection;
        return this;
    }
    
    public RdfObject build() throws JsonLdError {

        // 1. - 2.
        if (NodeObject.isNodeObject(item)) {
            
            JsonValue id = item.get(Keywords.ID);

            if (JsonUtils.isNotString(id) || JsonUtils.isNull(id)) {
                return null;
            }
            
            String idString = ((JsonString)id).getString();
     
            if (BlankNode.isWellFormed(idString)) {
                return Rdf.createObject(RdfObject.Type.BLANK_NODE, idString);
                
            } else if (UriUtils.isAbsoluteUri(idString)) {
                return Rdf.createObject(RdfObject.Type.IRI, idString);
            }
            return null;
        }
        
        // 3.
        if (ListObject.isListObject(item)) {
            return ListToRdf
                        .with(item.get(Keywords.LIST).asJsonArray(), triples, nodeMap)
                        .rdfDirection(rdfDirection)
                        .build();
        }

        // 4.
        if (!ValueObject.isValueObject(item)) {
            return null;
        }
        
        JsonValue value = item.get(Keywords.VALUE);
        
        // 5.
        String datatype = item.containsKey(Keywords.TYPE) && JsonUtils.isString(item.get(Keywords.TYPE))
                            ? item.getString(Keywords.TYPE)
                            : null;
        
        String valueString = null;
        
        // 6.
        if (datatype != null && !Keywords.JSON.equals(datatype) && !UriUtils.isAbsoluteUri(datatype)) {
            return null;
        }
        
        // 7.
        if (item.containsKey(Keywords.LANGUAGE) && (JsonUtils.isNotString(item.get(Keywords.LANGUAGE))
                || !LanguageTag.isWellFormed(item.getString(Keywords.LANGUAGE)))
                ) {
            
            return null;
        }
        
        // 8.
        if (Keywords.JSON.equals(datatype)) {
            valueString = JsonCanonicalizer.canonicalize(value);
            datatype = RdfContants.JSON;
            
        // 9.
        } else if (JsonUtils.isTrue(value)) {
            
            valueString = "true";
            
            if (datatype == null) {
                datatype = XsdContants.BOOLEAN;
            }
            
        } else if (JsonUtils.isFalse(value)) {

            valueString = "false";
            
            if (datatype == null) {
                datatype = XsdContants.BOOLEAN;
            }

            
        // 10. - 11.
        } else if (JsonUtils.isNumber(value)) {
            
            JsonNumber number = ((JsonNumber)value);
                  
            
            // 11.
            if ((!number.isIntegral()  && number.doubleValue() % -1 != 0)
                    || XsdContants.DOUBLE.equals(datatype)
                    || number.bigDecimalValue().compareTo(BigDecimal.ONE.movePointRight(21)) >= 0
                    ) {

                valueString = toXsdDouble(number.bigDecimalValue());
                
                if (datatype == null) {
                    datatype = XsdContants.DOUBLE;
                }
                
            // 10.
            } else {

                valueString = number.bigIntegerValue().toString();
                
                if (datatype == null) {
                    datatype = XsdContants.INTEGER;
                }

            }
                    
        // 12.
        } else if (datatype == null) {
            
            datatype = item.containsKey(Keywords.LANGUAGE)
                                ? RdfContants.LANG_STRING
                                : XsdContants.STRING
                                ;
        }
        
        if (valueString == null) {
            
            if (JsonUtils.isNotString(value)) {
                return null;
            }
            
            valueString = ((JsonString)value).getString();
        }
        
        RdfLiteral rdfLiteral = null;
        
        // 13.
        if (item.containsKey(Keywords.DIRECTION) && rdfDirection != null) {

            // 13.1.
            final String language = item.containsKey(Keywords.LANGUAGE)   
                                ? item.getString(Keywords.LANGUAGE).toLowerCase()
                                : "";
            // 13.2.
            if (RdfDirection.I18N_DATATYPE == rdfDirection) {
                datatype = "https://www.w3.org/ns/i18n#"
                                .concat(language)
                                .concat("_")
                                .concat(item.getString(Keywords.DIRECTION));
                
                rdfLiteral = Rdf.createTypedString(valueString, datatype);
                
            // 13.3.
            } else if (RdfDirection.COMPOUND_LITERAL == rdfDirection) {

                final String blankNodeId = nodeMap.createIdentifier();
                
                // 13.3.1.                
                final RdfSubject subject = Rdf.createSubject(RdfSubject.Type.BLANK_NODE, blankNodeId);
                
                // 13.3.2.
                triples.add(Rdf.createTriple(
                                    subject, 
                                    Rdf.createPredicate(RdfPredicate.Type.IRI, RdfContants.VALUE), 
                                    Rdf.createObject(RdfObject.Type.LITERAL, valueString))
                                    );
                
                // 13.3.3.
                if (item.containsKey(Keywords.LANGUAGE) && JsonUtils.isString(item.get(Keywords.LANGUAGE))) {
                    triples.add(Rdf.createTriple(
                                    subject, 
                                    Rdf.createPredicate(RdfPredicate.Type.IRI, RdfContants.LANGUAGE), 
                                    Rdf.createObject(RdfObject.Type.LITERAL, item.getString(Keywords.LANGUAGE).toLowerCase()))
                                    );
                }
                
                // 13.3.4.
                triples.add(Rdf.createTriple(
                                    subject, 
                                    Rdf.createPredicate(RdfPredicate.Type.IRI, RdfContants.DIRECTION), 
                                    Rdf.createObject(RdfObject.Type.LITERAL, item.getString(Keywords.DIRECTION)))
                                    );
                
                return Rdf.createObject(RdfObject.Type.BLANK_NODE, blankNodeId);
            }
            
        // 14.
        } else {
            if (item.containsKey(Keywords.LANGUAGE) && JsonUtils.isString(item.get(Keywords.LANGUAGE))) {  
            
                rdfLiteral = Rdf.createLangString(valueString, item.getString(Keywords.LANGUAGE));
                                
            } else {
                rdfLiteral = Rdf.createTypedString(valueString, datatype);
            }
        }
        
        // 15.
        return rdfLiteral != null ? Rdf.createObject(rdfLiteral) : null;
    }
    
    private static final String toXsdDouble(BigDecimal bigDecimal) {
        
        if (bigDecimal.compareTo(BigDecimal.ZERO) == 0) {
            return "0.0E0";
        }
        
        return new DecimalFormat("0.0##############E0").format(bigDecimal);        
    }
    
}
