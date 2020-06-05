package com.apicatalog.jsonld.deseralization;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.text.DecimalFormat;
import java.util.List;

import javax.json.JsonNumber;
import javax.json.JsonObject;
import javax.json.JsonString;
import javax.json.JsonValue;

import com.apicatalog.iri.IRI;
import com.apicatalog.jsonld.api.JsonLdError;
import com.apicatalog.jsonld.flattening.NodeMap;
import com.apicatalog.jsonld.json.JsonCanonicalizer;
import com.apicatalog.jsonld.json.JsonUtils;
import com.apicatalog.jsonld.lang.BlankNode;
import com.apicatalog.jsonld.lang.Keywords;
import com.apicatalog.jsonld.lang.LanguageTag;
import com.apicatalog.jsonld.lang.ListObject;
import com.apicatalog.jsonld.lang.NodeObject;
import com.apicatalog.jsonld.lang.ValueObject;
import com.apicatalog.rdf.RdfLiteral;
import com.apicatalog.rdf.RdfObject;
import com.apicatalog.rdf.RdfTriple;
import com.apicatalog.rdf.api.Rdf;

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
    private String rdfDirection;
    
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
    
    public ObjectToRdf rdfDirection(String rdfDirection) {
        this.rdfDirection = rdfDirection;
        return this;
    }
    
    public RdfObject build() throws JsonLdError {

        // 1. - 2.
        if (NodeObject.isNodeObject(item)) {
            
            JsonValue id = item.get(Keywords.ID);
            
            if (JsonUtils.isNotString(id)) {
                return null;
            }
            
            String idString = ((JsonString)id).getString();
     
            if (BlankNode.isWellFormed(idString)) {
                return Rdf.createObject(BlankNode.create(idString));
                
            } else if (IRI.isWellFormed(idString)) {
                return Rdf.createObject(IRI.create(idString));
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
        if (datatype != null && !Keywords.JSON.equals(datatype) && !IRI.isWellFormed(datatype)) {
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
            datatype = "http://www.w3.org/1999/02/22-rdf-syntax-ns#JSON";
            
        // 9.
        } else if (JsonUtils.isTrue(value)) {
            
            valueString = "true";
            
            if (datatype == null) {
                datatype = "http://www.w3.org/2001/XMLSchema#boolean";
            }
            
        } else if (JsonUtils.isFalse(value)) {

            valueString = "false";
            
            if (datatype == null) {
                datatype = "http://www.w3.org/2001/XMLSchema#boolean";
            }

            
        // 10. - 11.
        } else if (JsonUtils.isNumber(value)) {
            
            JsonNumber number = ((JsonNumber)value);
                  
            
            // 11.
            if ((!number.isIntegral()  && number.doubleValue() % -1 != 0)
                    || "http://www.w3.org/2001/XMLSchema#double".equals(datatype)
                    || number.bigIntegerValue().compareTo(BigInteger.TEN.pow(21)) >= 0 
                    ) {

                valueString = toXsdDouble(number.bigDecimalValue());
                
                if (datatype == null) {
                    datatype = "http://www.w3.org/2001/XMLSchema#double";
                }
                
            // 10.
            } else {

                valueString = number.bigIntegerValue().toString();
                
                if (datatype == null) {
                    datatype = "http://www.w3.org/2001/XMLSchema#integer";
                }

            }
                    
        // 12.
        } else if (datatype == null) {
            
            datatype = item.containsKey(Keywords.LANGUAGE)
                                ? "http://www.w3.org/1999/02/22-rdf-syntax-ns#langString" //TODO constants
                                : "http://www.w3.org/2001/XMLSchema#string"
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
            String language = item.containsKey(Keywords.LANGUAGE)   
                                ? item.getString(Keywords.LANGUAGE).toLowerCase()
                                : "";
            // 13.2.
            if ("i18n-datatype".equals(rdfDirection)) {
                datatype = "https://www.w3.org/ns/i18n#"
                                .concat(language)
                                .concat("_")
                                .concat(item.getString(Keywords.DIRECTION));
                
                rdfLiteral = Rdf.createLitteral(valueString, IRI.create(datatype));
                
            // 13.3.
            } else if ("compound-literal".equals(rdfDirection)) {
                
            }
            
            // 13.3.
            //TODO

            
        // 14.
        } else {
            if (item.containsKey(Keywords.LANGUAGE) && JsonUtils.isString(item.get(Keywords.LANGUAGE))) {  
            
                rdfLiteral = Rdf.createLitteral(valueString, item.getString(Keywords.LANGUAGE));
                                
            } else {
                rdfLiteral = Rdf.createLitteral(valueString, IRI.create(datatype));
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
